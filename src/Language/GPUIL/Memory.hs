{- Ported from Obsidian by Martin Dybdal, 2015

   Original code by Joel Svensson 2012, 2013 
-} 
module Language
       (MemMap,
        Memory,
        size, 
        Address,
        Bytes,
        memoryMap,
        SharedMemConfig(..) --,
        ) 
where 

import Data.Word (Word32)
import Data.Maybe (catMaybes, mapMaybe)
import Data.List (sort, isPrefixOf)
import qualified Data.Map as Map
import qualified Data.Set as Set

import Control.Monad.State

import Language.GPUIL.Syntax
import Language.GPUIL.Liveness

type MemMap = Map.Map VarName (AlignedAddress, CType)

type AlignedAddress = (Address,Address) 

type Address = Word32
type Bytes   = Word32 

data Memory = Memory {freeList  :: [(Address,Bytes)] ,
                      allocated :: [(Address,Bytes)] , 
                      size      :: Bytes} -- how much used
            deriving Show 

data SharedMemConfig =
  SharedMemConfig { smSize :: Bytes  -- amount of shared mem
                  , smBanks :: Word32   -- Number of banks 16/32
                  }

data AllocatorState =
  AState { memory :: Memory
         , memmap :: MemMap
         , smconfig :: SharedMemConfig
         }

initState :: SharedMemConfig -> AllocatorState
initState conf = AState { memory = createSharedMem conf
                        , memmap = Map.empty
                        , smconfig = conf
                        }

type M x = State AllocatorState x

addToMemMap :: VarName -> AlignedAddress -> M ()
addToMemMap name@(_,ty) address =
  modify (\s -> s {memmap = Map.insert name (address,ty) (memmap s) })

updateMax :: Memory -> M ()
updateMax mem = do
  let m = maximum [a+b|(a,b) <- allocated mem]
      m' = max m (size mem)
  modify (\s -> s { memory = mem {size = m'}})

createSharedMem :: SharedMemConfig -> Memory
createSharedMem conf = Memory [(0,smSize conf)] [] 0 

-- bank allign an address, returning a new aligned address
-- and the number of EXTRA bytes that needs to be present
-- from the old provided address in order to store aligned data
-- at the new location.
bankAlign :: Word32 -> Address -> (AlignedAddress, Bytes)
bankAlign banks address =
  let -- if address % bank_alignment == 0
     --   the address is aligned
     bank_alignment = banks * 4 -- number of banks * 4 bytes
     how_far_off = address `mod`  bank_alignment
     bump =  bank_alignment - how_far_off
  in if how_far_off == 0
        then ((address,address),0)
        else ((address,address + bump), bump)

---------------------------------------------------------------------------
-- Allocate memory
--------------------------------------------------------------------------- 
allocate :: VarName -> Bytes -> M AlignedAddress
allocate name b = do
  mm <- gets memmap
  m <- gets memory
  banks <- gets (smBanks . smconfig)

  case Map.lookup name mm of
    (Just (a, _)) -> error $ "mmIm: " ++ (fst name) ++ " is already mapped to " ++ show a
    Nothing -> return ()

  -- Original address canditades 
  let address_candidates = filter (\(_,y) -> y >= b) $ freeList m
  -- Candidates after aligning
  new_candidates <- liftM catMaybes $ mapM (tryCandidate banks b) address_candidates
  
  -- Does any memory location exist that
  -- allows for the allocation of this array 
  case new_candidates of
    [] -> error $ "allocate: out of shared memory:" ++
                  "\n  Allocating: " ++ show b ++ " bytes" ++
                  "\n  Free List: " ++ show (freeList m) ++ 
                  "\n  Potentials: " ++ show address_candidates ++ 
                  "\n  Fit with align: " ++ show new_candidates 

    ((aligned_address,free_space,alloc_size):_) -> do
      -- update free list
      -- Clear the allocated address from the free list 
      let fl  = filter (\(addr,_) -> (fst aligned_address /= addr)) (freeList m)
          -- if the chosen space is larger than what we need
          -- add the unused chunk to the free list 
          fl' = if alloc_size < free_space
                then (fst aligned_address + alloc_size,
                      free_space - alloc_size):fl
                else fl
      -- Update memory and return a result address
      updateMax $ m { freeList = fl'
                    , allocated = (fst aligned_address,alloc_size):allocated m}
      addToMemMap name aligned_address
      return aligned_address
  where
    -- try to align an address
    -- results in an AlignedAdress
    tryCandidate banks bytes (addr, free_space) =
      let (aligned_addr, extra_bytes) = bankAlign banks addr
          alloc_size = bytes + extra_bytes
      in if free_space >= alloc_size
           then return (Just (aligned_addr, free_space, alloc_size))
           else return Nothing

---------------------------------------------------------------------------
-- Free memory 
---------------------------------------------------------------------------
free :: AlignedAddress -> M ()
free (alloc_addr,_) =
  do m <- gets memory
     let bytes = lookup (alloc_addr) (allocated m)
         al    = filter (\(addr,_) -> alloc_addr /= addr) (allocated m)
         mem   =
           case bytes of 
             Nothing -> m
             Just b -> m {freeList = compress ((alloc_addr,b):(freeList m)),
                          allocated = al}
     modify (\s -> s { memory = mem })

freeAll :: [AlignedAddress] -> M ()
freeAll = mapM_ free

compress :: [(Address,Bytes)] -> [(Address,Bytes)]
compress = merge . sort
  where
    merge :: [(Address,Bytes)] -> [(Address,Bytes)]
    merge [] = [] 
    merge [x] = [x]
    merge ((x,b):(y,b2):xs) = if (x+b == y)
                              then merge ((x,b+b2):xs) 
                              else (x,b):merge((y,b2):xs)
---------------------------------------------------------------------------
-- Memory map the new IM
---------------------------------------------------------------------------
memoryMap :: SharedMemConfig -> Statements a ty -> (Memory, MemMap)
memoryMap conf stmts =
  let stmtsWithLiveness = liveness stmts
      endState = execState (memMap Set.empty stmtsWithLiveness) (initState conf)
  in (memory endState,
      memmap endState)

isfreeable :: VarName -> Bool
isfreeable (name,_) = not (("input" `isPrefixOf` name) || 
                           ("output" `isPrefixOf` name))

getFreeableSet :: (Statement LiveInfo ty, LiveInfo) -> Statements LiveInfo ty -> LiveInfo
getFreeableSet (_,_) [] = Set.empty
getFreeableSet (_,l) ((_,l1):_) = l Set.\\ l1

memMap :: LiveInfo -> Statements LiveInfo ty -> M ()
memMap _ [] = return ()
memMap nonfreeable (x:xs) =
  do process x
     mm <- gets memmap
     let freeable' = Set.filter isfreeable (getFreeableSet x xs)
         freeable  = freeable' Set.\\ nonfreeable
         freeableAddrs = mapMaybe (`Map.lookup` mm) (Set.toList freeable)
     freeAll (map fst freeableAddrs)
     memMap nonfreeable xs
  where 
    process :: (Statement LiveInfo ty, LiveInfo) -> M ()
    process (Allocate name (Word32E size') t,_) = allocate name size' >> return () -- TODO
          -- TODO: other cases
    process (For _ _ im, alive)       = memMap (nonfreeable `Set.union` alive) im
    process (SeqWhile _ im,_)         = memMap nonfreeable im
    process (ForAll _ _ _ im,_)         = memMap nonfreeable im
    process (DistrPar Warp _ _ im,_)    = memMap nonfreeable im 
    process (DistrPar Block _ _ im,_)   = memMap nonfreeable im 
    process (_,_) = return ()
