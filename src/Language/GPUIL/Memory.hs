{- Parts of the code is from Obsidian by
      Joel Svensson 2012, 2013 
 -} 
module Language
       (MemMap,
        Memory,
        size, 
        Address,
        Bytes,
        memMapIM,
        SharedMemConfig(..) --,
        ) 
where 

import Data.Word (Word32)
import Data.Maybe (catMaybes)
import Data.List (sort, isPrefixOf)
import qualified Data.Map as Map
import qualified Data.Set as Set

import Control.Monad.State

import Language.GPUIL.Syntax
import Language.GPUIL.Liveness

type MemMap = Map.Map VarName (AlignedAddress,IType)

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
                  , smBankAlign :: Bool 
                  }

data AllocatorState =
  AState { memory :: Memory
         , memmap :: MemMap
         , smconfig :: SharedMemConfig
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
  case Map.lookup name mm of
    (Just (a, _)) -> error $ "mmIm: " ++ (fst name) ++ " is already mapped to " ++ show a
    Nothing -> return ()

  m <- gets memory
  -- Original address canditades 
  let address_candidates = filter (\(_,y) -> y >= b) $ freeList m

  conf <- gets smconfig
  let banks = smBanks conf
  -- Candidates after aligning
  new_candidates <- mapM (tryCandidate banks b) address_candidates

  if smBankAlign conf
    then 
       -- Does any memory location exist that
       -- allows for the allocation of this array 
       case catMaybes new_candidates of
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
              
    else
      case map (pretend_align b) address_candidates of
        [] -> error "out of shared memory"
         
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
           updateMax (m { freeList = fl'
                        , allocated = (fst aligned_address,alloc_size):allocated m})
           return aligned_address
           

  where
    -- Create silly AlignedAddress (that are not really aligned at all)
    pretend_align bytes (addr, free_space) = ((addr,addr),free_space,bytes) 

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


memMapIM :: SharedMemConfig -> Statements a ty -> MemMap -> (Memory, MemMap)
memMapIM = undefined

-- ---------------------------------------------------------------------------
-- -- Memory map the new IM
-- ---------------------------------------------------------------------------

-- memMapIM :: SharedMemConfig -> IML -> MemMap -> (Memory, MemMap)
-- memMapIM conf im memmap = mmIM conf im memory memmap
--   where
--     memory = createSharedMem conf
  
mmIM :: Stmts ty -> M ()
mmIM [] = return ()
mmIM (x:xs) =
 do process x
    mm <- gets memmap
    let freeable = getFreeableSet x xs
        dontMap (name,_) = not (("input" `isPrefixOf` name) || 
                                ("output" `isPrefixOf` name))
    case mapM (flip Map.lookup mm) (filter dontMap (Set.toList freeable)) of
      Just as -> freeAll (map fst as)
      Nothing -> return ()
    mmIM xs
  where 
    process :: (Stmt ty, Live) -> M ()
--    process (Allocate name size t,_) = allocate name size
    process (For _ _ im,alive) = mmIMLoop alive im
    process (SeqWhile b im,_) = mmIM im
    process (ForAll _ _ _ im,_) = mmIM im
    process (DistrPar Warp _ _ im,_) = mmIM im
    process (DistrPar Block _ _ im,_) = mmIM im
    process (_,_) = return ()

-- Friday (2013 Mars 29, discovered bug)
-- 2014-Nov-25: was the "l" the bug ? (some details help)
getFreeableSet :: (Stmt ty, Live) -> Stmts ty -> Live 
getFreeableSet (_,l) [] = Set.empty -- not l ! 
getFreeableSet (_,l) ((_,l1):_) = l Set.\\ l1


mmIMLoop = undefined
-- ---------------------------------------------------------------------------
-- -- 
-- ---------------------------------------------------------------------------
-- mmIMLoop conf nonfreeable im memory memmap = r im (memory,memmap)
--   where 
--     r [] m = m
--     r (x:xs) (m,mm) =
--       let
--           (m',mm') = process conf x m mm
           
--           freeable' = getFreeableSet x xs
--           freeable  = freeable' Set.\\ nonfreeable
--           freeableAddrs = mapM (flip Map.lookup mm') (filter dontMap (Set.toList freeable))
--           dontMap name = not ((List.isPrefixOf "input" name) || 
--                               (List.isPrefixOf "output" name))
--           mNew =
--             case freeableAddrs of
--               (Just as) -> freeAll m' (map fst as)
--               Nothing   -> m'
--       in --trace ("freeable': " ++ show freeable' ++ "\n" ++
--          --       "freeable: " ++ show freeable   ++ "\n" ++ 
--          --       "nonfreeable: " ++ show nonfreeable) $
--          r xs (mNew,mm')
    
--     process :: SharedMemConfig -> (Statement Liveness,Liveness) -> Memory -> MemMap -> (Memory,MemMap)
--     process conf (SAllocate name size t,_) m mm = (m',mm') 
--       where (m',addr) = allocate conf m size
--             mm' =
--               case Map.lookup name mm of
--                 Nothing -> Map.insert name (addr,t) mm
--                 (Just (a, t)) -> error $ "mmIm: " ++ name ++ " is already mapped to " ++ show a

--     -- Boilerplate
--     process conf (SSeqFor _ n im,alive) m mm = mmIMLoop conf (nonfreeable `Set.union` alive) im m mm
--     process conf (SSeqWhile b im,_) m mm = mmIMLoop conf nonfreeable im m mm 
--     process conf (SForAll _ n im,_) m mm = mmIMLoop conf nonfreeable im m mm
--     -- 2014-Nov-25:
--     --   This one used mmIM' which was identical to mmIM.
--     --   This must have been a leftover from when I thought
--     --   warp memory needed some special attention here. 
--     process conf (SDistrPar Warp n im,_) m mm = mmIMLoop conf nonfreeable im m mm 
--     process conf (SDistrPar Block n im,_) m mm = mmIMLoop conf nonfreeable im m mm 
--     process conf (_,_) m mm = (m,mm) 
