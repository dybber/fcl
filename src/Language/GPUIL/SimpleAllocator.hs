{- Ported from Obsidian by Martin Dybdal, 2015

   Original code by Joel Svensson 2012, 2013 
-}
module Language.GPUIL.SimpleAllocator
       (Address,
        Bytes,
        MemMap,
        memoryMap
        ) 
where 

import Data.Maybe
import qualified Data.Map as Map
import Control.Monad.State

import Language.GPUIL.Syntax

type Address = IExp NoType
type Bytes   = IExp NoType

type MemMap = Map.Map VarName (Address, CType)


data AllocatorState =
  AState { memmap :: MemMap
         , used :: Maybe Bytes
         }

initState :: AllocatorState
initState = AState { memmap = Map.empty
                   , used = Nothing
                   }

memoryMap :: Statements a NoType -> (MemMap, Maybe Bytes)
memoryMap stmts =
  let endState = execState (memMap stmts) initState
  in (memmap endState,
      used endState)

memMap :: Statements a NoType -> State AllocatorState ()
memMap stmts = mapM_ (go . fst) stmts
  where
   go :: Statement a NoType -> State AllocatorState ()
   go (Allocate name size' ty) = allocate name (BinOpE MulI size' (IntE (sizeOf ty)))
   go (For _ _ im)             = memMap im
   go (SeqWhile _ im)          = memMap im
   go (ForAll _ _ _ im)        = memMap im
   go (DistrPar Warp _ _ im)   = memMap im
   go (DistrPar Block _ _ im)  = memMap im
   go _ = return ()

allocate :: VarName -> Bytes -> State AllocatorState ()
allocate (name, ty) bytes = do
  s <- get
  let offset = fromMaybe (IntE 0) (used s)
  put (s { memmap = Map.insert (name, ty) (offset, ty) (memmap s)
         , used = Just (BinOpE AddI offset bytes)
         })

-- memoryMap :: Statements a NoType -> (Statements a NoType, Bytes)
-- memoryMap stmts = runState (allocate stmts) (IntE 0)

-- allocate :: Statements a NoType -> State Bytes (Statements a NoType)
-- allocate stmts = mapM go stmts
--   where
--    go :: (Statement a NoType,a) -> State Bytes (Statement a NoType,a)
--    go (Allocate name size ty,i) =
--      do used <- get
--         let newoffset = BinOpE AddI used size
--         put newoffset
--         return $ (Decl name (Just (CastE ty (BinOpE AddI (VarE ("sbase", CWord8) NoType) used))),
--                   i)
--    go (For n e body,i)             = do body' <- allocate body
--                                         return (For n e body',i)
--    go (SeqWhile e body,i)          = do body' <- allocate body
--                                         return (SeqWhile e body',i)
--    go (ForAll lvl n e body,i)      = do body' <- allocate body
--                                         return (ForAll lvl n e body',i)
--    go (DistrPar Warp n e body,i)   = do body' <- allocate body
--                                         return (DistrPar Warp n e body',i)
--    go (DistrPar Block n e body,i)  = do body' <- allocate body
--                                         return (DistrPar Block n e body',i)
--    go stmt = return stmt

