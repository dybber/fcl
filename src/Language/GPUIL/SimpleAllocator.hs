module Language.GPUIL.SimpleAllocator
       (Bytes,
        memoryMap
        ) 
where 

import Data.Maybe
import Control.Monad.State



import Language.GPUIL.Syntax

type Bytes   = IExp NoType

memoryMap :: Statements a NoType -> (Statements a NoType, Maybe Bytes)
memoryMap stmts = runState (memMap stmts) Nothing

memMap :: Statements a NoType -> State (Maybe Bytes) (Statements a NoType)
memMap stmts = mapM go stmts
  where
   go :: (Statement a NoType, a) -> State (Maybe Bytes) (Statement a NoType, a)
   go (Allocate name size' ty,i) = do offset <- allocate (BinOpE MulI size' (IntE (sizeOf ty)))
                                      return $ (Decl name (Just (CastE ty (BinOpE AddI (VarE ("sbase", CWord8) NoType) offset))), i)
   go (For n e body, i)           = do body' <- memMap body
                                       return (For n e body', i)
   go (SeqWhile e body, i)        = do body' <- memMap body
                                       return (SeqWhile e body', i)
   go (ForAll lvl n e body, i)    = do body' <- memMap body
                                       return (ForAll lvl n e body', i)
   go (DistrPar lvl n e body, i)  = do body' <- memMap body
                                       return (DistrPar lvl n e body', i)
   go (a,i) = return (a,i)

allocate :: Bytes -> State (Maybe Bytes) Bytes
allocate bytes = do
  used <- get
  let offset = fromMaybe (IntE 0) used
  put (Just (BinOpE AddI offset bytes))
  return offset

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

