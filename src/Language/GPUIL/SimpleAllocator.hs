module Language.GPUIL.SimpleAllocator
       (Bytes,
        memoryMap
        ) 
where 

import Data.Maybe
import Control.Monad.State
import Language.GPUIL.Syntax

type Bytes   = IExp

sbase :: IExp
sbase = (VarE ("sbase", CPtr [Local] CWord8))

memoryMap :: Statements a -> (Statements a, Maybe Bytes)
memoryMap stmts = runState (memMap stmts) Nothing

memMap :: Statements a -> State (Maybe Bytes) (Statements a)
memMap stmts = mapM go stmts
  where
   go :: (Statement a, a) -> State (Maybe Bytes) (Statement a, a)
   go (Allocate name@(_,ty@(CPtr _ bty)) size',i) =
     do offset <- allocate (BinOpE MulI size' (IntE (sizeOf bty)))
        return $ (Decl name (CastE ty (BinOpE AddPtr sbase offset)), i)
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
