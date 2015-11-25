{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Language.GPUIL.ConvertLoops (convert) where

import Control.Monad.State

import Language.GPUIL.Syntax

localID, warpSize :: IExp NoType
localID = LocalID
warpSize = WarpSize

-- -- It would be nice if my Smart-constructors would be of any use here.
-- -- But it I must keep the same loop variable, the smart constructors
-- -- would create a new one.
-- --
-- -- Or maybe I could do an assignment inside the loop..? Later!
-- compileDistrPar :: Stmt ty -> [Stmt ty]
-- compileDistrPar (DistrPar Block i ub body) =
--   let numWorkgroups = (UnaryOpE NumGroups (IntE 0))
--       gid = (UnaryOpE GroupID (IntE 0))
--       blocksQ = BinOpE DivI ub numWorkgroups
--       blocksR = BinOpE ModI ub numWorkgroups
--       bodyQ = body ++ [SyncLocalMem]
--   in 
--      [For i blocksQ bodyQ,
--       If (BinOpE LtI gid blocksR) body [],
--       SyncLocalMem
--      ]
-- compileDistrPar (DistrPar Warp i (IntE n) body) =
--   let numWarps = workgroupSize `div` warpSize
--       tid = (UnaryOpE LocalID (IntE 0))
--       warpsQ = IntE (n `div` numWarps)
--       warpsR = IntE (n `mod` numWarps)
--       bodyQ = body
--   in
--      (For i warpsQ bodyQ)
--        : if n `mod` numWarps == 0
--            then []
--            else [If (BinOpE LtI (BinOpE DivI tid (IntE warpSize)) warpsR) body [],
--                  SyncLocalMem]
-- compileDistrPar _ = error "DistrPar currently only implemented at warp and block level."

type Conv x = State Int x

newVar :: Conv String
newVar = do
  count <- get
  put (count+1)
  return ("id" ++ show count)

convert :: Int -> Statements () NoType -> (Statements () NoType, Int)
convert varCount stmts = runState (convertLoops stmts) varCount

convertLoops :: Statements () NoType -> Conv (Statements () NoType)
convertLoops stmts = liftM concat $ mapM (convertLoop . fst) stmts

convertLoop :: Statement () NoType -> Conv (Statements () NoType)
convertLoop stmt@(ForAll _ _ _ _) = compileForAll stmt
convertLoop stmt@(DistrPar _ _ _ _) = compileDistrPar stmt
convertLoop (For v ty ss) = do
  ss' <- convertLoops ss
  return [(For v ty ss', ())]
convertLoop (SeqWhile cond ss) = do
  ss' <- convertLoops ss
  return [(SeqWhile cond ss', ())]
convertLoop (If e ss0 ss1) =
  do ss0' <- convertLoops ss0
     ss1' <- convertLoops ss1
     return [(If e ss0' ss1', ())]
convertLoop stmt = return [(stmt, ())]


compileForAll :: Statement () NoType -> Conv (Statements () NoType)
--TODO: implement specific cases for when "ub" is statically known -- see Obsidian implementation
compileForAll (ForAll Warp name ub body) =
  do body' <- convertLoops body
     let q = (BinOpE DivI ub warpSize)
         r = (BinOpE ModI ub warpSize)
         x = (BinOpE ModI localID warpSize)
         -- TODO: Transform to avoid the warpIx variable, see below for block level
         resetWarpIx = Assign ("warpIx", CInt32) x
      
         codeQ = For name q ((Assign ("warpIx", CInt32)
                                           (BinOpE AddI
                                               (BinOpE MulI (VarE name NoType) warpSize)
                                               x), ()) : body')
         codeR = If (BinOpE LtI x r)
                   ((Assign ("warpIx", CInt32)
                            (BinOpE AddI
                                (BinOpE MulI q warpSize)
                                x), ())
                    : body')
                   []
     return [(codeQ, ()),
             (resetWarpIx, ()),
             (codeR, ()),
             (resetWarpIx, ())]
compileForAll (ForAll Block name ub body) =
  do body' <- convertLoops body
     loopVar <- newVar
     let nt = LocalSize
         q = (BinOpE DivI ub nt)
         r = (BinOpE ModI ub nt)
         codeQ = For (loopVar,CInt32) q ((declLoopVar, ()) : body')
         declLoopVar = Decl name (Just (BinOpE AddI
                                                (BinOpE MulI (VarE (loopVar, CInt32) NoType) nt)
                                                localID))

         -- TODO: Don't do this if we know statically that num threads divides loop-bound evenly
         codeR = If (BinOpE LtI localID r)
                   ((Decl name
                         (Just ((BinOpE AddI
                                  (BinOpE MulI q nt)
                                  localID))), ())
                    : body')
                   []
     return [(Comment "ForAll", ()),
             (codeQ, ()),
             (codeR, ())
            ]
compileForAll (ForAll Thread _ _ _) = error "For all on thread-level not currently possible"
compileForAll (ForAll Grid _ _ _) = error "For all on grid-level not currently possible"
compileForAll _ = error "compileForAll should only be called with ForAll as argument"

compileDistrPar :: Statement () NoType -> Conv (Statements () NoType)
compileDistrPar (DistrPar Block name ub body) =
  do body' <- convertLoops body
     loopVar <- newVar
     let blocksQ = BinOpE DivI ub NumGroups
         blocksR = BinOpE ModI ub NumGroups
         codeQ = For (loopVar, CInt32) blocksQ bodyQ
         bodyQ = (Decl name (Just (BinOpE AddI (BinOpE MulI GroupID blocksQ)
                                              (VarE (loopVar, CInt32) NoType))),())
                 : body' ++ [(SyncLocalMem, ())]
         codeR = If (BinOpE LtI GroupID blocksR)
                    ((Decl name (Just (BinOpE AddI (BinOpE MulI NumGroups blocksQ)
                                               GroupID)),()) : body' ++ [(SyncLocalMem, ())])
                    []

     return [(codeQ, ()),
             (codeR,())]
compileDistrPar (DistrPar Warp _ _ _) = error "compileDistrPar: Warp level DistrPar not yet implemented"
compileDistrPar (DistrPar Thread _ _ _) = error "compileDistrPar: Thread level DistrPar not yet implemented"
compileDistrPar (DistrPar Grid _ _ _) = error "compileDistrPar: Grid level DistrPar not yet implemented"
compileDistrPar _ = error "compileDistrPar should only be called with DistrPar as argument"
