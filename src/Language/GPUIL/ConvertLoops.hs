module Language.GPUIL.ConvertLoops where

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

convertLoops :: Statements () NoType -> Statements () NoType
convertLoops = concatMap (convertLoop . fst)

convertLoop :: Statement () NoType -> Statements () NoType
convertLoop stmt =
  case stmt of
    (ForAll _ _ _ _) -> compileForAll (stmt)
    (For v ty ss) -> [(For v ty (convertLoops ss), ())]
    (SeqWhile cond ss) -> [(SeqWhile cond (convertLoops ss), ())]
    (If e ss0 ss1) -> [(If e (convertLoops ss0) (convertLoops ss1), ())]
    _ -> [(stmt, ())]


compileForAll :: Statement () NoType -> Statements () NoType
-- TODO: implement specific cases for when "ub" is statically known -- see Obsidian implementation
compileForAll (ForAll Warp name ub body) =
  let q = (BinOpE DivI ub warpSize)
      r = (BinOpE ModI ub warpSize)
      x = (BinOpE ModI localID warpSize)
      resetWarpIx = Assign ("warpIx", Int32T) x
      body' = convertLoops body
      codeQ = For name q ((Assign ("warpIx", Int32T)
                                           (BinOpE AddI
                                               (BinOpE MulI (VarE name NoType) warpSize)
                                               x), ()) : body')
      codeR = If (BinOpE LtI x r)
                   ((Assign ("warpIx", Int32T)
                            (BinOpE AddI
                                (BinOpE MulI q warpSize)
                                x), ())
                    : body')
                   []
  in [(codeQ, ()),
      (resetWarpIx, ()),
      (codeR, ()),
      (resetWarpIx, ())]
compileForAll (ForAll Block name ub body) =
  let q = (BinOpE DivI ub warpSize)
      resetTid = Assign ("tid", Int32T) localID
      body' = convertLoops body
      codeQ = For name q ((Assign ("tid", Int32T)
                                           (BinOpE AddI
                                               (BinOpE MulI (VarE name NoType) ub)
                                               localID), ()) : body')
      codeR = If (BinOpE LtI localID ub)
                   ((Assign ("tid", Int32T)
                            (BinOpE AddI
                                (BinOpE MulI q ub)
                                localID), ())
                    : body')
                   []
  in [(codeQ, ()),
      (resetTid, ()),
      (codeR, ()),
      (resetTid, ())]
compileForAll (ForAll Thread _ _ _) = error "For all on thread-level not currently possible"
compileForAll (ForAll Grid _ _ _) = error "For all on grid-level not currently possible"
compileForAll _ = error "compileForAll should only be called with ForAll as argument"

