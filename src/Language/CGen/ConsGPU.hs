module Language.CGen.ConsGPU
 ( attrLocal, attrGlobal,
   syncGlobal, syncLocal,
   globalID, localID, localSize, workgroupID, numWorkgroups, warpSize)
where

import Language.CGen.Syntax as AST
import Language.CGen.Monad

syncGlobal :: IL ()
syncGlobal =  addStmt (SyncGlobalMem ())

syncLocal :: IL ()
syncLocal =  addStmt (SyncLocalMem ())

attrLocal :: Attribute
attrLocal = Local

attrGlobal :: Attribute
attrGlobal = Global

globalID :: CExp
globalID = GlobalID

localID :: CExp
localID = LocalID

workgroupID :: CExp
workgroupID = GroupID

localSize :: CExp
localSize = LocalSize

numWorkgroups :: CExp
numWorkgroups =  NumGroups

warpSize :: CExp
warpSize = WarpSize
