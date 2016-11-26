module Language.FCL.ILKernel where

import Language.FCL.Syntax
import CGen

warpSize :: CExp
warpSize = constant (32 :: Int)

blockSize :: CExp
blockSize = constant (128 :: Int)

data CompileState =
  CompileState { allocPtrOffset :: CExp
               , sharedMemPointer :: VarName
               }

initializeState :: CompileState
initializeState =
  CompileState { allocPtrOffset = constant (0 :: Int)
               , sharedMemPointer = error "Shared memory is not initialized!" -- TODO, not nice.
               }

type ILKernel a = CGen CompileState a

allocateKernel :: CType -> CExp -> ILKernel VarName
allocateKernel cty n =
  do offset <- getsState allocPtrOffset
     sbase <- getsState sharedMemPointer
     let aty = pointer_t [attrLocal] cty
     v <- letVar "arr" aty (cast aty (var sbase `addPtr` offset))
     let bytes = n `muli` (sizeOf cty)
     modifyState (\s -> s { allocPtrOffset = offset `addi` bytes })
     return v
                                          
distrParKernel :: Level -> CExp -> (CExp -> ILKernel ()) -> ILKernel ()
distrParKernel (Step (Step Zero))        ub f = distrParBlock ub f
--distrParKernel (Step Zero)               ub f = distrParWarp ub f
distrParKernel Zero                      _  _ = error "Cannot parallelize on <thread> level."
distrParKernel _                         _  _ = error "No level above <grid> level."

distrParBlock :: CExp -> (CExp -> ILKernel ()) -> ILKernel ()
distrParBlock ub' f =
    do ub <- let_ "ub" int32_t ub'
       q <- let_ "blocksQ" int32_t (ub `divi` numWorkgroups)
       for q (\i -> do
                 j <- let_ "j" int32_t ((workgroupID `muli` q) `addi` i)
                 f j)
       iff (workgroupID `lti` (ub `modi` numWorkgroups))
           (do j <- let_ "j" int32_t ((numWorkgroups `muli` q) `addi` workgroupID)
               f j
           , return ())

distrParWarp :: CExp -> (CExp -> ILKernel ()) -> ILKernel ()
distrParWarp ub' f = -- warp level
    do ub <- let_ "ub" int32_t ub'
       numWarps <- let_ "numWarps" int32_t (blockSize `divi` warpSize)
       warpsQ <- let_ "warpsQ" int32_t (ub `divi` numWarps)
       warpsR <- let_ "warpsR" int32_t (ub `modi` numWarps)
       lwid <- let_ "lwid" int32_t (localID `divi` warpSize)
       for warpsQ
         (\i -> do warpID <- let_ "warpID" int32_t ((lwid `muli` warpsQ) `addi` i)
                   f warpID)
       iff (lwid `lti` warpsR)
           (do warpID <- let_ "warpID" int32_t ((numWarps `muli` warpsQ) `addi` lwid)
               f warpID
           , return ())

forAllKernel :: Level -> CExp -> (CExp -> ILKernel ()) -> ILKernel ()
forAllKernel (Step (Step Zero))        ub f = forAllBlock ub f
forAllKernel (Step Zero)               ub f = forAllWarp ub f
forAllKernel Zero                      _  _ = error "Cannot parallelize on <thread> leveL"
forAllKernel _                         _  _ = error "No level above <grid> level."


-- A block-level computation using blockSize threads, to evaluate
-- a parallel map
--  - evaluating "f j" for every j in [0..n-1]
forAllBlock :: CExp -> (CExp -> ILKernel ()) -> ILKernel ()
forAllBlock n f =
    do ub <- let_ "ub" int32_t n
       q <- let_ "q" int32_t (ub `divi` blockSize)
       for q (\i -> do j <- let_ "j" int32_t ((i `muli` blockSize) `addi` localID)
                       f j)
       iff (localID `lti` (ub `modi` blockSize))
         (do j <- let_ "j" int32_t ((q `muli` blockSize) `addi` localID)
             f j
         , return ())
       syncLocal

-- A warp-level computation using warpSize threads, to evaluate
-- a parallel map
--  - evaluating "f j" for every j in [0..n-1]
forAllWarp :: CExp -> (CExp -> ILKernel ()) -> ILKernel ()
forAllWarp n f =
  do ub <- let_ "ub" int32_t n
     warpID <- let_ "warpID" int32_t (localID `modi` warpSize)
     q <- let_ "q" int32_t (ub `divi` warpSize)
     r <- let_ "r" int32_t (ub `modi` warpSize)
     for q (\i -> do j <- let_ "j" int32_t ((i `muli` warpSize) `addi` warpID)
                     f j)
     iff (warpID `lti` r)
       (do j <- let_ "j" int32_t ((q `muli` warpSize) `addi` warpID)
           f j
       , return ())
     syncLocal
