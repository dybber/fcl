module Language.FCL.KernelProgram where

import Language.FCL.Syntax
import CGen

warpSize :: CExp
warpSize = constant (32 :: Int)

data CompileState =
  CompileState { kernelConfig :: KernelConfig
               , allocPtrOffset :: CExp
               , sharedMemPointer :: VarName
               }


type ILKernel a = CGen CompileState a

allocateKernel :: CType -> CExp -> ILKernel VarName
allocateKernel cty n =
  do offset <- getsState allocPtrOffset
     sbase <- getsState sharedMemPointer
     case sizeOf cty of
       Just bsize ->
          do let aty = pointer_t [attrLocal] cty
             v <- letVar "arr" aty (cast aty (var sbase `addPtr` offset))
             let bytes = n `muli` (constant bsize)
             modifyState (\s -> s { allocPtrOffset = offset `addi` bytes })
             return v
       Nothing -> error "unknown size of ty (in allocate)"
                                          
distrParKernel :: Level -> CExp -> (CExp -> ILKernel ()) -> ILKernel ()
distrParKernel (Step (Step Zero))        ub f = distrParBlock ub f
distrParKernel (Step Zero)               ub f = distrParWarp ub f
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
       cfg <- kernelConfig <$> getState
       numWarps <- let_ "numWarps" int32_t (constant (configBlockSize cfg `div` configWarpSize cfg))
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
forAllKernel (Step (Step (Step Zero))) ub f = forAllBlock ub f
forAllKernel (Step (Step Zero))        ub f = forAllBlock ub f
forAllKernel (Step Zero)               ub f = forAllWarp ub f
forAllKernel Zero                      _  _ = error "Cannot parallelize on <thread> leveL"
forAllKernel _                         _  _ = error "No level above <grid> level."

forAllGrid :: CExp -> (CExp -> ILKernel ()) -> ILKernel ()
forAllGrid ub' f =
  do -- ub <- let_ "ub" int32_t ub'
     --gridSize <- eval "gridSize" uint64_t "get_global_size" [constant (0 :: Int)]
     f globalID

forAllBlock :: CExp -> (CExp -> ILKernel ()) -> ILKernel ()
forAllBlock ub' f =
    do ub <- let_ "ub" int32_t ub'
       s <- getState
       let blockSize = constant (configBlockSize (kernelConfig s))
       q <- let_ "q" int32_t (ub `divi` blockSize)
       for q (\i -> do v <- let_ "j" int32_t ((i `muli` blockSize) `addi` localID)
                       f v)
       iff (localID `lti` (ub `modi` blockSize))
         (do v <- let_ "j" int32_t ((q `muli` blockSize) `addi` localID)
             f v
         , return ())
       syncLocal

forAllWarp :: CExp -> (CExp -> ILKernel ()) -> ILKernel ()
forAllWarp ub' f =
  do ub <- let_ "ub" int32_t ub'
     q <- let_ "q" int32_t (ub `divi` warpSize)
     r <- let_ "r" int32_t (ub `modi` warpSize)
     wid <- let_ "wid" int32_t (localID `modi` warpSize)
     for q (\i -> do warpID <- let_ "warpID" int32_t ((i `muli` warpSize) `addi` wid)
                     f warpID)
     iff (wid `lti` r)
       (do warpID <- let_ "warpID" int32_t ((q `muli` warpSize) `addi` wid)
           f warpID
       , return ())
     syncLocal

