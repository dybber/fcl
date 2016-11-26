module Language.FCL.Compile
--  (compileKernel, compileKernels)
where

import Language.FCL.Values
import Language.FCL.ILKernel

  
import qualified Data.Map as Map
import Control.Monad (liftM)

import Language.FCL.SourceRegion
import Language.FCL.Syntax
import CGen

type VarEnv = Map.Map Name Value

emptyEnv :: VarEnv
emptyEnv = Map.empty

compileKernels :: Int -> [Definition Type] -> [TopLevel]
compileKernels optIterations = map (compileKernel optIterations)

compileKernel :: Int -> Definition Type -> TopLevel
compileKernel optIterations def =
  let e = defBody def
      kernel_name = defVar def
      kernel_body = do
         sbase <- addParam "sbase" (pointer_t [attrLocal] uint8_t)
         modifyState (\s -> s { sharedMemPointer = sbase })
         compile 0 emptyEnv (typeOf e) e
      initialState = initializeState
  in fst (generateKernelOld initialState optIterations kernel_name kernel_body)

addArgument :: Type -> ILKernel Value
addArgument (PullArrayT bty) =
  do arrVar <- addParam "arrInput" (pointer_t [attrGlobal] (convertType bty))
     lenVar <- addParam "lenInput" int32_t
     return (TagArray (createPull arrVar bty (var lenVar)))
addArgument (PushArrayT _ _) = error "Push arrays can not be kernel arguments"
addArgument ty =
  do v <- addParam "input" (convertType ty)
     return (tagExp ty (var v))

compile :: Int -> VarEnv -> Type -> Exp Type -> ILKernel ()
compile i env (ty :> ty') e =
  do taggedExp <- addArgument ty
     let varName = "argument" ++ show i
     compile (i+1) (Map.insert varName taggedExp env) ty' (App e (Var varName ty Missing))
compile _ env _ e =
  case compBody env e of
    TagProgram body -> do
      kernelBody <- body
      case kernelBody of
        TagInt i -> do
          varOut <- addParam "output" int32_t
          assign varOut i
        TagBool b -> do
          varOut <- addParam "output" bool_t
          assign varOut b
        TagDouble d -> do
          varOut <- addParam "output" double_t
          assign varOut d
        TagFn _ -> error "compileFun: Cannot return functions"
        TagPair _ _ -> error "compileFun: TODO return pairs" -- TODO
        TagArray arr -> do
          varOut <- addParam "arrOutput" (pointer_t [attrGlobal] (convertType (baseType arr)))
          let writer tv i =
                case tv of
                  TagInt v -> assignArray varOut v i
                  t -> error (show t)
          forceTo writer arr
        TagProgram _ -> error "TODO"
    v -> error ("Not a program, but: " ++ show v)

-- compile :: Int -> [Definition Type] -> (ILKernel Value, [TopLevel])
-- compile optIterations defs =
--   let findMain [] = error "No 'main'-function defined"
--       findMain (d:ds) =
--           if defVar d == "main"
--           then d
--           else findMain ds
--       main = findMain defs
--   in case compBody emptyEnv (defBody main) of
--        TagProgram p -> (p, [])
--        _ -> error "'main'-function should return a value of type \"Program <grid> 'a\" for some 'a."

-- createHostProgram :: ILHost a -> [TopLevel]
-- createHostProgram hostProg = undefined
 -- includes
 -- initialize OpenCL context and device
 -- create main function with hostProg as body
 -- add instructions for moving return value back to host
 
compBody :: VarEnv -> Exp Type -> Value
compBody _ (IntScalar i _)    = TagInt (constant i)
compBody _ (DoubleScalar d _) = TagDouble (constant d)
compBody _ (BoolScalar b _)   = TagBool (constant b)
compBody env (App e0 e1) =
  case compBody env e0 of
    TagFn f -> f (compBody env e1)
    _ -> error "Unexpected value at function position in application"
compBody env (Pair e0 e1 _) =
  (TagPair (compBody env e0) (compBody env e1))
compBody env (Proj1E e reg) =
  case compBody env e of
    TagPair v0 _ -> v0
    _ -> error (show reg ++ ": fst expects a pair as argument")
compBody env (Proj2E e reg) =
  case compBody env e of
    TagPair _ v1 -> v1
    _ -> error (show reg ++ ": snd expects a pair as argument")
compBody env (Var x _ reg) =
  case Map.lookup x env of
    Just v -> v
    Nothing -> error (show reg ++ ": Variable not defined: " ++ x)
compBody env (Lamb x _ e _ _)      = TagFn (\v -> compBody (Map.insert x v env) e)
compBody env (LambLvl _ e _ _)     = compBody env e
compBody env (AppLvl e _)          = compBody env e
compBody env (Let x e0 e1 _ _)     = compBody (Map.insert x (compBody env e0) env) e1
compBody env (UnOp op e0 reg)      = compileUnOp op (compBody env e0) reg
compBody env (BinOp op e0 e1 reg)  = compileBinOp op (compBody env e0) (compBody env e1) reg
compBody env (Cond e0 e1 e2 _ reg) =
  case compBody env e0 of
    (TagBool b0) -> case (compBody env e1, compBody env e2) of
                     (TagInt i1, TagInt i2) -> TagInt (if_ b0 i1 i2)
                     (TagBool b1, TagBool b2) -> TagBool (if_ b0 b1 b2)
                     (TagArray _, TagArray _) -> error (show reg ++ ": TODO: not possible yet")
                     (TagFn _, TagFn _) -> error (show reg ++ ": TODO: yet to be implemented")
                     (_,_) -> error (show reg ++ ": branches are differing")
    _ -> error "Expecting boolean expression as conditional argument in branch"
compBody env (GeneratePull e0 e1 reg) =
  let (_ :> ty1) = typeOf e1
  in case (compBody env e0, compBody env e1) of
      (TagInt e0', TagFn f) -> (TagArray (ArrPull e0' ty1 (\i -> f (TagInt i))))
      _ -> error (show reg ++ ": generate expects integer expression as first argument and function as second argument")
compBody env (MapPull e0 e1 reg) = map_ env e0 e1 reg
compBody env (MapPush e0 e1 reg) = map_ env e0 e1 reg
compBody env (LengthPull e0 reg) = length_ env e0 reg
compBody env (LengthPush e0 reg) = length_ env e0 reg
compBody env (Index e0 e1 reg) =
  case (compBody env e0, compBody env e1) of
    (TagArray (ArrPull _ _ idx), TagInt i) -> idx i
    _ -> error (show reg ++ ": Index expects array and integer argument")
compBody env (Force e0 _) = force (compBody env e0)
compBody env (Push lvl e0 _) =
  case compBody env e0 of
    TagArray arr -> TagArray (push lvl arr)
    _ -> error "pull expects array"
compBody env (While e0 e1 e2 _)      = whileArray (compBody env e0) (compBody env e1) (compBody env e2)
compBody env (WhileSeq e0 e1 e2 _)   = whileSeq (compBody env e0) (compBody env e1) (compBody env e2)
compBody env (Interleave i ixf e0 _) = interleave (compBody env i) (compBody env ixf) (compBody env e0)
compBody env (Return _ e _)          = TagProgram (return (compBody env e))
compBody env (Bind e0 e1 _)   =
  case (compBody env e0, compBody env e1) of
    (TagProgram v0, TagFn f) ->
      TagProgram (do m0 <- v0
                     case f m0 of
                       TagProgram m1 -> m1
                       _ -> error "TODO")
    _ -> error "TODO"
-- compBody _ (BlockSize _)   = do
--   s <- getState
--   return (TagInt (constant (configBlockSize (kernelConfig s))))
  
length_ :: Map.Map Name Value -> Exp Type -> Region -> Value
length_ env e0 reg = do
  case compBody env e0 of
    TagArray arr -> TagInt (size arr)
    e -> error (show reg ++ ": Length expects array as argument got " ++ show e)

map_ :: Map.Map Name Value -> Exp Type -> Exp Type -> Region -> Value
map_ env e0 e1 reg =
  let (_ :> outType) = typeOf e0
  in case (compBody env e0, compBody env e1) of
       (TagFn f, TagArray arr) -> TagArray (mapArray f outType arr)
       (f,e) -> error (concat [show reg,
                           ": ",
                           "Map expects function as first argument and",
                           "an array as second argument, got:\n    ",
                           show f,
                           "\nand\n    ",
                           show e])

force :: Value -> Value
  -- force (TagArray(arr@(ArrPush len (Step (Step (Step Zero))) ty wf))) = undefined
  -- allocate space for return value
  -- determine input arrays
  -- create kernel
  -- create kernel invocation

force (TagArray(arr@(ArrPush _ _ _ _))) = TagProgram $ do
  let len = size arr                     -- calculate size of complete nested array structure
  name <- allocateKernel (convertType (baseType arr)) len    -- allocate shared memory
  let writer tv i =                     -- creater writer function (right now: only integer arrays supported!)
        case tv of
          TagInt v -> assignArray name v i
          e -> error (show e)
  forceTo writer arr                   -- recursively generate loops for each layer
  return (TagArray (createPull name (baseType arr) len))
force (TagArray (ArrPull _ _ _)) = error ("force: forcing a pull-array should raise type error." ++
                                   "Needs iteration scheme before it can be forced.")
force _ = error ("force expects array as argument")

push :: Level -> Array Value -> Array Value
push lvl (ArrPull len bty idx) =
  ArrPush len lvl bty
          (\wf -> forAllKernel lvl len (\i -> wf (idx i) i))
push _ _ = error "force: push can only be applied to pull-arrays, how did this force appear?"

interleave :: Value -> Value -> Value -> Value
interleave (TagInt n) (TagFn f) (TagArray (ArrPull len (ProgramT lvl (PushArrayT _ bty)) idx)) =
  TagProgram (return (
     TagArray (ArrPush (len `muli` n) (Step lvl) bty (\wf -> 
              distrParKernel lvl len $ \bix -> do
                let arrp = idx bix
                let writer' a ix = wf a (unInt (f (TagPair (TagInt bix) (TagInt ix))))
                case arrp of
                  TagProgram m ->
                    do arrp' <- m
                       case arrp' of
                         TagArray arrp'' -> forceTo writer' arrp''
                         _ -> error "expected pull-array of programs of push-arrays as argument to Concat"
                  _ -> error "Interleave should be applied to an array of arrays!"))))
interleave _ _ t = error ("interleave only accepts pull-arrays of push-arrays. Got: " ++ show t)

lets :: String -> Value -> ILKernel Value
lets name s =
  case s of
    TagInt x -> liftM TagInt (let_ name int32_t x)
    TagBool x -> liftM TagBool (let_ name bool_t x)
    TagDouble x -> liftM TagDouble (let_ name double_t x)
    TagFn _ -> return s
    TagProgram _ -> return s
    TagPair x y -> do x' <- lets name x
                      y' <- lets name y
                      return (TagPair x' y')
    TagArray (ArrPull len bty idx) ->
      do (n',_) <- letsVar "len" (TagInt len)
         return (TagArray (ArrPull (var n') bty idx))
    TagArray (ArrPush len lvl bty wf) ->
      do (n',_) <- letsVar "len" (TagInt len)
         return (TagArray (ArrPush (var n') lvl bty wf))

letsVar :: String -> Value -> ILKernel (VarName, Value)
letsVar name s =
  case s of
    TagInt x -> do var0 <- letVar name int32_t x
                   return (var0, TagInt (var var0))
    TagBool x -> do var0 <- letVar name bool_t x
                    return (var0, TagBool (var var0))
    TagDouble x -> do var0 <- letVar name double_t x
                      return (var0, TagDouble (var var0))
    TagFn _ -> error "letsVar TagFn" -- TODO, Impossible - what to do? Just err?
    TagPair _ _ -> error "letsVar TagPair" -- TODO
    TagArray _ -> error "letsVar TagArray" -- TODO
    TagProgram _ -> error "letsVar TagKernelProgram"

whileArray :: Value -> Value -> Value -> Value
whileArray (TagFn cond) (TagFn step) (TagArray arr@(ArrPush _ _ _ _)) =
  TagProgram $
    do -- Declare array
       len <- lets "len" (TagInt (size arr))
       var_array <- allocateKernel (convertType (baseType arr)) (unInt len)
       (var_len,_) <- letsVar "arraySize" len

       let writer tv i =
             case tv of
               TagInt v -> assignArray var_array v i
               e -> error (show e)

       forceTo writer arr

       let vararr = TagArray (createPull var_array (baseType arr) (var var_len))

       (var_cond,_) <- letsVar "cond" (cond vararr) -- stop condition
       whileLoop (var var_cond) $
         do let arr' = step vararr
            len' <- lets "len" (TagInt (size (unArray arr')))
            let arr'' = case unArray arr' of
                          ArrPush _ lvl ty wf -> ArrPush (unInt len') lvl ty wf
                          _ -> error "step-function in while loop didn't return a pull array"
            forceTo writer arr''
            assign var_len (unInt len')
            assign var_cond (unBool (cond (TagArray arr'')))
       return vararr
whileArray (TagFn _) (TagFn _) _ = error "third argument to while should be a push-array"
whileArray _ _ _ = error "first two arguments to while should be conditional and step function, respectively"

whileSeq :: Value -> Value -> Value -> Value
whileSeq (TagFn cond) (TagFn step) v =
  TagProgram $
    do (var0, var0tagged) <- letsVar "loopVar" v
       case cond var0tagged of
         TagBool b -> whileLoop b
                       (case step var0tagged of
                          TagInt x -> assign var0 x
                          TagBool x -> assign var0 x
                          TagDouble x -> assign var0 x
                          _ -> error "unsupported type in whileSeq")
         _ -> error "Conditional in 'while- should be boolean typed"  
       return v
whileSeq _ _ _ = error "incompatible arguments for whileSeq"

compileBinOp :: BinOp -> Value -> Value -> Region -> Value
compileBinOp AddI (TagInt i0) (TagInt i1) _ = TagInt (addi i0 i1)
compileBinOp SubI (TagInt i0) (TagInt i1) _ = TagInt (subi i0 i1)
compileBinOp MulI (TagInt i0) (TagInt i1) _ = TagInt (muli i0 i1)
compileBinOp DivI (TagInt i0) (TagInt i1) _ = TagInt (divi i0 i1)
compileBinOp ModI (TagInt i0) (TagInt i1) _ = TagInt (modi i0 i1)
compileBinOp MinI (TagInt i0) (TagInt i1) _ = TagInt (mini i0 i1)
compileBinOp EqI  (TagInt i0) (TagInt i1) _ = TagBool (eqi i0 i1)
compileBinOp NeqI (TagInt i0) (TagInt i1) _ = TagBool (neqi i0 i1)
compileBinOp op _ _ reg =
  error (concat [show reg,
                 ": ",
                 "Unexpected arguments to unary operator ",
                 show op,
                 ", expecting integer expression."])

compileUnOp :: UnOp -> Value -> Region -> Value
compileUnOp AbsI  (TagInt i0) _ = TagInt (absi i0)
compileUnOp SignI (TagInt i0) _ = TagInt (signi i0)
compileUnOp op _ reg =
  error (concat [show reg,
                 ": ",
                 "Unexpected arguments to unary operator ",
                 show op,
                 ", expecting integer expression."])
