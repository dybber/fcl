module FCL.Compile.CompileEnvironment
  (CompileEnv,
   emptyEnv,
   defaultCompileEnvironment,
   lookupSymbol,
   insertUserDefined)
where

import qualified Data.Map as Map
import Control.Monad (liftM)
import FCL.Core.Identifier

import FCL.Core.MonoLevel
import FCL.Core.Monotyped
import FCL.Compile.Value
import FCL.IL.Cons
import FCL.IL.Syntax (ILExp(EInt))

type CompileEnv = Map.Map Identifier Binding

data Binding = Predefined (Type -> Value)
             | UserDefined Value

emptyEnv :: CompileEnv
emptyEnv = Map.empty

insertUserDefined :: Identifier -> Value -> CompileEnv -> CompileEnv
insertUserDefined x v env = Map.insert x (UserDefined v) env

lookupSymbol :: Identifier -> Type -> CompileEnv -> Maybe Value
lookupSymbol x ty env =
  case Map.lookup x env of
    Just (Predefined f) -> Just (f ty)
    Just (UserDefined d) -> Just d
    Nothing -> Nothing

defaultCompileEnvironment :: CompileEnv
defaultCompileEnvironment =
  Map.fromList (basicOps ++ unaryOps ++ binaryOps ++ arrayOps)

ignoreType :: Value -> Binding
ignoreType v = Predefined (\_ -> v)


basicOps :: [(Identifier, Binding)]
basicOps = [("fst", ignoreType (unop opFst)),
            ("snd", ignoreType (unop opSnd))]

opFst :: Value -> Value
opFst (TagPair v1 _) = v1
opFst _ =  error ("expected pair as argument to fst")

opSnd :: Value -> Value
opSnd (TagPair _ v2) = v2
opSnd _ =  error ("expected pair as argument to snd")


unaryOps :: [(Identifier, Binding)]
unaryOps =
  [ i2i "absi" absi
  , i2i "signi" signi
  , d2d "absd" absd
  , i2i "clz" clz
  , int2double
  , bool2int
  ]

i2i :: String -> (ILExp -> ILExp) -> (Identifier, Binding)
i2i op f = (op, ignoreType (TagFn (\i0 -> TagInt (f (unInt op i0)))))

d2d :: String -> (ILExp -> ILExp) -> (Identifier, Binding)
d2d op f = (op, ignoreType (TagFn (\d0 -> TagDouble (f (unInt op d0)))))

int2double :: (Identifier, Binding)
int2double = ("i2d", ignoreType (TagFn (\i0 -> TagDouble (i2d (unInt "i2d" i0)))))

bool2int :: (Identifier, Binding)
bool2int = ("b2i", ignoreType (TagFn (\d0 -> TagInt (b2i (unBool d0)))))


binaryOps :: [(Identifier, Binding)]
binaryOps =
  [ ii2i "addi" addi,
    ii2i "subi" subi,
    ii2i "muli" muli,
    ii2i "divi" divi,
    ii2i "modi" modi,
    ii2i "mini" mini,
    ii2i "maxi" maxi,

    dd2d "addd" addd,
    dd2d "subd" subd,
    dd2d "muld" muld,
    dd2d "divd" divd,
    
    -- conditionals
    ii2b "eqi" eqi,
    ii2b "neqi" neqi,
    ii2b "lti" lti,

    -- bitwise
    ii2i "andi" land,
    ii2i "ori" lor,
    ii2i "xor" xor,
    ii2i "sll" sll,
    ii2i "srl" srl
    ]

ii2i :: String -> (ILExp -> ILExp -> ILExp) -> (Identifier, Binding)
ii2i op f =
  (op, ignoreType
         (TagFn (\i0 ->
           TagFn (\i1 -> TagInt (f (unInt op i0) (unInt op i1))))))

ii2b :: String -> (ILExp -> ILExp -> ILExp) -> (Identifier, Binding)
ii2b op f =
  (op, ignoreType
         (TagFn (\i0 ->
           TagFn (\i1 -> TagBool (f (unInt op i0) (unInt op i1))))))

dd2d :: String -> (ILExp -> ILExp -> ILExp) -> (Identifier, Binding)
dd2d op f =
  (op, ignoreType
         (TagFn (\d0 ->
           TagFn (\d1 -> TagDouble (f (unDouble d0) (unDouble d1))))))

arrayOps :: [([Char], Binding)]
arrayOps =
  [ ("lengthPull", ignoreType (lengthFn "lengthPull"))
  , ("lengthPull", ignoreType (lengthFn "lengthPull"))
  , ("generate", Predefined (\ty -> binop (generate ty)))
  , ("mapPull", Predefined (\ty -> binop (mapFn "mapPull" ty)))
  , ("mapPush", Predefined (\ty -> binop (mapPushFn "mapPush" ty)))
  , ("index", ignoreType (binop indexFn))
  , ("readIntCSV", ignoreType readIntCSVFn)
  , interleaveFn
  , ("push", Predefined (\ty -> unop (push ty)))
  , ("return", ignoreType (unop returnFn))
  , ("bind", ignoreType (binop bindFn))
  , ("force", ignoreType (unop force))
  , ("forceAndPrint", ignoreType (binop forceAndPrint))
  , ("forceAndPrintDouble", ignoreType (binop forceAndPrintDouble))
  , ("benchmark", ignoreType (binop benchmarkFn))
  , ("power", ignoreType (triop power))
  , ("while", ignoreType (triop whileArray))
  , ("whileSeq", ignoreType (triop whileSeq))
  , ("seqfor", Predefined (\ty -> triop (seqForFn ty)))
  ]

lengthFn :: String -> Value
lengthFn variant = TagFn (\arr -> TagInt (size (unArray variant arr)))

mapFn :: String -> Type -> Value -> Value -> Value
mapFn variant ((_ :> outType) :> _) f arr = TagArray (mapArray (unFn variant f) outType (unArray variant arr))
mapFn variant _ _ _ =  error ("non function type in argument to " ++ variant)

mapArray :: (Value -> Value) -> Type -> Array -> Array
mapArray f outType (ArrPull len _ g) =
  ArrPull len outType (f . g)
mapArray f outType (ArrPush lvl arr) = ArrPush lvl (mapArray f outType arr)
--  ArrPush len lvl outType (\w -> g (\e ix -> w (f e) ix))
mapArray f _ (ArrPushThread size' iterations ty g Nothing) =
  ArrPushThread size' iterations ty g (Just (const f))
mapArray f _ (ArrPushThread len iterations ty g (Just h)) =
  ArrPushThread len iterations ty g (Just (\i -> f . h i))
mapArray f outType (ArrInterleave lvl n ixt (ArrPull len ty g)) =
  ArrInterleave lvl n ixt (ArrPull len (ProgramT lvl (PushArrayT lvl outType))
                            (\i -> let p = unProgram "mapPush (interleave)" (g i)
                                   in TagProgram $
                                        do arr <- p
                                           let arr' = unArray "mapPush (interleave)" arr
                                           return (TagArray $ mapArray f outType arr')))


mapPushFn :: String -> Type -> Value -> Value -> Value
mapPushFn variant ((_ :> (_ :> outType)) :> _) f arr = TagArray (mapPush (unFn2 variant f) outType (unArray variant arr))
mapPushFn variant _ _ _ =  error ("non function type in argument to " ++ variant)

mapPush :: (Value -> Value -> Value) -> Type -> Array -> Array
mapPush f outType (ArrPull len _ g) =
  ArrPull len outType (\i -> f (TagInt i) (g i))
mapPush f outType (ArrPush lvl arr) = ArrPush lvl (mapPush f outType arr)
--  ArrPush len lvl outType (\w -> g (\e ix -> w (f e) ix))
mapPush f _ (ArrPushThread size' iterations ty g Nothing) =
  ArrPushThread size' iterations ty g (Just f)
mapPush f _ (ArrPushThread len iterations ty g (Just h)) =
  ArrPushThread len iterations ty g (Just (\i v -> f i (h i v)))
mapPush f outType (ArrInterleave lvl n ixt (ArrPull len ty g)) =
  ArrInterleave lvl n ixt (ArrPull len (ProgramT lvl (PushArrayT lvl outType))
                            (\i -> let p = unProgram "mapPush (interleave)" (g i)
                                   in TagProgram $
                                        do arr <- p
                                           let arr' = unArray "mapPush (interleave)" arr
                                           let f' j v = f (TagInt (addi (muli i n) (unInt "interleave" j))) v
                                           return (TagArray $ mapPush f' outType arr')))



generate :: Type -> Value -> Value -> Value
generate (_ :> ((_ :> ty1) :> _)) (TagInt n) (TagFn f) = TagArray (ArrPull n ty1 (\i -> f (TagInt i)))
generate _ _ _ = error "generate expects integer expression as first argument and function as second argument"

readIntCSVFn :: Value
readIntCSVFn =
  TagFn (\file ->
    TagProgram (do (name, len) <- readIntCSV (unString file)
                   return (TagArray (createPull name IntT len))))

interleaveFn :: (Identifier, Binding)
interleaveFn = ("interleave", ignoreType (triop interleave))

interleave :: Value -> Value -> Value -> Value
interleave (TagInt rn) (TagFn ixf) (TagArray (arr@(ArrPull _ ty _))) =
  case ty of
    ProgramT lvl _ -> TagProgram (return (TagArray (ArrInterleave lvl rn ixf arr)))
    _ -> error ("Interleave: third argument must be Program, was: " ++ show ty)
interleave v f a = error (unwords ["unexpected arguments to interleave",
                                   show v,
                                   show f,
                                   show a])

indexFn :: Value -> Value -> Value
indexFn (TagArray (ArrPull _ _ idx)) (TagInt i) = idx i
indexFn _ _ = error "Index expects array and integer argument"

push :: Type -> Value -> Value
push (_ :> PushArrayT lvl _) (TagArray arr) = TagArray (ArrPush lvl arr)
push _ _ = error "Push expects array as argument"

returnFn :: Value -> Value
returnFn v = TagProgram (return v)

bindFn :: Value -> Value -> Value
bindFn v0 v1 =
  case (v0, v1) of
    (TagProgram m0, TagFn f) ->
       TagProgram (do m0' <- m0
                      case f m0' of
                        TagProgram m1 -> m1 -- TODO create binding, if m1 is scalar
                        -- TagProgram m1 -> m1 >>= lets "x" -- maybe like this?
                        _ -> error "expected Program as result of second argument to Bind")
    _ -> error "program and function returning program as arguments to bind"




force :: Value -> Value
force (TagArray (ArrPull _ _ _)) = error ("force: forcing a pull-array should raise type error." ++
                                          "Needs iteration scheme before it can be forced.")
force (TagArray arr) = TagProgram $ do
  let len = size arr                     -- calculate size of complete nested array structure
  name <- allocate (convertType (baseType arr)) len    -- allocate shared memory
  let writer v ix =                     -- creater writer function (right now: only integer arrays supported!)
        case v of
          TagInt v' -> assignArray name v' ix
          TagDouble v' -> assignArray name v' ix
          e -> error ("cannot force: " ++ show e)
  forceTo writer arr                   -- recursively generate loops for each layer
  return (TagArray (createPull name (baseType arr) len))
force _ = error ("'force' expects array as argument")

forceAndPrint :: Value -> Value -> Value
forceAndPrint (TagInt n) (TagArray arr) = TagProgram $ do
  let len = size arr                     -- calculate size of complete nested array structure
  name <- allocate (convertType (baseType arr)) len    -- allocate shared memory
  let writer v i =                     -- creater writer function (right now: only integer arrays supported!)
        case v of
          TagInt v' -> assignArray name v' i
          e -> error ("cannot forceAndPrint: " ++ show e)
  forceTo writer arr                   -- recursively generate loops for each layer
  printIntArray n (var name)
  return (TagArray (createPull name (baseType arr) len))
forceAndPrint _ _ = error ("forceAndPrint expects int and grid-level push-array as argument")

forceAndPrintDouble :: Value -> Value -> Value
forceAndPrintDouble (TagInt n) (TagArray arr) = TagProgram $ do
  let len = size arr                     -- calculate size of complete nested array structure
  name <- allocate (convertType (baseType arr)) len    -- allocate shared memory
  let writer v i =                     -- creater writer function (right now: only integer arrays supported!)
        case v of
          TagDouble v' -> assignArray name v' i
          e -> error ("cannot forceAndPrintDouble: " ++ show e)
  forceTo writer arr                   -- recursively generate loops for each layer
  printDoubleArray n (var name)
  return (TagArray (createPull name (baseType arr) len))
forceAndPrintDouble _ _ = error ("forceAndPrint expects int and grid-level push-array as argument")

benchmarkFn :: Value -> Value -> Value
benchmarkFn (TagInt n) (TagProgram p) = TagProgram $ do
  benchmark n (p >> return ())
  return TagUnit
benchmarkFn _ _ = error ("'benchmark' expects int and grid-level Program-value as arguments")

forceTo :: Writer Value -> Array -> Program ()
forceTo writer (ArrPush Zero (ArrPull len _ idx)) =
  seqFor len
         (\i -> writer (idx i) i)
forceTo writer (ArrPush lvl (ArrPull len _ idx)) =
  parFor (convertLevel lvl)
         len
         (\i -> writer (idx i) i)
forceTo writer (arr@(ArrPushThread (EInt 1) iterations ty step post)) =
  do (temp_var,temp) <- letsVar "temp" (case ty of
                              IntT -> TagInt (int 0)
                              DoubleT -> TagDouble (double 0.0)
                              _ -> error "unsupported element type in forceTo")
     seqFor iterations
       (\i -> let (_, v) = step (\_ -> temp) i
              in assign temp_var (untagScalar ty v))
     case post of
       Nothing ->
         writer temp (int 0)
       Just postprocess -> writer (postprocess (TagInt (int 0)) temp) (int 0)
forceTo writer (arr@(ArrPushThread _ iterations ty step post)) =
  do temp <- allocate (convertType (baseType arr)) (size arr)
     seqFor iterations
       (\i -> let (j, v) = step (tagScalar ty . index temp) i
              in assignArray temp (untagScalar ty v) j)
     seqFor (size arr)
       (\i -> case post of
                Nothing ->
                  writer (tagScalar ty (index temp i)) i
                Just postprocess ->
                  let v = postprocess (TagInt i) (tagScalar ty (index temp i))
                  in writer v i)
forceTo _ (ArrPush _ _) = error "Push can only be applied to pull arrays."
forceTo _ (ArrPull _ _ _) = error "Pull arrays can not be forced."
forceTo writer (ArrInterleave lvl _ f (ArrPull n _ idx)) =
  distribute (convertLevel lvl) n $ \bix -> do
    let p = idx bix
    let writer' a ix = writer a (unInt "forceTo interleave" (f (TagPair (TagInt bix) (TagInt ix))))
    arr <- unProgram "forceTo" p
    forceTo writer' (unArray "forceTo" arr)
forceTo _ _ = error "force: interleave applied to non-pull-array."


lets :: String -> Value -> Program Value
lets name s =
  case s of
    TagInt x -> liftM TagInt (let_ name ILInt x)
    TagBool x -> liftM TagBool (let_ name ILBool x)
    TagDouble x -> liftM TagDouble (let_ name ILDouble x)
    TagString x -> liftM TagString (let_ name ILString x)
    TagFn _ -> return s
    TagProgram _ -> return s
    TagUnit -> return s
    TagPair x y -> do x' <- lets name x
                      y' <- lets name y
                      return (TagPair x' y')
    TagArray (ArrPull len bty idx) ->
      do (n',_) <- letsVar "len" (TagInt len)
         return (TagArray (ArrPull (var n') bty idx))
    TagArray (ArrPush lvl (ArrPull len bty idx)) ->
      do (n',_) <- letsVar "len" (TagInt len)
         return (TagArray (ArrPush lvl (ArrPull (var n') bty idx)))
    TagArray _ -> error "lets TagArray" -- TODO


letsVar :: String -> Value -> Program (ILName, Value)
letsVar name s =
  case s of
    TagInt x -> do var0 <- letVar name ILInt x
                   return (var0, TagInt (var var0))
    TagBool x -> do var0 <- letVar name ILBool x
                    return (var0, TagBool (var var0))
    TagDouble x -> do var0 <- letVar name ILDouble x
                      return (var0, TagDouble (var var0))
    TagString x -> do var0 <- letVar name ILString x
                      return (var0, TagString (var var0))
    TagUnit -> error "letsVar TagUnit"
    TagFn _ -> error "letsVar TagFn" -- TODO, Impossible - what to do? Just err?
    TagPair _ _ -> error "letsVar TagPair" -- TODO
    TagArray _ -> error "letsVar TagArray" -- TODO
    TagProgram _ -> error "letsVar TagKernelProgram"


power :: Value -> Value -> Value -> Value
power (TagInt n) (TagFn step) (TagProgram p) =
  TagProgram $
    do TagArray arr <- p
       -- Declare array
       len <- lets "len" (TagInt (size arr))
       var_array <- allocate (convertType (baseType arr)) (unInt "power" len)
       (var_len,_) <- letsVar "arraySize" len

       let writer tv i =
             case tv of
               TagInt v -> assignArray var_array v i
               TagDouble v -> assignArray var_array v i
               e -> error (show e)

       forceTo writer arr

       let vararr = TagArray (createPull var_array (baseType arr) (var var_len))

       (i,_) <- letsVar "i" (TagInt (int 0)) -- stop condition
       while (lti (var i) n) $
         do let TagProgram p' =
                   case step (TagInt (var i)) of
                     TagFn step' -> step' vararr
                     _ -> error "expected step function to take two arguments"
            arr' <- (unArray "power") `fmap` p'
            len' <- lets "len" (TagInt (size arr'))
            forceTo writer arr'
            assign var_len (unInt "power" len')
            assign i (addi (var i) (int 1))
       return vararr
power _ _ _ = error "first two arguments to power should be integer and step function, respectively"

whileArray :: Value -> Value -> Value -> Value
whileArray (TagFn cond) (TagFn step) (TagProgram p) =
  TagProgram $
    do -- Declare array
       TagArray arr <- p
       len <- lets "len" (TagInt (size arr))
       var_array <- allocate (convertType (baseType arr)) (unInt "whileArray" len)
       (var_len,_) <- letsVar "arraySize" len

       let writer tv i =
             case tv of
               TagInt v -> assignArray var_array v i
               TagDouble v -> assignArray var_array v i
               e -> error (show e)

       forceTo writer arr

       let vararr = TagArray (createPull var_array (baseType arr) (var var_len))

       (var_cond,_) <- letsVar "cond" (cond vararr) -- stop condition
       while (var var_cond) $
         do let TagProgram p' = step vararr
            TagArray arr' <- p'
            len' <- lets "len" (TagInt (size arr'))
            forceTo writer arr'
            assign var_len (unInt "whileArray" len')
            assign var_cond (unBool (cond vararr))
       return vararr
whileArray (TagFn _) (TagFn _) _ = error "third argument to while should be a push-array"
whileArray _ _ _ = error "first two arguments to while should be conditional and step function, respectively"

whileSeq :: Value -> Value -> Value -> Value
whileSeq (TagFn cond) (TagFn step) v =
  TagProgram $
    do (var0, var0tagged) <- letsVar "loopVar" v
       case cond var0tagged of
         TagBool b -> while b
                       (case step var0tagged of
                          TagInt x -> assign var0 x
                          TagBool x -> assign var0 x
                          TagDouble x -> assign var0 x
                          _ -> error "unsupported type in whileSeq")
         _ -> error "Conditional in 'while- should be boolean typed"  
       return v
whileSeq _ _ _ = error "incompatible arguments for whileSeq"


seqForFn :: Type -> Value -> Value -> Value -> Value
seqForFn ty size_ iter f =
  let size' = unInt "seqFor" size_
      iterations = unInt "seqFor" iter
      basety = case ty of
                 (_ :> (_ :> (((_ :> bty) :> _) :> _))) -> bty
                 _ -> error "incorrect type of third argument to seqfor"
      step :: (ILExp -> Value) -> ILExp -> (ILExp, Value)
      step ixf i =
        let ixf' = TagFn (ixf . (unInt "seqFor"))
            i' = TagInt i
        in case app2 f ixf' i' of
             TagPair (TagInt e) v -> (e,v)
             _ -> error "expected pair from step-function in seqfor"
  in TagArray (ArrPushThread size' iterations basety step Nothing)
