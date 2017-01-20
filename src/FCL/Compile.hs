module FCL.Compile (compile) where

import qualified Data.Map as Map
import Control.Monad (liftM)

import FCL.Core.SourceRegion
import FCL.Core.Identifier
import FCL.Core.Syntax

import FCL.Compile.Values
import FCL.Compile.Config

import FCL.IL.Cons
import FCL.IL.Syntax (Stmt)
import FCL.IL.Program (runProgram)

compile :: CompileConfig -> [Definition Type] -> [Stmt ()]
compile compileConfig defs =
  let findMain [] = error "No 'main'-function defined"
      findMain (d:ds) =
          if identToString (defVar d) == "main"
          then d
          else findMain ds
      main = findMain defs
  in case compBody emptyEnv (defBody main) of
       TagProgram p -> fst (runProgram p)
       _ -> error "'main'-function should return a value of type \"Program <grid> 'a\" for some 'a."

type VarEnv = Map.Map Identifier Value

emptyEnv :: VarEnv
emptyEnv = Map.empty

compBody :: VarEnv -> Exp Type -> Value
compBody _ (Literal (LiteralInt i) _)        = TagInt (int i)
compBody _ (Literal (LiteralDouble d) _)     = TagDouble (double d)
compBody _ (Literal (LiteralBool b) _)       = TagBool (bool b)
compBody _ (Literal (LiteralString s) _)     = TagString (string s)
compBody _ (Literal Unit _)                  = TagUnit
compBody env (App e0 e1)          = (compBody env e0) `app1` (compBody env e1)
compBody env (Lamb x _ e _ _)     = TagFn (\v -> compBody (Map.insert x v env) e)
compBody env (LambLvl _ e _ _)    = compBody env e
compBody env (AppLvl e _)         = compBody env e
compBody env (Let x e0 e1 _ _)    = compBody (Map.insert x (compBody env e0) env) e1
compBody env (UnaryOp op e0 reg)     = compileUnOp op (compBody env e0) reg
compBody env (BinaryOp op e0 e1 reg) = compileBinOp op (compBody env e0) (compBody env e1) reg
compBody env (Pair e0 e1 _)       = TagPair (compBody env e0) (compBody env e1)
compBody env (Proj1E e reg)       = fst (unPair "fst" reg (compBody env e))
compBody env (Proj2E e reg)       = snd (unPair "snd" reg (compBody env e))
compBody env (Var x _ reg) =
  case Map.lookup x env of
    Just v -> v
    Nothing -> error (show reg ++ ": Variable not defined: " ++ (identToString x))
compBody env (Cond e0 e1 e2 _ reg) =
  let b0 = unBool (compBody env e0)
  in case (compBody env e1, compBody env e2) of
       (TagInt i1, TagInt i2) -> TagInt (if_ b0 i1 i2)
       (TagDouble d1, TagDouble d2) -> TagBool (if_ b0 d1 d2)
       (TagBool b1, TagBool b2) -> TagBool (if_ b0 b1 b2)
       (TagString s1, TagString s2) -> TagString (if_ b0 s1 s2)
       (TagArray _, TagArray _) -> error (show reg ++ ": compilation of conditional on arrays not possible yet.")
       (TagFn _, TagFn _) -> error (show reg ++ ": compilation of conditionals on function not possible yet.")
       (TagProgram _, TagProgram _) -> error (show reg ++ ": compilation of conditionals on programs not possible yet.")
       (_,_) -> error (show reg ++ ": compilation of conditionals: branches are differing")
compBody env (GeneratePull e0 e1 reg) =
  let (_ :> ty1) = typeOf e1
  in case (compBody env e0, compBody env e1) of
      (TagInt e0', TagFn f) -> (TagArray (ArrPull e0' ty1 (\i -> f (TagInt i))))
      _ -> error (show reg ++ ": generate expects integer expression as first argument and function as second argument")
compBody env (MapPull e0 e1 reg) = map_ env e0 e1 reg
compBody env (MapPush e0 e1 reg) = map_ env e0 e1 reg
compBody env (LengthPull e0 reg) = length_ (compBody env e0) reg
compBody env (LengthPush e0 reg) = length_ (compBody env e0) reg
compBody env (Index e0 e1 reg) =
  case (compBody env e0, compBody env e1) of
    (TagArray (ArrPull _ _ idx), TagInt i) -> idx i
    _ -> error (show reg ++ ": Index expects array and integer argument")
compBody env (Push lvl e0 _) =
  case compBody env e0 of
    TagArray arr -> TagArray (ArrPush lvl arr)
    _ -> error "Not array argument to push"
compBody env (For e0 e1 e2 _)        =
  let size' = unInt (compBody env e0)
      iterations = unInt (compBody env e1)
      bty = case typeOf e2 of
              ((_ :> ty) :> _) -> ty
              _ -> error "incorrect type of third argument to seqfor"
      step :: (ILExp -> Value) -> ILExp -> (ILExp, Value)
      step ixf i =
        let ixf' = TagFn (ixf . unInt)
            i' = TagInt i
        in case app2 (compBody env e2) ixf' i' of
             TagPair (TagInt e) v -> (e,v)
             _ -> error "expected pair from step-function in seqfor"
  in 
   TagArray (ArrPushThread size' iterations bty step Nothing)
compBody env (Force e0 _)            = force (compBody env e0)
compBody env (Power e0 e1 e2 _)      = power (compBody env e0) (compBody env e1) (compBody env e2)
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
                       _ -> error "expected Program as result of second argument to Bind")
    _ -> error "program and function returning program as arguments to bind"
compBody env (ForceAndPrint e0 e1 _) = forceAndPrint (compBody env e0) (compBody env e1)
compBody env (Benchmark e0 e1 _) = benchmark_ (compBody env e0) (compBody env e1)
compBody env (ReadIntCSV e0 _) = readIntCSV_ (compBody env e0)

readIntCSV_ :: Value -> Value
readIntCSV_ file =
  TagProgram $
    do (name, len) <- readIntCSV (unString file)
       return (TagArray (createPull name IntT len))
                                       
length_ :: Value -> SourceRegion -> Value
length_ (TagArray arr) _ = TagInt (size arr)
length_ _ reg = error (show reg ++ ": Length expects array as argument.")

map_ :: VarEnv -> Exp Type -> Exp Type -> SourceRegion -> Value
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

interleave :: Value -> Value -> Value -> Value
interleave (TagInt rn) (TagFn ixf) (TagArray (arr@(ArrPull _ (ProgramT lvl _) _))) =
  TagProgram (return (TagArray (ArrInterleave lvl rn ixf arr)))
interleave _ _ _ = error "unexpected arguments to interleave"

force :: Value -> Value
force (TagArray (ArrPull _ _ _)) = error ("force: forcing a pull-array should raise type error." ++
                                   "Needs iteration scheme before it can be forced.")
force (TagArray arr) = TagProgram $ do
  let len = size arr                     -- calculate size of complete nested array structure
  name <- allocate (convertType (baseType arr)) len    -- allocate shared memory
  let writer v i =                     -- creater writer function (right now: only integer arrays supported!)
        case v of
          TagInt v' -> assignArray name v' i
          e -> error (show e)
  forceTo writer arr                   -- recursively generate loops for each layer
  return (TagArray (createPull name (baseType arr) len))
force _ = error ("force expects array as argument")

forceAndPrint :: Value -> Value -> Value
forceAndPrint (TagInt n) (TagArray arr) = TagProgram $ do
  let len = size arr                     -- calculate size of complete nested array structure
  name <- allocate (convertType (baseType arr)) len    -- allocate shared memory
  let writer v i =                     -- creater writer function (right now: only integer arrays supported!)
        case v of
          TagInt v' -> assignArray name v' i
          e -> error (show e)
  forceTo writer arr                   -- recursively generate loops for each layer
  printIntArray n (var name)
  return (TagArray (createPull name (baseType arr) len))
-- forceAndPrint _ (TagArray (ArrPull _ _ _)) = error ("force: forcing a pull-array should raise type error." ++
--                                                     "Needs iteration scheme before it can be forced.")
-- forceAndPrint _ (TagArray (ArrPushThread _ _ _ _ _)) = error ("force: cannot force thread-level push array on grid-level.")
-- forceAndPrint _ (TagArray (ArrPush lvl _)) = error ("forceAndPrint grid-level array as argument, got " ++ show lvl)
forceAndPrint _ _ = error ("forceAndPrint expects int and grid-level push-array as argument")

benchmark_ :: Value -> Value -> Value
benchmark_ (TagInt n) (TagProgram p) = TagProgram $ do
  benchmark n (p >> return ())
  return TagUnit
  -- let len = size arr                     -- calculate size of complete nested array structure
  -- name <- allocate (convertType (baseType arr)) len    -- allocate shared memory
  -- let writer v i =                     -- creater writer function (right now: only integer arrays supported!)
  --       case v of
  --         TagInt v' -> assignArray name v' i
  --         e -> error (show e)
benchmark_ _ _ = error ("forceAndPrint expects int and grid-level push-array as argument")


forceTo :: Writer Value -> Array -> Program ()
forceTo writer (ArrPush Zero (ArrPull len _ idx)) =
  seqFor len
         (\i -> writer (idx i) i)
forceTo writer (ArrPush lvl (ArrPull len _ idx)) =
  parFor (convertLevel lvl)
         len
         (\i -> writer (idx i) i)
forceTo writer (arr@(ArrPushThread _ iterations ty step post)) =
  do temp <- allocate (convertType (baseType arr)) (size arr)
     seqFor iterations
       (\i -> let (j, v) = step (tagScalar ty . index temp) i
              in assignArray temp (untagScalar ty v) j)
     seqFor (size arr)
       (\i -> case post of
                Nothing ->
                  writer (TagInt (index temp i)) i
                Just postprocess ->
                  let v = postprocess (TagInt (index temp i)) -- TODO tag using type info
                  in writer v i)
forceTo _ (ArrPush _ _) = error "Push can only be applied to pull arrays."
forceTo _ (ArrPull _ _ _) = error "Pull arrays can not be forced."
forceTo writer (ArrInterleave lvl _ f (ArrPull n _ idx)) =
  distribute (convertLevel lvl) n $ \bix -> do
    let p = idx bix
    let writer' a ix = writer a (unInt (f (TagPair (TagInt bix) (TagInt ix))))
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
       var_array <- allocate (convertType (baseType arr)) (unInt len)
       (var_len,_) <- letsVar "arraySize" len

       let writer tv i =
             case tv of
               TagInt v -> assignArray var_array v i
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
            assign var_len (unInt len')
            assign i (addi (var i) (int 1))
       return vararr
power _ _ _ = error "first two arguments to power should be integer and step function, respectively"

whileArray :: Value -> Value -> Value -> Value
whileArray (TagFn cond) (TagFn step) (TagProgram p) =
  TagProgram $
    do -- Declare array
       TagArray arr <- p
       len <- lets "len" (TagInt (size arr))
       var_array <- allocate (convertType (baseType arr)) (unInt len)
       (var_len,_) <- letsVar "arraySize" len

       let writer tv i =
             case tv of
               TagInt v -> assignArray var_array v i
               e -> error (show e)

       forceTo writer arr

       let vararr = TagArray (createPull var_array (baseType arr) (var var_len))

       (var_cond,_) <- letsVar "cond" (cond vararr) -- stop condition
       while (var var_cond) $
         do let TagProgram p' = step vararr
            TagArray arr' <- p'
            len' <- lets "len" (TagInt (size arr'))
            forceTo writer arr'
            assign var_len (unInt len')
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

compileBinOp :: BinaryOperator -> Value -> Value -> SourceRegion -> Value
compileBinOp AddI (TagInt i0) (TagInt i1) _ = TagInt (addi i0 i1)
compileBinOp SubI (TagInt i0) (TagInt i1) _ = TagInt (subi i0 i1)
compileBinOp MulI (TagInt i0) (TagInt i1) _ = TagInt (muli i0 i1)
compileBinOp DivI (TagInt i0) (TagInt i1) _ = TagInt (divi i0 i1)
compileBinOp ModI (TagInt i0) (TagInt i1) _ = TagInt (modi i0 i1)
compileBinOp MinI (TagInt i0) (TagInt i1) _ = TagInt (mini i0 i1)
compileBinOp MaxI (TagInt i0) (TagInt i1) _ = TagInt (maxi i0 i1)
compileBinOp EqI  (TagInt i0) (TagInt i1) _ = TagBool (eqi i0 i1)
compileBinOp NeqI (TagInt i0) (TagInt i1) _ = TagBool (neqi i0 i1)
compileBinOp LtI  (TagInt i0) (TagInt i1) _ = TagBool (lti i0 i1)
compileBinOp XorI (TagInt i0) (TagInt i1) _ = TagBool (xor i0 i1)
compileBinOp ShiftLI (TagInt i0) (TagInt i1) _ = TagInt (sll i0 i1)
compileBinOp ShiftRI (TagInt i0) (TagInt i1) _ = TagInt (srl i0 i1)
compileBinOp op _ _ reg =
  error (concat [show reg,
                 ": ",
                 "Unexpected arguments to binary operator ",
                 show op,
                 ", expecting integer expression."])

compileUnOp :: UnaryOperator -> Value -> SourceRegion -> Value
compileUnOp AbsI  (TagInt i0) _ = TagInt (absi i0)
compileUnOp SignI (TagInt i0) _ = TagInt (signi i0)
compileUnOp op _ reg =
  error (concat [show reg,
                 ": ",
                 "Unexpected arguments to unary operator ",
                 show op,
                 ", expecting integer expression."])
