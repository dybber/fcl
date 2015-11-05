{-# LANGUAGE FlexibleInstances #-}

-- | Untyped constructors for building GPUIL-kernels
module Language.GPUIL.Cons (
  
 -- Types
 int, double, bool, word8, word32, pointer,

 -- Expressions
 constant, if_, (?), let_, letVar, index, (!), cast,
 -- Getter's (launch parameters and current thread info)
 globalID, localID, localSize, workgroupID, numWorkgroups,
 var,

 -- Operators
 addi, subi, muli, divi, modi,
 addd, subd, muld, divd,
 lti, ltei, gti, gtei, eqi, neqi,
 ltd, lted, gtd, gted, eqd, neqd,
 land, lor, xor, sll, srl,
 (&&*), (||*),
 
 -- Statements
 for, iff, --distrPar, forAll,
 assign, (<==), assignArray,
 syncGlobal, syncLocal,

 -- Monad
 Exp,
 Type,
 Program,
 initialState,
 runProgram,
 addParam,
 generateKernel
)
where

import Data.Word (Word32, Word8)
import Control.Monad.State

import Language.GPUIL.Syntax as AST

--------------------------------
-- Program construction monad --
--------------------------------
data MState = MState
              { params :: [VarName]
              , varCount :: Int
              , statements :: [Stmt NoType]
              }
initialState :: MState
initialState = MState
               { params = []
               , varCount = 0
               , statements = []
               } 

type Exp = IExp NoType
type Type = IType
type Program x = State MState x

generateKernel :: String -> Program () -> Kernel NoType
generateKernel name m =
  let finalState = runProgram m initialState
  in Kernel { kernelName = name
            , kernelParams = params finalState
            , kernelBody = reverse $ statements finalState
            }

runProgram :: Program () -> MState -> MState
runProgram m init' = snd (runState m init')

addStmt :: Stmt NoType -> Program ()
addStmt stmt =
  modify (\s -> (s {statements = stmt : statements s }))

newVar :: IType -> String -> Program VarName
newVar ty name = do
  c <- gets varCount
  modify (\s -> s { varCount = 1 + varCount s })
  return (name ++ "_" ++ show c, ty) -- the underscore is important!


----------------------
-- Variable binding --
----------------------

-- Variable binder. Creates a fresh variable, adds a declaration
-- w. initialiser and passes it on
let_ :: String -> IType -> IExp NoType -> Program (IExp NoType)
let_ name ty e = do
  v <- newVar ty name
  addStmt (Decl v (Just e))
  return (VarE v NoType)

letVar :: String -> IType -> IExp NoType -> Program VarName
letVar name ty e = do
  v <- newVar ty name
  addStmt (Decl v (Just e))
  return v

addParam :: String -> IType -> Program VarName
addParam name ty = do
  v <- newVar ty name
  modify (\s -> s { params = v : params s })
  return v

var :: VarName -> IExp NoType
var v = VarE v NoType
----------------
-- Statements --
----------------

-- I think these two are wrong, we should not just start with an
-- initial state, the varCount at least has to be passed on.

-- construct a for loop, where the body is generated by a function
-- taking the index variable as parameter
for :: IExp NoType -> (IExp NoType -> Program ()) -> Program ()
for ub f = do
  i <- newVar int "i"
  let_ "ub" int ub >>= (\upperbound -> do
    let body = statements $ runProgram (f (VarE i NoType)) initialState -- Var count should be passed on!
    addStmt $ For i upperbound body)

iff :: IExp NoType -> (Program (), Program ()) -> Program ()
iff cond (f1, f2) =
  addStmt $ If cond (statements $ runProgram f1 initialState)
                    (statements $ runProgram f2 initialState)

-- distrPar :: Level -> IExp NoType -> (IExp NoType -> Program ()) -> Program ()
-- distrPar lvl ub f = do
--   i <- newVar int "i"
--   let_ "ub" int ub >>= (\(E upperbound) -> do
--     let body = statements $ runProgram (f (E $ VarE i NoType)) initialState
--     addStmt $ DistrPar lvl i upperbound body)

-- forAll :: Level -> IExp NoType -> (IExp NoType -> Program ()) -> Program ()
-- forAll lvl ub f = do
--   i <- newVar int "i"
--   let_ "ub" int ub >>= (\(E upperbound) -> do
--     let body = statements $ runProgram (f (E $ VarE i NoType)) initialState
--     addStmt $ ForAll lvl i upperbound body)


-- assign variable, and add to current list of operators
assign :: VarName -> IExp NoType -> Program ()
assign n e = addStmt (Assign n e)

(<==) :: VarName -> IExp NoType -> Program ()
n <== e = assign n e

syncGlobal :: Program ()
syncGlobal =  addStmt SyncLocalMem

syncLocal :: Program ()
syncLocal =  addStmt SyncGlobalMem

-- assign to an array
assignArray :: VarName -> IExp NoType -> IExp NoType -> Program ()
assignArray n idx e = addStmt (AssignSub n idx e)

-----------------
--    Types    --
-----------------
int :: IType
int = Int32T

double :: IType
double = DoubleT

bool :: IType
bool = BoolT

word8 :: IType
word8 = Word8T

word32 :: IType
word32 = Word32T

pointer :: [Attribute] -> IType -> IType
pointer attr t = Ptr attr t

-----------------
-- Expressions --
-----------------
class Scalar t where
  constant :: t -> IExp NoType

instance Scalar Int where
  constant = IntE

instance Scalar Double where
  constant = DoubleE

instance Scalar Bool where
  constant = BoolE

instance Scalar Word32 where
  constant = Word32E

instance Scalar Word8 where
  constant = Word8E

if_ :: IExp NoType -> IExp NoType -> IExp NoType -> IExp NoType
if_ econd etrue efalse =
  IfE econd etrue efalse NoType

(?) :: IExp NoType -> (IExp NoType, IExp NoType) -> IExp NoType
econd ? (e0,e1) = if_ econd e0 e1

index :: VarName -> IExp NoType -> IExp NoType
index n e =  IndexE n e

(!) :: VarName -> IExp NoType -> IExp NoType
(!) = index

-- TODO: This could be better
cast :: IType -> IExp NoType -> IExp NoType
cast _ e = e

globalID :: IExp NoType
globalID = (UnaryOpE GlobalID (IntE 0))

localID :: IExp NoType
localID = (UnaryOpE LocalID (IntE 0))

workgroupID :: IExp NoType
workgroupID = (UnaryOpE GroupID (IntE 0))

localSize :: IExp NoType
localSize = (UnaryOpE LocalSize (IntE 0))

numWorkgroups :: IExp NoType
numWorkgroups =  (UnaryOpE NumGroups (IntE 0))

-----------------
--  Operators  --
-----------------

-- Arithmetic (Int)
addi, subi, muli, divi, modi :: IExp NoType -> IExp NoType -> IExp NoType
e0 `addi` e1 = (BinOpE AddI e0 e1)
e0 `subi` e1 = (BinOpE SubI e0 e1)
e0 `muli` e1 = (BinOpE MulI e0 e1)
e0 `divi` e1 = (BinOpE DivI e0 e1)
e0 `modi` e1 = (BinOpE ModI e0 e1)

-- Arithmetic (Double)
addd, subd, muld, divd :: IExp NoType -> IExp NoType -> IExp NoType
e0 `addd` e1 = (BinOpE AddD e0 e1)
e0 `subd` e1 = (BinOpE SubD e0 e1)
e0 `muld` e1 = (BinOpE MulD e0 e1)
e0 `divd` e1 = (BinOpE DivD e0 e1)

-- Comparisons (Int)
lti, ltei, gti, gtei, eqi, neqi :: IExp NoType -> IExp NoType -> IExp NoType
lti  e0 e1 = (BinOpE LtI e0 e1)
ltei e0 e1 = (BinOpE LteI e0 e1)
gti  e0 e1 = (BinOpE GtI e0 e1)
gtei e0 e1 = (BinOpE GteI e0 e1)
eqi  e0 e1 = (BinOpE EqI e0 e1)
neqi e0 e1 = (BinOpE NeqI e0 e1)

-- Comparisons (Double)
ltd, lted, gtd, gted, eqd, neqd  :: IExp NoType -> IExp NoType -> IExp NoType
ltd  e0 e1 = (BinOpE LtD e0 e1)
lted e0 e1 = (BinOpE LteD e0 e1)
gtd  e0 e1 = (BinOpE GtD e0 e1)
gted e0 e1 = (BinOpE GteD e0 e1)
eqd  e0 e1 = (BinOpE EqD e0 e1)
neqd e0 e1 = (BinOpE NeqD e0 e1)

-- Bitwise operations
land, lor, xor, sll, srl :: IExp NoType -> IExp NoType -> IExp NoType
land e0 e1 = (BinOpE Land e0 e1)
lor  e0 e1 = (BinOpE Lor e0 e1)
xor  e0 e1 = (BinOpE Xor e0 e1)
sll  e0 e1 = (BinOpE Sll e0 e1)
srl  e0 e1 = (BinOpE Srl e0 e1)

-- Boolean 'and' and 'or'
(&&*), (||*) :: IExp NoType -> IExp NoType -> IExp NoType
e0 &&* e1 = (BinOpE And e0 e1)
e0 ||* e1 = (BinOpE Or e0 e1)