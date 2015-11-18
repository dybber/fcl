-- | Untyped constructors for building GPUIL-kernels
module Language.GPUIL.Cons (
  
 -- Types
 int, double, bool, word8, word32, pointer,
 attrLocal, attrGlobal, attrVolatile,

 -- Expressions
 constant, if_, (?), let_, letVar, index, (!), cast,
 -- Getter's (launch parameters and current thread info)
 globalID, localID, localSize, workgroupID, numWorkgroups,
 var,

 -- Unary operators
 not, i2d, negatei, negated,
 absi, absd, signi,
 negateBitwise,
 ceil, floor, exp, ln,
 
 -- Binary operators
 addi, subi, muli, divi, modi,
 addd, subd, muld, divd,
 lti, ltei, gti, gtei, eqi, neqi,
 ltd, lted, gtd, gted, eqd, neqd,
 land, lor, xor, sll, srl,
 (&&*), (||*),
 mini, maxi,
 
 -- Statements
 for, while, iff, distrPar, forAll,
 allocate, allocateVolatile,
 assign, (<==), assignArray,
 syncGlobal, syncLocal,

 -- Monad
 VarName,
 CExp,
 Program,
 Kernel(..), NoType,
 initialState,
 runProgram,
 addParam
)
where

import Data.Word (Word32, Word8)
import Control.Monad.State
import Control.Monad.Writer

import Language.GPUIL.Syntax as AST

import Prelude hiding (not, floor, exp)
--------------------------------
-- Program construction monad --
--------------------------------
data MState = MState
              { params :: [VarName]
              , varCount :: Int
              }
initialState :: MState
initialState = MState
               { params = []
               , varCount = 0
               } 

type CExp = IExp NoType
type Program x = WriterT (Statements () NoType) (State MState) x

runProgram :: Program () -> (Statements () NoType, [VarName], Int)
runProgram m =
  let (stmts, finalState) = runProg m initialState
  in (stmts, reverse $ params finalState, varCount finalState)

runProg :: Program () -> MState -> (Statements () NoType, MState)
runProg m init' =
  let (stmts, finalState) = runState (execWriterT m) init'
  in (stmts, finalState)

run :: Program () -> Program (Statements () NoType)
run m = do
  s <- get
  let (stmts, s') = runProg m s
  put s'
  return stmts

addStmt :: Statement () NoType -> Program ()
addStmt stmt = tell [(stmt,())]

newVar :: CType -> String -> Program VarName
newVar ty name = do
  c <- gets varCount
  modify (\s -> s { varCount = 1 + varCount s })
  return (name ++ "_" ++ show c, ty) -- the underscore is important!


----------------------
-- Variable binding --
----------------------

-- Variable binder. Creates a fresh variable, adds a declaration
-- w. initialiser and passes it on
let_ :: String -> CType -> CExp -> Program CExp
let_ name ty e = do
  v <- newVar ty name
  addStmt (Decl v (Just e))
  return (VarE v NoType)

letVar :: String -> CType -> CExp -> Program VarName
letVar name ty e = do
  v <- newVar ty name
  addStmt (Decl v (Just e))
  return v

addParam :: String -> CType -> Program VarName
addParam name ty = do
  v <- newVar ty name
  modify (\s -> s { params = v : params s })
  return v

var :: VarName -> CExp
var v = VarE v NoType
----------------
-- Statements --
----------------

-- I think these two are wrong, we should not just start with an
-- initial state, the varCount at least has to be passed on.

-- construct a for loop, where the body is generated by a function
-- taking the index variable as parameter
for :: CExp -> (CExp -> Program ()) -> Program ()
for ub f = do
  i <- newVar int "i"
  let_ "ub" int ub >>= (\upperbound -> do
    body <- run (f (VarE i NoType))
                               -- TODO: Var count should be passed on!
    addStmt $ For i upperbound body)

while :: CExp -> Program () -> Program ()
while f body = do
  body' <- run body
  addStmt (SeqWhile f body')
                                    -- TODO: Var count should be passed on!

iff :: CExp -> (Program (), Program ()) -> Program ()
iff cond (f1, f2) = do
  f1' <- run f1
  f2' <- run f2
  addStmt (If cond f1' f2')

distrPar :: Level -> CExp -> (CExp -> Program ()) -> Program ()
distrPar lvl ub f = do
  i <- newVar int "i"
  let_ "ub" int ub >>= (\upperbound -> do
    body <- run (f (VarE i NoType))
    addStmt $ DistrPar lvl i upperbound body)

forAll :: Level -> CExp -> (CExp -> Program ()) -> Program ()
forAll lvl ub f = do
  i <- newVar int "i"
  let_ "ub" int ub >>= (\upperbound -> do
    body <- run (f (VarE i NoType))
    addStmt $ ForAll lvl i upperbound body)


allocate :: CType -> CExp -> Program VarName
allocate ty n = do
  arr <- newVar (pointer [] ty) "arr"
  addStmt $ Allocate arr n ty
  return arr

allocateVolatile :: CType -> CExp -> Program VarName
allocateVolatile ty n = do
  arr <- newVar (pointer [Volatile] ty) "i"
  addStmt $ Allocate arr n ty
  return arr



-- assign variable, and add to current list of operators
assign :: VarName -> CExp -> Program ()
assign n e = addStmt (Assign n e)

(<==) :: VarName -> CExp -> Program ()
n <== e = assign n e

syncGlobal :: Program ()
syncGlobal =  addStmt SyncGlobalMem

syncLocal :: Program ()
syncLocal =  addStmt SyncLocalMem

-- assign to an array
assignArray :: VarName -> CExp -> CExp -> Program ()
assignArray n e idx = addStmt (AssignSub n idx e)

-----------------
--    Types    --
-----------------
int :: CType
int = CInt32

double :: CType
double = CDouble

bool :: CType
bool = CBool

word8 :: CType
word8 = CWord8

word32 :: CType
word32 = CWord32

pointer :: [Attribute] -> CType -> CType
pointer attr t = CPtr attr t

attrLocal :: Attribute
attrLocal = Local

attrGlobal :: Attribute
attrGlobal = Global

attrVolatile :: Attribute
attrVolatile = Volatile

-----------------
-- Expressions --
-----------------
class Scalar t where
  constant :: t -> CExp

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

if_ :: CExp -> CExp -> CExp -> CExp
if_ econd etrue efalse =
  IfE econd etrue efalse NoType

(?) :: CExp -> (CExp, CExp) -> CExp
econd ? (e0,e1) = if_ econd e0 e1

index :: VarName -> CExp -> CExp
index n e =  IndexE n e

(!) :: VarName -> CExp -> CExp
(!) = index

-- TODO: This could be better
cast :: CType -> CExp -> CExp
cast _ e = e

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

-----------------
--  Operators  --
-----------------

not :: CExp -> CExp
not = UnaryOpE Not

i2d :: CExp -> CExp
i2d = UnaryOpE I2D
negatei :: CExp -> CExp
negatei = UnaryOpE NegateInt
negated :: CExp -> CExp
negated = UnaryOpE NegateDouble
negateBitwise :: CExp -> CExp
negateBitwise = UnaryOpE NegateBitwise
ceil :: CExp -> CExp
ceil = UnaryOpE Ceil
floor :: CExp -> CExp
floor = UnaryOpE Floor
exp :: CExp -> CExp
exp = UnaryOpE Exp
ln :: CExp -> CExp
ln = UnaryOpE Ln
absi :: CExp -> CExp
absi = UnaryOpE AbsI
absd :: CExp -> CExp
absd = UnaryOpE AbsD

-- Arithmetic (Int)
addi, subi, muli, divi, modi :: CExp -> CExp -> CExp
e0 `addi` e1 = (BinOpE AddI e0 e1)
e0 `subi` e1 = (BinOpE SubI e0 e1)
e0 `muli` e1 = (BinOpE MulI e0 e1)
e0 `divi` e1 = (BinOpE DivI e0 e1)
e0 `modi` e1 = (BinOpE ModI e0 e1)

-- Arithmetic (Double)
addd, subd, muld, divd :: CExp -> CExp -> CExp
e0 `addd` e1 = (BinOpE AddD e0 e1)
e0 `subd` e1 = (BinOpE SubD e0 e1)
e0 `muld` e1 = (BinOpE MulD e0 e1)
e0 `divd` e1 = (BinOpE DivD e0 e1)

-- Comparisons (Int)
lti, ltei, gti, gtei, eqi, neqi :: CExp -> CExp -> CExp
lti  e0 e1 = (BinOpE LtI e0 e1)
ltei e0 e1 = (BinOpE LteI e0 e1)
gti  e0 e1 = (BinOpE GtI e0 e1)
gtei e0 e1 = (BinOpE GteI e0 e1)
eqi  e0 e1 = (BinOpE EqI e0 e1)
neqi e0 e1 = (BinOpE NeqI e0 e1)

-- Comparisons (Double)
ltd, lted, gtd, gted, eqd, neqd  :: CExp -> CExp -> CExp
ltd  e0 e1 = (BinOpE LtD e0 e1)
lted e0 e1 = (BinOpE LteD e0 e1)
gtd  e0 e1 = (BinOpE GtD e0 e1)
gted e0 e1 = (BinOpE GteD e0 e1)
eqd  e0 e1 = (BinOpE EqD e0 e1)
neqd e0 e1 = (BinOpE NeqD e0 e1)

-- Bitwise operations
land, lor, xor, sll, srl :: CExp -> CExp -> CExp
land e0 e1 = (BinOpE Land e0 e1)
lor  e0 e1 = (BinOpE Lor e0 e1)
xor  e0 e1 = (BinOpE Xor e0 e1)
sll  e0 e1 = (BinOpE Sll e0 e1)
srl  e0 e1 = (BinOpE Srl e0 e1)

-- Boolean 'and' and 'or'
(&&*), (||*) :: CExp -> CExp -> CExp
e0 &&* e1 = (BinOpE And e0 e1)
e0 ||* e1 = (BinOpE Or e0 e1)

mini, maxi :: CExp -> CExp -> CExp
mini a b = if_ (a `lti` b) a b
maxi a b = if_ (a `lti` b) b a

signi :: CExp -> CExp
signi a =
  let -- instead of putting type annotations in everywhere
      i :: Int -> CExp
      i = constant
  in if_ (a `eqi` i 0) (i 0) (if_ (a `lti` i 0) (i (-1)) (i 1))
