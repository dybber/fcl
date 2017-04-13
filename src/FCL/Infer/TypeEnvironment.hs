module FCL.Infer.TypeEnvironment where

import qualified Data.Map as Map

import FCL.Core.Identifier
import FCL.Core.Polytyped
import FCL.Core.PolyLevel

newtype TypeEnvironment = TypeEnvironment (Map.Map Identifier TypeScheme)

ext :: TypeEnvironment -> (Identifier, TypeScheme) -> TypeEnvironment
ext (TypeEnvironment env) (x, ty) = TypeEnvironment (Map.insert x ty env)

initialTypeEnvironment :: TypeEnvironment
initialTypeEnvironment =
  let typeSchemesBuiltins = basicOps ++ unaryOps ++ binaryOps ++ arrayOps
  in TypeEnvironment (Map.fromList typeSchemesBuiltins)

basicOps :: [(Identifier, TypeScheme)]
basicOps = [opFst, opSnd]
  
unaryOps :: [(Identifier, TypeScheme)]
unaryOps =
  [i2i "negatei",
   d2d "negated",
   i2i "absi",
   i2i "signi",
   i2d "i2d",
   b2i "b2i",
   i2i "clz"
  ]

binaryOps :: [(Identifier, TypeScheme)]
binaryOps =
  [-- integer arithmetic
   ii2i "addi",
   ii2i "subi",
   ii2i "muli",
   ii2i "divi",
   ii2i "modi",
   ii2i "mini",
   ii2i "maxi",
   ii2i "powi",

   -- floating point arithmetic
   dd2d "addd",
   dd2d "subd",
   dd2d "muld",
   dd2d "divd",
   
   -- conditionals
   ii2b "eqi",
   ii2b "neqi",
   ii2b "lti",

   -- bitwise
   ii2i "andi",
   ii2i "ori",
   ii2i "xor",
   ii2i "sll",
   ii2i "srl"]

arrayOps :: [(Identifier, TypeScheme)]
arrayOps =
  [opIndex,
   opLengthPull,
   opLengthPush,
   opGenerate,
   opMapPull,
   opMapPush,
   opForce,
   opPush,
   opReturn,
   opBind,
   opInterleave,
   opSeqFor,
   opPower,
   opWhile,
   opBlockSize,
   opReadIntCSV,
   opForceAndPrint,
   opForceAndPrintDouble,
   opBenchmark
  ]

opFst :: (Identifier, TypeScheme)
opFst =
  let tva = TyVar 0 Nothing
      tvb = TyVar 1 Nothing
      ta = VarT tva
      tb = VarT tvb
  in ("fst",
      TypeScheme [] [tva,tvb] ((ta :*: tb) :> ta))

opSnd :: (Identifier, TypeScheme)
opSnd =
  let tva = TyVar 0 Nothing
      tvb = TyVar 1 Nothing
      ta = VarT tva
      tb = VarT tvb
  in ("snd",
      TypeScheme [] [tva,tvb] ((ta :*: tb) :> tb))

i2i :: String -> (Identifier, TypeScheme)
i2i op = (op,
          TypeScheme [] [] (IntT :> IntT))

d2d :: String -> (Identifier, TypeScheme)
d2d op = (op,
          TypeScheme [] [] (DoubleT :> DoubleT))

i2d :: String -> (Identifier, TypeScheme)
i2d op = (op,
          TypeScheme [] [] (IntT :> DoubleT))

b2i :: String -> (Identifier, TypeScheme)
b2i op = (op,
          TypeScheme [] [] (BoolT :> IntT))


ii2i :: String -> (Identifier, TypeScheme)
ii2i op = (op,
           TypeScheme [] [] (IntT :> IntT :> IntT))

dd2d :: String -> (Identifier, TypeScheme)
dd2d op = (op,
           TypeScheme [] [] (DoubleT :> DoubleT :> DoubleT))

ii2b :: String -> (Identifier, TypeScheme)
ii2b op = (op,
           TypeScheme [] [] (IntT :> IntT :> BoolT))

opIndex :: (Identifier, TypeScheme)
opIndex =
  let tva = TyVar 0 Nothing
      ta = VarT tva
  in ("index",
      TypeScheme [] [tva] (PullArrayT ta :> IntT :> ta))

opLengthPull :: (Identifier, TypeScheme)
opLengthPull =
  let tva = TyVar 0 Nothing
  in ("lengthPull",
      TypeScheme [] [tva] (PullArrayT (VarT tva) :> IntT))

opLengthPush :: (Identifier, TypeScheme)
opLengthPush =
  let tva = TyVar 0 Nothing
      lvlVar = LvlVar 0 Nothing
  in ("lengthPush",
      TypeScheme [lvlVar] [tva] (PushArrayT (VarL lvlVar) (VarT tva) :> IntT))

opGenerate :: (Identifier, TypeScheme)
opGenerate =
  let tva = TyVar 0 Nothing
      ta = VarT tva
  in ("generate",
      TypeScheme [] [tva] (IntT
                           :> (IntT :> ta)
                           :> PullArrayT ta))

opMapPull :: (Identifier, TypeScheme)
opMapPull =
  let tva = TyVar 0 Nothing
      tvb = TyVar 1 Nothing
      ta = VarT tva
      tb = VarT tvb
  in ("mapPull",
      TypeScheme [] [tva,tvb] ((ta :> tb)
                               :> PullArrayT ta
                               :> PullArrayT tb))

opMapPush :: (Identifier, TypeScheme)
opMapPush =
  let tva = TyVar 0 Nothing
      tvb = TyVar 1 Nothing
      lvlVar = LvlVar 0 Nothing
      ta = VarT tva
      tb = VarT tvb
      lvl = VarL lvlVar
  in  ("mapPush",
       TypeScheme [lvlVar] [tva,tvb] ((ta :> tb)
                                      :> PushArrayT lvl ta
                                      :> PushArrayT lvl tb))

opForce :: (Identifier, TypeScheme)
opForce =
  let tva = TyVar 0 Nothing
      lvlVar = LvlVar 0 Nothing
      ta = VarT tva
      lvl = VarL lvlVar
  in ("force",
      TypeScheme [lvlVar] [tva] (PushArrayT lvl ta
                                 :> ProgramT lvl (PullArrayT ta)))

opPush :: (Identifier, TypeScheme)
opPush =
  let tva = TyVar 0 Nothing
      lvlVar = LvlVar 0 Nothing
      ta = VarT tva
      lvl = VarL lvlVar
  in  ("push",
       TypeScheme [lvlVar] [tva] (PullArrayT ta
                                  :> PushArrayT lvl ta))

opReturn :: (Identifier, TypeScheme)
opReturn =
  let tva = TyVar 0 Nothing
      lvlVar = LvlVar 0 Nothing
      ta = VarT tva
      lvl = VarL lvlVar
  in  ("return",
       TypeScheme [lvlVar] [tva] (ta
                                  :> ProgramT lvl ta))

opBind :: (Identifier, TypeScheme)
opBind =
  let tva = TyVar 0 Nothing
      tvb = TyVar 1 Nothing
      lvlVar = LvlVar 0 Nothing
      ta = VarT tva
      tb = VarT tvb
      lvl = VarL lvlVar
  in  ("bind",
       TypeScheme [lvlVar] [tva,tvb] (ProgramT lvl ta
                                      :> (ta :> ProgramT lvl tb)
                                      :> ProgramT lvl tb))

opInterleave :: (Identifier, TypeScheme)
opInterleave =
  let tva = TyVar 0 Nothing
      lvlVar = LvlVar 0 Nothing
      ta = VarT tva
      lvl = VarL lvlVar
  in ("interleave",
      TypeScheme [lvlVar] [tva] (IntT
                                 :> ((IntT :*: IntT) :> IntT)
                                 :> PullArrayT (ProgramT lvl (PushArrayT lvl ta))
                                 :> ProgramT (Step lvl) (PushArrayT (Step lvl) ta)))

opSeqFor :: (Identifier, TypeScheme)
opSeqFor =
  let tva = TyVar 0 Nothing
  in  ("seqfor",
       TypeScheme [] [tva] (IntT
                            :> IntT
                            :> ((IntT :> VarT tva) :> (IntT :> (IntT :*: VarT tva)))
                            :> PushArrayT Zero (VarT tva)))

opPower :: (Identifier, TypeScheme)
opPower =
  let tva = TyVar 0 Nothing
      lvlVar = LvlVar 0 Nothing
      ty = VarT tva
      lvl = VarL lvlVar
  in ("power",
      TypeScheme [lvlVar] [tva] (IntT
                                 :> (IntT :> (PullArrayT ty :> ProgramT lvl (PushArrayT lvl ty)))
                                 :> (ProgramT lvl (PushArrayT lvl ty))
                                 :> ProgramT lvl (PullArrayT ty)))

opWhile :: (Identifier, TypeScheme)
opWhile =
  let tva = TyVar 0 Nothing
      lvlVar = LvlVar 0 Nothing
      ta = VarT tva
      lvl = VarL lvlVar
  in ("while",
      TypeScheme [lvlVar] [tva] ((PullArrayT ta :> BoolT)
                                 :> (PullArrayT ta :> ProgramT lvl (PushArrayT lvl ta))
                                 :> (ProgramT lvl (PushArrayT lvl ta))
                                 :> ProgramT lvl (PullArrayT ta)))

opBlockSize :: (Identifier, TypeScheme)
opBlockSize = ("#BlockSize", TypeScheme [] [] IntT)

opReadIntCSV :: (Identifier, TypeScheme)
opReadIntCSV =
  ("readIntCSV",
   TypeScheme [] [] (StringT :> ProgramT gridLevel (PullArrayT IntT)))

opForceAndPrint :: (Identifier, TypeScheme)
opForceAndPrint =
  ("forceAndPrint",
   TypeScheme [] [] (IntT
                     :> PushArrayT gridLevel IntT
                     :> ProgramT gridLevel (PullArrayT IntT)))

opForceAndPrintDouble :: (Identifier, TypeScheme)
opForceAndPrintDouble =
  ("forceAndPrintDouble",
   TypeScheme [] [] (IntT
                     :> PushArrayT gridLevel DoubleT
                     :> ProgramT gridLevel (PullArrayT DoubleT)))


opBenchmark :: (Identifier, TypeScheme)
opBenchmark =
  let tva = TyVar 0 Nothing
      ta = VarT tva
  in ("benchmark",
      TypeScheme [] [tva] (IntT
                           :> ProgramT gridLevel ta
                           :> ProgramT gridLevel UnitT))
