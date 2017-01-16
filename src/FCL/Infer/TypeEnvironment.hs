module FCL.Infer.TypeEnvironment where

import qualified Data.Map as Map

import FCL.Core.Identifier
import FCL.Type.Polymorphic
import FCL.Infer.Monad

type TypeEnvironment = Map.Map Identifier (TypeScheme Type)

lkup :: TypeEnvironment -> Identifier -> TI (TypeScheme Type)
lkup env x =
  case Map.lookup x env of
    Just ty  -> return ty
    Nothing -> throwError (UnboundVariableError x)

ext :: TypeEnvironment -> Identifier -> TypeScheme Type -> TypeEnvironment
ext env x ty = Map.insert x ty env

initialTypeEnvironment :: TI TypeEnvironment
initialTypeEnvironment =
  do typeSchemesBuiltins <-
       sequence [opMaxI,
                 opMinI,
                 opI2D,
                 opAddR,
                 opSubI,
                 opDivI,
                 opLtI,
                 opFst,
                 opSnd,
                 opIndex,
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
                 opBenchmark]
     return (Map.fromList typeSchemesBuiltins)

opMaxI :: TI (Identifier, TypeScheme Type)
opMaxI =
  return (Identifier "maxi",
          TypeScheme [] [] (IntT :> IntT :> IntT))

opMinI :: TI (Identifier, TypeScheme Type)
opMinI =
  return (Identifier "mini",
          TypeScheme [] [] (IntT :> IntT :> IntT))

opI2D :: TI (Identifier, TypeScheme Type)
opI2D =
  return (Identifier "i2d",
          TypeScheme [] [] (IntT :> DoubleT))

opAddR :: TI (Identifier, TypeScheme Type)
opAddR =
  return (Identifier "addr",
          TypeScheme [] [] (DoubleT :> DoubleT :> DoubleT))

opDivI :: TI (Identifier, TypeScheme Type)
opDivI =
  return (Identifier "divi",
          TypeScheme [] [] (IntT :> IntT :> IntT))

opSubI :: TI (Identifier, TypeScheme Type)
opSubI =
  return (Identifier "subi",
          TypeScheme [] [] (IntT :> IntT :> IntT))

opLtI :: TI (Identifier, TypeScheme Type)
opLtI =
  return (Identifier "lti",
          TypeScheme [] [] (IntT :> IntT :> BoolT))

opFst :: TI (Identifier, TypeScheme Type)
opFst =
  do tva <- newtv
     tvb <- newtv
     let ta = VarT tva
     let tb = VarT tvb
     return (Identifier "fst",
             TypeScheme [tva,tvb] [] ((ta :*: tb) :> ta))

opSnd :: TI (Identifier, TypeScheme Type)
opSnd =
  do tva <- newtv
     tvb <- newtv
     let ta = VarT tva
     let tb = VarT tvb
     return (Identifier "snd",
             TypeScheme [tva,tvb] [] ((ta :*: tb) :> tb))

opIndex :: TI (Identifier, TypeScheme Type)
opIndex =
  do tva <- newtv
     let ta = VarT tva
     return (Identifier "index",
             TypeScheme [tva] [] (PullArrayT ta :> IntT :> ta))

opLengthPull :: TI (Identifier, TypeScheme Type)
opLengthPull =
  do tva <- newtv
     return (Identifier "lengthPull",
             TypeScheme [tva] [] (PullArrayT (VarT tva) :> IntT))

opLengthPush :: TI (Identifier, TypeScheme Type)
opLengthPush =
  do tva <- newtv
     lvlVar <- newLvlVar
     return (Identifier "lengthPush",
             TypeScheme [tva] [lvlVar] (PushArrayT (VarL lvlVar) (VarT tva) :> IntT))

opGenerate :: TI (Identifier, TypeScheme Type)
opGenerate =
  do tva <- newtv
     let ta = VarT tva
     return (Identifier "generate",
             TypeScheme [tva] [] (IntT
                                  :> (IntT :> ta)
                                  :> PullArrayT ta))

opMapPull :: TI (Identifier, TypeScheme Type)
opMapPull =
  do tva <- newtv
     tvb <- newtv
     let ta = VarT tva
     let tb = VarT tvb
     return (Identifier "mapPull",
             TypeScheme [tva,tvb] [] ((ta :> tb)
                                      :> PullArrayT ta
                                      :> PullArrayT tb))

opMapPush :: TI (Identifier, TypeScheme Type)
opMapPush =
  do tva <- newtv
     tvb <- newtv
     lvlVar <- newLvlVar
     let ta = VarT tva
     let tb = VarT tvb
     let lvl = VarL lvlVar
     return (Identifier "mapPush",
             TypeScheme [tva,tvb] [lvlVar] ((ta :> tb)
                                            :> PushArrayT lvl ta
                                            :> PushArrayT lvl tb))

opForce :: TI (Identifier, TypeScheme Type)
opForce =
  do tva <- newtv
     lvlVar <- newLvlVar
     let ta = VarT tva
     let lvl = VarL lvlVar
     return (Identifier "force",
             TypeScheme [tva] [lvlVar] (PushArrayT lvl ta
                                        :> ProgramT lvl (PullArrayT ta)))

opPush :: TI (Identifier, TypeScheme Type)
opPush =
  do tva <- newtv
     lvlVar <- newLvlVar
     let ta = VarT tva
     let lvl = VarL lvlVar
     return (Identifier "push",
             TypeScheme [tva] [lvlVar] (lvlVar
                                        :-> PullArrayT ta
                                        :> PushArrayT lvl ta))

opReturn :: TI (Identifier, TypeScheme Type)
opReturn =
  do tva <- newtv
     lvlVar <- newLvlVar
     let ta = VarT tva
     let lvl = VarL lvlVar
     return (Identifier "return",
             TypeScheme [tva] [lvlVar] (lvlVar
                                        :-> ta
                                        :> ProgramT lvl ta))

opBind :: TI (Identifier, TypeScheme Type)
opBind =
  do tva <- newtv
     tvb <- newtv
     lvlVar <- newLvlVar
     let ta = VarT tva
     let tb = VarT tvb
     let lvl = VarL lvlVar
     return (Identifier "bind",
             TypeScheme [tva,tvb] [lvlVar] (ProgramT lvl ta
                                            :> (ta :> ProgramT lvl tb)
                                            :> ProgramT lvl tb))

opInterleave :: TI (Identifier, TypeScheme Type)
opInterleave =
  do tva <- newtv
     lvlVar <- newLvlVar
     let ta = VarT tva
     let lvl = VarL lvlVar
     return (Identifier "interleave",
             TypeScheme [tva] [lvlVar] (IntT
                                        :> ((IntT :*: IntT) :> IntT)
                                        :> PullArrayT (ProgramT lvl (PushArrayT lvl ta))
                                        :> ProgramT (Step lvl) (PushArrayT (Step lvl) ta)))

opSeqFor :: TI (Identifier, TypeScheme Type)
opSeqFor =
  do tva <- newtv
     return (Identifier "seqfor",
             TypeScheme [tva] [] (IntT
                                  :> IntT
                                  :> ((IntT :> VarT tva) :> (IntT :> (IntT :*: VarT tva)))
                                  :> PushArrayT Zero (VarT tva)))

opPower :: TI (Identifier, TypeScheme Type)
opPower =
  do tva <- newtv
     lvlVar <- newLvlVar
     let ty = VarT tva
     let lvl = VarL lvlVar
     return (Identifier "power",
             TypeScheme [tva] [lvlVar] (IntT
                                        :> (IntT :> (PullArrayT ty :> ProgramT lvl (PushArrayT lvl ty)))
                                        :> (ProgramT lvl (PushArrayT lvl ty))
                                        :> ProgramT lvl (PullArrayT ty)))

opWhile :: TI (Identifier, TypeScheme Type)
opWhile =
  do tva <- newtv
     lvlVar <- newLvlVar
     let ta = VarT tva
     let lvl = VarL lvlVar
     return (Identifier "while",
             TypeScheme [tva] [lvlVar] ((PullArrayT ta :> BoolT)
                                        :> (PullArrayT ta :> ProgramT lvl (PushArrayT lvl ta))
                                        :> (ProgramT lvl (PushArrayT lvl ta))
                                        :> ProgramT lvl (PullArrayT ta)))

opBlockSize :: TI (Identifier, TypeScheme Type)
opBlockSize =
  return (Identifier "#BlockSize",
          TypeScheme [] [] IntT)

opReadIntCSV :: TI (Identifier, TypeScheme Type)
opReadIntCSV =
  return (Identifier "readIntCSV",
          TypeScheme [] [] (StringT :> ProgramT gridLevel (PullArrayT IntT)))

opForceAndPrint :: TI (Identifier, TypeScheme Type)
opForceAndPrint =
  return (Identifier "forceAndPrint",
          TypeScheme [] [] (IntT
                            :> PushArrayT gridLevel IntT
                            :> ProgramT gridLevel (PullArrayT IntT)))

opBenchmark :: TI (Identifier, TypeScheme Type)
opBenchmark =
  do tva <- newtv
     let ta = VarT tva
     return (Identifier "benchmark",
             TypeScheme [tva] [] (IntT
                                  :> ProgramT gridLevel ta
                                  :> ProgramT gridLevel UnitT))
