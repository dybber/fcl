module Language.FCL.Host.Syntax where

import Language.FCL.ILKernel
import CGen

type Name = String

data HostExp =
    EInt Int
  | EBool Bool
  | EString String
  | EVar VarName
  | EAlloc CType HostExp
  | EReadIntCSV HostExp
  | EReadDoubleCSV HostExp
  | ELength HostExp
  | EBinOp BinOp HostExp HostExp
  -- | RandomIntVector VarName HostExp
  -- | RandomDoubleVector VarName HostExp

data BinOp = AddI | SubI | MulI | DivI

data Type = IntT | BoolT | StringT | KernelT | DeviceArray Type

-- Statements are either:
--   - An allocation: a variable name (incl. type) and size in number of elements
--   - A kernel definition: name and representation of the kernel
--   - Assignment: name and expression
--   - Kernel call: kernel name, work group size, non-explicit arguments

data HostStmt =
    Declare VarName HostExp
  | DefKernel Name ([VarName], ILKernel ())
  | Call Name HostExp [VarName]
  | PrintArray HostExp HostExp
  | While HostExp [HostStmt]
  -- | Benchmark Name [HostStmt]

type HostProgram = [HostStmt]
