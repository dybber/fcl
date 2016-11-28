module Language.FCL.Host.Syntax where

import Language.FCL.ILKernel
import CGen

type Name = String

data HostExp =
    EInt Int
  | EBool Bool
  | EString String
  | EVar VarName
--  | EBinOp BinOp HostExp HostExp

--data BinOp = AddI | MulI | EqI | LtI

data Type = IntT | BoolT | StringT | KernelT | DeviceArray Type

-- Statements are either:
--   - An allocation: a variable name (incl. type) and size in number of elements
--   - A kernel definition: name and representation of the kernel
--   - Assignment: name and expression
--   - Kernel call: kernel name, work group size, non-explicit arguments

data HostStmt =
    Declare VarName HostExp
  | Alloc VarName CType HostExp
  | DefKernel Name ([VarName], ILKernel ())
  | Call Name HostExp [VarName]
  | ReadIntCSV VarName HostExp
  | ReadDoubleCSV VarName HostExp
  | RandomIntVector VarName HostExp
  | RandomDoubleVector VarName HostExp
  | While HostExp [HostStmt]
  | Benchmark Name [HostStmt]

type HostProgram = [HostStmt]
