{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TemplateHaskell #-}
-- |

module Hask8080.CPU where

import           Control.Lens
import           Control.Monad.State.Strict
import           Data.Array                 (Array)
import           Data.Bits.Lens
import qualified Data.Vector.Unboxed        as V
import           System.IO                  (Handle)

import           Hask8080.ASM

data Registers = Registers
  { _regA :: Byte
  , _regB :: Byte
  , _regC :: Byte
  , _regD :: Byte
  , _regE :: Byte
  , _regF :: Byte
  , _regH :: Byte
  , _regL :: Byte
  } deriving (Show, Eq)

makeLenses ''Registers

reg :: Reg -> Lens' Registers Byte
reg A = regA
reg B = regB
reg C = regC
reg D = regD
reg E = regE
reg F = regF
reg H = regH
reg L = regL

data Flag = Sign | Zero | Partial | Carry | AuxCarry deriving (Show, Eq)

flag :: Flag -> Lens' Registers Bool
flag Sign     = reg F . bitAt 7
flag Zero     = reg F . bitAt 6
flag AuxCarry = reg F . bitAt 4
flag Partial  = reg F . bitAt 2
flag Carry    = reg F . bitAt 1

type Memory = V.Vector Byte

data CPU = CPU
  { _registers :: !Registers
  , _memory    :: !Memory
  , _inter     :: !Bool
  , _sp        :: !Address
  , _pc        :: !Address
  , _in        :: !(IO Handle)
  , _out       :: !(IO Handle)
  }

makeLenses ''CPU

newtype Emulator m a = Emulator (StateT CPU m a)

runEmulator :: Monad m => Emulator m a -> CPU -> m (a, CPU)
runEmulator (Emulator k) = runStateT k
