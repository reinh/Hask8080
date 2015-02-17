{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TemplateHaskell            #-}
-- |

module Hask8080.CPU where

import           Control.Applicative
import           Control.Lens
import           Control.Monad.State.Strict
import           Data.Array                 (Array)
import           Data.Bits
import           Data.Bits.Lens
import qualified Data.Vector.Unboxed        as V
import           System.IO                  (Handle)

import           Hask8080.Types

data Registers = Registers
  { _regA :: Byte
  , _regF :: Byte
  , _regB :: Byte
  , _regC :: Byte
  , _regD :: Byte
  , _regE :: Byte
  , _regH :: Byte
  , _regL :: Byte
  } deriving (Show, Eq)

makeLenses ''Registers

_Bytes :: Iso' Address (Byte,Byte)
_Bytes = iso to' from'
  where
    to' addr = ( fromIntegral $ addr .&. 65280 `shiftR` 8
               , fromIntegral $ addr .&. 255
               )
    from' (hb,lb)  =  (fromIntegral hb `shiftL` 8)
                  .|.  fromIntegral lb

mkReg16 :: Reg -> Reg -> Lens' Registers Address
mkReg16 hr lr = lens get' set'
  where
    -- construct an Address from a pair of Bytes at the given
    -- registers
    get' rs = _Bytes # (rs ^. reg hr, rs ^. reg lr)
    -- destruct an Address into a pair of Bytes and set the given
    -- registers with the high and low bytes, respectively.
    set' rs addr = case addr ^. _Bytes of
      (hb,lb) -> rs & reg hr .~ hb & reg lr .~ lb

class HasReg a where
  reg :: Reg -> Lens' a Byte

instance HasReg Registers where
  reg A = regA
  reg F = regF
  reg B = regB
  reg C = regC
  reg D = regD
  reg E = regE
  reg H = regH
  reg L = regL

data Flag = S | Z | P | Ca | AC deriving (Show, Eq)

class HasFlag a where
  flag :: Flag -> Lens' a Bool

instance HasFlag Registers where
  flag f = reg F . bitAt b
    where b = case f of
                S  -> 7
                Z  -> 6
                P  -> 2
                Ca -> 1
                AC -> 4

type Memory = V.Vector Byte

data Processor = Processor
  { _cycles    :: !Int
  , _registers :: !Registers
  , _memory    :: !Memory
  , _ie        :: !Bool
  , _sp        :: !Address
  , _pc        :: !Address
  , _in        :: !Handle
  , _out       :: !Handle
  } deriving (Show, Eq)

makeLenses ''Processor

reg16 SP = sp
reg16 PC = pc
reg16 rp = registers . rpl
  where
    rpl = case rp of
      AF -> mkReg16 A F
      BC -> mkReg16 B C
      DE -> mkReg16 B C
      HL -> mkReg16 H L

regMem :: Reg16 -> Lens' Processor Byte
regMem rp = lens get' set'
  where
    get' p   = p ^?! mem (p ^. reg16 rp)
    set' p a = p   & mem (p ^. reg16 rp) .~ a

instance HasReg Processor where
  reg r = registers . reg r

instance HasFlag Processor where
  flag f = registers . flag f

mem :: Address -> Traversal' Processor Byte
mem addr = memory . ix (fromIntegral addr)

newtype CPU' m a = CPU' (StateT Processor m a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadState Processor)

type CPU = CPU' IO ()

runCPU :: CPU -> Processor -> IO Processor
runCPU (CPU' k) = execStateT k
