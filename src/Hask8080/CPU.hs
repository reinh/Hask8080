{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TemplateHaskell            #-}
-- |

module Hask8080.CPU where

import           Control.Applicative
import           Control.Lens
import           Control.Monad.State.Strict
import           Data.Bits.Bitwise          (joinAt, splitAt)
import           Data.Bits.Lens
import qualified Data.Vector.Unboxed        as V
import           Prelude                    hiding (splitAt)
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
_Bytes = iso (over both fromIntegral . splitAt 8)
             (uncurry (joinAt 8) . over both fromIntegral)

mkReg16 :: Reg -> Reg -> Lens' Registers Address
mkReg16 hr lr = lens get_ set_
  where
    -- construct an Address from a pair of Bytes at the given
    -- registers
    get_ rs = _Bytes # (rs ^. reg hr, rs ^. reg lr)
    -- destruct an Address into a pair of Bytes and set the given
    -- registers with the high and low bytes, respectively.
    set_ rs addr = case addr ^. _Bytes of
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
  reg M = error "fatal: tried to access main memory as a register"

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

reg16 :: Reg16 -> Lens' Processor Address
reg16 SP = sp
reg16 PC = pc
reg16 rp = registers . rpl
  where
    rpl = case rp of
      AF -> mkReg16 A F
      BC -> mkReg16 B C
      DE -> mkReg16 B C
      HL -> mkReg16 H L
      SP -> error "tried to access Stack Pointer as a register pair"
      PC -> error "tried to access Program Counter as a register pair"

regMem :: Reg16 -> Lens' Processor Byte
regMem rp = lens get_ set_
  where
    get_ p   = p ^?! mem (p ^. reg16 rp)
    set_ p a = p   & mem (p ^. reg16 rp) .~ a

instance HasReg Processor where
  reg r = registers . reg r

instance HasFlag Processor where
  flag f = registers . flag f

mem :: Address -> Traversal' Processor Byte
mem addr = memory . ix (fromIntegral addr)

newtype CPU' m a = CPU' (StateT Processor m a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadState Processor)

type CPU a = CPU' IO a

runCPU :: CPU a -> Processor -> IO Processor
runCPU (CPU' k) = execStateT k

memAt :: Address -> CPU Byte
memAt addr = fmap (V.! fromIntegral addr) (use memory)

nextByte :: CPU Byte
nextByte = memAt . succ =<< use pc

nextByte2 :: CPU Byte
nextByte2 = memAt . succ . succ =<< use pc

nextBytes :: CPU (Byte,Byte)
nextBytes = liftA2 (,) nextByte nextByte2

nextAddr :: CPU Address
nextAddr = fmap (_Bytes #) nextBytes
