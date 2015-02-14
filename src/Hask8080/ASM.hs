-- |

module Hask8080.ASM
  ( Reg (..)  -- regular registers
  , Reg16(..) -- double registers
  , ASM(..)   -- the ASM data type
  , Byte      -- Byte is Word8
  , Address   -- Address is Word16
  ) where

import           Control.Applicative
import           Data.Binary.Get     (Get, getWord16le, getWord8, isEmpty)
import           Data.Word           (Word16, Word8)
import           Numeric             (showHex)

type Byte = Word8
type Address = Word16

data Reg = PSW | A | B | C | D | E | F | H | L | M deriving (Show, Eq, Enum)

data Reg16 = BC | DE | HL | SP | AF | IX | IY deriving (Show, Eq, Enum)

data ASM
  -- Move, load, and store
  = MOV Reg Reg
  | MVI Reg Byte
  | LXI Reg16 Byte Byte
  | STAX Reg
  | LDAX Reg
  | STA Address
  | LDA Address
  | SHLD Address
  | LHLD Address
  | XCHG

  -- Stack ops
  | PUSH Reg
  | POP Reg
  | XTHL
  | SPHL

  -- Jump
  | JMP Address
  | JC Address
  | JNC Address
  | JZ Address
  | JNZ Address
  | JP Address
  | JM Address
  | JPE Address
  | JPO Address
  | PCHL

  -- Call
  | CALL Address
  | CPE Address
  | CNZ Address
  | CC Address
  | CP Address
  | CM Address
  | CZ Address
  | CNC Address
  | CPO Address

  -- return
  | RET
  | RC
  | RNC
  | RZ
  | RNZ
  | RP
  | RM
  | RPE
  | RPO

  -- Restart
  | RST Word8

  -- Increment and decrement
  | INR Reg
  | DCR Reg
  | INX Reg16
  | DCX Reg16

  -- Add
  | ADD Reg
  | ADC Reg
  | ADI Byte
  | ACI Byte
  | DAD Reg16

  -- Subtract
  | SUB Reg
  | SBB Reg
  | SUI Byte
  | SBI Byte

  -- Logical
  | ANA Reg
  | XRA Reg
  | ORA Reg
  | CMP Reg
  | ANI Byte
  | XRI Byte
  | ORI Byte
  | CPI Byte

  -- Rotate
  | RLC
  | RRC
  | RAL
  | RAR

  -- Specials
  | CMA
  | STC
  | CMC
  | DAA

  -- Input/Output
  | IN Byte
  | OUT Byte

  -- Control
  | EI
  | DI
  | HLT
  | NOP

  | RIM
  | SIM
  | Unknown String
  deriving (Show,Eq)

isUnknown :: ASM -> Bool
isUnknown (Unknown _) = True
isUnknown _ = False
