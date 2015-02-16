-- |

module Hask8080.Types
  ( Reg (..)  -- regular registers
  , Reg16(..) -- double registers
  , Byte
  , Address
  ) where

import           Data.Word           (Word16, Word8)

type Byte = Word8
type Address = Word16

data Reg = A | F | B | C | D | E | H | L | M deriving (Show, Eq, Enum)

data Reg16 = AF | BC | DE | HL | SP | PC deriving (Show, Eq, Enum)
