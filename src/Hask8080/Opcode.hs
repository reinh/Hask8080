-- |
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedLists      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Hask8080.Opcode (interpret) where

import           Control.Lens          hiding (op)
import           Data.Bits.Bitwise     (fromListBE, toListBE)
import           GHC.Exts

import           Hask8080.CPU          (CPU)
import           Hask8080.Instructions
import           Hask8080.Types

instance IsList Byte where
  type Item Byte = Int
  toList = toBitList
  fromList = fromBitList

-- | Map a Bool to a bit (Int in {0,1}).
bitToBool :: Iso' Int Bool
bitToBool =
  iso (== 1)
      (\case
         True -> 1
         False -> 0)

-- | Map a byte to a list of its bits as 0s and 1s, big endian.
toBitList :: Byte -> [Int]
toBitList = map (review bitToBool) . toListBE

-- | Mp a list of bits (0s and 1s) to a byte.
fromBitList :: [Int]-> Byte
fromBitList = fromListBE . map (view bitToBool)

-- | Interpret a byte as a 'CPU' instruction
--   See: http://altairclone.com/downloads/manuals/8080%20Programmers%20Manual.pdf
interpret :: Byte -> CPU ()
interpret = \case
  [0,0,0,0,0,0,0,0] -> nop
  [0,1,1,1,0,1,1,0] -> hlt
  [0,1,a,b,c,e,f,g] -> mov (r a b c) (r e f g)
  [0,0,a,b,c,1,0,0] -> inr (r a b c)
  [0,0,a,b,c,1,0,1] -> dcr (r a b c)
  [0,0,0,x,0,0,1,0] -> stax (r16' x)
  [0,0,0,x,1,0,1,0] -> ldax (r16' x)
  [1,0,a,b,c,d,e,f] ->
    let op = case (a, b, c) of
          (0,0,0) -> add
          (0,0,1) -> adc
          (0,1,0) -> sub
          (0,1,1) -> sbb
          (1,0,0) -> ana
          (1,0,1) -> xra
          (1,1,0) -> ora
          (1,1,1) -> cmp
    in op (r d e f)
  [0,0,0,a,b,1,1,1] -> case (a, b) of
    (0,0) -> rlc
    (0,1) -> rrc
    (1,0) -> ral
    (1,1) -> rar
  [1,1,a,b,0,1,0,1] -> push (r16 a b)
  [1,1,a,b,0,0,0,1] -> pop (r16 a b)
  [0,0,a,b,1,0,0,1] -> dad (r16 a b)
  [0,0,a,b,0,0,1,1] -> inx (r16 a b)
  [0,0,a,b,1,0,1,1] -> dcx (r16 a b)
  [1,1,1,0,1,0,1,1] -> xchg
  [1,1,1,0,0,0,1,1] -> xthl
  [0,0,a,b,0,0,0,1] -> lxi (r16 a b)
  [0,0,a,b,c,1,1,0] -> mvi (r a b c)
  [1,1,a,b,c,1,1,0] -> case (a, b, c) of
    (0,0,0) -> adi
    (0,0,1) -> aci
    (0,1,0) -> sui
    (0,1,1) -> sbi
    (1,0,0) -> ani
    (1,0,1) -> xri
    (1,1,0) -> ori
    (1,1,1) -> cpi
  [0,0,1,a,b,0,1,0] -> case (a, b) of
    (1,0) -> sta
    (1,1) -> lda
    (0,0) -> shld
    (0,1) -> lhld
  [1,1,1,0,1,0,0,1] -> pchl
  [1,1,0,0,0,0,1,0] -> jnz
  [1,1,0,0,0,0,1,1] -> jmp
  [1,1,a,b,c,0,1,0] -> case (a, b, c) of
    (0,0,1) -> jz
    (0,1,0) -> jnc
    (0,1,1) -> jc
    (1,0,0) -> jpo
    (1,0,1) -> jpe
    (1,1,0) -> jp
    (1,1,1) -> jm
  [1,1,0,0,1,1,0,0] -> cz
  [1,1,0,0,1,1,0,1] -> call
  [1,1,a,b,c,1,0,0] -> case (a, b, c) of
    (0,0,0) -> cnz
    (0,1,0) -> cnc
    (0,1,1) -> cc
    (1,0,0) -> cpo
    (1,0,1) -> cpe
    (1,1,0) -> cp
    (1,1,1) -> cm
  [1,1,0,0,1,0,0,x] -> rz
  [1,1,0,0,1,0,0,1] -> ret
  [1,1,a,b,c,0,0,0] -> case (a, b, c) of
    (0,0,0) -> rnz
    (0,1,0) -> rnc
    (0,1,1) -> rc
    (1,0,0) -> rpo
    (1,0,1) -> rpe
    (1,1,0) -> rp
    (1,1,1) -> rm
  [1,1,a,b,c,1,1,1] -> rst (fromBitList [a, b, c])
  [1,1,a,1,b,0,1,1] -> case (a, b) of
    (1,1) -> ei
    (1,0) -> di
    (0,1) -> in'
    (0,0) -> out
  _ -> nop

-- | Map a 2-bit pattern to a Reg16
r16 :: Int -> Int -> Reg16
r16 0 0 = BC
r16 0 1 = DE
r16 1 0 = HL
r16 1 1 = AF

-- | Map a bit to a Reg16
r16' :: Int -> Reg16
r16' 0 = BC
r16' 1 = DE

-- | Map a 3-bit pattern to a 'Reg'
r :: Int -> Int -> Int -> Reg
r 0 0 0 = B
r 0 0 1 = C
r 0 1 0 = D
r 0 1 1 = E
r 1 0 0 = H
r 1 0 1 = L
r 1 1 0 = M
r 1 1 1 = A
