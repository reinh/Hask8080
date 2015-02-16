-- | Instructions for the 8080 CPU

module Hask8080.Instructions where

import           Control.Lens

import           Hask8080.CPU   (CPU, pc, reg, reg16, regMem)
import           Hask8080.Types (Address, Byte, Reg (..), Reg16 (..))

tick n = pc += n

mov :: Reg -> Reg -> CPU
mov r1 M  = (assign (regMem HL) =<< use (reg r1)) >> tick 7
mov M  r2 = assign (reg r2)    =<< use (regMem HL)
mov r1 r2 = assign (reg r1)    =<< use (reg r2)

lxi :: Reg16 -> Address -> CPU
lxi rp addr = reg16 rp .= addr

stax :: Reg16 -> CPU
stax rp = assign (regMem rp) =<< use (reg A)

inr :: Reg -> CPU
inr M = regMem HL += 1
inr r = reg r += 1

inx :: Reg16 -> CPU
inx rp = reg16 rp += 1

dcr :: Reg -> CPU
dcr M = regMem HL -= 1
dcr r = reg r -= 1

dcx :: Reg16 -> CPU
dcx rp = reg16 rp += 1

mvi :: Reg -> Byte -> CPU
mvi M b = regMem HL .= b
mvi r b = reg r .= b

dad :: Reg16 -> CPU
dad rp = do a <- use (reg16 rp)
            reg16 HL += a

ldax :: Reg16 -> CPU
ldax rp = assign (reg A) =<< use (regMem rp)

nop :: CPU
nop = return ()

hlt :: CPU
hlt = error "halt"
