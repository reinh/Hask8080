-- | Instructions for the 8080 CPU

module Hask8080.Instructions where

import Control.Applicative
import Control.Lens

import Hask8080.CPU (CPU, pc, reg, reg16, regMem)
import Hask8080.Types (Address, Byte, Reg (..), Reg16 (..))

tick n      = pc += n

mov         :: Reg -> Reg -> CPU
mov r1 M    = tick 7  >> regMem HL <~ use (reg r1)
mov M  r2   = tick 7  >> reg r2    <~ use (regMem HL)
mov r1 r2   = tick 5  >> reg r1    <~ use (reg r2)

lxi         :: Reg16 -> Address -> CPU
lxi rp addr = tick 10 >> reg16 rp .= addr

stax        :: Reg16 -> CPU
stax rp     = tick 7  >> regMem rp <~ use (reg A)

inr         :: Reg -> CPU
inr M       = tick 10 >> regMem HL += 1
inr r       = tick 5  >> reg r += 1

inx         :: Reg16 -> CPU
inx rp      = tick 5  >> reg16 rp += 1

dcr         :: Reg -> CPU
dcr M       = tick 10 >> regMem HL -= 1
dcr r       = tick 5  >> reg r -= 1

dcx         :: Reg16 -> CPU
dcx rp      = tick 5  >> reg16 rp += 1

mvi         :: Reg -> Byte -> CPU
mvi M b     = tick 10 >> regMem HL .= b
mvi r b     = tick 7  >> reg r .= b

ldax        :: Reg16 -> CPU
ldax rp     = tick 7  >> reg A <~ use (regMem rp)

nop         :: CPU
nop         = return ()

hlt         :: CPU
hlt         = error "halt"

dad         :: Reg16 -> CPU
dad rp      = do tick 10
                 a <- use (reg16 rp)
                 reg16 HL += a
