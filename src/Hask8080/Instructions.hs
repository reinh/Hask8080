-- | Instructions for the 8080 CPU

module Hask8080.Instructions where

import           Control.Lens

import           Hask8080.CPU        (CPU, cycles, reg, reg16, regMem, nextByte, nextAddr)
import           Hask8080.Types      (Reg (..), Reg16 (..))

tick        :: Int -> CPU ()
tick n      = cycles += n

-- | MOV dest src
mov         :: Reg -> Reg -> CPU ()
mov M  r    = tick 7  >> regMem HL <~ use (reg r)
mov r  M    = tick 7  >> reg r     <~ use (regMem HL)
mov r1 r2   = tick 5  >> reg r1    <~ use (reg r2)

lxi         :: Reg16 -> CPU ()
lxi rp = tick 10 >> reg16 rp <~ nextAddr

stax        :: Reg16 -> CPU ()
stax rp     = tick 7  >> regMem rp <~ use (reg A)

inr         :: Reg -> CPU ()
inr M       = tick 10 >> regMem HL += 1
inr r       = tick 5  >> reg r += 1

inx         :: Reg16 -> CPU ()
inx rp      = tick 5  >> reg16 rp += 1

dcr         :: Reg -> CPU ()
dcr M       = tick 10 >> regMem HL -= 1
dcr r       = tick 5  >> reg r -= 1

dcx         :: Reg16 -> CPU ()
dcx rp      = tick 5  >> reg16 rp += 1

mvi         :: Reg -> CPU ()
mvi M       = tick 10 >> regMem HL <~ nextByte
mvi r       = tick 7  >> reg r     <~ nextByte

ldax        :: Reg16 -> CPU ()
ldax rp     = tick 7  >> reg A <~ use (regMem rp)

nop         :: CPU ()
nop         = return ()

hlt         :: CPU ()
hlt         = error "halt"

dad         :: Reg16 -> CPU ()
dad rp      = do tick 10
                 a <- use (reg16 rp)
                 reg16 HL += a

add r       = error "fatal: add not yet implemented"
adc r       = error "fatal: adc not yet implemented"
sub r       = error "fatal: sub not yet implemented"
sbb r       = error "fatal: sbb not yet implemented"
ana r       = error "fatal: ana not yet implemented"
xra r       = error "fatal: xra not yet implemented"
ora r       = error "fatal: ora not yet implemented"
cmp r       = error "fatal: cmp not yet implemented"

rlc         = error "fatal: rlc not yet implemented"
rrc         = error "fatal: rcc not yet implemented"
ral         = error "fatal: ral not yet implemented"
rar         = error "fatal: rar not yet implemented"

push rp     = error "fatal: push not yet implemented"
pop rp      = error "fatal: pop not yet implemented"

xchg        = error "fatal: xchg not yet implemented"
xthl        = error "fatal: xthl not yet implemented"

adi         = error "fatal: adi not yet implemented"
aci         = error "fatal: aci not yet implemented"
sui         = error "fatal: sui not yet implemented"
sbi         = error "fatal: sbi not yet implemented"
ani         = error "fatal: ani not yet implemented"
xri         = error "fatal: xri not yet implemented"
ori         = error "fatal: ori not yet implemented"
cpi         = error "fatal: cpi not yet implemented"

sta         = error "fatal: sta not yet implemented"
lda         = error "fatal: lda not yet implemented"
shld        = error "fatal: shld not yet implemented"
lhld        = error "fatal: lhld not yet implemented"


pchl        = error "fatal: pchl not yet implemented"
jnz         = error "fatal: jnz not yet implemented"
jmp         = error "fatal: jmp not yet implemented"
jz          = error "fatal: jz not yet implemented"
jnc         = error "fatal: jnc not yet implemented"
jc          = error "fatal: jc not yet implemented"
jpo         = error "fatal: jpo not yet implemented"
jpe         = error "fatal: jpe not yet implemented"
jp          = error "fatal: jp not yet implemented"
jm          = error "fatal: jm not yet implemented"

cnz         = error "fatal: cnz not yet implemented"
call        = error "fatal: cmp not yet implemented"
cz          = error "fatal: cz not yet implemented"
cnc         = error "fatal: cnc not yet implemented"
cc          = error "fatal: cc not yet implemented"
cpo         = error "fatal: cpo not yet implemented"
cpe         = error "fatal: cpe not yet implemented"
cp          = error "fatal: cp not yet implemented"
cm          = error "fatal: cm not yet implemented"

rnz         = error "fatal: rnz not yet implemented"
ret         = error "fatal: rmp not yet implemented"
rz          = error "fatal: rz not yet implemented"
rnc         = error "fatal: rnc not yet implemented"
rc          = error "fatal: rc not yet implemented"
rpo         = error "fatal: rpo not yet implemented"
rpe         = error "fatal: rpe not yet implemented"
rp          = error "fatal: rp not yet implemented"
rm          = error "fatal: rm not yet implemented"

rst _       = error "fatal: rst not yet implemented"

ei          = error "fatal: ei not yet implemented"
di          = error "fatal: di not yet implemented"

in'         = error "fatal: in' not yet implemented"
out         = error "fatal: out not yet implemented"
