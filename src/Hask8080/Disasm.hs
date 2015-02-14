-- |

module Hask8080.Disasm (disasm,disasmFile) where

import           Control.Applicative
import           Data.Binary.Get      (Get, getWord16le, getWord8, isEmpty,
                                       runGet)
import qualified Data.ByteString.Lazy as Lazy
import           Data.Word            (Word16, Word8)

import           Hask8080.ASM

byte :: Get Byte
byte = getWord8

addr :: Get Address
addr = getWord16le

getASMs :: Get [ASM]
getASMs =
  do done <- isEmpty
     if done
        then return []
        else do asm <- getASM
                asms <- getASMs
                return (asm : asms)

getASM :: Get ASM
getASM = parseOp =<< getWord8

parseOp :: Word8 -> Get ASM
parseOp op =
  case op of
    0x00 -> pure NOP
    0x01 -> lxi BC
    0x02 -> pure (STAX B)
    0x03 -> pure (INX BC)
    0x04 -> pure (INR B)
    0x05 -> pure (DCR B)
    0x06 -> mvi B
    0x07 -> pure RLC
    0x08 -> pure NOP
    0x09 -> pure (DAD BC)
    0x0a -> pure (LDAX B)
    0x0b -> pure (DCX BC)
    0x0c -> pure (INR C)
    0x0d -> pure (DCR C)
    0x0e -> mvi C
    0x0f -> pure RRC

    0x10 -> pure NOP
    0x11 -> lxi DE
    0x12 -> pure (STAX D)
    0x13 -> pure (INX DE)
    0x14 -> pure (INR D)
    0x15 -> pure (DCR D)
    0x16 -> mvi D
    0x17 -> pure RAL
    0x18 -> pure NOP
    0x19 -> pure (DAD DE)
    0x1a -> pure (LDAX D)
    0x1b -> pure (DCX DE)
    0x1c -> pure (INR E)
    0x1d -> pure (DCR E)
    0x1e -> mvi E
    0x1f -> pure RAR

    0x20 -> pure RIM
    0x21 -> lxi HL
    0x22 -> SHLD <$> addr
    0x23 -> pure (INX HL)
    0x24 -> pure (INR H)
    0x25 -> pure (DCR H)
    0x26 -> mvi H
    0x27 -> pure DAA
    0x28 -> pure NOP
    0x29 -> pure (DAD HL)
    0x2a -> LHLD <$> addr
    0x2b -> pure (DCX HL)
    0x2c -> pure (INR L)
    0x2d -> pure (DCR L)
    0x2e -> mvi L
    0x2f -> pure CMA

    0x30 -> pure SIM
    0x31 -> lxi SP
    0x32 -> STA <$> addr
    0x33 -> pure (INX SP)
    0x34 -> pure (INR M)
    0x35 -> pure (DCR M)
    0x36 -> mvi M
    0x37 -> pure STC
    0x38 -> pure NOP
    0x39 -> pure (DAD SP)
    0x3a -> LDA <$> addr
    0x3b -> pure (DCX SP)
    0x3c -> pure (INR A)
    0x3d -> pure (DCR A)
    0x3e -> mvi A
    0x3f -> pure CMC

    0x40 -> mov B B
    0x41 -> mov B C
    0x42 -> mov B D
    0x43 -> mov B E
    0x44 -> mov B H
    0x45 -> mov B L
    0x46 -> mov B M
    0x47 -> mov B A
    0x48 -> mov C B
    0x49 -> mov C C
    0x4a -> mov C D
    0x4b -> mov C E
    0x4c -> mov C H
    0x4d -> mov C L
    0x4e -> mov C M
    0x4f -> mov C A

    0x50 -> mov D B
    0x51 -> mov D C
    0x52 -> mov D D
    0x53 -> mov D E
    0x54 -> mov D H
    0x55 -> mov D L
    0x56 -> mov D M
    0x57 -> mov D A
    0x58 -> mov E B
    0x59 -> mov E C
    0x5a -> mov E D
    0x5b -> mov E E
    0x5c -> mov E H
    0x5d -> mov E L
    0x5e -> mov E M
    0x5f -> mov E A

    0x60 -> mov H B
    0x61 -> mov H C
    0x62 -> mov H D
    0x63 -> mov H E
    0x64 -> mov H H
    0x65 -> mov H L
    0x66 -> mov H M
    0x67 -> mov H A
    0x68 -> mov L B
    0x69 -> mov L C
    0x6a -> mov L D
    0x6b -> mov L E
    0x6c -> mov L H
    0x6d -> mov L L
    0x6e -> mov L M
    0x6f -> mov L A

    0x70 -> mov M B
    0x71 -> mov M C
    0x72 -> mov M D
    0x73 -> mov M E
    0x74 -> mov M H
    0x75 -> mov M L
    0x76 -> pure HLT
    0x77 -> mov M A
    0x78 -> mov A B
    0x79 -> mov A C
    0x7a -> mov A D
    0x7b -> mov A E
    0x7c -> mov A H
    0x7d -> mov A L
    0x7e -> mov A M
    0x7f -> mov A A

    0x80 -> pure (ADD B)
    0x81 -> pure (ADD C)
    0x82 -> pure (ADD D)
    0x83 -> pure (ADD E)
    0x84 -> pure (ADD H)
    0x85 -> pure (ADD L)
    0x86 -> pure (ADD M)
    0x87 -> pure (ADD A)
    0x88 -> pure (ADC B)
    0x89 -> pure (ADC C)
    0x8a -> pure (ADC D)
    0x8b -> pure (ADC E)
    0x8c -> pure (ADC H)
    0x8d -> pure (ADC L)
    0x8e -> pure (ADC M)
    0x8f -> pure (ADC A)

    0xa0 -> pure (ANA B)
    0xa1 -> pure (ANA C)
    0xa2 -> pure (ANA D)
    0xa3 -> pure (ANA E)
    0xa4 -> pure (ANA H)
    0xa5 -> pure (ANA L)
    0xa6 -> pure (ANA M)
    0xa7 -> pure (ANA A)
    0xa8 -> pure (XRA B)
    0xa9 -> pure (XRA C)
    0xaa -> pure (XRA D)
    0xab -> pure (XRA E)
    0xac -> pure (XRA H)
    0xad -> pure (XRA L)
    0xae -> pure (XRA M)
    0xaf -> pure (XRA A)

    0xb0 -> pure (ORA B)
    0xb1 -> pure (ORA C)
    0xb2 -> pure (ORA D)
    0xb3 -> pure (ORA E)
    0xb4 -> pure (ORA H)
    0xb5 -> pure (ORA L)
    0xb6 -> pure (ORA M)
    0xb7 -> pure (ORA A)
    0xb8 -> pure (CMP B)
    0xb9 -> pure (CMP C)
    0xba -> pure (CMP D)
    0xbb -> pure (CMP E)
    0xbc -> pure (CMP H)
    0xbd -> pure (CMP L)
    0xbe -> pure (CMP M)
    0xbf -> pure (CMP A)
    0xc1 -> pure (POP B)

    0xc0 -> pure RNZ
    0xc2 -> JNZ <$> addr
    0xc3 -> JMP <$> addr
    0xc4 -> CNZ <$> addr
    0xc5 -> pure (PUSH B)
    0xc6 -> ADI <$> byte
    0xc7 -> pure (RST 0)
    0xc8 -> pure RZ
    0xc9 -> pure RET
    0xca -> JZ <$> addr
    0xcb -> pure NOP
    0xcc -> CZ <$> addr
    0xcd -> CALL <$> addr
    0xce -> ACI <$> byte
    0xcf -> pure (RST 1)

    0xd0 -> pure RNC
    0xd1 -> pure (POP D)
    0xd2 -> JNC <$> addr
    0xd3 -> OUT <$> byte
    0xd4 -> CNC <$> addr
    0xd5 -> pure (PUSH D)
    0xd6 -> SUI <$> byte
    0xd7 -> pure (RST 2)
    0xd8 -> pure RC
    0xd9 -> pure NOP
    0xda -> JC <$> addr
    0xdb -> IN <$> byte
    0xdc -> CC <$> addr
    0xdd -> pure NOP
    0xde -> SBI <$> byte
    0xdf -> pure (RST 3)

    0xe0 -> pure RPO
    0xe1 -> pure (POP H)
    0xe2 -> JPO <$> addr
    0xe3 -> pure XTHL
    0xe4 -> CPO <$> addr
    0xe5 -> pure (PUSH H)
    0xe6 -> ANI <$> byte
    0xe7 -> pure (RST 4)
    0xe8 -> pure RPE
    0xe9 -> pure PCHL
    0xea -> JPE <$> addr
    0xeb -> pure XCHG
    0xec -> CPE <$> addr
    0xed -> pure NOP
    0xee -> XRI <$> byte
    0xef -> pure (RST 5)

    0xf0 -> pure RP
    0xf1 -> pure (POP PSW)
    0xf2 -> JP <$> addr
    0xf3 -> pure DI
    0xf4 -> CP <$> addr
    0xf5 -> pure (PUSH PSW)
    0xf6 -> ORI <$> byte
    0xf7 -> pure (RST 6)
    0xf8 -> pure RM
    0xf9 -> pure SPHL
    0xfa -> JM <$> addr
    0xfb -> pure EI
    0xfc -> CM <$> addr
    0xfd -> pure NOP
    0xfe -> CPI <$> byte
    0xff -> pure (RST 7)

    _ -> pure (unknown op)

lxi :: Reg16 -> Get ASM
lxi reg = LXI <$> pure reg <*> byte <*> byte

mvi :: Reg -> Get ASM
mvi reg = MVI <$> pure reg <*> byte

mov :: Reg -> Reg -> Get ASM
mov r1 r2 = pure (MOV r1 r2)

unknown :: Word8 -> ASM
unknown op = Unknown $ show op

type Memory = V.Vector ASM

disasm :: Lazy.ByteString -> [ASM]
disasm = runGet getASMs

disasmFile :: FilePath -> IO [ASM]
disasmFile fh = disasm <$> Lazy.readFile fh
