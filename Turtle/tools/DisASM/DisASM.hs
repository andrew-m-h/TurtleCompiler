--------------------------------------------------------------------------
-- PDPlot-2 machine language disassembler
-- Principles of Programming Languages Assignment 2

-- Clem Baker-Finch
-- Updates by Ben Lippmeier
--------------------------------------------------------------------------

module Main

where

import Data.Bits
import GHC.Word
import GHC.Int

import System.Environment
import System.IO

--------------------------------------------------------------------------
-- Machine status registers.
--------------------------------------------------------------------------

data Register = PC | GP | FP | SP
              | PenX | PenY deriving (Eq, Ord, Show)


--------------------------------------------------------------------------
-- Representation of instructions when decoded.
--------------------------------------------------------------------------

type Address = Int16
type Offset  = Int8

data Instr = Halt | Up | Down | Move
           | Add | Sub | Neg | Mul | Test
           | Load Offset Register | Loadi 
           | Store Offset Register | Pop 
           | Read Offset Register
           | Jump | Jeq | Jlt
           | Jsr  | Rts
           | Word Word16
           deriving Show
           
--------------------------------------------------------------------------
-- Functions to pull instructions apart into opcodes, offsets etc.
--------------------------------------------------------------------------

--------------------------------------------------------------------------
-- Split a word into (instruction, offset) bytes.
--------------------------------------------------------------------------
bytes :: Word16 -> (Word8, Offset)
bytes w = (fromIntegral (w `div` 0x100), fromIntegral (w `rem`  0x100))


--------------------------------------------------------------------------
-- Get the op code by masking.
--------------------------------------------------------------------------
opCode :: Word8 -> Word8
opCode b = b .&. 0x7E

--------------------------------------------------------------------------
-- Get the index register.  GP=0, FP=1.
--------------------------------------------------------------------------
ixReg :: Word8 -> Register
ixReg b
  | testBit b 0   = FP
  | otherwise     = GP

--------------------------------------------------------------------------
-- Decode.
--------------------------------------------------------------------------

decode :: Word16 -> Instr
decode word =
  case op of
    0x00 -> Halt
    0x0A -> Up
    0x0C -> Down
    0x0E -> Move
    0x10 -> Add
    0x12 -> Sub
    0x22 -> Neg
    0x14 -> Mul
    0x16 -> Test
    0x5E -> Pop
    0x06 -> Load offset reg
    0x04 -> Store offset reg
    0x02 -> Read offset reg
    0x68 -> Jsr
    0x28 -> Rts
    0x70 -> Jump
    0x72 -> Jeq
    0x74 -> Jlt
    0x56 -> Loadi
    _    -> error (show word ++ " is not an instruction.\n")
  where (top, offset) = bytes  word
        op            = opCode top
        reg           = ixReg  top

is1word :: Instr -> Bool
is1word instr = 
  case instr of
    Jsr   -> False
    Jump  -> False
    Jeq   -> False
    Jlt   -> False
    Loadi -> False
    Pop   -> False
    _     -> True


--------------------------------------------------------------------------
-- The disassembly process.
--------------------------------------------------------------------------

--------------------------------------------------------------------------
-- Extract instructions from a list of Word16s.
-- Handle 2-word instrs.
--------------------------------------------------------------------------

decodeAll :: [Word16] -> Bool -> [Instr]
decodeAll []     _     = []
decodeAll (w:ws) True  = instr : decodeAll ws (is1word instr)
  where instr = decode w
decodeAll (w:ws) False = Word w : decodeAll ws True

disAsm :: [Word16] -> [Instr]
disAsm codes = decodeAll codes True

--------------------------------------------------------------------------
-- Extract all from the contents of a text file
--------------------------------------------------------------------------

disAsmAll :: String -> String
disAsmAll instrs =
  unlines listing
  where codes   = map read (lines instrs)
        asm     = disAsm codes
        listing = zipWith (\x y ->x++"  "++y)(map show [0..])(map show asm)

--------------------------------------------------------------------------
  
main 
 = do	args	<- getArgs
	case args of
	 [progFileName, asmFileName]
	  -> do	instrs		<- readFile progFileName
		hAsm		<- openFile asmFileName WriteMode
		hPutStr hAsm (progFileName ++ "\n\n" ++ (disAsmAll instrs))
		hClose  hAsm

	 _ -> 	putStr "usage: DisASM code.p out.asm\n"
		
