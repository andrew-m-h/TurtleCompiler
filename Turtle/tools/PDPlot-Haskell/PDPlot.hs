--------------------------------------------------------------------------
-- Line plotter machine simulator
-- Principles of Programming Languages Assignment 2

-- Clem Baker-Finch

-- Minor changes by Robert 'Probie' Offner to make it compliant
-- with GHC 7

-- Uses ancient graphics package which produces a postscript page output.
-- Should be updated to use newer ANUPlot package... one day.
-- (Perhaps the whole thing should be rewritten
--   or abandoned for another language.)
--------------------------------------------------------------------------

module Main

where

import Control.Monad.ST
import Data.STRef
import GHC.Arr
import Graphics (Picture, Object(Line), Point, Path, draw, drawTo)
import Data.Bits
import Data.Word
import Data.Int
import Data.Array

import System.IO
import System.Environment

--------------------------------------------------------------------------
-- Contractions for long-winded ST module functions
--------------------------------------------------------------------------

rRef :: STRef a b -> ST a b
rRef = readSTRef
wRef :: STRef a b -> b -> ST a ()
wRef = writeSTRef
rArr :: Ix a => STArray b a c -> a -> ST b c
rArr = readSTArray
wArr  :: Ix a => STArray b a c -> a -> c -> ST b ()
wArr = writeSTArray

--------------------------------------------------------------------------
-- Machine status registers.
--------------------------------------------------------------------------

data Register = PC | GP | FP | SP
              | PenX | PenY deriving (Eq, Ord, Ix, Show)

-- Condition code value
data CondCode = N | Z | Off deriving (Eq, Show)


-- Pen up/down status.  True ==> pen down.
type PenStatus = Bool


--------------------------------------------------------------------------
-- Representation of instructions when decoded.
-- Notice that some represent 2-word instructions.
--------------------------------------------------------------------------

type Address = Int16
type Offset  = Int8

data Instr = Halt | Up | Down | Move
           | Add | Sub | Neg | Mul | Test
           | Load Register Offset | Loadi Int16
           | Store Register Offset | Pop Int16
           | Read Register Offset
           | Jump Address | Jeq Address | Jlt Address
           | Jsr Address | Rts
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
-- Decode.  Take the second word as well, just in case.
-- Advance the PC 1 or 2 handled in instruction execution phase.
--------------------------------------------------------------------------
decode :: Word16 -> Int16 -> Instr
decode w1 w2 =
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
    0x5E -> Pop w2
    0x06 -> Load reg offset
    0x04 -> Store reg offset
    0x02 -> Read reg offset
    0x68 -> Jsr w2
    0x28 -> Rts
    0x70 -> Jump w2
    0x72 -> Jeq w2
    0x74 -> Jlt w2
    0x56 -> Loadi w2
    _    -> error "No such instruction."
  where (top, offset) = bytes  w1
        op            = opCode top
        reg           = ixReg  top

--------------------------------------------------------------------------
-- The machine state and its representation consists of:
-- 6 registers: an array of Addresses, indexed by datatype Register;
-- The store: an array of 16-bit words, indexed by an Address range;
-- A single condition code: a reference with CondCode value;
-- A boolean indication of whether the pen is up or down;
-- A list of 16-bit integers read from the data file;
-- The path of the current line: collected as a list of points visited
--   between pen down and pen up.
-- See the type declaration of run below.
--------------------------------------------------------------------------
--------------------------------------------------------------------------
-- Fetch and execute cycle.
--------------------------------------------------------------------------

run :: STArray s Register Address ->  -- registers and pen position
       STArray s Address Int16 ->     -- store
       STRef s CondCode ->            -- condition code
       STRef s PenStatus ->           -- pen down (True) or up (False)
       STRef s [Int16] ->             -- input file
       STRef s Path ->                -- current line
       ST s Picture

run regs store condCode penDown input path =
  fetch regs store                           >>= \instr->
  exec instr regs store condCode penDown input path
  
--------------------------------------------------------------------------
-- Fetch and decode the next instruction.
--------------------------------------------------------------------------

fetch :: STArray s Register Address ->  -- registers and pen position
         STArray s Address Int16 ->     -- store
         ST s Instr
    
fetch regs store =
  rArr regs PC                        >>= \instrAddr->
  rArr store instrAddr                >>= \instr->     -- 1st word
  wArr regs PC (instrAddr+1)          >>
  rArr store (instrAddr+1)            >>= \addr->      -- 2nd word
--  return (decode (coerce instr) addr)
  return (decode (fromIntegral instr) addr)
    
--------------------------------------------------------------------------
-- Execute individual instructions and loop or run.
--------------------------------------------------------------------------
exec :: Instr ->                       -- decoded instruction
        STArray s Register Address ->  -- registers and pen position
        STArray s Address Int16 ->     -- store
        STRef s CondCode ->            -- condition code
        STRef s PenStatus ->           -- pen down (True) or up (False)
        STRef s [Int16] ->             -- input file
        STRef s Path ->                -- current line
        ST s Picture

----------
-- Halt --
----------
exec Halt regs store condCode penDown input path =
  rRef path                                     >>= \line->
  return [Line line]

----------
-- Up   --
----------
exec Up regs store condCode penDown input path =
  wRef penDown False                            >> 
  rRef path                                     >>= \line->
  wRef path []                                  >>
  run regs store condCode penDown input path    >>= \lines->
  return (Line line : lines)

----------
-- Down --
----------
exec Down regs store condCode penDown input path =
  wRef penDown True                                 >>
  rRef path                                         >>= \line->
  rArr regs PenX                                    >>= \x->
  rArr regs PenY                                    >>= \y->
  wRef path ((fromIntegral x, fromIntegral y):line) >>
  run regs store condCode penDown input path

----------
-- Move --
----------
exec Move regs store condCode penDown input path =
  rRef penDown                             >>= \marking->
  rArr regs SP                             >>= \tos->
  rArr store tos                           >>= \y->
  rArr store (tos-1)                       >>= \x->
  wArr regs SP (tos-2)                     >>
  wArr regs PenX x                         >>
  wArr regs PenY y                         >>
  (if marking then
      rRef path                            >>= \line->
      wRef path ((fromIntegral x, fromIntegral y):line)
    else return ())                        >>
  run regs store condCode penDown input path

----------
-- Add  --
----------
exec Add regs store condCode penDown input path =
  rArr regs SP                             >>= \tos->
  rArr store tos                           >>= \y->
  rArr store (tos-1)                       >>= \x->
  wArr store (tos-1) (x+y)                 >>
  wArr regs SP (tos-1)                     >>
  run regs store condCode penDown input path

----------
-- Sub  --
----------
exec Sub  regs store condCode penDown input path =
  rArr regs SP                             >>= \tos->
  rArr store tos                           >>= \y->
  rArr store (tos-1)                       >>= \x->
  wArr store (tos-1) (x-y)                 >>
  wArr regs SP (tos-1)                     >>
  run regs store condCode penDown input path

----------
-- Neg  --
----------
exec Neg  regs store condCode penDown input path =
  rArr regs SP                             >>= \tos->
  rArr store tos                           >>= \val->
  wArr store tos (-val)                    >>
  run regs store condCode penDown input path

----------
-- Mul --
----------
exec Mul regs store condCode penDown input path =
  rArr regs SP                             >>= \tos->
  rArr store tos                           >>= \y->
  rArr store (tos-1)                       >>= \x->
  wArr store (tos-1) (x*y)                 >>
  wArr regs SP (tos-1)                     >>
  run regs store condCode penDown input path
  
----------
-- Test --
----------
exec Test regs store condCode penDown input path =
  rArr regs SP                             >>= \tos->
  rArr store tos                           >>= \val->
  (case compare val 0 of
    LT -> wRef condCode N
    EQ -> wRef condCode Z
    GT -> wRef condCode Off )              >>
  run regs store condCode penDown input path
          
----------
-- Pop  --
----------
exec (Pop num) regs store condCode penDown input path =
  -- inc PC for 2nd instr word
  rArr regs PC                             >>= \pc->
  wArr regs PC (pc+1)                      >>
  rArr regs SP                             >>= \tos->
  wArr regs SP (tos - num)                 >>
  run regs store condCode penDown input path

----------
-- Load --
----------
exec (Load reg offset) regs store condCode penDown input path =
  rArr regs SP                             >>= \tos->
  rArr regs reg                            >>= \base->
  rArr store (base + fromIntegral offset)  >>= \val->
  wArr regs SP (tos+1)                     >>
  wArr store (tos+1) val                   >>
  run regs store condCode penDown input path

----------
-- Store --
----------
exec (Store reg offset) regs store condCode penDown input path =
  rArr regs SP                                 >>= \tos->
  rArr regs reg                                >>= \base->
  rArr store tos                               >>= \val->
  wArr regs SP (tos-1)                         >>
  wArr store (base + fromIntegral offset) val  >>
  run regs store condCode penDown input path

----------
-- Read --
----------
exec (Read reg offset) regs store condCode penDown input path =
  rRef input                                   >>= \(n:ns)->
  wRef input ns                                >>
  rArr regs reg                                >>= \base->
  wArr store (base + fromIntegral offset) n    >>
  run regs store condCode penDown input path

----------
-- Jsr  --
----------
exec (Jsr address) regs store condCode penDown input path =
  -- inc PC for 2nd instr word
  rArr regs PC                             >>= \pc->
  rArr regs SP                             >>= \tos->
  rArr regs FP                             >>= \fp->
  wArr store (tos+1) fp                    >>
  wArr store (tos+2) (pc+1)                >>
  wArr regs SP (tos+2)                     >>
  wArr regs FP (tos+2)                     >>
  wArr regs PC address                     >>
  run regs store condCode penDown input path

----------
-- Rts --
----------
exec Rts regs store condCode penDown input path =
  rArr regs FP                             >>= \fp->
  wArr regs SP (fp-2)                      >>
  rArr store fp                            >>= \oldpc->
  wArr regs PC oldpc                       >>
  rArr store (fp-1)                        >>= \oldfp->
  wArr regs FP oldfp                       >>
  run regs store condCode penDown input path

----------
-- Jump --
----------
exec (Jump address) regs store condCode penDown input path =
  wArr regs PC address                     >>
  run regs store condCode penDown input path

----------
-- Jeq  --
----------
exec (Jeq address) regs store condCode penDown input path =
  rRef condCode                            >>= \cc->
  (if cc == Z
     then wArr regs PC address
     else rArr regs PC                     >>= \pc->
          wArr regs PC (pc+1))             >>
  run regs store condCode penDown input path

----------
-- Jlt --
----------
exec (Jlt address) regs store condCode penDown input path =
  rRef condCode                            >>= \cc->
  (if cc == N
     then wArr regs PC address
     else rArr regs PC                     >>= \pc->
          wArr regs PC (pc+1))             >>
  run regs store condCode penDown input path

-----------
-- Loadi --
-----------
exec (Loadi num) regs store condCode penDown input path =
  -- inc PC for 2nd instr word
  rArr regs PC                             >>= \pc->
  wArr regs PC (pc+1)                      >>
  rArr regs SP                             >>= \tos->
  wArr store (tos+1) num                   >>
  wArr regs SP (tos+1)                     >>
  run regs store condCode penDown input path

--------------------------------------------------------------------------
-- Given an (immutable) array representing the stored instructions,
-- and the length of the code segment, set up the machine configuration
-- and run the program.
--------------------------------------------------------------------------
simulate :: Array Address Int16 ->          -- store image with code
            Address ->                      -- end code address
            [Int16] ->                      -- input data file
            Picture
simulate code codeLen nums = runST(
  thawSTArray code         >>= \store->
  newSTArray (PC, PenY) 0  >>= \regs->
  wArr regs GP codeLen     >>
  wArr regs FP codeLen     >>
  wArr regs SP codeLen     >>
  newSTRef Off             >>= \condCode->
  newSTRef False           >>= \penDown->
  newSTRef nums            >>= \input->
  newSTRef []              >>= \path->
  run regs store condCode penDown input path )

--------------------------------------------------------------------------
-- Read the program and data from input files,
-- build the store image,
-- pass it to the simulator
-- then render the resulting picture to a fixed postscript file. 
--------------------------------------------------------------------------

hRunAndDraw :: (Handle, FilePath) -> Handle -> FilePath -> IO()
hRunAndDraw (prog, file) datafile outFileName = 
  hGetContents prog                                         >>= \instrs->
  return (map read (lines instrs))                          >>= \codes->
  return (listArray (0,0x1000) (codes ++ repeat 0))         >>= \code->
  hGetContents datafile                                     >>= \numerals->
  return (map read (lines numerals))                        >>= \nums->
  return (simulate code (fromIntegral (length codes)) nums) >>= \picture->
  drawTo outFileName picture 

--------------------------------------------------------------------------
-- Main with dialogue
--------------------------------------------------------------------------

main 
 = do	args	<- getArgs
 	
	case args of
	 [codeFileName, dataFileName, outFileName] 
	  -> do hCode	<- openFile codeFileName ReadMode
		hData	<- openFile dataFileName ReadMode
		hRunAndDraw (hCode, codeFileName) hData outFileName
	  
	 _ -> 	putStr "usage: PDPlot code.p data.d out.ps\n"
		


