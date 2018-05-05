{-#LANGUAGE MultiParamTypeClasses#-}

{-
This module will provide an interface to allow O(1) manipulation and outputting
of Turtle byte code. This is built around mutable, vectors and it makes
use of the ST monad for this.

This module is probably my favourite part of the code base, it's type generality,
allows for a 'plug and play' backend, in that program memory (the data structure
holding instructions and data) can be any type, so long as has the ability to:
  - marshall Integral types into it's storage type (Word, Int16,...)
  - write instructions to arbitrary locations
  - append instructions
  - output it's self.

Theoretically, any list-like storage type could be used, but since list's have
O(n) access, they are inappropriate, instead there are two storage types
build as vectors of 1) boxed 'Instruction' types, 2) unboxed Int16's.
1) allows for nice debugging output, where as 2) allows for higher performance
machine code output.
-}

module Turtle (
  Marshallable(..),
  PMemory(..),
  Outputter(..),
  Memory,
  Program,
  Instruction(..),
  Int16,
  ST,

  freezeMem,  -- ^stop mutations on memory, assuming memory is Int16's
  freezeProg, -- ^stop mutations on memory, assuming memory is Instruction
  addressSpace,
  runST
              )where

import Prelude

import Analyse (Offset)
import Text.Printf

import Data.Int
import Data.Bits
import Control.Monad.ST
import qualified Data.Vector.Unboxed.Mutable as UM
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Mutable as BM
import qualified Data.Vector as B

{-
These two definitions allow mutating either unboxed int16's or
boxed haskell 'Instruction' types. This module allows both.
-}
-- |A Memory type will be the unboxed vector of Int16's
type Memory s = UM.MVector s Int16
-- |A Program type will be the boxed instruction vector
type Program s = BM.MVector s Instruction

{-
To assist with this generallity, we shall introduce several type classes, which
shall abstract away the details of which memory representation we are using.
Both memory types must implement:
  Marshallable - allows converting an integer into a 'word' of memory
  PMemory - allows allocation of memory, writing Instructions and writing specific
            cells of memory (for back patching etc.)
  Outputter - allows printing the memory to stdout
-}

-- |Marshallable can convert an Integral to something storeable as program data
class Marshallable a where
  marshall :: (Integral b) => b -> a

--simple instance for Int16
instance Marshallable Int16 where
  marshall = fromIntegral

--instruction has a specific type for words of memory (distinguished from instructions)
instance Marshallable Instruction where
  marshall = Word . fromIntegral

{-
With Marshallable data, one can then construct a program memory with a
combination of a vector and some data. This allows programs to be based
on an array of 'Instruction' or an array of 'Int16'
-}
-- |class implements the initialisation of memory, and writing to it.
class Marshallable a => PMemory v a where
  -- ^used to allocate the initial array
  initMem :: ST s (v s a)
  -- ^add an instruction into your memory (automatically converted to a marshallable type)
  insertInstrMem :: Int -> Instruction -> v s a -> ST s Int
  -- ^write a specific cell of memory, do not update the index
  writeAtMem :: Int -> a -> v s a -> ST s ()

instance PMemory BM.MVector Instruction where
  initMem = BM.new addressSpace  --allocate addressSpace of Instructions

  --simple write a single cell with no index update
  writeAtMem ix val vec = BM.write vec ix val

  --Writing instructions involves placing operands in adjacent words in memory
  insertInstrMem ix instr mem = case instr of
    --These instructions have operands which go in adjacent memory cells
    Jsr off -> write (Jsr 0) >> write' (Word off) >> return ix2
    Jump off -> write (Jump 0) >> write' (Word off) >> return ix2
    Jeq off -> write (Jeq 0) >> write' (Word off) >> return ix2
    Jlt off -> write (Jlt 0) >> write' (Word off) >> return ix2
    Loadi off -> write (Loadi 0) >> write' (Word off) >> return ix2
    Pop off -> write (Pop 0) >> write' (Word off) >> return ix2
    --All other instructions have 1 word encodings (including operands)
    _ -> write instr >> return ix1
    where
      ix1 = succ ix
      ix2 = succ ix1
      write = BM.write mem ix
      write' = BM.write mem (succ ix)

{-
This instance takes care of converting instructions to their
binary representation.
-}
instance PMemory UM.MVector Int16 where
  initMem = UM.new addressSpace
  writeAtMem ix val vec = UM.write vec ix val
  insertInstrMem ix instr mem = case instr of
    Halt -> write 0x0000 >> return ix1
    Up   -> write 0x0A00 >> return ix1
    Down -> write 0x0C00 >> return ix1
    Move -> write 0x0E00 >> return ix1
    Add  -> write 0x1000 >> return ix1
    Sub  -> write 0x1200 >> return ix1
    Neg  -> write 0x2200 >> return ix1
    Mul  -> write 0x1400 >> return ix1
    Test -> write 0x1600 >> return ix1
    Rts  -> write 0x2800 >> return ix1
    Word val -> write val >> return ix1
    LoadGP off -> write (0x0600 |: off) >> return ix1
    LoadFP off -> write (0x0700 |: off) >> return ix1
    StoreGP off -> write (0x0400 |: off) >> return ix1
    StoreFP off -> write (0x0500 |: off) >> return ix1
    ReadGP off -> write (0x0200 |: off) >> return ix1
    ReadFP off -> write (0x0300 |: off) >> return ix1
    --immediate instructions with argument in next cell
    Jsr off -> write 0x6800 >> write' off >> return ix2
    Jump off -> write 0x7000 >> write' off >> return ix2
    Jeq off -> write 0x7200 >> write' off >> return ix2
    Jlt off -> write 0x7400 >> write' off >> return ix2
    Loadi off -> write 0x5600 >> write' off >> return ix2
    Pop off -> write 0x5E00 >> write' off >> return ix2
    where
      --helper operator. assists in combining opcodes and byte operands
      (|:) a b = (a .|. (0x00FF .&. b))
      ix1 = succ ix
      ix2 = succ ix1
      write = UM.write mem ix
      write' = UM.write mem (succ ix)

-- |Output a vector of memory to stdout
class Outputter v a where
  output :: (Int, v a) -> IO ()

--Simply print Int16's line by line
instance Outputter U.Vector Int16 where
  output = U.mapM_ print . uncurry U.take

--Convert Instructions into strings and the print.
instance Outputter B.Vector Instruction where
  output (len, vec) = outputProg len vec 0

outputProg :: Int -> B.Vector Instruction -> Int -> IO ()
outputProg len vec ix
  | ix == len = return ()
  | otherwise = outputProg' ix vec >>= outputProg len vec

outputProg' :: Int -> B.Vector Instruction -> IO Int
outputProg' ix vec = do
  putStr $ printf "0x%04x: " ix
  case vec B.! ix of
    --two word instructions involve fetching instruction from next cell
    Jsr _ -> (putStrLn $ printf "Jsr #0x%04x" (wordOff $ vec B.! ix1)) >> return ix2
    Jump _ -> (putStrLn $ printf "Jump #0x%04x" (wordOff $ vec B.! ix1)) >> return ix2
    Jeq  _ -> (putStrLn $ printf "Jeq #0x%04x" (wordOff $ vec B.! ix1)) >> return ix2
    Jlt _  -> (putStrLn $ printf "Jlt #0x%04x" (wordOff $ vec B.! ix1)) >> return ix2
    Loadi _ -> (putStrLn $ printf "Loadi #%d" (wordOff $ vec B.! ix1)) >> return ix2
    Pop _ -> (putStrLn $ printf "Pop #%d" (wordOff $ vec B.! ix1)) >> return ix2
    --simple translation for 1 word instructions with 1 operand
    LoadGP off -> (putStrLn $ printf "Load %d(GP)" off) >> return ix1
    LoadFP off -> (putStrLn $ printf "Load %d(FP)" off) >> return ix1
    StoreGP off-> (putStrLn $ printf "Store %d(GP)" off) >> return ix1
    StoreFP off-> (putStrLn $ printf "Store %d(FP)" off) >> return ix1
    ReadGP off -> (putStrLn $ printf "Read %d(GP)" off) >> return ix1
    ReadFP off -> (putStrLn $ printf "Read %d(FP)" off) >> return ix1
    Word off   -> (putStrLn $ printf "Word %d" off) >> return ix1
    --0 operand instructions have standard encoding given by deriving show
    instr -> (putStrLn $ show instr) >> return ix1
    where
      ix1 = succ ix
      ix2 = succ ix1

data Instruction =
  LoadGP Offset
  | LoadFP Offset
  | Loadi Offset
  | StoreGP Offset
  | StoreFP Offset
  | Pop Offset
  | Add
  | Sub
  | Neg
  | Mul
  | Test
  | Up
  | Down
  | Move
  | ReadGP Offset
  | ReadFP Offset
  | Jump Offset
  | Jeq Offset
  | Jlt Offset
  | Halt
  | Jsr Offset
  | Rts
  | Word {wordOff :: Offset}
  deriving (Show, Eq)

-- |Address space of the program is small enough to allocate a vector of
addressSpace :: (Integral a) => a
addressSpace = 0xFFFF

--Freezeing vectors stops mutations on them and allows them to be viewed
-- |Freeze Memory (vector int16)
freezeMem :: Memory s -> ST s (U.Vector Int16)
freezeMem = U.freeze

-- |Freeze Program (vector Instruction)
freezeProg :: Program s -> ST s (B.Vector Instruction)
freezeProg = B.freeze
