{-# LANGUAGE PatternGuards #-}

module StackMachine.SMAssembler 
(
    assembleAndLoadFile

    -- testing
,   assemble
)   where 

import Control.Monad.State
import Data.Char (isSpace)
import System.IO

import StackMachine.Emulator

data AssState = Init | Label | SingleByteOp | MultiByteIntOp | MultiByteStringOp | Address | Comment | Invalid deriving (Show)

type Code = [Int]
type Literals = [Int]

-- returns codelen, base pointer, mem 
assembleAndLoadFile :: String -> IO (Int,Int,[Int])
assembleAndLoadFile fn = do
    hdl <- openFile fn ReadMode
    sourceCode <- hGetContents hdl
    let (c,l) = assemble sourceCode
        codelen = length c
        bp = memSize - length l
    return (codelen, bp, c ++ replicate (memSize - codelen - (length l)) 0 ++ l)

assemble :: String -> (Code,Literals)
assemble = foldl (\(cagg,lagg) x -> let (c,l) = fst $ runState (foldM assembleLine ([],lagg) $ tokenize x) Init in 
    (cagg++(reverse $ c), l)) ([],[0]) . lines

assembleLine :: (Code,Literals) -> String -> State AssState (Code,Literals)
assembleLine (c,l) word = state $ \s -> transition (c,l) word s

transition :: (Code,Literals) -> String -> AssState -> ((Code,Literals), AssState)

transition (c,l) word Init 
    | [(_,"")] <- tryRead word = ((c,l),Label)
    | word == ";" = ((c,l),Comment)
    | otherwise = ((c,l),Invalid)
    where tryRead w = reads w :: [(Int,String)]

transition (c,l) word Label 
    | [(Prs,"")] <- tryRead word = (((fromEnum Prs):c,l),MultiByteStringOp)
    | [(oc,"")] <- tryRead word = let ocNum = fromEnum oc in 
        if oc `elem` multiByteInstructions then ((ocNum:c,l),MultiByteIntOp) else ((ocNum:c,l),SingleByteOp)
    | otherwise = ((c,l),Invalid)
    where tryRead w = reads w :: [(Opcode,String)]

transition (c,l) word SingleByteOp
    | word == ";" = ((c,l),Comment)
    | otherwise = ((c,l),Invalid)

transition (c,l) word MultiByteIntOp 
    | [(n,"")] <- reads word = ((n:c,l),Address)
    | otherwise = ((c,l),Invalid)

transition (c,l) word Address
    | word == ";" = ((c,l),Comment)
    | otherwise = ((c,l),Invalid)

transition (c,l) word MultiByteStringOp
    | not $ null word = (((memSize - 1 - length l):c, 0:(map fromEnum rw)++l),Comment)
    | otherwise = ((c,l),Invalid)
    where rw = reverse word

transition (c,l) _ Comment = ((c,l),Comment)

transition (c,l) _ Invalid = ((c,l), Invalid)

tokenize :: String -> [String]
tokenize s = case dropWhile isSpace s of
                "" -> []
                ('\'':s') -> w : tokenize s''
                    where (w, ('\'':s'')) = break (=='\'') s'
                s' -> w : tokenize s''
                    where (w, s'') = break isSpace s'
