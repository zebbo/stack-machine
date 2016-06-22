{-# LANGUAGE PatternGuards #-}

module StackMachine.SMAssembler 
(
  assembleAndLoadFile
) where 

import Control.Monad.State
import System.IO

import StackMachine.Emulator

data AssState = Init | Label | SingleByteOp | MultiByteIntOp | MultiByteStringOp | Address | Literal | Comment | Invalid deriving (Show)

type Code = [Int]
type Literals = [Int]

--main = do
--    [fn] <- getArgs
--    (codelen,bp,initmem) <- assembleAndLoadFile fn
--    emulate (initState 0 bp codelen initmem) getLine putStr False

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
assemble = foldl (\(cagg,lagg) x -> let (c,l) = fst $ runState (foldM assembleLine ([],lagg) $ words x) Init in 
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

transition (c,l) (w:xw) MultiByteStringOp
    | w == '\'' = transition ((memSize - 1 - length l):c,l) xw Literal
    | otherwise = ((c,l),Invalid)

transition (c,l) word Literal 
    | ('\''):xw <- rw = ((c, 0:(map fromEnum xw)++l),Comment)
    | otherwise = ((c, (fromEnum ' '):(map fromEnum rw)++l),Literal)
    where rw = reverse word

transition (c,l) _ Comment = ((c,l),Comment)

transition (c,l) _ Invalid = ((c,l), Invalid)
