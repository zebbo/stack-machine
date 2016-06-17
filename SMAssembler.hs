import Control.Monad.State
import System.IO

import StackMachine

data AssState = Init | Label | SingleByteOp | MultiByteIntOp | MultiByteStringOp | Address | Literal | Comment | Invalid deriving (Show)

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
assemble = foldl (\(cagg,lagg) x -> let (c,l) = fst $ runState (foldM assembleLine ([],lagg) $ words x) Init in 
    (cagg++(reverse $ c), l)) ([],[0]) . lines

assembleLine :: (Code,Literals) -> String -> State AssState (Code,Literals)
assembleLine (c,l) word = state $ \s -> transition (c,l) word s

transition :: (Code,Literals) -> String -> AssState -> ((Code,Literals), AssState)

transition (c,l) word Init = case reads word :: [(Int,String)] of 
     [(_,"")] -> ((c,l),Label)
     _ -> if word == ";" 
        then ((c,l),Comment) 
        else ((c,l),Invalid)

transition (c,l) word Label = case reads word :: [(Opcode,String)] of 
    [(oc,"")] -> let ocNum = fromEnum oc in
        if oc == Prs 
        then ((ocNum:c,l),MultiByteStringOp)
        else
            if oc `elem` multiByteInstructions 
            then ((ocNum:c,l),MultiByteIntOp)
            else ((ocNum:c,l),SingleByteOp)
    _ -> ((c,l),Invalid)

transition (c,l) word SingleByteOp
    | word == ";" = ((c,l),Comment)
    | otherwise = ((c,l),Invalid)

transition (c,l) word MultiByteIntOp = case reads word of 
    [(n,"")] -> ((n:c,l),Address)
    _ -> ((c,l),Invalid)

transition (c,l) word Address
    | word == ";" = ((c,l),Comment)
    | otherwise = ((c,l),Invalid)

transition (c,l) (w:xw) MultiByteStringOp
    | w == '\'' = transition ((memSize - 1 - length l):c,l) xw Literal
    | otherwise = ((c,l),Invalid)

transition (c,l) word Literal = let rw = reverse word in
    case rw of
        ('\''):xw -> ((c, 0:(map fromEnum xw)++l),Comment)
        _ -> ((c, (fromEnum ' '):(map fromEnum rw)++l),Literal)

transition (c,l) _ Comment = ((c,l),Comment)

transition (c,l) _ Invalid = ((c,l), Invalid)
