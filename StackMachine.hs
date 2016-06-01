module StackMachine
( Opcode (..)
, initState
, emulate
) where

import Control.Monad

data State = State {ps :: ProgramState, pc :: Int, bp :: Int, sp :: Int, codelen :: Int, mem :: [Int]}

data Opcode = Adr | Lit | Dsp | Brn | Bze | Prs | Add 
            | Sub | Mul | Dvd | Eql | Neq | Lss | Geq 
            | Gtr | Leq | Neg | Val | Sto | Ind | Stk
            | Hlt | Inn | Prn | Nln | Nop | Nul deriving (Show, Eq, Enum)

data ProgramState = Running | Finished | Badmem | Baddata | Nodata | Divzero | Badop | Badind deriving (Show, Eq, Enum)

--memSize = 512

initState :: Int -> Int -> Int -> [Int] -> State
initState initpc initbp cl initmem = State { ps = Running, pc = initpc, bp = initbp, sp = initbp, codelen = cl, mem = initmem }

emulate :: State -> IO String -> (String -> IO ()) -> IO ()
emulate st@(State ps pc bp sp codelen mem) input output = do
    when (ir `elem` [Prs, Stk, Prn]) (executeIO st ir)
    let st'@(State ps _ _ _ _ _) = execute st $ ir
    case ps of Running -> emulate st' input output
               Finished -> stackdump st'
               _ -> handleError st'
    where ir = toEnum (mem!!pc)

execute :: State -> Opcode -> State
execute (State ps pc bp sp codelen mem) ir
    | ir == Adr = State ps (pc+2) bp (sp-1) codelen (setMem (sp-1,bp+mem!!(pc+1)) mem)
    | ir == Dsp = State ps (pc+2) bp (sp-(mem!!(pc+1))) codelen mem
    | ir == Lit = State ps (pc+2) bp (sp-1) codelen (setMem (sp-1,mem!!(pc+1)) mem)
    | ir == Sto = State ps (pc+1) bp (sp+2) codelen (setMem (sp, 0) . setMem(sp+1,0) . setMem(sos,tos) $ mem)
    | ir == Val = State ps (pc+1) bp sp codelen (setMem (sp,mem!!tos) mem)
    | ir == Prs = State ps (pc+2) bp sp codelen mem
    | ir == Stk = State ps (pc+1) bp sp codelen mem
    | ir == Prn = State ps (pc+1) bp (sp+1) codelen (setMem (sp,0) mem)
    | ir == Hlt = State Finished pc bp sp codelen mem
    where tos = mem!!sp
          sos = mem!!(sp+1)

executeIO :: State -> Opcode -> IO ()
executeIO st@(State ps pc bp sp codelen mem) ir
    | ir == Prs = execPrs (mem!!(pc+1)) mem
    | ir == Stk = stackdump st
    | ir == Prn = putStr $ show tos
    where tos = mem!!sp
          sos = mem!!(sp+1)

handleError :: State -> IO ()
handleError st@(State ps pc bp sp codelen mem) = do putStrLn $ "ERROR: " ++ show ps ; stackdump st

execPrs :: Int -> [Int] -> IO ()
execPrs add mem = when (mem!!add /= 0) (do putChar (toEnum $ mem!!add :: Char); execPrs (add-1) mem;)

setMem :: (Int, Int) -> [Int] -> [Int]
setMem (i,v) mem = take i mem ++ [v] ++ drop (i+1) mem

printMem :: Int -> [Int] -> IO ()
printMem codelen mem = do
    putStr $ replicate 8 ' '
    putStr " | "
    mapM_ (\n -> putStr $ replicate 4 ' ' ++ ['0'] ++ (if(n < 10) then show n else [toEnum ((n `mod` 10)+65)])) [0..15]
    putStr "\n"
    putStr $ replicate (16*6+11) '-'
    foldM_ (\(i,instrParam) n -> 
        let sym = (
                if (not instrParam)
                    && i < codelen 
                then show (toEnum n :: Opcode) 
                else show n
                )
            padding = 6 in do 
                    when (i `mod` 16 == 0) $ (do 
                        putStr "\n" 
                        putStr $ (replicate 2 ' ') 
                               ++ ['0'] 
                               ++ (let i' = (i `div` (16^2)) in (if i' < 10 then show i' else [toEnum $ (i' `mod` 10) + 65]))
                        putStr $ (replicate 2 ' ') 
                               ++ (let i' = (i `div` 16) `mod` 16 in (if i' < 10 then show i' else [toEnum $ (i' `mod` 10) + 65])) 
                               ++ ['0']
                        putStr " | " 
                        )
                    putStr $ (replicate (padding-(length sym)) ' ')++sym 
                    return (i+1,if ((not instrParam) && i < codelen && (toEnum n) `elem` [Adr,Dsp,Lit,Prs]) then True else False )) (0,False) mem


stackdump :: State -> IO ()
stackdump (State ps pc bp sp codelen mem) = do
    putStr "\n\n"
    putStr $ (replicate 20 '*') ++ (replicate 10 ' ') ++ "Stack Dump at: " ++ show pc ++ (replicate 10 ' ') ++ (replicate 20 '*')
    putStr "\n\n"
    putStr $ replicate 5 ' '
    putStr $ "ps: "++(show ps)++"  "
    putStr $ "pc: "++(show pc)++"  "
    putStr $ "bp: "++(show bp)++"  "
    putStr $ "sp: "++(show sp)++"  "
    putStr "\n\n"
    printMem codelen mem
    putStr "\n"
