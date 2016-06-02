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
            | Hlt | Inn | Prn | Nln | Nop | Nul deriving (Show, Eq, Enum, Bounded)

data ProgramState = Running | Finished | Badmem | Baddata | Nodata | Divzero | Badop | Badind deriving (Eq, Enum)

instance Show ProgramState where
    show Running = "Running"
    show Finished = "Finished"
    show Badmem = "Memory violation"
    show Baddata = "Invalid data"
    show Nodata = "No more data"
    show Divzero = "Division by zero"
    show Badop = "Illegal opcode"
    show Badind = "Subscript out of range"

--memSize = 512

initState :: Int -> Int -> Int -> [Int] -> State
initState initpc initbp cl initmem = State { ps = Running, pc = initpc, bp = initbp, sp = initbp, codelen = cl, mem = initmem }

emulate :: State -> IO String -> (String -> IO ()) -> Bool -> IO ()
emulate st@(State ps pc bp sp codelen mem) input output tracing = do
    if (mem!!pc > fromEnum Nul) 
    then do when tracing (trace st); handleError (State Badop pc bp sp codelen mem)
    else do 
        let ir = toEnum (mem!!pc)
        when tracing (trace st)
        when (ir `elem` [Prs, Stk, Prn]) (executeIO st ir)
        let st'@(State ps' _ _ _ _ _) = execute st $ ir
        case ps' of Running -> emulate st' input output tracing
                    Finished -> return ()
                    _ -> handleError st'    

execute :: State -> Opcode -> State
execute st@(State ps pc bp sp codelen mem) ir
    | ir == Adr = if inbounds (sp-1) st 
                  then State ps (pc+2) bp (sp-1) codelen (setMem (sp-1,bp+mem!!(pc+1)) mem)
                  else State Badmem pc bp (sp-1) codelen mem 
    | ir == Lit = if inbounds (sp-1) st
                  then State ps (pc+2) bp (sp-1) codelen (setMem (sp-1,mem!!(pc+1)) mem)
                  else State Badmem pc bp (sp-1) codelen mem
    | ir == Dsp = if inbounds (sp-(mem!!(pc+1))) st
                  then State ps (pc+2) bp (sp-(mem!!(pc+1))) codelen mem
                  else State Badmem pc bp (sp-(mem!!(pc+1))) codelen mem
    | ir == Brn = State ps (mem!!(pc+1)) bp sp codelen mem
    | ir == Bze = if inbounds (sp+1) st
                  then if tos == 0 
                       then State ps (mem!!(pc+1)) bp (sp+1) codelen (setMem (sp,0) mem)
                       else State ps (pc+2) bp (sp+1) codelen (setMem (sp,0) mem)
                  else State Badmem pc bp (sp+1) codelen mem
    | ir == Prs = State ps (pc+2) bp sp codelen mem
    | ir == Add = if inbounds (sp+1) st
                  then let sos = mem!!(sp+1) in State ps (pc+1) bp (sp+1) codelen (setMem (sp, 0) . setMem (sp+1, sos + tos) $ mem)
                  else State Badmem pc bp (sp+1) codelen mem
    | ir == Sub = if inbounds (sp+1) st
                  then let sos = mem!!(sp+1) in State ps (pc+1) bp (sp+1) codelen (setMem (sp, 0) . setMem (sp+1, sos - tos) $ mem)
                  else State Badmem pc bp (sp+1) codelen mem
    | ir == Mul = if inbounds (sp+1) st
                  then let sos = mem!!(sp+1) in State ps (pc+1) bp (sp+1) codelen (setMem (sp, 0) . setMem (sp+1, sos * tos) $ mem)
                  else State Badmem pc bp (sp+1) codelen mem
    | ir == Dvd = if tos == 0
                  then State Divzero pc bp sp codelen mem
                  else if inbounds (sp+1) st
                       then let sos = mem!!(sp+1) in State ps (pc+1) bp (sp+1) codelen (setMem (sp, 0) . setMem (sp+1, sos `div` tos) $ mem)
                       else State Badmem pc bp (sp+1) codelen mem
    | ir == Eql = if inbounds (sp+1) st
                  then let sos = mem!!(sp+1) in State ps (pc+1) bp (sp+1) codelen (setMem (sp, 0) . setMem (sp+1, if sos == tos then 1 else 0) $ mem)
                  else State Badmem pc bp (sp+1) codelen mem
    | ir == Neq = if inbounds (sp+1) st
                  then let sos = mem!!(sp+1) in State ps (pc+1) bp (sp+1) codelen (setMem (sp, 0) . setMem (sp+1, if sos /= tos then 1 else 0) $ mem)
                  else State Badmem pc bp (sp+1) codelen mem
    | ir == Lss = if inbounds (sp+1) st
                  then let sos = mem!!(sp+1) in State ps (pc+1) bp (sp+1) codelen (setMem (sp, 0) . setMem (sp+1, if sos < tos then 1 else 0) $ mem)
                  else State Badmem pc bp (sp+1) codelen mem
    | ir == Gtr = if inbounds (sp+1) st
                  then let sos = mem!!(sp+1) in State ps (pc+1) bp (sp+1) codelen (setMem (sp, 0) . setMem (sp+1, if sos > tos then 1 else 0) $ mem)
                  else State Badmem pc bp (sp+1) codelen mem
    | ir == Leq = if inbounds (sp+1) st
                  then let sos = mem!!(sp+1) in State ps (pc+1) bp (sp+1) codelen (setMem (sp, 0) . setMem (sp+1, if sos <= tos then 1 else 0) $ mem)
                  else State Badmem pc bp (sp+1) codelen mem
    | ir == Geq = if inbounds (sp+1) st
                  then let sos = mem!!(sp+1) in State ps (pc+1) bp (sp+1) codelen (setMem (sp, 0) . setMem (sp+1, if sos >= tos then 1 else 0) $ mem)
                  else State Badmem pc bp (sp+1) codelen mem
    | ir == Neg = State ps (pc+1) bp sp codelen (setMem (sp,-tos) mem)
    | ir == Val = if inbounds sp st && inbounds tos st
                  then State ps (pc+1) bp sp codelen (setMem (sp,mem!!tos) mem)
                  else State Badmem pc bp sp codelen mem
    | ir == Sto = if inbounds (sp+1) st && inbounds (mem!!(sp+1)) st
                  then let sos = mem!!(sp+1) in State ps (pc+1) bp (sp+2) codelen (setMem (sp, 0) . setMem(sp+1,0) . setMem(sos,tos) $ mem)
                  else State Badmem pc bp (sp+1) codelen mem
    | ir == Ind = if inbounds (sp+2) st
                  then let size = mem!!sp 
                           tos' = mem!!(sp+1) 
                           sos' = mem!!(sp+2) 
                       in if tos' `elem` [0..size-1]
                          then State ps (pc+1) bp sp codelen (setMem (sp,0) . setMem (sp+1,0) . setMem (sp+2,sos'-tos') $ mem)
                          else State Badind pc bp sp codelen mem
                  else State Badmem pc bp (sp+2) codelen mem
    | ir == Stk = State ps (pc+1) bp sp codelen mem
    | ir == Hlt = State Finished pc bp sp codelen mem
    | ir == Inn = if inbounds (tos) st 
                  then State ps (pc+1) bp (sp+1) codelen (setMem (sp,0) . setMem (tos,1) $ mem) -- TODO: Acutally read int
                  else State Badmem pc bp sp codelen mem
    | ir == Prn = if inbounds (sp+1) st
                  then State ps (pc+1) bp (sp+1) codelen (setMem (sp,0) mem)
                  else State Badmem pc bp (sp+1) codelen mem
    | ir == Nln = State ps (pc+1) bp sp codelen mem
    | ir == Nop = State ps (pc+1) bp sp codelen mem
    | ir == Nul = State Badop pc bp sp codelen mem
    | otherwise = State Badop pc bp sp codelen mem
    where tos = mem!!sp

executeIO :: State -> Opcode -> IO ()
executeIO st@(State ps pc bp sp codelen mem) ir
    | ir == Prs = execPrs (mem!!(pc+1)) mem
    | ir == Stk = stackdump st
    | ir == Prn = putStr $ show tos
    | ir == Nln = putStr "\n"
    where tos = mem!!sp

handleError :: State -> IO ()
handleError (State ps pc _ _ _ _) = putStrLn $ "ERROR: " ++ show ps ++ " at: " ++ show pc

inbounds :: Int -> State -> Bool
inbounds p (State ps pc bp sp codelen mem) = p < length mem && p >= codelen

execPrs :: Int -> [Int] -> IO ()
execPrs add mem = when (mem!!add /= 0) (do putChar (toEnum $ mem!!add :: Char); execPrs (add-1) mem;)

setMem :: (Int, Int) -> [Int] -> [Int]
setMem (i,v) mem = take i mem ++ [v] ++ drop (i+1) mem

trace :: State -> IO ()
trace (State ps pc bp sp codelen mem) = do
    putStr "\nDEBUG"
    putStr $ " pc: "++(show pc)++" bp: "++(show bp)++"  sp: "++(show sp)++" tos: "
    if (sp < length mem) then (putStrLn $ show (mem!!sp)) else putStrLn "????"

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
                    && n `elem` map fromEnum ([minBound..maxBound] :: [Opcode])
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
                    return (i+1,
                        if ((not instrParam) 
                            && i < codelen 
                            && n `elem` map fromEnum ([minBound..maxBound] :: [Opcode])
                            && (toEnum n) `elem` [Adr,Dsp,Lit,Prs]
                           ) 
                        then True 
                        else False 
                        )) (0,False) mem


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
