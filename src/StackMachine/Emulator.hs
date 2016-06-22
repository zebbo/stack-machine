module StackMachine.Emulator
( Opcode (..)
, multiByteInstructions
, memSize
, initState
, emulate
)
where
import Control.Monad (foldM_, when)

data SMState = SMState {ir :: Opcode, ps :: ProgramState, pc :: Int, bp :: Int, sp :: Int, codelen :: Int, mem :: [Int]} deriving (Show)

data Opcode = Adr | Lit | Dsp | Brn | Bze | Prs | Add
            | Sub | Mul | Dvd | Eql | Neq | Lss | Geq
            | Gtr | Leq | Neg | Val | Sto | Ind | Stk
            | Hlt | Inn | Prn | Nln | Nop | Nul deriving (Show, Eq, Enum, Bounded, Ord, Read)

multiByteInstructions = [Adr, Lit, Dsp, Brn, Bze]
memSize = 256 :: Int

data ProgramState = Running | Finished | Badmem | Baddata | Nodata | Divzero | Badop | Badind deriving (Eq, Enum, Show)

initState :: Int -> Int -> Int -> [Int] -> SMState
initState initpc initbp cl initmem = SMState { ir = Nop, ps = Running, pc = initpc, bp = initbp, sp = initbp, codelen = cl, mem = initmem }

emulate :: SMState -> IO String -> (String -> IO ()) -> Bool -> IO()
emulate st input output tracing = do
    let stf = fetch st
    inp <- preExecIO stf input output tracing
    let stx = stf >>= execute inp
    postExecIO stx output
    case stx of Right newst -> emulate newst input output tracing
                Left  newst -> handleDead newst output

-- dry run, maybe useful for testing
--run :: SMState -> Either SMState SMState
--run st = return st >>= fetch >>= execute >>= run

fetch :: SMState -> Either SMState SMState
fetch (SMState ir ps pc bp sp codelen mem)
    | pc < codelen && opNum `elem` opNumRange = Right (SMState (toEnum opNum) ps (pc+1) bp sp codelen mem)
    | otherwise = Left (SMState ir Badop pc bp sp codelen mem)
    where opNum = mem!!pc
          opNumRange = map fromEnum ([minBound..maxBound] :: [Opcode])

execute :: Int -> SMState -> Either SMState SMState
execute input st@(SMState ir ps pc bp sp codelen mem) 
    | ir == Adr = return st >>= inbounds (sp-1) >>= push (bp+mem!!(pc)) >>= incPC
    | ir == Lit = return st >>= inbounds (sp-1) >>= push (mem!!pc) >>= incPC
    | ir == Dsp = return st >>= inbounds (sp-mem!!pc) >>= decSP (mem!!pc) >>= incPC
    | ir == Brn = return st >>= setPC (mem!!pc)
    | ir == Bze = return st >>= inbounds (sp+1) >> let (val, newst) = pop st in return newst >>= if val == 0 then setPC (mem!!pc) else incPC
    | ir == Prs = return st >>= incPC
    | ir == Add = return st >>= inbounds (sp+1) >> let (tos, stack') = pop st; (sos, stack'') = pop stack' in return stack'' >>= push (sos+tos)
    | ir == Sub = return st >>= inbounds (sp+1) >> let (tos, stack') = pop st; (sos, stack'') = pop stack' in return stack'' >>= push (sos-tos)
    | ir == Mul = return st >>= inbounds (sp+1) >> let (tos, stack') = pop st; (sos, stack'') = pop stack' in return stack'' >>= push (sos*tos)
    | ir == Dvd = let (tos, stack') = pop st in return stack' >>= nonZero tos >>= inbounds (sp+1) >> 
                    let (sos, stack'') = pop stack' in return stack'' >>= push (sos `div` tos)
    | ir == Eql = return st >>= inbounds (sp+1) >> 
                    let (tos, stack') = pop st; (sos, stack'') = pop stack' in return stack'' >>= push (if sos==tos then 1 else 0)
    | ir == Neq = return st >>= inbounds (sp+1) >> 
                    let (tos, stack') = pop st; (sos, stack'') = pop stack' in return stack'' >>= push (if sos/=tos then 1 else 0)
    | ir == Lss = return st >>= inbounds (sp+1) >> 
                    let (tos, stack') = pop st; (sos, stack'') = pop stack' in return stack'' >>= push (if sos<tos then 1 else 0)
    | ir == Gtr = return st >>= inbounds (sp+1) >> 
                    let (tos, stack') = pop st; (sos, stack'') = pop stack' in return stack'' >>= push (if sos>tos then 1 else 0)
    | ir == Leq = return st >>= inbounds (sp+1) >> 
                    let (tos, stack') = pop st; (sos, stack'') = pop stack' in return stack'' >>= push (if sos<=tos then 1 else 0)
    | ir == Geq = return st >>= inbounds (sp+1) >> 
                    let (tos, stack') = pop st; (sos, stack'') = pop stack' in return stack'' >>= push (if sos>=tos then 1 else 0)
    | ir == Neg = let (tos, stack') = pop st in return stack' >>= push (-tos)
    | ir == Val = return st >>= inbounds sp >>
                    let (tos, stack') = pop st in return stack' >>= inbounds tos >>= push (mem!!tos)
    | ir == Sto = return st >>= inbounds (sp+1) >>= inbounds (mem!!(sp+1)) >>
                    let (tos, stack') = pop st; (sos, stack'') = pop stack' in return stack'' >>= alterMem (sos, tos)
    | ir == Ind = return st >>= inbounds (sp+2) >>
                    let (size, stack') = pop st; (idx, stack'') = pop stack'; (offset, stack''') = pop stack'' in 
                        return stack''' >>= validIdx idx size >>= push (offset-idx)
    | ir == Stk = return st
    | ir == Hlt = Left (SMState ir Finished pc bp sp codelen mem)
    | ir == Inn = let (val, newst) = pop st in return newst >>= alterMem (val,input)
    | ir == Prn = return st >>= inbounds (sp+1)
    | ir == Nln = return st
    | ir == Nul = Left (SMState ir Badop pc bp sp codelen mem)
    | otherwise = Left st

preExecIO :: Either SMState SMState -> IO String -> (String -> IO()) -> Bool -> IO Int
preExecIO st input output tracing = do when tracing (trace st output); preExecOutput st output; preExecInput st input

preExecInput :: Either SMState SMState -> IO String -> IO Int
preExecInput (Right (SMState Inn _ _ _ _ _ _)) inp = do l <- inp; return (read l)
preExecInput _ _ = return 0

preExecOutput :: Either SMState SMState -> (String -> IO()) -> IO ()
preExecOutput (Right (SMState Prs _ pc _ _ _ mem)) output = output $ execPrs (mem!!pc) mem
preExecOutput _ _ = return ()

postExecIO :: Either SMState SMState -> (String -> IO()) -> IO ()
postExecIO (Right st@(SMState ir ps pc bp sp codelen mem)) output
    | ir == Stk = stackdump st output
    | ir == Prn = let tos = mem!!sp in output (show tos)
    | ir == Nln = output "\n"
postExecIO _ _ = return ()

handleDead :: SMState -> (String -> IO ()) -> IO()
handleDead st@(SMState _ Finished _ _ _ _ _) _ = return ()
handleDead st output = output (show st ++ "\n")

push :: Int -> SMState -> Either SMState SMState
push val (SMState ir ps pc bp sp codelen mem) = Right (SMState ir ps pc bp (sp-1) codelen (setMem (sp-1,val) mem))

pop :: SMState -> (Int, SMState)
pop (SMState ir ps pc bp sp codelen mem) = let val = mem!!sp in (val, SMState ir ps pc bp (sp+1) codelen mem)

incPC :: SMState -> Either SMState SMState
incPC (SMState ir ps pc bp sp codelen mem) = Right (SMState ir ps (pc+1) bp sp codelen mem)

setPC :: Int -> SMState -> Either SMState SMState
setPC newPC (SMState ir ps pc bp sp codelen mem) = Right (SMState ir ps newPC bp sp codelen mem)

decSP :: Int -> SMState -> Either SMState SMState
decSP dec (SMState ir ps pc bp sp codelen mem) = Right (SMState ir ps pc bp (sp-dec) codelen mem)

inbounds :: Int -> SMState -> Either SMState SMState
inbounds p st@(SMState ir ps pc bp sp codelen mem)
    | p < length mem && p >= codelen = Right st
    | otherwise = Left (SMState ir Badmem pc bp sp codelen mem)

nonZero :: Int -> SMState -> Either SMState SMState
nonZero n st@(SMState ir ps pc bp sp codelen mem)
    | n /= 0 = Right st
    | otherwise = Left (SMState ir Divzero pc bp sp codelen mem)

validIdx :: Int -> Int -> SMState -> Either SMState SMState
validIdx idx size st@(SMState ir ps pc bp sp codelen mem)
    | idx `elem` [0..size-1] = Right st
    | otherwise = Left (SMState ir Badind pc bp sp codelen mem)

setMem :: (Int, Int) -> [Int] -> [Int]
setMem (i,v) mem = take i mem ++ [v] ++ drop (i+1) mem

alterMem :: (Int, Int) -> SMState -> Either SMState SMState
alterMem (pos, val) st@(SMState ir ps pc bp sp codelen mem) = return (SMState ir ps pc bp sp codelen (setMem (pos,val) mem))

execPrs :: Int -> [Int] -> String
execPrs add mem
    | mem!!add /= 0 = (toEnum $ mem!!add :: Char):(execPrs (add-1) mem)
    | otherwise = []

trace :: Either SMState SMState -> (String -> IO()) -> IO ()
trace (Right (SMState ir ps pc bp sp codelen mem)) output = do
    output "\nDEBUG"
    output $ " ir: "++(show ir)++" pc: "++(show pc)++" bp: "++(show bp)++" sp: "++(show sp)++" tos: "
    if (sp < length mem) then (output $ show (mem!!sp)) else output "????"
    output "\n"
trace (Left (SMState ir ps pc bp sp codelen mem)) output = do
    output "\nDEBUG"
    output $ " ir: "++(show ir)++" pc: "++(show pc)++" bp: "++(show bp)++" sp: "++(show sp)++" tos: "
    if (sp < length mem) then (output $ show (mem!!sp)) else output "????"
    output "\n"

printMem :: Int -> [Int] -> (String -> IO ()) -> IO ()
printMem codelen mem output = do
    output $ replicate 8 ' '
    output " | "
    mapM_ (\n -> output $ replicate 4 ' ' ++ ['0'] ++ (if(n < 10) then show n else [toEnum ((n `mod` 10)+65)])) [0..15]
    output "\n"
    output $ replicate (16*6+11) '-'
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
                        output "\n"
                        output $ (replicate 2 ' ')
                               ++ ['0']
                               ++ (let i' = (i `div` (16^2)) in (if i' < 10 then show i' else [toEnum $ (i' `mod` 10) + 65]))
                        output $ (replicate 2 ' ')
                               ++ (let i' = (i `div` 16) `mod` 16 in (if i' < 10 then show i' else [toEnum $ (i' `mod` 10) + 65]))
                               ++ ['0']
                        output " | "
                        )
                    output $ (replicate (padding-(length sym)) ' ')++sym
                    return (i+1,
                        if ((not instrParam)
                            && i < codelen
                            && n `elem` map fromEnum ([minBound..maxBound] :: [Opcode])
                            && (toEnum n) `elem` [Adr,Dsp,Lit,Prs]
                           )
                        then True
                        else False
                        )) (0,False) mem


stackdump :: SMState -> (String -> IO()) -> IO ()
stackdump (SMState ir ps pc bp sp codelen mem) output = do
    output "\n\n"
    output $ (replicate 20 '*') ++ (replicate 10 ' ') ++ "Stack Dump at: " ++ show pc ++ (replicate 10 ' ') ++ (replicate 20 '*')
    output "\n\n"
    output $ replicate 5 ' '
    output $ "ir: "++(show ir)++"  "
    output $ "ps: "++(show ps)++"  "
    output $ "pc: "++(show pc)++"  "
    output $ "bp: "++(show bp)++"  "
    output $ "sp: "++(show sp)++"  "
    output "\n\n"
    printMem codelen mem output
    output "\n"
