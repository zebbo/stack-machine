import StackMachine.Emulator
import StackMachine.SMAssembler

import System.Environment

main = do
    args <- getArgs
    case args of
        ["--help"] -> usage
        ("run":file:[]) -> run file
        _ -> usage

run :: String -> IO ()
run file = do
    (codelen,bp,initmem) <- assembleAndLoadFile file
    emulate (initState 0 bp codelen initmem) getLine putStr False
    return ()

usage :: IO()
usage = do
    self <- getProgName
    putStr . unlines $ 
        concat ["Usage: ", self, " [OPTIONS]"] :
        "--help                 Print this message" :
        "run <file>             Assemble, load and execute code in <file>" :
        []
