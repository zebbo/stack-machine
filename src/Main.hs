import StackMachine.Emulator
import StackMachine.SMAssembler

import System.Environment

main = do
    [fn] <- getArgs
    (codelen,bp,initmem) <- assembleAndLoadFile fn
    emulate (initState 0 bp codelen initmem) getLine putStr False
