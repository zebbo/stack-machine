import StackMachine

memSize = 512
code = [fromEnum Stk, fromEnum Dsp, 2, fromEnum Adr, -1, fromEnum Lit, 8, fromEnum Sto, fromEnum Stk, fromEnum Prs, 510, fromEnum Adr, -2, fromEnum Val, fromEnum Prn, fromEnum Hlt]
variables = [3,0]
literals = [0,32,61,32,89,0]
initMem = code ++ (replicate (memSize-(length code + length variables + length literals)) 0) ++ variables ++ literals
prog = initState 0 (memSize-(length literals)) (length code) initMem -- pc bp codelen mem

main = do
    emulate prog getLine putStr True
