import StackMachine3

memSize = 512
code = [fromEnum Stk, fromEnum Dsp, 2, fromEnum Adr, -1, fromEnum Lit, 8, fromEnum Sto, fromEnum Stk, fromEnum Prs, 510, fromEnum Adr, -2, fromEnum Val, fromEnum Prn, fromEnum Hlt]
variables = [3,0]
literals = [0,32,61,32,89,0]
--initMem = code ++ (replicate (memSize-(length code + length variables + length literals)) 0) ++ variables ++ literals
initMem1 = initMem memSize code variables literals
prog = initState 0 (memSize-(length literals)) (length code) initMem1 -- pc bp codelen mem


code2 = [fromEnum Dsp, 2, fromEnum Adr, -2, fromEnum Lit, 0, fromEnum Sto, fromEnum Adr, -1, fromEnum Inn, fromEnum Adr, -2, fromEnum Adr, -1, fromEnum Val, fromEnum Adr, -2, fromEnum Val, fromEnum Add, fromEnum Sto, fromEnum Adr, -1, fromEnum Val, fromEnum Lit, 0, fromEnum Eql, fromEnum Bze, 7, fromEnum Prs, 510, fromEnum Adr, -2, fromEnum Val, fromEnum Prn, fromEnum Nln, fromEnum Hlt]
variables2 = [0,0]
literals2 = [0]++(reverse $ map fromEnum "Total is: ")++[0]
initMem2 = initMem memSize code2 variables2 literals2
prog2 = initState 0 (memSize-(length literals2)) (length code2) initMem2 -- pc bp codelen mem

main = do
    emulate prog2 getLine putStr False

initMem :: Int -> [Int] -> [Int] -> [Int] -> [Int]
initMem memSize code variables literals = code ++ (replicate (memSize-(length code + length variables + length literals)) 0) ++ variables ++ literals

