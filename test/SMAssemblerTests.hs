import System.Exit
import Test.HUnit

import StackMachine.Emulator (Opcode(..),memSize)
import StackMachine.SMAssembler

main = do
    Counts _ _ errors failures <- runTestTT tests
    if errors > 0 || failures > 0 then exitWith $ ExitFailure 1 else exitWith ExitSuccess

testAdr = TestCase $ assertEqual "test Adr assembly" 
                                ([fromEnum Adr,-2],[0])
                                (assemble "0 Adr -2 ; some comment")

testLit = TestCase $ assertEqual "test Lit assembly"
                                ([fromEnum Lit,0],[0])
                                (assemble "0 Lit 0 ; some comment")

testPrsA = TestCase $ assertEqual "test Prs assembly - A"
                                  ([fromEnum Prs,memSize-2], [0,fromEnum 'c',fromEnum 'b',fromEnum 'a',0])
                                  (assemble "0 Prs 'abc'")

testPrsB = TestCase $ assertEqual "test Prs assembly - B"
                                  ([fromEnum Prs,memSize-2], [0,fromEnum 'c',fromEnum 'b',fromEnum 'a',fromEnum ' ',0])
                                  (assemble "0 Prs ' abc'")

testPrsC = TestCase $ assertEqual "test Prs assembly - C"
                                  ([fromEnum Prs,memSize-2], [0,fromEnum ' ', fromEnum 'c',fromEnum 'b',fromEnum 'a',0])
                                  (assemble "0 Prs 'abc '")

testPrsD = TestCase $ assertEqual "test Prs assembly - D"
                                  ([fromEnum Prs,memSize-2], [0,fromEnum 'c',fromEnum 'b',fromEnum ' ',fromEnum 'a',0])
                                  (assemble "0 Prs 'a bc'")

testPrsE = TestCase $ assertEqual "test Prs assembly - E"
                                  ([fromEnum Prs,memSize-2], [0,fromEnum ' ',fromEnum 'c',fromEnum 'b',fromEnum 'a',fromEnum ' ',0])
                                  (assemble "0 Prs ' abc '")

testPrsF = TestCase $ assertEqual "test Prs assembly - F"
                                  ([fromEnum Prs,memSize-2], [0,fromEnum 'c',fromEnum 'b',fromEnum ' ', fromEnum ' ',fromEnum 'a',0])
                                  (assemble "0 Prs 'a  bc'")
testPrs = TestList [ testPrsA
                   , testPrsB
                   , testPrsC
                   , testPrsD
                   , testPrsE
                   , testPrsF
                   ]

tests = TestList [ TestLabel "testAdrAssembly" testAdr
                 , TestLabel "testLitAssembly" testLit
                 , TestLabel "testPrsAssembly" testPrs
                 ]
