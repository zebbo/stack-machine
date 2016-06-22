import System.Exit
import Test.HUnit

import Emulator

main = do
    Counts _ _ errors failures <- runTestTT tests
    if errors > 0 || failures > 0 then exitWith $ ExitFailure 1 else exitWith ExitSuccess

testMultibyteInstructions = TestCase (assertEqual "multi byte instructions initialization" 
                                                  ([Adr, Lit, Dsp, Brn, Bze])
                                                  (multiByteInstructions))

tests = TestList [TestLabel "testMultibyteInstructions" testMultibyteInstructions]
