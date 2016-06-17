import Control.Monad.State

import StackMachine3

--[InstrNo -> Opcode [-> Adress]] [-> Comment] (-> InstrNo | -> Comment | -> EndState)

--InitState (-> InstrNo | -> Comment)

data AssState = Label | Mnemonic | Address | Literal | Comment deriving (Show)


--main = do
--    source <- getContents
--

assemble :: String -> [Int]
assemble = concat . reverse . foldl (\agg x -> (reverse $ fst $ runState (foldM assembleLine [] $ words x) Label) : agg) [] . lines

assembleLine :: [Int] -> String -> State AssState [Int]
--assembleLine agg word = state $ \s -> case s of Label -> (agg, if word == ";" then Comment else Mnemonic)
--                                                Mnemonic -> let oc = read word :: Opcode in ((fromEnum oc):agg, 
--                                                    if oc `elem` multiByteInstructions 
--                                                    then Address 
--                                                    else 
--                                                        if oc == Prs
--                                                        then Literal
--                                                        else Comment)
--                                                Address -> ((read word):agg, Comment)
--                                                Literal -> (agg, Comment)
--                                                Comment -> (agg, Comment)

assembleLine agg word = state $ \s -> transition agg word s

transition :: [Int] -> String -> AssState -> ([Int], AssState)

transition agg word Label
    | word == ";" = (agg, Comment)
    | otherwise   = (agg, Mnemonic)

transition agg word Mnemonic
    | oc == Prs                         = (ocNum:agg, Address)
    | oc `elem` multiByteInstructions   = (ocNum:agg, Address)
    | otherwise                         = (ocNum:agg, Comment)
    where oc = read word :: Opcode
          ocNum = fromEnum oc

transition agg word Address = ((read word):agg, Comment)

transition agg word Comment = (agg, Comment)
