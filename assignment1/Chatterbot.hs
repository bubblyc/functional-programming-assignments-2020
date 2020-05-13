module Chatterbot where
import Utilities
import System.Random
import Data.Char
import Data.Maybe

--to run: load Chatterbot.hs and Eliza.hs
-- then: chatterbot "Eliza" eliza
--to test: ???

chatterbot :: String -> [(String, [String])] -> IO ()
chatterbot botName botRules = do
    putStrLn ("\n\nHi! I am " ++ botName ++ ". How are you?")
    botloop
  where
    brain = rulesCompile botRules
    botloop = do
      putStr "\n: "
      question <- getLine
      answer <- stateOfMind brain
      putStrLn (botName ++ ": " ++ (present . answer . prepare) question)
      if (not . endOfDialog) question then botloop else return ()

--------------------------------------------------------

type Phrase = [String]
type PhrasePair = (Phrase, Phrase)
type BotBrain = [(Phrase, [Phrase])]

--------------------------------------------------------

--Why can't I do middle steps, why does every return statement have to be same type!??? As for example:
-- answers <- pickAnswers r
stateOfMind :: BotBrain -> IO (Phrase -> Phrase)
stateOfMind brain = do    --brain is a list of tuples, (Phrase, [Phrase])
  r <- randomIO :: IO Float
  return . rulesApply $ pickAnswers r brain

-- picks an answer for every question, if applied to brain, returns list of tuple [(Phrase, Phrase)], i.e. [PhrasePair]
-- takes a random variable and a BotBrain as argument
pickAnswers :: RealFrac r => r -> [(b, [d])] -> [(b, d)]
pickAnswers r = (map . map2) (id, pick r)

-- Takes a pair of phrases (like many frenchPresentation) and a phrase from user
-- Returns a phrase according to the application of for example frenchPresentation-tuples
rulesApply :: [PhrasePair] -> Phrase -> Phrase
--rulesApply = try . transformationsApply "*" reflect    --If no match, answers with the same phrase as user, "echo eliza"
rulesApply = (maybe [] id .) . (transformationsApply "*" reflect) -- what does the dot do? If no match, doesn't answer, "rude eliza"


-- With help of reflections below, changes each word in the phrase to its matching word, if it has one
-- "lookup key assocs" looks up a key in an association list.
reflect :: Phrase -> Phrase
reflect = map $ try $ flip lookup reflections

reflections :: [([Char], [Char])] --[(String, String)]
reflections =
  [ ("am",     "are"),
    ("was",    "were"),
    ("i",      "you"),
    ("i'm",    "you are"),
    ("i'd",    "you would"),
    ("i've",   "you have"),
    ("i'll",   "you will"),
    ("my",     "your"),
    ("me",     "you"),
    ("are",    "am"),
    ("you're", "i am"),
    ("you've", "i have"),
    ("you'll", "i will"),
    ("your",   "my"),
    ("yours",  "mine"),
    ("you",    "me")
  ]


---------------------------------------------------------------------------------

endOfDialog :: String -> Bool
endOfDialog = (=="quit") . map toLower

present :: Phrase -> String
present = unwords

-- Takes the in-string (the users string),
-- Prepares by removing all non-letter elements, mapping them to lower and making the String to a Phrase (list of words)
prepare :: String -> Phrase
prepare = reduce . words . map toLower . filter (not . flip elem ".,:;*!#%&|")

-- Takes for example the "Eliza-text" (the rules) as argument, and converts it to a usable BotBrain
-- I.e. makes all strings to phrases (list of strings), and makes all strings supposed to be matched later to lower case
rulesCompile :: [(String, [String])] -> BotBrain --BotBrain = [(Phrase, [Phrase])] = [([String], [[String]])]
rulesCompile = map . map2 $ ((words . map toLower), map words)


--------------------------------------


reductions :: [PhrasePair]
reductions = (map . map2) (words, words)
  [ ( "please *", "*" ),
    ( "can you *", "*" ),
    ( "could you *", "*" ),
    ( "tell me if you are *", "are you *" ),
    ( "tell me who * is", "who is *" ),
    ( "tell me what * is", "what is *" ),
    ( "do you know who * is", "who is *" ),
    ( "do you know what * is", "what is *" ),
    ( "are you very *", "are you *" ),
    ( "i am very *", "i am *" ),
    ( "hi *", "hello *")
  ]

reduce :: Phrase -> Phrase
reduce = reductionsApply reductions

--Takes a (Phrase, Phrase) (list of reductions) and a Phrase, gives back the reducted phrase
-- Tries to apply all transformations (in reductions, which is decided later in "reduce")
-- If no match, returns unchanged phrase (because of "try")
-- Fix: Keeps checking for reductions until checking returns unchanged phrase, i.e. no more reductions possible
reductionsApply :: [PhrasePair] -> Phrase -> Phrase
reductionsApply = fix . try . transformationsApply "*" id



-------------------------------------------------------
-- Match and substitute
--------------------------------------------------------

-- Replaces each wildcard in a list with the list given as the third argument
substitute :: Eq a => a -> [a] -> [a] -> [a]
substitute wildcard [] _ = []
substitute wildcard t [] = t
substitute wildcard (t:ts) s
    | t == wildcard = s ++ substitute wildcard ts s
    | otherwise = t : substitute wildcard ts s


-- Tries to match two lists. If they match, the result consists of the sublist
-- bound to the wildcard in the pattern list.
match :: Eq a => a -> [a] -> [a] -> Maybe [a]
match wildcard [] [] = Just []
match wildcard [] _ = Nothing
match wildcard _ [] = Nothing
match wildcard (p:ps) (s:ss)
    | p == wildcard = singleWildcardMatch (p:ps) (s:ss) `orElse` longerWildcardMatch (p:ps) (s:ss)
    | p == s = match wildcard ps ss
    | otherwise = Nothing


-- Helper functions to match
singleWildcardMatch, longerWildcardMatch :: Eq a => [a] -> [a] -> Maybe [a]
--singleWildcardMatch (wc:ps) (x:xs) = if match wc ps xs == Nothing then Nothing else (Just [x])
singleWildcardMatch (wc:ps) (x:xs) = mmap (const [x]) (match wc ps xs)
longerWildcardMatch (wc:ps) (x:xs) = mmap (x:) (match wc (wc:ps) xs)


-- Test cases --------------------

testPattern =  "a=*;"
testSubstitutions = "32"
testString = "a=32;"

substituteTest = substitute '*' testPattern testSubstitutions
substituteCheck = substituteTest == testString

matchTest = match '*' testPattern testString
matchCheck = matchTest == Just testSubstitutions

--Both substituteCheck and matchCheck should evaluate to True


-------------------------------------------------------
-- Applying patterns
--------------------------------------------------------

-- Match the list against the first pattern and then, if the match succeeds, substitute the result into the second list
-- Applying a single pattern

transformationApply :: Eq a => a -> ([a] -> [a]) -> [a] -> ([a], [a]) -> Maybe [a]
--transformationApply wildcard f s (p1, p2) = if match wildcard p1 s /= Nothing then Just (substitute wildcard p2 (f $ fromJust $ match wildcard p1 s)) else Nothing
transformationApply wildcard f s (p1, p2) = mmap (substitute wildcard p2) (mmap f $ match wildcard p1 s)


--This function uses transformationApply on the patterns in the list until one succeeds.
--The result of that transformation is then returned.
--If all patterns fail, the function returns Nothing.
transformationsApply :: Eq a => a -> ([a] -> [a]) -> [([a], [a])] -> [a] -> Maybe [a]
transformationsApply _ _ [] _ = Nothing --Empty pattern list, no more patterns to check
transformationsApply wildcard f (p:ps) s = transformationApply wildcard f s p `orElse` transformationsApply wildcard f ps s
