{-
  Functional Programming for Logicians 2019 Spring

  Markov interpreter

  This program runs Markov algorithms. It differs from the usual implementation in that it handles variables. For further info, see
  https://en.wikipedia.org/wiki/Markov_algorithm

  Algorithms are stored in separate files. Call 'main' to run an algorithm. Call 'test' to check if everything works properly.
-}

-- A rule is either a normal rule or an end rule.
data RuleType = Normal | End deriving Eq
data Rule = NoneRule |
            Rule {ruleLeft :: String,
            ruleType :: RuleType,
            ruleRight :: String}
            deriving Eq
data Algorithm = Alg [Rule] deriving Eq

showWord :: String -> String
showWord ""           = "0"
showWord s            = s

instance Show RuleType where
  show Normal         = " ->  "
  show End            = " ->* "

instance Show Rule where
  show (Rule s rt t)  = showWord s ++ show rt ++ showWord t
  -- 'Rule "abc" Normal "de"' is shown as '"abc ->  de"'
  -- 'Rule "abc" End "de"' is shown as '"abc ->* de"'

instance Show Algorithm where
  show (Alg rs)       = unlines $ map show rs

-- Start here.
main :: IO ()
main = do
  putStrLn "\nMarkov Interpreter"
  putStrLn "\nAlgorithm name (file name without extension):"
  algName <- getLine
  algString <- readFile $ "algorithms/" ++ algName ++ ".alg"
  let algorithm = readAlgScheme algString
  putStrLn "\nInput word:"
  inputWord <- getLine
  let result = runAlg algorithm inputWord
  let showResultPairs = [(showWord x, show y) | (x, y) <- result]
  let width = maximum [length y | (y, _) <- showResultPairs]
  let adjust x = x ++ replicate (width - length x) ' '
  let showResult = unlines [adjust x ++ " | " ++ y | (x, y) <- showResultPairs]
  putStrLn "\nOutput:"
  putStrLn $ showResult

-- Applies a rule. If it is applicable and the result is 'x', returns 'Just x'.
-- Otherwise it returns 'Nothing'.
applyRule ::  Rule -> String -> Maybe String
applyRule rule word = app rule "" "" word where
  applyAux ::  Rule -> String -> String -> String -> Maybe String
  applyAux  (Rule left ruleType right) behind matching ahead
    | fullMatch             = Just $ behind ++ right ++ ahead
    | outOfLetters          = Nothing
    | firstLettersMatch     = applyAux ruleMinusFirstLetter
                              behind (newMatching) (tail ahead)
    | noFirstMatch          = applyAux ruleRestored
                              (behind ++ [head newMatching])
                              "" (tail newMatching ++ tail ahead)
    where
      -- Case 1: A full match is found. The rule applies.
      fullMatch             = left == ""
      -- Case 2: Out of letters; no match. The rule doesn't apply.
      outOfLetters          = ahead == ""
      -- Case 3: First letters match. Checking continues with the rest.
      firstLettersMatch     = head left == head ahead
      -- Case 4: First letters don't match. Checking continues with
      -- the original rule restored.
      noFirstMatch          = otherwise
      ruleMinusFirstLetter  = Rule (tail left) ruleType right
      ruleRestored          = Rule (matching ++ left) ruleType right
      newMatching           = matching ++ [head ahead]
  app :: Rule -> String -> String -> String -> Maybe String
  app (Rule left ruleType right) passed match toDo
    | left == ""
      = Just $ passed ++ right ++ toDo
    | toDo == ""
      = Nothing
    | head left == head toDo
      = app (Rule (tail left) ruleType right) passed (match ++ [head left]) (tail toDo)
    | head left /= head toDo && head (match ++ left) == head toDo
      = app (Rule (tail (match ++ left)) ruleType right) (passed ++ match) [head toDo] (tail toDo)
    | otherwise
      = app (Rule (match ++ left) ruleType right) (passed ++ match ++ [head toDo]) "" (tail toDo)

-- Executes an algorithm.
runAlg :: Algorithm -> String -> [(String, Rule)]
runAlg (Alg rules) word = runAlgA rules rules word where
  runAlgA :: [Rule] -> [Rule] -> String -> [(String, Rule)]
  runAlgA allRules rulesAhead word
    | outOfRules            = [(word, NoneRule)]
    | nextRuleDoesntApply   = runAlgA allRules (tail rulesAhead) word
    | endRuleApplies        = [(unJust nextResult, nextRule)]
    | normalRuleApplies     = (unJust nextResult, nextRule) :
                              runAlgA allRules allRules (unJust nextResult)
    where
      -- Case 1: We have run out of rules to apply. The algorithm halts.
      outOfRules            = rulesAhead == []
      -- Case 2: The next rule doesn't apply. We move one rule further.
      nextRuleDoesntApply   = nextResult == Nothing
      -- Case 3: The next rule applies, and it is an endrule.
      -- The algorithm halts.
      endRuleApplies        = ruleType nextRule == End
      -- Case 4: The next rule applies, and it is not an endrule.
      -- We move to the first rule.
      normalRuleApplies     = otherwise
      nextRule = head rulesAhead
      nextResult = applyRule nextRule word
      unJust (Just x) = x

{-
  Parsing
-}

-- Reads concrete variable-free rules
readRule :: String -> Rule
readRule xs = readRuleA "" $ removeWhiteAndEmpty xs where
  readRuleA _ [] = error "Ill-formed rule!"
  readRuleA left right
    | right `beginsWith` "->*" = Rule left End $ drop 3 right
    | right `beginsWith` "->"  = Rule left Normal $ drop 2 right
    | otherwise                = readRuleA (left ++ [head right]) (tail right)

-- Instantiates a rule scheme, and passes the instances to readRule.
readScheme :: String -> [Rule]
readScheme xs = map readRule $ instances instanceList scheme where
  instanceList = snd parseResult
  scheme = fst parseResult
  parseResult = parseScheme $ removeWhiteAndEmpty xs

-- More general than readAlg; allows for rule schemes.
readAlgScheme :: String -> Algorithm
readAlgScheme xs = Alg $ (removeExtraLines . lines) xs >>= readScheme

{-
  String and list manipulation methods
-}

-- Checks whether the second argument is an initial segment of the first.
beginsWith :: Eq a => [a] -> [a] -> Bool
_ `beginsWith` []               = True
[] `beginsWith` _               = False
(y:ys) `beginsWith` (x:xs)
  | x == y                    = ys `beginsWith` xs
  | otherwise                 = False

-- Deletes spaces, tabs, and '0's. ('0' stands for the empty word.)
removeWhiteAndEmpty :: String -> String
removeWhiteAndEmpty xs = filter (\x -> not $ x `elem` " \t0") xs

removeExtraLines :: [String] -> [String]
removeExtraLines xss = [xs | xs <- xss, not $ tooShort xs, not $ comment xs] where
  tooShort xs = xs == "" || tail xs == ""
  comment xs  = take 2 xs == "--"

-- instantiation is based on an instance list, containing pairs that serve as the first two arguments of replaceAllFromList
instances :: Eq a => [(a, [a])] -> [a] -> [[a]]
instances [] scheme = [scheme]
instances ((x, ys):furtherPairs) scheme =
  concat [replaceAllFromList x ys zs | zs <- instances furtherPairs scheme]
-- > instances [('x',"ei"),('y',"aou")] "prxsxdxnt Dynyld Trymp"
-- ["presedent Danald Tramp","prisidint Danald Tramp",
-- "presedent Donold Tromp","prisidint Donold Tromp",
-- "presedent Dunuld Trump","prisidint Dunuld Trump"]

-- replaces all occurrences of x with each element of ys in zs
replaceAllFromList :: Eq a => a -> [a] -> [a] -> [[a]]
replaceAllFromList x ys zs = [replaceAll x y zs| y <- ys]
-- > replaceAllFromList 'x' "aeiou" "Dxnxld Trxmp"
-- ["Danald Tramp","Deneld Tremp","Dinild Trimp","Donold Tromp","Dunuld Trump"]

-- replaces all occurrences of x with y in zs
replaceAll :: Eq a => a -> a -> [a] -> [a]
replaceAll x y zs = [if z == x then y else z | z <- zs]
-- > replaceAll 'x' 'u' "Dxnxld Trxmp"
-- "Dunuld Trump"

-- Parses a rule scheme string into a rule string
parseScheme :: String -> (String, [(Char, String)])
parseScheme xs = (head pipeSplit, fmap colonSplit (tail pipeSplit)) where
  pipeSplit = splitAtElem '|' xs
  colonSplit ys
    | length ys < 2 || head (tail ys) /= ':'  = error "Ill-formed instances!"
    | otherwise                               = (head ys, drop 2 ys)

-- Splits a list at every occurrence of a certain element.
splitAtElem :: Eq a => a -> [a] -> [[a]]
splitAtElem c xs = splitAtElemA c [] xs where
  splitAtElemA c sofar []       = [sofar]
  splitAtElemA c sofar (x:xs)
    | c == x                    = sofar : splitAtElemA c [] xs
    | otherwise                 = splitAtElemA c (sofar ++ [x]) xs

{-
  A test of two core functions: runAlg and readAlgScheme
-}

testCases :: [(String, [(String, String)])]
testCases = [ ("reverse", [("abc", "cba"), ("www", "www")]),
              ("parenthesis", [("(c(a)(c)(((b)(c)a)))", "T"),
                ("(c(a)(c)(((b)(c)a))", "F")]),
              ("parenthesis2",
                [("(c(a)(c)(((b)(c)a)))", "(c(a)(c)(((b)(c)a)))T"),
                ("(c(a)(c)(((b)(c)a))", "(c(a)(c)(((b)(c)a))F")]),
              ("palindrome", [("abacaba", "T"), ("abacaca", "F")]),
              ("binary_addition", [("11o+1o1", "1o11"), ("1111+1", "1oooo")]),
              ("propositional_wff", [("(pD~(~p+Dp++))", "T"),
              ("p~Dp+)", "F")]),
              ("from_polish", [("NANp+Cp++p", "~(~p+V(p++Dp))")]),
              ("to_polish", [("~(~p+V(p++Dp))", "NANp+Cp++p")])
            ]

test :: IO ()
test = sequence_
  [ getAlg algName >>= \algString ->
    putStrLn $ (text algName inputWord outputWord) ++
    (if success algString inputWord outputWord then "OK" else "not OK")
    | (algName, cases) <- testCases, (inputWord, outputWord) <- cases
  ]
  where
    getAlg algName = readFile $ "algorithms/" ++ algName ++ ".alg"
    success algString inputWord outputWord
      = outputWord == fst (last $ runAlg (readAlgScheme algString) inputWord)
    text algName inputWord outputWord
      = "alg: " ++ algName    ++ replicate n1 ' ' ++
        "in: "   ++ inputWord  ++ replicate n2 ' ' ++
        "out: "  ++ outputWord ++ replicate n3 ' ' ++ "-- "
        where
          m1 = maximum [length x | (x,_) <- testCases] + 2
          m2 = maximum [length y | (_,x) <- testCases, (y, _) <- x] + 2
          m3 = maximum [length y | (_,x) <- testCases, (_, y) <- x] + 2
          n1 = m1 - length algName
          n2 = m2 - length inputWord
          n3 = m3 - length outputWord
