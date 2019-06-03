data RuleType = Normal | End
                deriving (Eq, Show)
data Rule     = Rule String RuleType String
                deriving (Eq, Show)

apply ::  Rule -> String -> Maybe String
apply rule word = applyAux rule "" "" word

applyAux ::  Rule ->
          String -> String -> String ->
          Maybe String
applyAux  (Rule left ruleType right)
          passed matching toDo
  | left == ""
    = Just $ passed ++ toDo
  | toDo == ""
    = Nothing
  | head left == head toDo
    = applyAux (Rule (tail left) ruleType right)
      passed (matching ++ [head toDo]) (tail toDo)
  | otherwise
    = applyAux (Rule (tail left) ruleType right)
      (passed ++ [head toDo]) matching (tail toDo)
