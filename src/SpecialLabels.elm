module SpecialLabels exposing 
  (extractVerbatim, makeVerbatimLabel, removeVerbatim, SpecialLabel(..),
     makeSpecial, extractSpecial, isDependency, removeAny, removeSpecial)
-- a special label is a label that starts with a key:
-- %verb for verbatim
-- %dep for dependency

removeVerbatim : String -> String
removeVerbatim = removeSpecial Verbatim

isDependency : String -> Bool 
isDependency s = extractSpecial Dependency s /= Nothing



removeSpecial : SpecialLabel -> String -> String
removeSpecial key s = extractSpecial key s |> Maybe.withDefault s

-- extractOldVerbatim : String -> Maybe String
-- extractOldVerbatim s =
--    let prefix = verbatimCmd ++ "{" in
--    if String.startsWith prefix s && String.endsWith "}" s then
--       Just (String.slice (String.length prefix) (-1) s)
--    else
--       Nothing

-- verbatimOldCmd = "\\coqverb"


type SpecialLabel = Dependency | Verbatim | Normal
allLabels = [ Dependency, Verbatim, Normal ]

removeAny : String -> String
removeAny s = List.foldl removeSpecial s allLabels

specialToKey : SpecialLabel -> String
specialToKey l =
   case l of
      Dependency -> "dep"
      Verbatim -> "verb"
      Normal -> ""

prefixFromKey : SpecialLabel -> String
prefixFromKey key = "%" ++ specialToKey key ++ " "

extractSpecial : SpecialLabel -> String -> Maybe String
extractSpecial key s = 
   if key == Normal then Just s else
   let prefix = prefixFromKey key in

   if String.startsWith prefix s then
         Just (String.dropLeft (String.length prefix) s)
      else
         Nothing

makeSpecial : SpecialLabel -> String -> String
makeSpecial key s = 
  if key == Normal then s else prefixFromKey key ++ s

extractVerbatim : String -> Maybe String
extractVerbatim = extractSpecial Verbatim


makeVerbatimLabel : Bool -> String -> String
makeVerbatimLabel isVerbatim =
   if isVerbatim then makeSpecial Verbatim else identity

