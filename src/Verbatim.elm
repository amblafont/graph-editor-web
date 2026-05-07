module Verbatim exposing (extractVerbatim, makeVerbatimLabel, removeVerbatim)

removeVerbatim : String -> String
removeVerbatim s = extractVerbatim s |> Maybe.withDefault s

-- extractOldVerbatim : String -> Maybe String
-- extractOldVerbatim s =
--    let prefix = verbatimCmd ++ "{" in
--    if String.startsWith prefix s && String.endsWith "}" s then
--       Just (String.slice (String.length prefix) (-1) s)
--    else
--       Nothing

-- verbatimOldCmd = "\\coqverb"
verbatimCmd = "%verb "

extractVerbatim : String -> Maybe String
extractVerbatim s =
   let prefix = verbatimCmd in
   if String.startsWith prefix s then
      Just (String.dropLeft (String.length prefix) s)
   else
      Nothing

makeVerbatimLabel : Bool -> String -> String
makeVerbatimLabel isVerbatim s =
   if isVerbatim then
      verbatimCmd ++ s
   else
      s

