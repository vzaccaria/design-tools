module Snippets where

import Data.Maybe
import Control.Monad
import Data.List


matchDefinition :: String -> String -> Maybe ([String] -> [String])
matchDefinition decl line =
  listToMaybe $ do
    (form, f) <- [ ("", id)
                 , ("-- # ", tail)
                 , ("type family ", id)
                 , ("data family ", id)
                 , ("data ", id)
                 , ("type ", id)
                 , ("newtype ", id)
                 , ("class ", id)
                 ]
    guard $ isPrefixOf (form ++ decl) line
    pure f


getDefinition :: String -> Maybe String -> String
getDefinition file (Just decl)
    = unlines . func $ ls
  where
    ls = takeWhile (not . null)
       . dropWhile (isNothing . matchDefinition decl)
       $ lines file
    func = fromJust . matchDefinition decl $ head ls
getDefinition file Nothing = file

