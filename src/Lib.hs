{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns    #-}

module Lib where

import           Control.Arrow
import           CSV
import qualified Data.ByteString.Lazy as BS
import           Data.Foldable
import           Data.List
import           Data.List            (sort, uncons)
import qualified Data.Vector          as V
import           Snippets
import           Text.Pandoc

isSnippet :: String -> IO (Maybe (FilePath, Maybe String))
isSnippet args =  do
   qs <- pure $ splitArgs args
   if (length qs > 2) then do
           (file: sp: fp : more_args) <- pure $ splitArgs args
           return $ if (sp == "snippet") && file == "file" then
             Just (fp, fmap fst $ uncons more_args)
             else
             Nothing
   else return Nothing

isSnipBlock :: Block -> IO (Maybe (FilePath, Maybe String))
isSnipBlock (Para [Link _ (Strs t) (_, _)])  = isSnippet t
isSnipBlock (Plain [Link _ (Strs t) (_, _)]) = isSnippet t
isSnipBlock _                                = return Nothing

inlineSnippets :: Block -> IO Block
inlineSnippets b = do
        sb <-  (isSnipBlock b)
        case sb of
         (Just (fp,xx)) -> snippet fp xx
         _              -> return b

  where

    runSnippet :: String -> IO Block
    runSnippet args = do
      (fp : more_args) <- pure $ splitArgs args
      snippet fp $ fmap fst $ uncons more_args

    snippet :: FilePath -> Maybe String -> IO Block
    snippet fp defn = do
      file <- readFile fp
      pure $ codeBlock $ getDefinition fp file defn


showCSV :: Block -> IO Block
showCSV = \case
  Para [Link _ (Strs t) ("CSV", _)]  -> runCSV t
  Plain [Link _ (Strs t) ("CSV", _)] -> runCSV t
  t -> pure t

  where
    runCSV :: String -> IO Block
    runCSV args = do
      (fp : more_args) <- pure $ splitArgs args
      csv <- loadCSV <$> BS.readFile fp
      pure $ case more_args of
        [proj] -> showVector $ selectCSV proj csv
        [field, value, proj] ->
          showVector $ selectCSV proj $ filterCSV field value csv
        _ -> error $ "bad argument format given to CSV: " <> args


pattern Strs :: String -> [Inline]
pattern Strs ts <-
  ((id &&& id)
    ->
      ( all isStr -> True
      , foldMap fromStr -> ts
      )
  )

isStr :: Inline -> Bool
isStr (Str _) = True
isStr Space   = True
isStr _       = False

fromStr :: Inline -> String
fromStr (Str s) = s
fromStr Space   = " "


showVector :: V.Vector String -> Block
showVector = BulletList
           . fmap (pure . Plain . pure . Str)
           . sort
           . toList


codeBlock :: String -> Block
codeBlock = CodeBlock ("", ["haskell"], [])


splitArgs :: String -> [String]
splitArgs s =
  case break (== ':') s of
    ("", "")           -> []
    (as, "")           -> [as]
    (as, drop 1 -> bs) -> as : splitArgs bs

