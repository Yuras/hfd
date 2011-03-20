
-- | This modele defines user comands
module UCmd
(
UCmd(..),
parseUCmd
)
where

import Data.List (isPrefixOf)

data UCmd
  = UCmdContinue
  | UCmdStep
  | UCmdNext
  deriving Show

parseUCmd :: String -> Maybe UCmd
parseUCmd = parse . words
  where
  parse [] = Nothing
  parse (c:_) = do
    cmd <- parseBaseCmd c
    case cmd of
      "continue" -> Just UCmdContinue
      "step"     -> Just UCmdStep
      "next"     -> Just UCmdNext
      _          -> Nothing

parseBaseCmd :: String -> Maybe String
parseBaseCmd s =
  if length condidates == 1
    then Just $ head condidates
    else Nothing
  where
  condidates = suggestBaseCmd s

-- | List of base commands
baseCommands :: [String]
baseCommands = ["continue", "step", "next"]

-- | Returns list of base commands that maches the given prefix
suggestBaseCmd :: String -> [String]
suggestBaseCmd s = filter (isPrefixOf s) baseCommands

