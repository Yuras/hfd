
-- | This modele defines user comands
module UCmd
(
UCmd(..),
parseUCmd,
InfoCmd(..)
)
where

import Data.List (isPrefixOf)

-- | User commands
data UCmd
  = UCmdContinue
  | UCmdStep
  | UCmdNext
  | UCmdQuit
  | UCmdInfo InfoCmd
  | UCmdPrint String
  | UCmdTest  -- ^ Just for tests
  deriving Show

-- | Parse user command
parseUCmd :: String -> Maybe UCmd
parseUCmd = parse . words
  where
  parse [] = Nothing
  parse (c:cs) = do
    cmd <- parseBaseCmd c
    case cmd of
      "continue" | cs == [] -> Just UCmdContinue
      "step"     | cs == [] -> Just UCmdStep
      "next"     | cs == [] -> Just UCmdNext
      "quit"     | cs == [] -> Just UCmdQuit
      "info"                -> fmap UCmdInfo (parseInfoCmd cs)
      "print"               -> fmap UCmdPrint (parsePrintCmd cs)
      "test"                -> Just UCmdTest
      _                     -> Nothing

-- | Parse base command
parseBaseCmd :: String -> Maybe String
parseBaseCmd s =
  if length condidates == 1
    then Just $ head condidates
    else Nothing
  where
  condidates = suggestBaseCmd s

-- | Returns list commands that maches the given prefix
suggestCmd :: [String]  -- ^ Possible commands
           -> String    -- ^ Prefix
           -> [String]  -- ^ suggestions
suggestCmd cmds s = filter (isPrefixOf s) cmds

-- | List of base commands
baseCommands :: [String]
baseCommands = ["continue", "step", "next", "quit", "info", "print", "test"]

-- | Returns list of base commands that maches the given prefix
suggestBaseCmd :: String -> [String]
suggestBaseCmd = suggestCmd baseCommands

-- | Parse @print@ commands
-- Just name of varible to print
parsePrintCmd :: [String] -> Maybe String
parsePrintCmd [v] = Just v
parsePrintCmd _ = Nothing

-- | Info commands
data InfoCmd = ICFiles  -- ^ @info files@
             deriving Show

-- | Parse info commands
parseInfoCmd :: [String] -> Maybe InfoCmd
parseInfoCmd [] = Nothing
parseInfoCmd (c:cs) = do
  cmd <- mcmd
  case cmd of
    "files" | cs == [] -> Just ICFiles
    _                  -> Nothing
  where
  condidates = suggestInfoCmd c
  mcmd = if length condidates == 1
           then Just $ head condidates
           else Nothing

-- | List of info commands
infoCommands :: [String]
infoCommands = ["files"]

-- | Returns list of info commands that maches the given prefix
suggestInfoCmd :: String -> [String]
suggestInfoCmd = suggestCmd infoCommands

