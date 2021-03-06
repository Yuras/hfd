
-- | This modele defines user comands
module UCmd
(
UCmd(..),
parseUCmd,
InfoCmd(..),
suggestBaseCmd
)
where

import Data.List (isPrefixOf, find)
import Data.Char (isDigit)
import Control.Monad (when, unless)

-- | User commands
data UCmd
  = UCmdEmpty           -- ^ empty command, previous command should be used
  | UCmdContinue
  | UCmdStep
  | UCmdNext
  | UCmdFinish
  | UCmdQuit
  | UCmdInfo InfoCmd
  | UCmdPrint [String]
  | UCmdBreakpoint Int Int
  | UCmdHelp
  | UCmdStack
  | UCmdList
  | UCmdDelete (Maybe Int)
  | UCmdTest  -- ^ Just for tests
  deriving Show

-- | Parse user command
parseUCmd :: String -> Maybe UCmd
parseUCmd = parse . words
  where
  parse [] = Just UCmdEmpty
  parse (c:cs) = do
    cmd <- parseBaseCmd c
    case cmd of
      "continue" | cs == [] -> Just UCmdContinue
      "step"     | cs == [] -> Just UCmdStep
      "next"     | cs == [] -> Just UCmdNext
      "finish"   | cs == [] -> Just UCmdFinish
      "quit"     | cs == [] -> Just UCmdQuit
      "help"     | cs == [] -> Just UCmdHelp
      "info"                -> fmap UCmdInfo (parseInfoCmd cs)
      "print"               -> fmap UCmdPrint (parsePrintCmd cs)
      "breakpoint"          -> parseBreakpointCmd cs
      "b"                   -> parseBreakpointCmd cs
      "backtrace" | cs == [] -> Just UCmdStack
      "bt"       | cs == [] -> Just UCmdStack
      "list"                -> parseListCmd cs
      "delete"              -> parseDeleteCmd cs
      "test"                -> Just UCmdTest
      _                     -> Nothing

-- | Parse @delete@ command
parseDeleteCmd :: [String] -> Maybe UCmd
parseDeleteCmd [] = Just $ UCmdDelete Nothing
parseDeleteCmd [iD] =
  if all isDigit iD
    then Just $ UCmdDelete $ Just $ read iD
    else Nothing
parseDeleteCmd _ = Nothing

-- | Parse @list@ command
parseListCmd :: [String] -> Maybe UCmd
parseListCmd [] = Just UCmdList
parseListCmd _ = Nothing

-- | Parse @breakpoint@ command
--
-- XXX: very pure code, rewrite
parseBreakpointCmd :: [String] -> Maybe UCmd
parseBreakpointCmd [pos] = do
  (m, res) <- head' pos
  when (m /= '#') Nothing
  let fl = takeWhile isDigit res
  when (null fl) Nothing
  let res1 = drop (length fl) res
  (m1, ln) <- head' res1
  when (null ln) Nothing
  when (m1 /= ':') Nothing
  unless (all isDigit ln) Nothing
  return $ UCmdBreakpoint (read fl) (read ln)
  where
  head' [] = Nothing
  head' (x:xs) = Just (x, xs)
parseBreakpointCmd _     = Nothing

-- | Parse base command
parseBaseCmd :: String -> Maybe String
parseBaseCmd s =
  if length condidates == 1
    then Just $ head condidates
    else find (== s) condidates
  where
  condidates = suggestBaseCmd s

-- | Returns list commands that maches the given prefix
suggestCmd :: [String]  -- ^ Possible commands
           -> String    -- ^ Prefix
           -> [String]  -- ^ suggestions
suggestCmd cmds s = filter (isPrefixOf s) cmds

-- | List of base commands
baseCommands :: [String]
baseCommands = ["continue", "step", "next", "finish", "quit", "info", "print",
  "breakpoint", "help", "backtrace", "bt", "b", "list", "delete"]

-- | Returns list of base commands that maches the given prefix
suggestBaseCmd :: String -> [String]
suggestBaseCmd = suggestCmd baseCommands

-- | Parse @print@ commands
-- Just name of varible to print
parsePrintCmd :: [String] -> Maybe [String]
parsePrintCmd [v] = Just $ props v
  where
  props s = let (l, s') = break (== '.') s
            in l : case s' of
                     []      -> []
                     (_:s'') -> props s''
parsePrintCmd _ = Nothing

-- | Info commands
data InfoCmd = ICFiles        -- ^ @info files@
             | ICBreakpoints  -- ^ @info breakpoints@
             deriving Show

-- | Parse info commands
parseInfoCmd :: [String] -> Maybe InfoCmd
parseInfoCmd [] = Nothing
parseInfoCmd (c:cs) = do
  cmd <- mcmd
  case cmd of
    "files"       | cs == [] -> Just ICFiles
    "breakpoints" | cs == [] -> Just ICBreakpoints
    _                        -> Nothing
  where
  condidates = suggestInfoCmd c
  mcmd = if length condidates == 1
           then Just $ head condidates
           else Nothing

-- | List of info commands
infoCommands :: [String]
infoCommands = ["files", "breakpoints"]

-- | Returns list of info commands that maches the given prefix
suggestInfoCmd :: String -> [String]
suggestInfoCmd = suggestCmd infoCommands

