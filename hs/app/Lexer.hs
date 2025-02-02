module Lexer (Keyword(..), Token(..), nextToken, regex, lex0, Error(..), fromIdent) where

import Control.Applicative ((<|>))
import Data.List (findIndex)
import Data.Maybe (fromJust)
import Text.Read (readMaybe)
import Text.Regex.TDFA ((=~))

data Keyword
  = If
  | Else
  | Int
  | Unsigned
  | Void
  | Bool
  | Goto
  | Return
  deriving (Eq, Show)

-- Try to recognize a keyword.
makeKeyword :: String -> Maybe Keyword
makeKeyword a =
  case a of
    "if" -> Just If
    "else" -> Just Else
    "int" -> Just Int
    "unsigned" -> Just Unsigned
    "void" -> Just Void
    "bool" -> Just Bool
    "goto" -> Just Goto
    "return" -> Just Return
    _ -> Nothing

data Token
  = Eq
  | Gq
  | Lq
  | Shl
  | Shr
  | Tilda
  | LPar
  | RPar
  | LSq
  | RSq
  | LCb
  | RCb
  | Plus
  | Minus
  | Star
  | Slash
  | Percent
  | Assign
  | Gt
  | Lt
  | And
  | Xor
  | Or
  | Not
  | Comma
  | Semicolon
  | Colon
  | Keyword Keyword
  | Ident String
  | Number Int
  deriving (Eq, Show)

fromIdent :: Token -> String
fromIdent (Ident n) = n
fromIdent _ = error "expected an ident"

-- TODO: more informative.
data Error = Unexpected String | ParseError (Maybe [Token])
  deriving (Show)

{-
 - Match groups:
 - \* 0 - String without whitespaces
 - \* 1 - EOF
 - \* 2+ Tokens
-}
tokenRegexs :: [(String, String -> Maybe Token)]
tokenRegexs =
  [ ( "[0-9]+",
      fmap Number . readMaybe
    ),
    ( "[_a-zA-Z][_a-zA-Z0-9]*",
      -- Firstly try to extract keyword, then make an ident.
      \x -> fmap Keyword (makeKeyword x) <|> Just (Ident x)
    )
  ]
    ++ map
      (\(x, cons) -> (x, Just . const cons))
      primitiveTokens
  where
    primitiveTokens :: [(String, Token)]
    primitiveTokens =
      [ ("==", Eq),
        (">=", Gq),
        ("<=", Lq),
        ("<<", Shl),
        (">>", Shr),
        ("~", Tilda),
        ("\\(", LPar),
        ("\\)", RPar),
        ("\\[", LSq),
        ("\\]", RSq),
        ("\\{", LCb),
        ("\\}", RCb),
        ("\\+", Plus),
        ("\\-", Minus),
        ("\\*", Star),
        ("/", Slash),
        ("%", Percent),
        ("=", Assign),
        (">", Gt),
        ("<", Lt),
        ("&", And),
        ("\\^", Xor),
        ("\\|", Or),
        ("!", Not),
        (",", Comma),
        (";", Semicolon),
        (":", Colon)
      ]

-- Regexp that used to match tokens.
regex :: String
regex =
  "^[[:space:]]*(($)" ++ foldl f "" tokenRegexs ++ ")"
  where
    f s (re, _) = s ++ "|(" ++ re ++ ")"

-- Recognize a next token from the input.
nextToken :: String -> Either Error (String, Maybe Token)
nextToken s =
  if s == ""
    then return ("", Nothing)
    else
      let (_, m, suf, groups) = s =~ regex :: (String, String, String, [String])
       in if m == ""
            then
              -- No match at all.
              Left $ Unexpected s
            else
              let matched = head groups
                  -- As the string was matched => there is
                  -- some "token" group that was matched (or it is EOF).
                  --
                  -- Token groups started from 2, so take a suffix starting
                  -- from third element and try to find a match.
                  grp = (findIndex (/= "") $ (tail . tail) groups)
                  unwrapOrElse :: Maybe Token -> Either Error Token
                  unwrapOrElse mt = fromJust $ fmap return mt <|> Just (Left (Unexpected s))
               in case grp of
                    -- No token groups => EOF matched.
                    Nothing -> return ("", Nothing)
                    Just g -> do
                      -- Extract constructor for token by group.
                      let cons = fmap snd tokenRegexs !! g
                      -- Create a token from matched.
                      token <- unwrapOrElse $ cons matched
                      return (suf, Just token)

-- Lex input. In the case of error, returns a value that contains a suffix
-- where error token begins.
--
lex0 :: String -> Either Error [Token]
lex0 s = do
  (suf, token) <- nextToken s
  case token of
    Nothing -> return []
    Just tok -> do
      tokens <- lex0 suf
      return $ tok : tokens
