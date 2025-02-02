{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Parser where

import qualified Ast
import qualified BasicBlock
import Control.Applicative (Alternative (empty), (<|>))
import Control.Monad ((>=>))
import Data.Bifunctor (second)
import Data.List (find)
import qualified Function
import qualified Insn
import Lexer
import qualified Op
import qualified Type
import qualified Value

type Stream = [Token]

newtype Parser a = Parser {parseFun :: Stream -> Maybe (Stream, a)}

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  -- Help: (>=>) :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c
  fmap f (Parser g) = Parser (g >=> (return . second f))

instance Applicative Parser where
  pure :: a -> Parser a
  -- Construct a parser that does not consume string, only return a value.
  pure x = Parser (\s -> return (s, x))

  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  Parser f <*> g =
    Parser (f >=> (\(s, cons) -> parseFun (fmap cons g) s))

instance Alternative Parser where
  empty :: Parser a
  empty = Parser (const Nothing)

  (<|>) :: Parser a -> Parser a -> Parser a
  f <|> g = Parser (\s -> parseFun f s <|> parseFun g s)

-- Construct a parser that extracts a first token.
take1 :: Parser Token
take1 =
  Parser
    ( \case
        (x : xs) -> return (xs, x)
        _ -> Nothing
    )

-- Construct a parser that check if an input starts from
-- one of the enumerated tokens.
oneOf :: [Token] -> Parser Token
oneOf l =
  Parser
    (parseFun take1 >=> (\(rest, token) -> fmap (rest,) (find (token ==) l)))

-- Construct a parser that replaces other parse error with Nothing.
optional :: Parser a -> Parser (Maybe a)
optional p =
  Parser
    ( \s -> case parseFun p s of
        Just (rest, e) -> return (rest, Just e)
        Nothing -> return (s, Nothing)
    )

-- Parse a type.
typ :: Parser Type.Type
typ =
  (cons <$> fmap f (oneOf [Keyword Void, Keyword Unsigned, Keyword Int, Keyword Bool]))
    <*> fold0 (\x _ -> x + 1) 0 (tok Star)
  where
    f :: Token -> Type.Type
    f = \case
      Keyword Void -> Type.Void
      Keyword Unsigned -> Type.Unsigned32
      Keyword Int -> Type.Int32
      Keyword Bool -> Type.Bool
      _ -> error "unreachable"

    -- TODO: not build list here.
    cons :: Type.Type -> Int -> Type.Type
    cons tp num = foldr (\_ t -> Type.Ref t) tp [0 .. num - 1]

-- Parse specified token.
tok :: Token -> Parser Token
tok t = oneOf [t]

-- Parse any ident.
ident :: Parser Token
ident =
  Parser
    ( parseFun take1
        >=> ( \(rest, token) -> case token of
                Ident _ -> return (rest, token)
                _ -> Nothing
            )
    )

-- Parse a number.
number :: Parser Int
number =
  Parser
    ( parseFun take1
        >=> ( \(rest, token) -> case token of
                Number n -> return (rest, n)
                _ -> Nothing
            )
    )

-- Parse a literal.
literal :: Parser Token
literal =
  (cons <$> optional (oneOf [Plus, Minus])) <*> number
  where
    cons sg n =
      case sg of
        (Just Plus) -> Number n
        Nothing -> Number n
        Just Minus -> Number (-n)
        _ -> error "unrechable"

-- Parse a separated with delimeter `delim` list of `p`.
separated_list0 :: Parser a -> Parser e -> Parser [e]
separated_list0 delim p =
  Parser
    ( \s -> case parseFun p s of
        Just (stail, e0) -> return $ second (e0 :) (h stail)
        Nothing -> return (s, [])
    )
  where
    h s = case parseFun delim s of
      Nothing -> (s, [])
      Just (no_delim, _) -> case parseFun p no_delim of
        Nothing -> (s, [])
        Just (rest, e) -> second (e :) (h rest)

-- Parse a pair.
pair :: Parser a -> Parser b -> Parser (a, b)
pair f g = ((,) <$> f) <*> g

-- Parse a function signature.
sign :: Parser (Type.Type, String, [(Type.Type, Value.Variable)])
sign =
  (cons <$> typ)
    <*> fmap fromIdent ident
    <*> ( tok LPar
            *> separated_list0
              (tok Comma)
              ( pair
                  typ
                  (fmap ((\n -> Value.Variable {Value.name = n}) . fromIdent) ident)
              )
            <* tok RPar
        )
  where
    cons foo_typ name args = (foo_typ, name, args)

-- Parse a value.
value :: Parser Value.Value
value = cons <$> (ident <|> literal)
  where
    cons t = case t of
      Ident n -> Value.ValueVariable {Value.var = Value.Variable {Value.name = n}}
      Number n -> Value.ValueLiteral {Value.lit = Value.Literal {Value.val = n}}
      _ -> error "unreachable"

-- Parse a return instruction.
ret :: Parser Insn.Insn
ret =
  cons <$> (tok (Keyword Return) *> value)
  where
    cons x = Insn.Return {Insn.ret = x}

-- Parse a binary operation.
binaryOp :: Parser (Value.Value, Op.BinaryOp, Value.Value)
binaryOp = (cons <$> value) <*> operator <*> value
  where
    operator :: Parser Op.BinaryOp
    operator =
      opCons
        <$> oneOf
          [ Plus,
            Minus,
            Star,
            Slash,
            Percent,
            Gt,
            Lt,
            And,
            Xor,
            Or,
            Eq,
            Gq,
            Lq,
            Shl,
            Shr
          ]
      where
        opCons token = case token of
          Plus -> Op.Arithm Op.Add
          Minus -> Op.Arithm Op.Sub
          Star -> Op.Arithm Op.Mul
          Slash -> Op.Arithm Op.Div
          Percent -> Op.Arithm Op.Mod
          Gt -> Op.Logical Op.Gt
          Lt -> Op.Logical Op.Lt
          And -> Op.Logical Op.And
          Xor -> Op.Logical Op.Xor
          Or -> Op.Logical Op.Or
          Eq -> Op.Logical Op.Eq
          Gq -> Op.Logical Op.Gq
          Lq -> Op.Logical Op.Lq
          Shl -> Op.Bit Op.Shl
          Shr -> Op.Bit Op.Shr
          _ -> error "unreachable"

    cons lhs op rhs = (lhs, op, rhs)

-- Parse an unary operation.
unaryOp :: Parser (Op.UnaryOp, Value.Value)
unaryOp = (cons <$> operator) <*> value
  where
    operator :: Parser Op.UnaryOp
    operator =
      opCons
        <$> oneOf
          [Minus, Plus, Star, Not, And, Tilda]
      where
        opCons token = case token of
          Minus -> Op.Minus
          Plus -> Op.Plus
          Not -> Op.Not
          Star -> Op.Deref
          And -> Op.RefOf
          Tilda -> Op.Neg
          _ -> error "unreachable"

    cons op arg = (op, arg)

-- Parse a function call.
call :: Parser Value.FCall
call =
  (cons <$> ident)
    <*> (tok LPar *> separated_list0 (tok Comma) value <* tok RPar)
  where
    cons name args = Value.FCall {Value.fun = fromIdent name, Value.args = args}

-- Parse a rvalue.
rvalue :: Parser Value.RValue
rvalue =
  (toUnaryOp <$> unaryOp)
    <|> ( toBinaryOp
            <$> binaryOp
        )
    -- Call must be considered prior to value, because of:
    -- `int a = b();` vs `int a = b;`
    <|> (Value.Call <$> call)
    <|> (Value.Val <$> value)
  where
    toUnaryOp (op, arg) = Value.Unary {Value.uop = op, Value.arg = arg}
    toBinaryOp (lhs, op, rhs) = Value.Binary {Value.lhs = lhs, Value.bop = op, Value.rhs = rhs}

variable :: Parser Value.Variable
variable = (\n -> Value.Variable {Value.name = fromIdent n}) <$> ident

-- Parse an assign statement.
assign :: Parser Insn.Insn
assign = (cons <$> variable) <*> (tok Assign *> rvalue)
  where
    cons var rhs = Insn.Assign {Insn.lhs = var, Insn.rhs = rhs}

-- Parse an assign deref statement.
assignDeref :: Parser Insn.Insn
assignDeref = (cons <$> (tok Star *> variable <* tok Assign)) <*> value
  where
    cons var rhs = Insn.AssignDeref {Insn.lhs = var, Insn.drhs = rhs}

-- Parse a goto instruction.
gotoInsn :: Parser Insn.GotoInsn
gotoInsn = cons <$> (tok (Keyword Goto) *> ident)
  where
    cons n = Insn.GotoInsn {Insn.label = fromIdent n}

-- Parse a goto instruction as a generic instruction.
goto :: Parser Insn.Insn
goto = fmap Insn.Goto gotoInsn

-- Parse a conditional operator.
iff :: Parser Insn.Insn
iff =
  (cons <$> (tok (Keyword If) *> tok LPar *> value <* tok RPar))
    <*> gotoInsn
    <*> optional (tok Semicolon *> tok (Keyword Else) *> gotoInsn)
  where
    cons condition thn oth =
      Insn.If {Insn.condition = condition, Insn.thn = thn, Insn.oth = oth}

-- Parse a declaration.
decl :: Parser Insn.Insn
decl = (cons <$> typ) <*> variable <*> optional (tok Assign *> rvalue)
  where
    cons tp var rhs = Insn.Declaration {Insn.typ = tp, Insn.lhs = var, Insn.declRhs = rhs}

-- Parse an instruction.
insn :: Parser Insn.Insn
insn = ret <|> assign <|> assignDeref <|> goto <|> iff <|> decl

-- TODO(@askalt): think about laziness vs energy here.
fold0 :: (b -> a -> b) -> b -> Parser a -> Parser b
fold0 f ini p = Parser (Just . h ini)
  where
    h acc s = case parseFun p s of
      Nothing -> (s, acc)
      Just (suff, e) ->
        h (f acc e) suff

-- Apply parser until it not fail and collect results to the list.
many0 :: Parser a -> Parser [a]
many0 p = reverse <$> fold0 (\xs x -> x : xs) [] p

-- Parse a basic block.
basicBlock :: Parser BasicBlock.BasicBlock
basicBlock = (cons <$> ident) <*> (tok Colon *> many0 (pair insn (tok Semicolon)))
  where
    cons n insns =
      BasicBlock.BasicBlock
        { BasicBlock.label = fromIdent n,
          BasicBlock.insns = map fst insns
        }

-- Parse a function body.
body :: Parser [BasicBlock.BasicBlock]
body = many0 basicBlock

-- Parse a function.
function :: Parser Function.Function
function = (cons <$> sign) <*> (tok LCb *> body <* tok RCb)
  where
    cons (tp, name, args) b =
      Function.Function
        { Function.return_type = tp,
          Function.name = name,
          Function.args = args,
          Function.blocks = b
        }

-- Parse a module.
ast :: Parser Ast.Ast
ast = cons <$> many0 function
  where
    cons funs = Ast.Ast {Ast.funs = funs}

-- Parse a token list with specified parser.
parseWith :: Parser a -> String -> Either Error a
parseWith p s = do
  tokens <- lex0 s
  case parseFun p tokens of
    Nothing -> Left $ ParseError Nothing
    Just (rest, parsed) -> case rest of
      [] -> return parsed
      _ -> Left $ ParseError (Just rest)

-- Main parse runner.
parse :: String -> Either Error Ast.Ast
parse = parseWith ast
