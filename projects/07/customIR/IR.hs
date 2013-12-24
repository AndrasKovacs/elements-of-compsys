{-# LANGUAGE LambdaCase #-}

module IR (compile) where


import qualified Text.Trifecta as T
import           Text.Trifecta (Parser)

import Control.Applicative 
import Control.Monad.State.Strict 
import Data.List
import Data.Maybe
import Data.Monoid
import Text.Printf


retBase = 5 :: Int 
retEnd = 12 :: Int


-- TODO : inline comment 


-- *************** Codegen ****************


data Expr = I Int 
          | BinOp String Expr Expr 
          | Cmp String Expr Expr 
          | UnOp String Expr 
          | Deref Expr deriving (Eq, Show) 


data Ins = Assign Expr Expr 
         | Goto String 
         | GotoIf String Expr -- goto label if eval(expr) == True 
         | Label String deriving (Eq, Show)


type Labeled = State Int 


newLabel :: Labeled String
newLabel = gets (printf "__%d") <* modify (+1)


ldi   = printf "@%d"
ldl   = printf "@%s"
label = printf "(%s)"


cmp :: String -> Int -> Labeled [String]
cmp op r = do -- values in r and (r + 1)
    [cond, end] <- replicateM 2 newLabel
    pure [
        "D=M", -- r + 1 already loaded
        ldi r,
        "D=M-D",
        ldl cond,
        printf "D;J%s" op,
        ldi r,
        "M=0",
        ldl end,
        "0;JMP",
        label cond,
        ldi r,
        "M=1",
        label end]


evalE :: Expr -> Int -> Labeled [String]
evalE _ r | r > retEnd = error "maximum expression depth exceeded"

evalE (I i)          r = pure [ldi i, "D=A", ldi r, "M=D"]
evalE (UnOp "dec" e) r = (++) <$> evalE e r <*> pure ["M=M-1"]
evalE (UnOp "inc" e) r = (++) <$> evalE e r <*> pure ["M=M+1"]
evalE (UnOp op e)    r = (++) <$> evalE e r <*> pure [printf "M=%sM" op]
evalE (Deref e)      r = (++) <$> evalE e r <*> pure ["A=M", "D=M", ldi r, "M=D"]

evalE (Cmp op a b) r = concat <$> sequence [
    evalE a r, evalE b (r + 1), cmp op r]

evalE (BinOp op a b) r = concat <$> sequence [
    evalE b r, evalE a (r + 1), pure ["D=M", ldi r, printf "M=D%sM" op]]


evalIns :: Ins -> Labeled [String]
evalIns (Assign (Deref (I x)) (Deref (I y))) = pure [ldi y, "D=M", ldi x, "A=M", "M=D"]
evalIns (Assign (I x)         (Deref (I y))) = pure [ldi y, "D=M", ldi x,        "M=D"]
evalIns (Assign (Deref (I x)) (I y))         = pure [ldi y, "D=A", ldi x, "A=M", "M=D"]
evalIns (Assign (I x)         (I y))         = pure [ldi y, "D=A", ldi x,        "M=D"]

evalIns (Assign (I x) (BinOp op (Deref (I y)) (I c))) | y == x = 
    pure [ldi y, "D=M", ldi c, printf "D=D%sA" op, ldi y, "M=D"]

evalIns (Assign (I x) (UnOp op (Deref (I y)))) | y == x = 
    pure $ case op of
        "dec" -> [ldi x, "M=M-1"]
        "inc" -> [ldi x, "M=M+1"]
        _     -> [ldi x, printf "M=%sM" op]

evalIns (Assign a b) = concat <$> sequence [
    evalE a retBase, evalE b (retBase + 1), pure ["D=M", ldi retBase, "A=M", "M=D"]]

evalIns (Goto   label)   = pure [ldl label, "0;JMP"] 
evalIns (GotoIf label e) = (++) <$> evalE e retBase <*> pure ["D=M", ldl label, "D;JNE"]
evalIns (Label  l)       = pure [label l]


eval :: [Ins] -> String
eval xs = intercalate "\n" $ concat $ evalState (mapM evalIns xs) 0
 


-- ***************- Parser ****************


spaces :: Parser [Char]
spaces = T.many (T.char ' ')


pUnOp :: Parser Expr
pUnOp = do
    op <- (:[]) <$> T.oneOf "-!*" -- ! BITWISE op, not logical!
    e  <- pExpr'
    if op == "*"
        then pure $ Deref e
        else pure $ UnOp op e 

pBinary :: Parser (Expr -> Expr -> Expr)
pBinary = T.choice [
    BinOp "+"   <$ T.char '+',
    BinOp "-"   <$ T.char '-',
    Cmp   "GT"  <$ T.char '>',
    Cmp   "LT"  <$ T.char '<',
    Cmp   "EQ"  <$ T.string "==",
    BinOp "&"   <$ T.char '&',
    BinOp "|"   <$ T.char '|'] 
    <* spaces

pBuiltinVar :: Parser Expr
pBuiltinVar = do
    let vars = ["SP", "LCL", "ARG", "THIS", "THAT", "KBD", "SCREEN"]
        vals = [0, 1, 2, 3, 4, 0x4000, 0x6000 :: Int]
    var <- T.choice $ map T.string vars 
    maybe undefined (pure . I) $ lookup var (zip vars vals)


pExpr' :: Parser Expr
pExpr' = (pUnOp  
    <|> (I . fromIntegral <$> T.integer') 
    <|> pBuiltinVar 
    <|> T.char '(' *> spaces *> pExpr <* T.char ')') <* spaces


pExpr :: Parser Expr
pExpr = T.chainl1 pExpr' pBinary <|> pExpr'


pAssign :: Parser Ins
pAssign = 
    T.sepBy pExpr (T.char '=' <* spaces) >>= \case
        [lhs, rhs] -> pure $ Assign lhs rhs
        _          -> fail "parse error"


pSymbol :: Parser String
pSymbol = (:) <$> nonDigit <*> T.many (nonDigit <|> T.digit) where
    nonDigit = T.letter <|> T.oneOf "_.$:"


pGoto :: Parser Ins
pGoto = Goto <$> (T.string "goto" *> spaces *> pSymbol <* spaces)


pGotoIf :: Parser Ins
pGotoIf = do
    T.string "gotoif" <* spaces
    GotoIf <$> (pSymbol <* spaces) <*> pExpr 


pLabel :: Parser Ins
pLabel = T.string "label" *> spaces *> (Label <$> pSymbol) <* spaces


pIns :: Parser Ins
pIns = (pAssign <|> pGoto <|> pGotoIf <|> pLabel) <* (T.newline <|> T.char ';')


pComment :: Parser ()
pComment = void $ T.string "//" *> T.manyTill T.anyChar T.newline


pLine :: Parser (Maybe Ins)
pLine = spaces *> ( 
        (Nothing <$  pComment) 
    <|> (Just    <$> pIns) 
    <|> (Nothing <$  T.newline))


parseIR :: Parser [Ins]
parseIR = catMaybes <$> T.many pLine


parse :: String -> [Ins]
parse s = 
    case T.parseString parseIR mempty (s ++ "\n") of
        T.Failure e  -> error $ show e 
        T.Success xs -> xs 


compile :: String -> String
compile = eval . parse


main = putStrLn $ compile "*SP - 2 = *(*SP - 2) + *(SP - 1)\n SP = *SP - 1"