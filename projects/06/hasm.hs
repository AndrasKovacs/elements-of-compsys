{-# LANGUAGE LambdaCase, BangPatterns #-}

import Control.Applicative
import Control.Arrow
import Control.Monad
import Control.Lens
import Numeric.Lens
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Text.Trifecta as P
import Data.List
import Data.Ord
import Data.Monoid
import Data.Maybe
import System.Environment 

sortCmds :: [(String, String)] -> [(String, String)]
sortCmds = sortBy (flip $ comparing (length . fst))

padToACmd :: String -> String
padToACmd s = replicate (16 - length s) '0' ++ s

varNub :: [String] -> [String]
varNub = reverse . snd . foldl' go (S.empty, []) where
    go (!s, !acc) x | S.member x s = (s, acc)
                    | otherwise    = (S.insert x s, x:acc)

reserved :: M.Map String String
reserved = M.map (padToACmd . (binary#)) $ M.fromList reserved' where
    registers   = map (('R':) . show &&& id) [0..15]
    regSynonyms = zip (words "SP LCL ARG THIS THAT") [0..]
    mmaps       = zip (words "SCREEN KBD") [0x4000, 0x6000]
    reserved'   = registers ++ regSynonyms ++ mmaps 

dest = sortCmds [
    ("null",    "000"),
    ("0",       "000"),
    ("M",       "001"),
    ("D",       "010"),
    ("MD",      "011"),
    ("A",       "100"),
    ("AM",      "101"),
    ("AD",      "110"),
    ("AMD",     "111")]

jump = sortCmds [
    ("null",    "000"),
    ("JGT",     "001"),
    ("JEQ",     "010"),
    ("JGE",     "011"),
    ("JLT",     "100"),
    ("JNE",     "101"),
    ("JLE",     "110"),
    ("JMP",     "111")]

rhs = sortCmds [
    ("0",   "0101010"),
    ("1",   "0111111"),
    ("-1",  "0111010"),
    ("D",   "0001100"),
    ("A",   "0110000"),
    ("!D",  "0001101"),
    ("!A",  "0110001"),
    ("-D",  "0001111"),
    ("-A",  "0110011"),
    ("D+1", "0011111"),
    ("A+1", "0110111"),
    ("D-1", "0001110"),
    ("A-1", "0110010"),
    ("D+A", "0000010"),
    ("D-A", "0010011"),
    ("A-D", "0000111"),
    ("D&A", "0000000"),
    ("D|A", "0010101"),
    ("M",   "1110000"),
    ("!M",  "1110001"),
    ("-M",  "1110011"),
    ("M+1", "1110111"),
    ("M-1", "1110010"),
    ("D+M", "1000010"),
    ("D-M", "1010011"),
    ("M-D", "1000111"), 
    ("D&M", "1000000"), 
    ("D|M", "1010101")]

data Asm = C String | AImmediate String | ASymbol String | Label String deriving (Eq, Show)

pCmd :: [(String, String)] -> P.Parser String
pCmd = P.choice . map (\(sym, cmd) -> cmd <$ P.string sym)

pAssign :: P.Parser Asm
pAssign = do
    dest <- pCmd dest
    P.char '='
    comp <- pCmd rhs
    pure $ C $ concat ["111", comp, dest, "000"]

pJump :: P.Parser Asm 
pJump = do
    comp <- pCmd rhs
    P.char ';'
    jump <- pCmd jump
    pure $ C $ concat ["111", comp, "000", jump]

pSymbol :: P.Parser String
pSymbol = (:) <$> nonDigit <*> P.many (nonDigit <|> P.digit) where
    nonDigit = P.letter <|> P.oneOf "_.$:"

pLabel :: P.Parser Asm
pLabel = do
    P.char '('
    sym <- pSymbol
    P.char ')'
    when (M.member sym reserved) $ fail "cannot use reserved symbol"
    pure $ Label sym

pImmediate :: P.Parser Asm
pImmediate = do
    n <- (binary#) . read <$> P.some P.digit
    when (length n > 15) $ fail "Immediate value greater than 32767"
    pure $ AImmediate $ padToACmd n 

pACmd :: P.Parser Asm 
pACmd = P.char '@' >> pImmediate <|> (ASymbol <$> pSymbol)

pLine :: P.Parser (Maybe Asm)
pLine = let
    comment = P.string "//" >> P.manyTill P.anyChar P.newline
    spaces' = P.many $ P.char ' '
    close   = void comment <|> void P.newline 
    label   = pLabel <* spaces' <* close
    ins     = (pACmd <|> P.try pJump <|> pAssign) <* spaces' <* close
    in spaces' >> P.choice [Nothing <$ close, Just <$> label, Just <$> ins]

parseAsm :: P.Parser [Asm]
parseAsm = catMaybes <$> P.many pLine

parse :: String -> [Asm]
parse s = 
    case P.parseString parseAsm mempty (s ++ "\n") of
        P.Failure e  -> error $ show e 
        P.Success xs -> xs 

procLabels :: [Asm] -> String
procLabels xs = let
    xs' = snd $ foldl go (0, []) xs where
        go (i, acc) x@Label{} = (i,     (i, x):acc)
        go (i, acc) x         = (i + 1, (i, x):acc)

    labelList = [(l, padToACmd $ binary # i) | (i, Label l) <- xs']
    labelMap  = M.fromListWithKey (\l _ _ -> error $ "multiple label definition: " ++ l) labelList

    varList = varNub [ v | ASymbol v <- xs, M.notMember v labelMap, M.notMember v reserved]
    varMap  = M.fromList $ zip varList (map (padToACmd . (binary#)) [16..])
    symMap  = M.unions [labelMap, varMap, reserved]

    in xs >>= \case 
        ASymbol v    -> symMap M.! v ++ "\n";
        Label l      -> "";
        C cmd        -> cmd ++ "\n";
        AImmediate s -> s   ++ "\n"

main :: IO ()
main = 
    getArgs >>= \case
        [inp, out] -> writeFile out =<< (procLabels . parse <$> readFile inp)
        _          -> putStrLn "usage: hasm [input] [output]"
