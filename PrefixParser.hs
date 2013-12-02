module PrefixParser(readGameDescription) where

import Text.ParserCombinators.Parsec
import Control.Monad
import Representation
import System.IO

readGameDescriptionFromFile :: String -> IO GameDescription
readGameDescriptionFromFile fileName = do
	handle <- openFile fileName ReadMode
	contents <- hGetContents handle
	return $ readGameDescription contents

readGameDescription :: String -> GameDescription
readGameDescription input = case parse matchGameDescription 
	"prefix GDL parser"
	(unlines (filter (\line -> ((length line) > 0) && (head line) /= ';') (lines input))) of
     Left err -> error $ show err
     Right description -> description

matchGameDescription :: Parser GameDescription
matchGameDescription = do
	expressions <- spaces >> endBy matchExpression spaces
	return $ GD expressions

--Parser for expressions
matchExpression :: Parser Expression
matchExpression = do try matchAtomExpr <|> matchRuleExpr

matchAtomExpr :: Parser Expression
matchAtomExpr = do
	a <- matchAtom
	return $ AExpr a

matchRuleExpr :: Parser Expression
matchRuleExpr = do
	r <- matchRule
	return $ RExpr r
	
--Parser for rules
matchRule :: Parser Rule
matchRule = do
	head <- char '(' >> spaces >> string "<=" >> spaces >> matchAtom
	subgoals <- spaces >> endBy matchLiteral spaces
	spaces >> char ')'
	return $ R head subgoals

--Parsers for literals
matchLiteral :: Parser Literal
matchLiteral = do try matchAtomLiteral <|> matchNegationLiteral

matchAtomLiteral :: Parser Literal
matchAtomLiteral = do
	a <- matchAtom
	return $ ALit a
	
matchNegationLiteral :: Parser Literal
matchNegationLiteral = do
	n <- matchNegation
	return $ NLit n

matchNegation :: Parser Negation
matchNegation = do
	char '(' >> spaces >> string "not" >> spaces
	a <- matchAtom
	spaces >> char ')'
	return $ Neg a
	
matchAtom :: Parser Atom
matchAtom = do try matchHighArityAtom <|> matchArityZeroAtom

--Match an atom with arity greater than or equal to 1
matchHighArityAtom :: Parser Atom
matchHighArityAtom = do
	head <- char '(' >> spaces >> matchRelConst
	spaces
	terms <- endBy matchTerm spaces
	spaces >> char ')'
	return $ A head terms

--Match an atom whose relation constant has arity 0
matchArityZeroAtom :: Parser Atom
matchArityZeroAtom = do
	head <- spaces >> matchRelConst
	spaces
	return $ A head []
	
--Parser for functional terms
matchFunctionalTerm :: Parser FunctionalTerm
matchFunctionalTerm = do
	head <- char '(' >> spaces >> matchFuncConst
	spaces
	terms <- endBy matchTerm spaces
	spaces >> char ')'
	return $ FT head terms
	
matchTerm :: Parser Term
matchTerm = try matchVarTerm <|> try matchObjConstTerm <|> matchFuncTerm

matchVarTerm :: Parser Term
matchVarTerm = do
	x <- matchVar
	return $ TVar x
	
matchObjConstTerm :: Parser Term
matchObjConstTerm = do
	x <- matchObjConst
	return $ TObj x
	
matchFuncTerm :: Parser Term
matchFuncTerm = do
	x <- matchFunctionalTerm
	return $ TFunc x
	
--Parsers for basic types
matchVar :: Parser Variable
matchVar = do
	char '?'
	x <- many1 nameChars
	return $ Var x
	
matchFuncConst :: Parser FunctionConst
matchFuncConst = do
	x <- many1 nameChars
	return $ FC x
	
matchRelConst :: Parser RelationConst
matchRelConst = do
	x <- many1 nameChars
	return $ RC x

matchObjConst :: Parser ObjectConst
matchObjConst = do
	x <- many1 nameChars
	return $ OC x
	
nameChars :: Parser Char
nameChars = oneOf "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890_"