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
readGameDescription input = case parse matchGameDescription "prefix GDL parser" (unlines (filter (\line -> ((length line) > 0) && (head line) /= ';') (lines input))) of
     Left err -> error $ show err
     Right description -> description

matchGameDescription :: Parser GameDescription
matchGameDescription = do
	expressions <- spaces >> endBy matchExpression spaces
	return $ gameDescription expressions

--Parser for expressions
matchExpression :: Parser Expression
matchExpression = do try matchAtomExpr <|> matchRuleExpr

matchAtomExpr :: Parser Expression
matchAtomExpr = do
	a <- matchAtom
	return $ atomExpression a

matchRuleExpr :: Parser Expression
matchRuleExpr = do
	r <- matchRule
	return $ ruleExpression r
	
--Parser for rules
matchRule :: Parser Rule
matchRule = do
	head <- char '(' >> spaces >> string "<=" >> spaces >> matchAtom
	subgoals <- spaces >> endBy matchLiteral spaces
	spaces >> char ')'
	return $ rule head subgoals

--Parsers for literals
matchLiteral :: Parser Literal
matchLiteral = do try matchAtomLiteral <|> matchNegationLiteral

matchAtomLiteral :: Parser Literal
matchAtomLiteral = do
	a <- matchAtom
	return $ atomLiteral a
	
matchNegationLiteral :: Parser Literal
matchNegationLiteral = do
	n <- matchNegation
	return $ negLiteral n

matchNegation :: Parser Negation
matchNegation = do
	char '(' >> spaces >> string "not" >> spaces
	a <- matchAtom
	spaces >> char ')'
	return $ negation a
	
matchAtom :: Parser Atom
matchAtom = do try matchHighArityAtom <|> matchArityZeroAtom

--Match an atom with arity greater than or equal to 1
matchHighArityAtom :: Parser Atom
matchHighArityAtom = do
	head <- char '(' >> spaces >> matchRelConst
	spaces
	terms <- endBy matchTerm spaces
	spaces >> char ')'
	return $ atom head terms

--Match an atom whose relation constant has arity 0
matchArityZeroAtom :: Parser Atom
matchArityZeroAtom = do
	head <- spaces >> matchRelConst
	spaces
	return $ atom head []
	
--Parser for functional terms
matchFunctionalTerm :: Parser FunctionalTerm
matchFunctionalTerm = do
	head <- char '(' >> spaces >> matchFuncConst
	spaces
	terms <- endBy matchTerm spaces
	spaces >> char ')'
	return $ functionalTerm head terms
	
matchTerm :: Parser Term
matchTerm = try matchVarTerm <|> try matchObjConstTerm <|> matchFuncTerm

matchVarTerm :: Parser Term
matchVarTerm = do
	x <- matchVar
	return $ varTerm x
	
matchObjConstTerm :: Parser Term
matchObjConstTerm = do
	x <- matchObjConst
	return $ objConstTerm x
	
matchFuncTerm :: Parser Term
matchFuncTerm = do
	x <- matchFunctionalTerm
	return $ funcTermTerm x
	
--Parsers for basic types
matchVar :: Parser Variable
matchVar = do
	char '?'
	x <- many1 nameChars
	return $ variable x
	
matchFuncConst :: Parser FunctionConst
matchFuncConst = do
	x <- many1 nameChars
	return $ functionConst x
	
matchRelConst :: Parser RelationConst
matchRelConst = do
	x <- many1 nameChars
	return $ relationConst x

matchObjConst :: Parser ObjectConst
matchObjConst = do
	x <- many1 nameChars
	return $ objectConst x
	
nameChars :: Parser Char
nameChars = oneOf "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890_"