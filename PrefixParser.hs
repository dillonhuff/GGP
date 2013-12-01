module PrefixParser(readGameDescription) where

import Text.ParserCombinators.Parsec
import System.Environment
import Control.Monad
import Representation

readGameDescription :: String -> String
readGameDescription input = case parse matchGameDescription "prefix GDL parser" input of
     Left err -> "No match: " ++ show err
     Right _ -> "Found value"

matchGameDescription :: Parser GameDescription
matchGameDescription = do
	expressions <- spaces >> sepBy matchExpression spaces
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
	subgoals <- spaces >> sepBy matchLiteral spaces
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
matchAtom = do
	head <- char '(' >> spaces >> matchRelConst
	spaces
	terms <- sepBy matchTerm spaces
	spaces >> char ')'
	return $ atom head terms

--Parser for functional terms
matchFunctionalTerm :: Parser FunctionalTerm
matchFunctionalTerm = do
	head <- char '(' >> spaces >> matchFuncConst
	spaces
	terms <- sepBy matchTerm spaces
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
	x <- many1 alphaNum
	return $ variable x
	
matchFuncConst :: Parser FunctionConst
matchFuncConst = do
	x <- many1 alphaNum
	return $ functionConst x
	
matchRelConst :: Parser RelationConst
matchRelConst = do
	x <- many1 alphaNum
	return $ relationConst x

matchObjConst :: Parser ObjectConst
matchObjConst = do
	x <- many1 alphaNum
	return $ objectConst x