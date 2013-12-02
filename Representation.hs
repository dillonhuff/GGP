--This module contains the intermediate representation of
--a game that is constructed by the PrefixParser function
--readGameDescription
module Representation(FunctionConst,
					  RelationConst,
					  ObjectConst,
					  Variable,
					  functionConst,
					  relationConst,
					  objectConst,
					  variable,
					  Term,
					  varTerm,
					  objConstTerm,
					  funcTermTerm,
					  FunctionalTerm,
					  functionalTerm,
					  Atom,
					  atom,
					  Negation,
					  negation,
					  Literal,
					  atomLiteral,
					  negLiteral,
					  Rule,
					  rule,
					  Expression,
					  atomExpression,
					  ruleExpression,
					  GameDescription,
					  gameDescription) where
					  
--Representation of full game description
data GameDescription = GD [Expression] deriving Show

gameDescription :: [Expression] -> GameDescription
gameDescription expressions = GD expressions

data Expression = AExpr Atom
				| RExpr Rule
				deriving Show
				
atomExpression :: Atom -> Expression
atomExpression a = AExpr a

ruleExpression :: Rule -> Expression
ruleExpression r = RExpr r
					  
--Representation of rules
data Rule = R Atom [Literal] deriving Show

rule :: Atom -> [Literal] -> Rule
rule head body = R head body
					  
--Representation of literals
data Literal = ALit Atom
			 | NLit Negation
			 deriving Show
			 
atomLiteral :: Atom -> Literal
atomLiteral a = ALit a

negLiteral :: Negation -> Literal
negLiteral n = NLit n

data Negation = Neg Atom deriving Show

negation :: Atom -> Negation
negation a = Neg a

data Atom = A RelationConst [Term] deriving Show

atom :: RelationConst -> [Term] -> Atom
atom relConst terms = A relConst terms

--Representation of functional terms, terms
data FunctionalTerm = FT FunctionConst [Term] deriving Show

functionalTerm :: FunctionConst -> [Term] -> FunctionalTerm
functionalTerm funcConst terms = FT funcConst terms

data Term = TObj ObjectConst
		  | TVar Variable
		  | TFunc FunctionalTerm
		  deriving Show
		  
varTerm :: Variable -> Term
varTerm  v = TVar v

objConstTerm :: ObjectConst -> Term
objConstTerm o = TObj o

funcTermTerm :: FunctionalTerm -> Term
funcTermTerm f = TFunc f
					  
--Basic data types for GDL
data FunctionConst = FC String deriving Show

functionConst :: String -> FunctionConst
functionConst name = FC name

data RelationConst = RC String deriving Show

relationConst :: String -> RelationConst
relationConst name = RC name

data ObjectConst = OC String deriving Show

objectConst :: String -> ObjectConst
objectConst name = OC name

data Variable = Var String deriving Show

variable :: String -> Variable
variable name = Var name

--Functions for decomposing larger components into smaller pieces
expressionToAtoms :: Expression -> [Atom]
expressionToAtoms (AExpr a) = [a]
expressionToAtoms (RExpr r) = ruleToAtoms r

ruleToAtoms :: Rule -> [Atom]
ruleToAtoms (R head subgoals) = head:(map literalToAtom subgoals)

literalToAtom :: Literal -> Atom
literalToAtom (ALit a) = a
literalToAtom (NLit (Neg a)) = a

