--This module contains the intermediate representation of
--a game that is constructed by the PrefixParser function
--readGameDescription
module Representation(FunctionConst(FC),
					  RelationConst(RC),
					  ObjectConst(OC),
					  Variable(Var),
					  Term(TObj, TFunc, TVar),
					  FunctionalTerm(FT),
					  Atom(A),
					  Negation(Neg),
					  Literal(ALit, NLit),
					  Rule(R),
					  Expression(AExpr, RExpr),
					  GameDescription(GD)
					  ) where
					  
--Representation of full game description
data GameDescription = GD [Expression] deriving Show

data Expression = AExpr Atom
				| RExpr Rule
				deriving Show
					  
--Representation of rules
data Rule = R Atom [Literal] deriving Show

--Representation of literals
data Literal = ALit Atom
			 | NLit Negation
			 deriving Show
			 
data Negation = Neg Atom deriving Show

data Atom = A RelationConst [Term] deriving Show

--Representation of functional terms, terms
data FunctionalTerm = FT FunctionConst [Term] deriving Show

data Term = TObj ObjectConst
		  | TVar Variable
		  | TFunc FunctionalTerm
		  deriving Show
					  
--Basic data types for GDL
data FunctionConst = FC String deriving Show

data RelationConst = RC String deriving Show

data ObjectConst = OC String deriving Show

data Variable = Var String deriving Show

--Functions for decomposing larger components into smaller pieces
expressionToAtoms :: Expression -> [Atom]
expressionToAtoms (AExpr a) = [a]
expressionToAtoms (RExpr r) = ruleToAtoms r

ruleToAtoms :: Rule -> [Atom]
ruleToAtoms (R head subgoals) = head:(map literalToAtom subgoals)

literalToAtom :: Literal -> Atom
literalToAtom (ALit a) = a
literalToAtom (NLit (Neg a)) = a

