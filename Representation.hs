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
data GameDescription = GD [Expression]

gameDescription :: [Expression] -> GameDescription
gameDescription expressions = GD expressions

data Expression = AExpr Atom
				| RExpr Rule
				
atomExpression :: Atom -> Expression
atomExpression a = AExpr a

ruleExpression :: Rule -> Expression
ruleExpression r = RExpr r
					  
--Representation of rules
data Rule = R Atom [Literal]

rule :: Atom -> [Literal] -> Rule
rule head body = R head body
					  
--Representation of literals
data Literal = ALit Atom
			 | NLit Negation
			 
atomLiteral :: Atom -> Literal
atomLiteral a = ALit a

negLiteral :: Negation -> Literal
negLiteral n = NLit n

data Negation = Neg Atom

negation :: Atom -> Negation
negation a = Neg a

data Atom = A RelationConst [Term]

atom :: RelationConst -> [Term] -> Atom
atom relConst terms = A relConst terms

--Representation of functional terms, terms
data FunctionalTerm = FT FunctionConst [Term]

functionalTerm :: FunctionConst -> [Term] -> FunctionalTerm
functionalTerm funcConst terms = FT funcConst terms

data Term = TObj ObjectConst
		  | TVar Variable
		  | TFunc FunctionalTerm
		  
varTerm :: Variable -> Term
varTerm  v = TVar v

objConstTerm :: ObjectConst -> Term
objConstTerm o = TObj o

funcTermTerm :: FunctionalTerm -> Term
funcTermTerm f = TFunc f
					  
--Basic data types for GDL
data FunctionConst = FC String

functionConst :: String -> FunctionConst
functionConst name = FC name

data RelationConst = RC String

relationConst :: String -> RelationConst
relationConst name = RC name

data ObjectConst = OC String

objectConst :: String -> ObjectConst
objectConst name = OC name

data Variable = Var String

variable :: String -> Variable
variable name = Var name

