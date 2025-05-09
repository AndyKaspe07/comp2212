{
module SiftParser where
import SiftLexer
}

%name parseProgram
%tokentype { Token }
%error { happyError }

%token
  "="            { TokenAssign p }
  "("            { TokenLParen p }
  ")"            { TokenRParen p }
  ","            { TokenComma p }
  "*"            { TokenAllColumns p }
  "FROM"         { TokenFrom p }
  "AND"          { TokenAnd p }
  "OR"           { TokenOr p }
  "IF"           { TokenIf p }
  "THEN"         { TokenThen p }
  "ELSE"         { TokenElse p }
  "Distinct"     { TokenDistinct p }
  "Read"         { TokenRead p }
  "Select"       { TokenSelect p }
  "Filter"       { TokenFilter p }
  "Union"        { TokenUnion p }
  "Intersection" { TokenIntersection p }
  "Append"       { TokenAppend p }
  "Difference"   { TokenDifference p }
  "Top"          { TokenTop p }
  "Bottom"       { TokenBottom p }
  "Product"      { TokenProduct p }
  "LeftJoin"     { TokenLeftJoin p }
  "RightJoin"    { TokenRightJoin p }
  "InnerJoin"    { TokenInnerJoin p }
  "OuterJoin"    { TokenOuterJoin p }
  "Output"       { TokenOutput p }
  "=="           { TokenEq p }
  "!="           { TokenInEq p }
  "<"            { TokenLt p }
  ">"            { TokenGt p }
  "^="           { TokenStartsWith p }
  "=^"           { TokenEndsWith p }
  "~="           { TokenContains p }
  int            { TokenColumn p s }
  str            { TokenString p s }
  file           { TokenFileName p s }


%nonassoc "==" "!=" "<" ">" "^=" "=^" "~="
%left "AND" "OR"

%%

Program
  : StatementList                     { $1 }

StatementList
  : Statement                         { [$1] }
  | Statement StatementList           { $1 : $2 }

Statement
  : Assignment                        { $1 }
  | Operation                         { StmtOp $1 }
  | Output                            { $1 }

Assignment
  : file "=" Operation                { case $1 of TokenFileName _ s -> Assign s $3 }

Output
  : "Output" "(" file ")"             { case $3 of TokenFileName _ s -> OutputStmt s }

Operation
  : "Select" "(" SelectArgs "FROM" file ")" {
    let { TokenFileName _ f = $5 } in SelectOp $3 f
    }
  | "Filter" "(" file "," ConditionList ")" {
      let { TokenFileName _ f = $3 } in FilterOp f $5
    }
  | "Distinct" "(" file "," int ")" {
      let { TokenFileName _ f = $3; TokenColumn _ i = $5 } in DistinctOp f (read i)
    }
  | "Product" "(" file "," file ")" {
      let { TokenFileName _ f1 = $3; TokenFileName _ f2 = $5 } in ProductOp f1 f2
    }
  | "Union" "(" file "," file ")" {
      let { TokenFileName _ f1 = $3; TokenFileName _ f2 = $5 } in UnionOp f1 f2
    }
  | "Intersection" "(" file "," file ")" {
      let { TokenFileName _ f1 = $3; TokenFileName _ f2 = $5 } in IntersectionOp f1 f2
    }
  | "Append" "(" file "," file ")" {
      let { TokenFileName _ f1 = $3; TokenFileName _ f2 = $5 } in AppendOp f1 f2
    }
  | "Difference" "(" file "," file ")" {
      let { TokenFileName _ f1 = $3; TokenFileName _ f2 = $5 } in DifferenceOp f1 f2
    }
  | "Read" "(" file ")" {
      let { TokenFileName _ f = $3 } in ReadOp f
    }
  | "Top" "(" file "," int ")" {
      let { TokenFileName _ f = $3; TokenColumn _ i = $5 } in TopOp f (read i)
    }
  | "Bottom" "(" file "," int ")" {
      let { TokenFileName _ f = $3; TokenColumn _ i = $5 } in BottomOp f (read i)
    }
  | "LeftJoin" "(" file "," file ")" {
      let { TokenFileName _ f1 = $3; TokenFileName _ f2 = $5 } in LeftJoinOp f1 f2
    }
  | "RightJoin" "(" file "," file ")" {
      let { TokenFileName _ f1 = $3; TokenFileName _ f2 = $5 } in RightJoinOp f1 f2
    }
  | "InnerJoin" "(" file "," file ")" {
      let { TokenFileName _ f1 = $3; TokenFileName _ f2 = $5 } in InnerJoinOp f1 f2
    }
  | "OuterJoin" "(" file "," file ")" {
      let { TokenFileName _ f1 = $3; TokenFileName _ f2 = $5 } in OuterJoinOp f1 f2
    }



SelectArgs
  : SelectItem                        { [$1] }
  | SelectItem "," SelectArgs         { $1 : $3 }

SelectItem
  : int                               { case $1 of TokenColumn _ n -> SelCol (read n) }
  | str                               { case $1 of TokenString _ s -> SelStr s }
  | "*"                               { SelAll }
  | ConditionalExpression             { SelCond $1 }

ConditionalExpression
  : "IF" ConditionList "THEN" SelectItem "ELSE" SelectItem {
      Conditional $2 $4 $6
    }

ConditionList
  : Condition                         { CondSimple $1 }
  | ConditionList "AND" ConditionList { CondAnd $1 $3 }
  | ConditionList "OR" ConditionList  { CondOr $1 $3 }

Condition
  : Identifier Comparator Identifier  { Cond $1 $2 $3 }

Identifier
  : file                              { case $1 of TokenFileName _ s -> IdFile s }
  | int                               { case $1 of TokenColumn _ s -> IdCol (read s) }
  | str                               { case $1 of TokenString _ s -> IdStr s }

Comparator
  : "=="                              { Eq }
  | "!="                              { InEq }
  | "<"                               { Lt }
  | ">"                               { Gt }
  | "~="                              { Contains }
  | "^="                              { StartsWith }
  | "=^"                              { EndsWith }

{
happyError :: [Token] -> a
happyError [] = error "Unknown Parse Error"
happyError (t:_) = error ("Parse error at line:column " ++ tokenPosn t)

showPos :: AlexPosn -> String
showPos (AlexPn _ line col) = show line ++ ":" ++ show col

data Stmt
  = Assign String Operation
  | StmtOp Operation
  | OutputStmt String
  deriving Show

data Operation
  = SelectOp [SelectItem] String
  | FilterOp String CondExpr
  | ProductOp String String
  | DistinctOp String Int
  | UnionOp String String
  | IntersectionOp String String
  | AppendOp String String
  | DifferenceOp String String
  | TopOp String Int
  | BottomOp String Int
  | ReadOp String
  | LeftJoinOp String String
  | RightJoinOp String String
  | InnerJoinOp String String
  | OuterJoinOp String String
  deriving Show

data SelectItem
  = SelCol Int
  | SelStr String
  | SelCond Conditional
  | SelAll
  deriving Show

data Conditional = Conditional CondExpr SelectItem SelectItem
  deriving Show

data CondExpr
  = CondSimple Cond
  | CondAnd CondExpr CondExpr
  | CondOr CondExpr CondExpr
  deriving Show

data Cond = Cond Ident Comparator Ident
  deriving Show

data Ident = IdFile String | IdCol Int | IdStr String
  deriving Show

data Comparator = Eq | Lt | Gt | InEq | Contains | StartsWith | EndsWith
  deriving Show

tokenPosn :: Token -> String
tokenPosn tok = case tok of
  TokenAssign p         -> showPos p
  TokenLParen p         -> showPos p
  TokenRParen p         -> showPos p
  TokenComma p          -> showPos p
  TokenFrom p           -> showPos p
  TokenDistinct p       -> showPos p
  TokenIf p             -> showPos p
  TokenAnd p            -> showPos p
  TokenOr p             -> showPos p
  TokenThen p           -> showPos p
  TokenElse p           -> showPos p
  TokenRead p           -> showPos p
  TokenSelect p         -> showPos p
  TokenFilter p         -> showPos p
  TokenUnion p          -> showPos p
  TokenAppend p         -> showPos p
  TokenDifference p     -> showPos p
  TokenIntersection p   -> showPos p
  TokenBottom p         -> showPos p
  TokenTop p            -> showPos p
  TokenProduct p        -> showPos p
  TokenLeftJoin p       -> showPos p
  TokenRightJoin p      -> showPos p
  TokenInnerJoin p      -> showPos p
  TokenOuterJoin p      -> showPos p
  TokenOutput p         -> showPos p
  TokenEq p             -> showPos p
  TokenInEq p           -> showPos p
  TokenAllColumns p     -> showPos p
  TokenLt p             -> showPos p
  TokenGt p             -> showPos p
  TokenContains p       -> showPos p
  TokenStartsWith p     -> showPos p
  TokenEndsWith p       -> showPos p
  TokenColumn p _       -> showPos p
  TokenFileName p _     -> showPos p
  TokenString p _       -> showPos p
}