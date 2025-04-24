{
module Parser where
import Lexer
}

%name parseProgram
%tokentype { Token }
%error { parseError }

%token
  "="         { TokenAssign _ }
  "("         { TokenLParen _ }
  ")"         { TokenRParen _ }
  ","         { TokenComma _ }
  "*"         { TokenAllColumns _ }
  "FROM"      { TokenFrom _ }
  "AND"       { TokenAnd _ }
  "OR"        { TokenOr _ }
  "IF"        { TokenIf _ }
  "THEN"      { TokenThen _ }
  "ELSE"      { TokenElse _ }
  "Distinct"  { TokenDistinct _ }
  "Read"      { TokenRead _ }
  "Select"    { TokenSelect _ }
  "Filter"    { TokenFilter _ }
  "Product"   { TokenProduct _ }
  "LeftJoin"  { TokenLeftJoin _ }
  "RightJoin" { TokenRightJoin _ }
  "InnerJoin" { TokenInnerJoin _ }
  "Output"    { TokenOutput _ }
  "=="        { TokenEq _ }
  "!="        { TokenInEq _ }
  "<"         { TokenLt _ }
  ">"         { TokenGt _ }
  "^="        { TokenStartsWith _ }
  "=^"        { TokenEndsWith _ }
  "~="        { TokenContains _ }
  int         { TokenColumn _ $$ }
  str         { TokenString _ $$ }
  file        { TokenFileName _ $$ }

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
  : file "=" Operation                { Assign $1 $3 }

Output
  : "Output" "(" file ")"             { OutputStmt $3 }

Operation
  : "Select" "(" SelectArgs "FROM" FileList ")" {
      SelectOp $3 $5
    }
  | "Filter" "(" file "," ConditionList ")" {
      FilterOp $3 $5
    }
  | "Distinct" "(" int "," file ")"  {
      DistinctOp (read $3) $5
    }
  | "Product" "(" file "," file ")" {
      ProductOp $3 $5
    }
  | "Read" "(" file ")" {
      ReadOp $3
    }
  | "LeftJoin" "(" file "," file ")" {
      LeftJoinOp $3 $5
    }
  | "RightJoin" "(" file "," file ")" {
      RightJoinOp $3 $5
    }
  | "InnerJoin" "(" file "," file ")" {
      InnerJoinOp $3 $5
    }


SelectArgs
  : SelectItem                        { [$1] }
  | SelectItem "," SelectArgs         { $1 : $3 }

SelectItem
  : int                               { SelCol (read $1) }
  | str                               { SelStr $1 }
  | "*"                               { SelAll }
  | ConditionalExpression             { SelCond $1 }

ConditionalExpression
  : "IF" ConditionList "THEN" SelectItem "ELSE" SelectItem {
      Conditional $2 $4 $6
    }

FileList
  : file                              { [$1] }
  | file "," FileList                 { $1 : $3 }

ConditionList
  : Condition                         { CondSimple $1 }
  | ConditionList "AND" ConditionList { CondAnd $1 $3 }
  | ConditionList "OR" ConditionList  { CondOr $1 $3 }

Condition
  : Identifier Comparator Identifier  { Cond $1 $2 $3 }

Identifier
  : file                              { IdFile $1 }
  | int                               { IdCol (read $1) }
  | str                               { IdStr $1 }

Comparator
  : "=="                              { Eq }
  | "!="                              { InEq }
  | "<"                               { Lt }
  | ">"                               { Gt }
  | "~="                              { Contains }
  | "^="                              { StartsWith }
  | "=^"                              { EndsWith }

{
parseError :: [Token] -> a
parseError [] = error "Unknown Parse Error"
parseError (t:_) = error ("Parse error at line:column " ++ tokenPosn t)

showPos :: AlexPosn -> String
showPos (AlexPn _ line col) = show line ++ ":" ++ show col

data Stmt
  = Assign String Operation
  | StmtOp Operation
  | OutputStmt String
  deriving Show

data Operation
  = SelectOp [SelectItem] [String]
  | FilterOp String CondExpr
  | ProductOp String String
  | DistinctOp Int String
  | ReadOp String
  | LeftJoinOp String String
  | RightJoinOp String String
  | InnerJoinOp String String
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
  TokenProduct p        -> showPos p
  TokenLeftJoin p       -> showPos p
  TokenRightJoin p      -> showPos p
  TokenInnerJoin p      -> showPos p
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