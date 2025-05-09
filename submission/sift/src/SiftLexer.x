{
module SiftLexer where
}

%wrapper "posn"

$digit     = 0-9
$alpha     = [a-zA-Z]
$alphanum  = [$alpha$digit]

tokens :-
  $white+                ;
  "//"[^\n]*             ;
  "="                   { \p _ -> TokenAssign p }
  "("                   { \p _ -> TokenLParen p }
  ")"                   { \p _ -> TokenRParen p }
  ","                   { \p _ -> TokenComma p }
  "FROM"                { \p _ -> TokenFrom p }
  "AND"                 { \p _ -> TokenAnd p }
  "OR"                  { \p _ -> TokenOr p }
  "IF"                  { \p _ -> TokenIf p }
  "THEN"                { \p _ -> TokenThen p }
  "ELSE"                { \p _ -> TokenElse p }
  "Read"                { \p _ -> TokenRead p }
  "Select"              { \p _ -> TokenSelect p }
  "Union"               { \p _ -> TokenUnion p }
  "Intersection"        { \p _ -> TokenIntersection p }
  "Append"              { \p _ -> TokenAppend p }
  "Difference"          { \p _ -> TokenDifference p }
  "Filter"              { \p _ -> TokenFilter p }
  "Top"                 { \p _ -> TokenTop p }
  "Bottom"              { \p _ -> TokenBottom p }
  "Distinct"            { \p _ -> TokenDistinct p}
  "Product"             { \p _ -> TokenProduct p }
  "LeftJoin"            { \p _ -> TokenLeftJoin p }
  "RightJoin"           { \p _ -> TokenRightJoin p }
  "InnerJoin"           { \p _ -> TokenInnerJoin p }
  "OuterJoin"           { \p _ -> TokenOuterJoin p }
  "Output"              { \p _ -> TokenOutput p }
  "=="                  { \p _ -> TokenEq p }
  "!="                  { \p _ -> TokenInEq p }
  "<"                   { \p _ -> TokenLt p }
  ">"                   { \p _ -> TokenGt p }
  "*"                   { \p _ -> TokenAllColumns p}
  "^="                  { \p _ -> TokenStartsWith p}
  "=^"                  { \p _ -> TokenEndsWith p}
  "~="                  { \p _ -> TokenContains p}
  \"[^\"]*\"            { \p s -> TokenString p (read s) }
  $digit+               { \p s -> TokenColumn p s }
  $alphanum+            { \p s -> TokenFileName p s }
 

{
data Token
  = TokenAssign AlexPosn
  | TokenLParen AlexPosn
  | TokenRParen AlexPosn
  | TokenComma AlexPosn
  | TokenFrom AlexPosn
  | TokenIf AlexPosn
  | TokenThen AlexPosn
  | TokenElse AlexPosn
  | TokenRead AlexPosn
  | TokenAnd AlexPosn
  | TokenOr AlexPosn
  | TokenSelect AlexPosn
  | TokenDistinct AlexPosn
  | TokenUnion AlexPosn
  | TokenTop AlexPosn
  | TokenBottom AlexPosn
  | TokenFilter AlexPosn
  | TokenProduct AlexPosn
  | TokenIntersection AlexPosn
  | TokenAppend AlexPosn
  | TokenDifference AlexPosn
  | TokenLeftJoin AlexPosn
  | TokenRightJoin AlexPosn
  | TokenInnerJoin AlexPosn
  | TokenOuterJoin AlexPosn
  | TokenOutput AlexPosn
  | TokenAllColumns AlexPosn
  | TokenEq AlexPosn
  | TokenInEq AlexPosn
  | TokenLt AlexPosn
  | TokenGt AlexPosn
  | TokenStartsWith AlexPosn
  | TokenEndsWith AlexPosn
  | TokenContains AlexPosn
  | TokenColumn AlexPosn String
  | TokenFileName AlexPosn String
  | TokenString AlexPosn String
  deriving (Show, Eq)
}