{-# LANGUAGE FlexibleInstances, DeriveFunctor, StandaloneDeriving #-}
module Syntax.Ast where

import           Sources

data Ident = Ident String Span
           | OpId Operator
           deriving (Eq, Show)
data Operator = Operator String Span
  deriving (Eq, Show)
data Namespace = Namespace [Ident] Span
  deriving (Eq, Show)

data Declaration = Declaration Ident Type Span
  deriving (Eq, Show)
data Binding = Binding (Maybe Ident) [Pattern] Expr Span
  deriving (Eq, Show)
data RecordItem = Product Ident [Type] Span
                | NamedProduct Ident [Declaration] Span
                deriving (Eq, Show)

data Fixity = Binary
            | Prefix
            | Postfix
            deriving (Eq, Show)

data Associativity = LeftA
                   | RightA
                   | Nonassoc
                   deriving (Eq, Show)

data OpInfo = OpInfo
  { assoc :: Associativity
  , fix   :: Fixity
  , prec  :: Integer
  }
  deriving (Eq, Show)

data Statement = BindingS [Binding] Span
                    | DeclarationS Declaration Span
                    | ImplS Namespace Type [Statement] Span
                    | TraitS Ident [PolyIdent] [Statement] Span
                    | RecordS Ident [PolyIdent] [RecordItem] Span
                    | ImportS Namespace (Maybe Ident) Span
                    | FromImportS Namespace (Maybe [Ident]) Span
                    | OpDefS Operator OpInfo Span
                    deriving (Eq, Show)

data Literal = IntegerL Integer Span
             | FloatL Double Span
             | StringL String Span
             deriving (Eq, Show)

data Type = Infer Span
          | Never Span
          | NamespaceType Namespace Span
          | TypeList (OpToks Type) Span
          | TupleType [Type] Span
          | OpType (Oped Type) Span
          | PolyType PolyIdent Span
          deriving (Eq, Show)

data PolyIdent = PolyIdent String Span
  deriving (Eq, Show)

data OpTok a = OpTok Operator | OtherTok a
                deriving (Functor)
deriving instance Show a => Show (OpTok a)
deriving instance Eq a => Eq (OpTok a)

type Pattern = Expr

type OpToks a = [OpTok a]

data Expr = LiteralE Literal Span
                    | ExprList (OpToks Expr) Span
                    | OpE (Oped Expr) Span
                    | TupleE [Expr] Span
                    | CondE (Expr, Expr) [(Expr, Expr)] Expr Span
                    | LetInE [Binding] Expr Span
                    | VariableE Namespace Span
                    | LambdaE [Pattern] Expr Span
                    | MatchE Expr [MatchBranch] Span
                    deriving (Show, Eq)

data MatchBranch = MatchBranch Pattern Expr Span
  deriving (Eq, Show)

data Oped a = BinOp Operator a a
            | PreOp Operator a
            | PostOp Operator a
            deriving (Eq, Show, Functor)

instance Spanned Ident where
  getSpan (Ident _ s) = s
  getSpan (OpId op  ) = getSpan op

instance Spanned Operator where
  getSpan (Operator _ s) = s

instance Spanned Namespace where
  getSpan (Namespace _ s) = s

instance Spanned Declaration where
  getSpan (Declaration _ _ s) = s

instance Spanned Binding where
  getSpan (Binding _ _ _ s) = s

instance Spanned RecordItem where
  getSpan (Product      _ _ s) = s
  getSpan (NamedProduct _ _ s) = s

instance Spanned a => Spanned (OpTok a) where
  getSpan (OpTok    o) = getSpan o
  getSpan (OtherTok e) = getSpan e

instance Spanned PolyIdent where
  getSpan (PolyIdent _ s) = s

instance Spanned Statement where
  getSpan (BindingS     _ s ) = s
  getSpan (DeclarationS _ s ) = s
  getSpan (ImplS   _ _ _ s  ) = s
  getSpan (TraitS  _ _ _ s  ) = s
  getSpan (RecordS _ _ _ s  ) = s
  getSpan (ImportS     _ _ s) = s
  getSpan (FromImportS _ _ s) = s
  getSpan (OpDefS      _ _ s) = s

instance Spanned Literal where
  getSpan (IntegerL _ s) = s
  getSpan (FloatL   _ s) = s
  getSpan (StringL  _ s) = s

instance Spanned Type where
  getSpan (Infer s          ) = s
  getSpan (Never s          ) = s
  getSpan (NamespaceType _ s) = s
  getSpan (OpType        _ s) = s
  getSpan (TypeList      _ s) = s
  getSpan (TupleType     _ s) = s
  getSpan (PolyType      _ s) = s

instance Spanned Expr where
  getSpan (LiteralE _ s ) = s
  getSpan (OpE      _ s ) = s
  getSpan (TupleE   _ s ) = s
  getSpan (CondE _ _ _ s) = s
  getSpan (LetInE _ _ s ) = s
  getSpan (VariableE _ s) = s
  getSpan (LambdaE _ _ s) = s
  getSpan (MatchE  _ _ s) = s
  getSpan (ExprList _ s ) = s