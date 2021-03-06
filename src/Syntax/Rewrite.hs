{-# LANGUAGE GeneralizedNewtypeDeriving, LambdaCase #-}

module Syntax.Rewrite where

import           Data.Map                       ( Map )
import qualified Data.Map                      as M
import           Data.Generics.Uniplate.Operations
import           Control.Monad                  ( when
                                                , forever
                                                )
import           Control.Monad.Except           ( throwError
                                                , liftEither
                                                , catchError
                                                )
import           Control.Monad.State            ( StateT
                                                , get
                                                , gets
                                                , put
                                                , evalStateT
                                                , execStateT
                                                , MonadState
                                                )
import           Control.Monad.Reader           ( ReaderT
                                                , MonadReader
                                                , runReaderT
                                                , asks
                                                )
import           Control.Monad.IO.Class         ( MonadIO )
import           Control.Monad.Trans.Class      ( MonadTrans(..) )
import           Control.Monad.Trans.Except     ( ExceptT
                                                , runExceptT
                                                , throwE
                                                )
import           Control.Monad.Cont             ( MonadCont )
import           Control.Monad.Writer           ( MonadWriter )
import           Syntax.Ast
import           Sources
import           Errors.Diagnostics
import           Text.Printf

data Rules = Rules
  { prefixOps  :: Map String Prec
  , binaryOps  :: Map String (Prec, Associativity)
  , postfixOps :: Map String Prec
  }
  deriving (Eq, Show)

normalizePrec :: Prec -> Prec
normalizePrec prec = prec * 10

type PrattM s = ReaderT Rules (StateT (OpToks s) (Either Diagnostics))

getOpRules :: [Statement] -> Rules
getOpRules = foldl
  (\(Rules pre bin post) s -> case s of
    OpDefS (Operator op _) info _ -> case info of
      Binary v assoc ->
        Rules pre (M.insert op (normalizePrec v, assoc) bin) post
      Prefix  v -> Rules (M.insert op (normalizePrec v) pre) bin post
      Postfix v -> Rules pre bin (M.insert op (normalizePrec v) post)
    _ -> Rules pre bin post
  )
  (Rules M.empty M.empty M.empty)

rewrite :: [Statement] -> Failable [Statement]
rewrite ss = do
  exprRewrite <- rewriteBiM (rewriteExpr rs) ss
  rewriteBiM (rewriteType rs) exprRewrite
  where rs = getOpRules ss

rewriteExpr :: Rules -> Expr -> Failable (Maybe Expr)
rewriteExpr rs (ExprList es _) = Just <$> fixPrec funcAppOp rs es OpE
rewriteExpr _  _               = return Nothing

rewriteType :: Rules -> Type -> Failable (Maybe Type)
rewriteType rs (TypeList ts _) = Just <$> fixPrec tyAppOp rs ts OpType
rewriteType _  _               = return Nothing

fixPrec
  :: Spanned a
  => OpCons
  -> Rules                 -- Precedence Rules
  -> OpToks a              -- List of tokens
  -> (Oped a -> Span -> a) -- Construct an `a` from an operator and span
  -> Failable a            -- Produced `a`
fixPrec opCons rs toks cons = liftEither
  $ evalStateT (runReaderT (exprBp opCons s cons 0) rs) toks
  where s = bt (head toks) (last toks)

opInfoStr :: (a -> OpInfo) -> String
opInfoStr cons = case cons undefined of
  Prefix  _  -> "prefix"
  Postfix _  -> "postfix"
  Binary _ _ -> "binary"

next :: PrattM a (Maybe (OpTok a))
next = do
  stream <- get
  case stream of
    (t : ts) -> do
      put ts
      return $ Just t
    [] -> return Nothing

peek :: PrattM a (Maybe (OpTok a))
peek = do
  stream <- get
  case stream of
    (t : _) -> do
      return $ Just t
    [] -> return Nothing

sLen :: PrattM a Int
sLen = gets length

type NodeCons a = (Oped a -> Span -> a)

newtype Break r m a = Break { unBreak :: ExceptT r m a }
    deriving
    ( Functor
    , Applicative
    , Monad
    , MonadTrans
    , MonadIO
    , MonadCont
    , MonadState  s
    , MonadWriter w
    , MonadReader d
    )

quit :: Monad m => r -> Break r m a
quit r = Break (throwE r)

loop :: Monad m => Break r m a -> m r
loop m = do
  x <- runExceptT (unBreak (forever m))
  case x of
    Left  r -> return r
    Right r -> return r

fnAppName :: String
fnAppName = "<fnapp>"
funcAppOp :: Span -> Operator
funcAppOp = Operator fnAppName

tyAppName :: String
tyAppName = "<tyapp>"
tyAppOp :: OpCons
tyAppOp = Operator tyAppName

type OpCons = Span -> Operator

-- NOTE: This is extremely messy.
-- Is there a way to make it more functional? Right now is very "imperative"
processMany
  :: Spanned a
  => OpCons
  -> Span
  -> NodeCons a
  -> Prec
  -> Break () (StateT a (PrattM a)) ()
processMany opCons s cons minBp = do
  t              <- liftPratt peek
  lhs            <- lift get
  (breakOut, op) <- case t of
    Nothing         -> quit ()
    Just (OpTok op) -> do
      return (True, op)
    Just (OtherTok a) -> return (False, opCons $ gp lhs a)
  maybeLRBp <- liftPratt $ (Just <$> binaryBp op) `catchError` const
    (return Nothing)
  streamLen <- liftPratt sLen
  case maybeLRBp of
    Just (lBp, rBp) | (streamLen > 1) || (not breakOut && streamLen >= 1) -> do
      when (lBp < minBp) $ quit ()
      when breakOut $ () <$ liftPratt next
      rhs <- liftPratt $ exprBp opCons s cons rBp
      lift . put $ cons (BinOp op lhs rhs) $ bt lhs rhs
      return ()
    _ -> do
      (lBp, ()) <- liftPratt $ postfixBp op
      when (lBp < minBp) $ quit ()
      when breakOut $ () <$ liftPratt next
      lift . put $ cons (PostOp op lhs) $ bt lhs op
  where liftPratt = lift . lift

exprBp :: Spanned a => OpCons -> Span -> NodeCons a -> Prec -> PrattM a a
exprBp opCons s cons minBp = do
  t   <- next
  lhs <- case t of
    Just (OtherTok a ) -> return a
    Just (OpTok    op) -> do
      (_, preBp) <- prefixBp op
      e          <- exprBp opCons s cons preBp
      liftPratt . return $ cons (PreOp op e) $ bt op e
    Nothing -> liftPratt . Left $ unexpectedEos s
  execStateT (loop $ processMany opCons s cons minBp) lhs
  where liftPratt = lift . lift

postfixBp :: Operator -> PrattM a (Prec, ())
postfixBp (Operator op s) = do
  rs <- asks postfixOps
  case M.lookup op rs of
    Just bp -> return (bp, ())
    Nothing -> throwError $ unknownOp
      (opInfoStr Postfix ++ " or " ++ opInfoStr (Binary undefined))
      s
      op

prefixBp :: Operator -> PrattM a ((), Prec)
prefixBp (Operator op s) = do
  rs <- asks prefixOps
  case M.lookup op rs of
    Just bp -> return ((), bp)
    Nothing -> throwError $ unknownOp (opInfoStr Prefix) s op

binaryBp :: Operator -> PrattM a (Prec, Prec)
binaryBp (Operator op s) = do
  rs <- asks binaryOps
  case M.lookup op rs of
    Just (bp, asc) ->
      let (d1, d2) =
            (case asc of
              LeftA    -> (0, 1)
              RightA   -> (1, 0)
              Nonassoc -> error "Non assoc not implemented yet"
            )
      in  return (bp + d1, bp + d2)
    Nothing -> throwError $ unknownOp (opInfoStr $ Binary undefined) s op

errorWithAnn :: Annotation -> Span -> Diagnostics
errorWithAnn ann s = Diagnostics [Diagnostic Error SyntaxError [ann] s []]


unknownOp :: String -> Span -> String -> Diagnostics
unknownOp opTy s op = errorWithAnn
  (Annotation s (Just $ printf "unknown %s operator `%s`" opTy op) Error)
  s

unexpectedTok :: Span -> Diagnostics
unexpectedTok s =
  errorWithAnn (Annotation s (Just $ printf "expected operator") Error) s

unexpectedEos :: Span -> Diagnostics
unexpectedEos s = errorWithAnn
  (Annotation s' (Just "unexpected end of expression") Error)
  s'
 where
  s' = case s of
    Span sid _ e -> Span sid e (e + 1)
    Eof _        -> s
