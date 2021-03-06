module Errors.Render where

import           Control.Monad.Writer
import           Control.Monad.Reader
import           Errors.Diagnostics
import           Sources
import           Text.Printf
import qualified Data.Map                      as D
import           Data.Maybe                     ( fromJust )
import           Data.List                      ( dropWhileEnd
                                                , intercalate
                                                )
import           Data.Char                      ( isSpace )
import           Data.Sequence                  ( Seq )

trim :: String -> String
trim = dropWhileEnd isSpace

getS :: DiagnosticType -> String
getS Error   = "^"
getS Warning = "~"
getS Info    = "'"

blue :: String
blue = "\x1b[94m"
red :: String
red = "\x1b[91m"
yellow :: String
yellow = "\x1b[93m"
green :: String
green = "\x1b[32m"
bold :: String
bold = "\x1b[1m"
reset :: String
reset = "\x1b[0m"

getC :: DiagnosticType -> String
getC Error   = red
getC Warning = yellow
getC Info    = blue

defaultCharSet :: CharSet
defaultCharSet = CharSet "┌─" '│' '·' '=' '┌' '─' '│' '└' '─' getS

defaultColorSet :: ColorSet
defaultColorSet = ColorSet blue blue blue green bold blue getC

defaultConfig :: Config
defaultConfig = RenderC defaultCharSet defaultColorSet 4 3 0

countMaybe :: Maybe a -> Int
countMaybe (Just _) = 1
countMaybe Nothing  = 0

data CharSet = CharSet
  { startS           :: String
  , borderLeftS      :: Char
  , borderBreakS     :: Char
  , noteBullet       :: Char
  , multiTopLeft     :: Char
  , multiTop         :: Char
  , multiLeft        :: Char
  , multiBottomLeft  :: Char
  , multiBottom      :: Char
  , getSeverityUnder :: DiagnosticType -> String
  }
data ColorSet = ColorSet
  { gutterColor   :: String
  , lineNoColor   :: String
  , filenameColor :: String
  , bulletColor   :: String
  , errorStyle    :: String
  , emittedError  :: String
  , color         :: DiagnosticType -> String
  }
data Config = RenderC
  { charSet      :: CharSet
  , colorSet     :: ColorSet
  , tabWidth     :: Int
  , contextLines :: Int
  , sidePadding  :: Int
  }
data RenderEnv = RS
  { config      :: Config
  , sources     :: Sources
  , filemap     :: FileMap
  , sourceLines :: SourceLines
  }

-- Render driver
type RenderD = ReaderT RenderEnv (Writer String)

type Message = String
type DiagnosticStr = String
data SingleLabel = SingleLabel DiagnosticType Int Int (Maybe DiagnosticStr)
  deriving Show
data MultiLabel = TopMultiLabel Int | LeftLabel | BottomLabel Int (Maybe DiagnosticStr) deriving (Show)
type MultiLabels = [(Int, DiagnosticType, MultiLabel)]
data VerticalBound = Top | Bottom
type Underline = (DiagnosticType, VerticalBound)

singleDTy :: SingleLabel -> DiagnosticType
singleDTy (SingleLabel ty _ _ _) = ty

renderVertBound :: VerticalBound -> RenderD ()
renderVertBound Top    = renderCharSet multiTop
renderVertBound Bottom = renderCharSet multiBottom

getFilename :: SourceId -> RenderD FilePath
getFilename sid = do
  flmap <- asks filemap
  return $ fromJust $ D.lookup sid flmap

getFileLines :: SourceId -> RenderD (Seq String)
getFileLines sid = do
  sourceLinesMap <- asks sourceLines
  return $ fromJust $ D.lookup sid sourceLinesMap

getFile :: SourceId -> RenderD String
getFile sid = do
  flmap <- asks sources
  return $ fromJust $ D.lookup sid flmap

getLineCol :: SourceId -> Int -> RenderD (Int, Int)
getLineCol sid p = do
  contents <- getFile sid
  return $ getLineColStr contents p (0, 0)

-- Top message
-- ```test
-- error[E001]: message
-- ```
renderHeader :: DiagnosticType -> String -> Int -> Message -> RenderD ()
renderHeader style prefix errId msg = do
  indentation <- asks $ sidePadding . config
  tell $ replicate indentation ' '
  renderDiagnosticC style
  renderColorSet errorStyle
  tell $ printf "%s[E%03d]: %s\n" prefix errId msg
  renderReset

-- Top left border and span:
-- ```test
-- ┌─ test.fl:2:4\n
-- ```
renderSnippetState :: Int -> SourceId -> Int -> RenderD ()
renderSnippetState padding sid p = do
  renderGutterOuterSpace padding
  leading <- asks $ startS . charSet . config
  renderColorSet gutterColor
  tell leading
  renderColorSet filenameColor
  renderSourcePos sid p
  renderReset

renderSourcePos :: SourceId -> Int -> RenderD ()
renderSourcePos sid p = do
  filename <- getFilename sid
  (l, c)   <- getLineCol sid p
  tell $ printf " %s:%d:%d\n" filename (l + 1) (c + 1)

renderNotes :: Int -> [String] -> RenderD ()
renderNotes outerPadding msg = forM_
  msg
  (\line -> do
    renderGutterOuterSpace outerPadding
    renderColorSet bulletColor
    renderCharSet noteBullet
    spacingSpace
    tell line
    renderReset
    emptyLine
  )

hangingLabels :: Maybe (Int, SingleLabel) -> [SingleLabel] -> [SingleLabel]
hangingLabels trailingLabel ls =
  map snd
    $ filter (\(i, _) -> maybe True (\(j, _) -> i /= j) trailingLabel)
    $ filter (\(_, SingleLabel _ _ _ m) -> maybe False (not . null) m)
    $ zip [0 ..] ls

renderReset :: RenderD ()
renderReset = tell reset

renderGutter :: RenderD ()
renderGutter = do
  renderColorSet gutterColor
  border <- asks $ borderLeftS . charSet . config
  tell [border]
  renderReset

renderGutterNum :: Int -> Int -> RenderD ()
renderGutterNum lineNo padding = do
  renderColorSet lineNoColor
  tell $ printf "%*d " padding lineNo
  renderReset

renderGutterSpace :: RenderD ()
renderGutterSpace = tell "  "

renderGutterOuterSpace :: Int -> RenderD ()
renderGutterOuterSpace n = tell $ replicate (n + 1) ' '

renderDiagnosticC :: DiagnosticType -> RenderD ()
renderDiagnosticC ty = do
  c <- asks $ color . colorSet . config
  tell $ c ty

renderCarretDiagnostic :: DiagnosticType -> RenderD ()
renderCarretDiagnostic ty = do
  caretMap <- asks $ getSeverityUnder . charSet . config
  tell $ caretMap ty

renderCharSet :: (CharSet -> Char) -> RenderD ()
renderCharSet gets = do
  asks (gets . charSet . config) >>= tell . (: [])

renderColorSet :: (ColorSet -> String) -> RenderD ()
renderColorSet gets = do
  asks (gets . colorSet . config) >>= tell

renderTopLeft :: DiagnosticType -> RenderD ()
renderTopLeft ty = do
  tell " "
  renderDiagnosticC ty
  renderCharSet multiTopLeft
  renderReset

renderLeftChar :: RenderD ()
renderLeftChar = asks (multiLeft . charSet . config) >>= tell . (: [])

renderLeft :: DiagnosticType -> Maybe DiagnosticType -> RenderD ()
renderLeft dTy' underlineTy = do
  case underlineTy of
    Nothing -> tell " "
    Just ty -> do
      renderDiagnosticC ty
      renderCharSet multiTop
      renderReset
  renderDiagnosticC dTy'
  renderCharSet multiLeft
  renderReset

renderLabels :: Int -> Int -> Int -> MultiLabels -> RenderD ()
renderLabels _             _         _           []       = return ()
renderLabels leadingSpaces numMultis labelColumn (x : xs) = case x of
  (labelIdx, style, label) | labelIdx == labelColumn -> do
    case label of
      TopMultiLabel start | start <= leadingSpaces -> renderTopLeft style
      TopMultiLabel _                              -> renderGutterSpace
      LeftLabel                                    -> renderLeft style Nothing
      BottomLabel _ _                              -> renderLeft style Nothing
    renderLabels leadingSpaces numMultis (labelColumn + 1) xs
  (_, _, _) -> do
    renderGutterSpace
    renderLabels leadingSpaces numMultis (labelColumn + 1) (x : xs)

getSinglesInfo
  :: [SingleLabel]
  -> Int
  -> (Int, Int, Int, Maybe (Int, SingleLabel))
  -> (Int, Int, Int, Maybe (Int, SingleLabel))
getSinglesInfo [] _ old = old
getSinglesInfo (x : xs) idx (numMsgs, maxStart, maxEnd, trailingLabel) =
  getSinglesInfo xs (idx + 1) (numMsgs', maxStart', maxEnd', trailingLabel')
 where
  (SingleLabel _ s e msg) = x
  numMsgs'                = if not $ null msg then numMsgs + 1 else numMsgs
  maxStart'               = max maxStart s
  maxEnd'                 = max maxEnd e
  trailingLabel'          = if e == maxEnd'
    then (if null msg then Nothing else Just (idx, x))
    else trailingLabel

isOverlapping :: (Int, Int) -> (Int, Int) -> Bool
isOverlapping (s, e) (s', e') = start < end
 where
  start = max s s'
  end   = min e e'

isIn :: (Int, Int) -> (Int, Int) -> Bool
isIn (s, _) (s', _) = s == s'

spacingSpace :: RenderD ()
spacingSpace = tell " "

renderInnerGutter :: Int -> Int -> MultiLabels -> RenderD ()
renderInnerGutter _         _           []       = return ()
renderInnerGutter numMultis labelColumn (x : xs) = case x of
  (labelIdx, style, label) | labelIdx == labelColumn -> do
    case label of
      LeftLabel       -> renderLeft style Nothing
      BottomLabel _ _ -> renderLeft style Nothing
      TopMultiLabel _ -> renderGutterSpace
    renderInnerGutter numMultis (labelColumn + 1) xs
  (_, _, _) -> do
    renderGutterSpace
    renderInnerGutter numMultis (labelColumn + 1) (x : xs)

maxByKey
  :: Ord a => (DiagnosticType -> a) -> [DiagnosticType] -> Maybe DiagnosticType
maxByKey _  [] = Nothing
maxByKey fn a  = Just $ foldr1 (\x y -> if fn x >= fn y then x else y) a

currentStatus
  :: ((Int, Int) -> (Int, Int) -> Bool)
  -> Int
  -> [SingleLabel]
  -> Maybe DiagnosticType
currentStatus f idx labels = maxByKey dTyPriority $ map singleDTy $ filter
  (\(SingleLabel _ s e _) -> f (s, e) (idx, idx + 1))
  labels

last' :: [a] -> Maybe a
last' [] = Nothing
last' xs = Just $ last xs

renderCarets :: Int -> [Int] -> [SingleLabel] -> RenderD ()
renderCarets maxLabelEnd idxs labels = forM_
  idxs
  (\idx ->
    let currentStyle = currentStatus isOverlapping idx labels
    in  do
          case currentStyle of
            Just d -> do
              renderDiagnosticC d
              renderCarretDiagnostic d
              renderReset
            Nothing | idx < maxLabelEnd -> tell " "
            Nothing                     -> do
              return ()
  )

renderSource :: Int -> String -> [SingleLabel] -> RenderD ()
renderSource _   []       _      = return ()
renderSource idx (x : xs) labels = do
  case currentStatus isOverlapping idx labels of
    Just e -> do
      renderDiagnosticC e
      tell [x]
      renderReset
    Nothing -> tell [x]
  renderSource (idx + 1) xs labels

renderHangingCarets
  :: Int -> [Int] -> [SingleLabel] -> Maybe (Int, SingleLabel) -> RenderD ()
renderHangingCarets maxStart idxs labels trailingLabel = forM_
  idxs
  (\idx ->
    let labelStyle =
          currentStatus isIn idx $ hangingLabels trailingLabel labels
    in  (do
          case labelStyle of
            Just style -> do
              renderDiagnosticC style
              renderLeftChar
              renderReset
            Nothing -> when (idx <= maxStart) (tell " ")
        )
  )

renderInnerGutterColumn :: Maybe Underline -> RenderD ()
renderInnerGutterColumn Nothing                   = renderGutterSpace
renderInnerGutterColumn (Just (style, vertBound)) = do
  renderDiagnosticC style
  renderVertBound vertBound
  renderReset

renderLabelMultiLeft :: DiagnosticType -> Maybe DiagnosticType -> RenderD ()
renderLabelMultiLeft style underline = do
  case underline of
    Nothing     -> tell " "
    Just style' -> do
      renderDiagnosticC style'
      renderCharSet multiTop
      renderReset
  renderDiagnosticC style
  renderCharSet multiLeft
  renderReset

renderLabelMultiTopLeft :: DiagnosticType -> RenderD ()
renderLabelMultiTopLeft style = do
  tell " "
  renderDiagnosticC style
  renderCharSet multiTopLeft
  renderReset

renderLabelMultiBottomLeft :: DiagnosticType -> RenderD ()
renderLabelMultiBottomLeft style = do
  tell " "
  renderDiagnosticC style
  renderCharSet multiBottomLeft
  renderReset

renderFinalLabels
  :: DiagnosticType
  -> Int
  -> Maybe Underline
  -> Int
  -> [(Int, (Int, DiagnosticType, MultiLabel))]
  -> RenderD ()
renderFinalLabels _ _ underline _ [] = renderInnerGutterColumn underline
renderFinalLabels labelStyle multiLabelIndex underline labelColumn (x : xs) =
  if labelIdx == labelColumn
    then do
      underline' <- case label of
        LeftLabel -> do
          renderLabelMultiLeft style underlineStyle
          return underline
        TopMultiLabel _ | multiLabelIndex > i -> do
          renderLabelMultiLeft style underlineStyle
          return underline
        BottomLabel _ _ | multiLabelIndex < i -> do
          renderLabelMultiLeft style underlineStyle
          return underline
        TopMultiLabel _ | multiLabelIndex == i -> do
          renderLabelMultiTopLeft labelStyle
          return $ Just (style, Top)
        BottomLabel _ _ | multiLabelIndex == i -> do
          renderLabelMultiBottomLeft labelStyle
          return $ Just (style, Bottom)
        _ -> do
          renderInnerGutterColumn underline
          return underline
      renderFinalLabels labelStyle
                        multiLabelIndex
                        underline'
                        (labelColumn + 1)
                        xs
    else do
      renderInnerGutterColumn underline
      renderFinalLabels labelStyle
                        multiLabelIndex
                        underline
                        (labelColumn + 1)
                        (x : xs)
 where
  underlineStyle                = fst <$> underline
  (i, (labelIdx, style, label)) = x

labelMultiTopCaret :: DiagnosticType -> Int -> RenderD ()
labelMultiTopCaret style start = do
  renderDiagnosticC style
  c <- asks $ multiTop . charSet . config
  tell $ replicate start c
  renderCarretDiagnostic style
  renderReset
  emptyLine

labelMultiBottomCaret :: DiagnosticType -> Int -> Maybe String -> RenderD ()
labelMultiBottomCaret style start msg = do
  renderDiagnosticC style
  c <- asks $ multiTop . charSet . config
  tell $ replicate start c
  renderCarretDiagnostic style
  forM_
    msg
    (\msg' -> do
      tell " "
      tell msg'
    )
  renderReset
  emptyLine

renderSnippetSource
  :: Int            -- ^ Outer padding
  -> Int            -- ^ Line number
  -> String         -- ^ Source
  -> [SingleLabel]  -- ^ Single labels
  -> Int            -- ^ Number of multi labels
  -> MultiLabels    -- ^ Multiple labels
  -> RenderD ()
renderSnippetSource outerPadding lineNo source' singles' numMultis multis = do
  -- `10`
  renderGutterNum lineNo outerPadding
  -- `10 |`
  renderGutter
  -- `10 | | |`
  renderLabels leadingSpaces numMultis 0 multis
  -- `10 | | | `
  spacingSpace
  -- `10 | | | this is a test line`
  renderSource 0 source singles
  emptyLine
  -- `10 | | | this is a test line
  --     | | | ^^ ~~~--- error`
  unless
    (null singles)
    (do
      renderGutterOuterSpace outerPadding
      renderGutter
      renderInnerGutter numMultis 0 multis
      spacingSpace
      renderCarets maxEnd [0 .. length source] singles
      case trailingLabel of
        Just (_, SingleLabel style _ _ (Just msg)) -> do
          spacingSpace
          renderDiagnosticC style
          tell msg
          renderReset
        _ -> return ()
      emptyLine
    )
  when
    (numMsgs > countMaybe trailingLabel)
    (do
      renderGutterOuterSpace outerPadding
      renderGutter
      renderInnerGutter numMultis 0 multis
      spacingSpace
      renderHangingCarets maxStart
                          [0 .. length source - 1]
                          singles
                          trailingLabel
      emptyLine
      forM_
        (reverse $ hangingLabels trailingLabel singles)
        (\(SingleLabel labelStyle s _ (Just msg)) ->
          (do
            renderGutterOuterSpace outerPadding
            renderGutter
            renderInnerGutter numMultis 0 multis
            spacingSpace
            renderHangingCarets maxStart [0 .. (s - 1)] singles trailingLabel
            renderDiagnosticC labelStyle
            tell msg
            renderReset
            emptyLine
          )
        )
    )
  forM_
    (zip [0 ..] multis)
    (\(multiLabelIndex, (_, style, label)) ->
      (do
        let labelValues =
              (case label of
                LeftLabel                            -> Nothing
                TopMultiLabel s | s <= leadingSpaces -> Nothing
                TopMultiLabel r                      -> Just (r, Nothing)
                BottomLabel r msg                    -> Just (r, Just msg)
              )
        forM_
          labelValues
          (\(range, bottomMsg) -> do
            renderGutterOuterSpace outerPadding
            renderGutter
            renderFinalLabels style
                              multiLabelIndex
                              Nothing
                              0
                              (zip [0 ..] multis)
            case bottomMsg of
              Nothing  -> labelMultiTopCaret style range
              Just msg -> labelMultiBottomCaret style range msg
          )
      )
    )
 where
  singles =
    (\(SingleLabel dTy' s e msg) ->
        let newE = if s == e then e + 1 else e in SingleLabel dTy' s newE msg
      )
      <$> singles'
  (numMsgs, maxStart, maxEnd, tl) = getSinglesInfo singles 0 (0, 0, 0, Nothing)
  trailingLabel                   = case tl of
    Just (idx, SingleLabel _ s e _)
      | any (\(_, SingleLabel _ s' e' _) -> isOverlapping (s, e) (s', e'))
        $ filter (\(idx', _) -> idx' /= idx)
        $ zip [0 ..] singles
      -> Nothing
    _ -> tl
  source        = trim source'
  leadingSpaces = length source - length (dropWhile isSpace source)

renderEmptyLine :: Int -> Int -> MultiLabels -> RenderD ()
renderEmptyLine outerPadding numMultis multis = do
  renderGutterOuterSpace outerPadding
  renderGutter
  renderInnerGutter numMultis 0 multis
  emptyLine

renderLineBreak :: Int -> Int -> MultiLabels -> RenderD ()
renderLineBreak outerPadding numMultis multis = do
  renderGutterOuterSpace outerPadding
  renderCharSet borderBreakS
  renderInnerGutter numMultis 0 multis
  emptyLine

count :: Eq a => a -> [a] -> Int
count x = length . filter (x ==)

renderNumDiagnostics :: [DiagnosticType] -> RenderD ()
renderNumDiagnostics dTys = do
  let errs  = count Error dTys
  let warns = count Warning dTys
  let info  = count Info dTys
  let msgs = filter ((/= 0) . snd)
                    [("errors", errs), ("warnings", warns), ("infos", info)]
  emptyLine
  renderColorSet emittedError
  tell "Emitted "
  tell $ intercalate ", " $ map
    (\(a, b) ->
      let txt = show b ++ " " ++ a in if b == 1 then init txt else txt
    )
    msgs
  renderReset
  emptyLine

emptyLine :: RenderD ()
emptyLine = tell "\n"

runRender :: RenderD a -> RenderEnv -> (a, String)
runRender r env = runWriter $ runReaderT r env
