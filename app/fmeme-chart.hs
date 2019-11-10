{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wall #-}

-- import FMeme.Funks
-- import Moo.GeneticAlgorithm

-- import Numeric.LinearAlgebra hiding (Range, append)

import Chart
-- import qualified Data.HashMap.Strict as HashMap

import Control.Category (id)
import Control.Lens
import qualified Data.Attoparsec.Text as A
import Data.Biapplicative
import Data.Scientific
import Data.Sv
import qualified Data.Sv.Decode as D
-- import Formatting
import Lucid hiding (b_)
import Lucid.Base
import Network.JavaScript
import Network.Wai.Middleware.Static ((>->), addBase, noDots, staticPolicy)
import NumHask.Space
import Numeric.Backprop
import Protolude hiding ((<<*>>), Meta, Rep, replace)
import System.IO.Unsafe (unsafePerformIO)
import Web.Page hiding (foldl')
import Web.Scotty
import qualified Data.Text as Text

data AB a = AB {alpha :: a, beta :: a} deriving (Eq, Show)

fromAB :: AB a -> [a]
fromAB (AB a b) = [a, b]

toAB :: [a] -> Maybe (AB a)
toAB (a : b : _) = Just (AB a b)
toAB _ = Nothing

repAB :: (Monad m) => Int -> AB Double -> AB (Range Double) -> SharedRep m (AB Double)
repAB steps (AB a0 b0) (AB (Range amin amax) (Range bmin bmax)) = do
  a' <- slider (Just "alpha") amin amax ((amax - amin) / fromIntegral steps) a0
  b' <- slider (Just "beta") bmin bmax ((amax - amin) / fromIntegral steps) b0
  pure $ AB a' b'

data Meta a = Meta {maxn :: Int, tol :: a, rate :: a} deriving (Eq, Show, Generic)

repMeta :: (Monad m) => Meta Double -> SharedRep m (Meta Double)
repMeta m = do
  n' <- sliderI (Just "max iterations") 1 10000 100 (m ^. #maxn)
  t' <- slider (Just "tolerance (t)") 0 0.1 0.001 (m ^. #tol)
  r' <- slider (Just "convergence rate (r)") 0 0.1 0.001 (m ^. #rate)
  pure $ Meta n' t' r'

ex1File :: FilePath
ex1File = "app/ex1data1.txt"

opts :: ParseOptions
opts = ParseOptions (fromIntegral $ fromEnum ' ') Unheaded

data Ex1Data = Ex1Data {v1 :: Double, v2 :: Double} deriving (Show, Eq)

toLR :: Ex1Data -> ([Double], Double)
toLR (Ex1Data x y) = ([x], y)

ex1Decoder :: Decode' ByteString Ex1Data
ex1Decoder = Ex1Data <$> D.double <*> D.double

ex1 :: IO [Ex1Data]
ex1 = do
  r <- parseDecodeFromFile ex1Decoder opts ex1File
  pure $ case r of
    Success xs -> xs
    Failure _ -> []

xy :: [([Double], Double)]
{-# NOINLINE xy #-}
xy = unsafePerformIO $ do
  ex1' <- ex1
  pure $ (\(Ex1Data x y) -> ([x], y)) <$> ex1'

repXY :: (Monad m) => [(Double, Double)] -> SharedRep m [Spot Double]
repXY d = do
  a <-
    dropdown
      A.takeText
      show
      (Just "type")
      [ "exercise 1"
      ]
      "exercise 1"
  pure
    ( case a of
        "exercise 1" -> uncurry SP <$> d
        _ -> uncurry SP <$> d
    )

abLine :: Range Double -> AB Double -> [Spot Double]
abLine (Range x0 x1) (AB a b) = [SP x0 (a + b * x0), SP x1 (a + b * x1)]

modelText :: AB Double -> Text
modelText (AB a b) =
  Text.pack $ "y = "
    <> formatScientific Exponent (Just 3) (fromFloatDigits a)
    <> " + "
    <> formatScientific Exponent (Just 3) (fromFloatDigits b)
    <> " * x"

midChart ::
  SharedRep IO Text ->
  Application ->
  Application
midChart sr = midShared sr initChartRender updateChart

initChartRender ::
  Engine ->
  Rep Text ->
  StateT (HashMap Text Text) IO ()
initChartRender e r =
  void $
    oneRep
      r
      ( \(Rep h fa) m -> do
          append e "input" (toText h)
          replace e "output" (either id id . snd $ fa m)
      )

updateChart :: Engine -> Either Text (HashMap Text Text, Either Text Text) -> IO ()
updateChart e (Left err) = append e "log" ("map error: " <> err)
updateChart e (Right (_, Left err)) = append e "log" ("parse error: " <> err)
updateChart e (Right (_, Right c)) = replace e "output" c

repMain ::
  (Monad m) =>
  ChartSvgStyle ->
  GlyphStyle ->
  HudConfig ->
  LineStyle ->
  LineStyle ->
  Meta Double ->
  [Ex1Data] ->
  SharedRep m Text
repMain cscfg gcfg hcfg lcfg gdcfg mcfg xs =
  bimap hmap mmap cs <<*>> gs <<*>> d <<*>> h <<*>> ls <<*>> ls2 <<*>> ab <<*>> meta
  where
    p0 = [0, 0]
    ab = repAB 100 (AB 0 0) (AB (Range -5 5) (Range -2 2))
    h = repHudConfigDefault hcfg
    cs = repChartSvgStyle cscfg
    gs = repGlyphStyle gcfg
    ls = repLineStyle lcfg
    ls2 = repLineStyle gdcfg
    meta = repMeta mcfg
    d = repXY ((\(Ex1Data x y) -> (x, y)) <$> xs)
    xrange = space1 $ v1 <$> xs :: Range Double
    mmap cs' gs' d' h' ls' ls2' ab' meta' =
      renderHudChartWith
        cs'
        h''
        [ Chart (GlyphA gs') d',
          Chart (LineA ls') (abLine xrange ab'),
          Chart (LineA ls2') (gdLine xrange gdp)
        ]
        <> renderChartSvg 400 400 (chartSvg (Rect -1.0 1.0 -1.0 1.0) (pixel'' (toLR <$> xs)))
      where
        gdp = linBatch meta' p0 (toLR <$> xs)
        h'' =
          h'
            & #hudTitles %~ (<> [modelTitle ab'] <> [gdTitle gdp])
    hmap cs' gs' d' h' ls' ls2' ab' meta' =
      accordion_
        "acca"
        (Just "Chart Configuration")
        [ ("Chart Svg Stylings", cs'),
          ( "Chart",
            accordion_
              "accb"
              Nothing
              [ ("Scatter Style", gs'),
                ("Line Style", ls'),
                ("GD Line Style", ls2'),
                ("Data", d')
              ]
          ),
          ("Chart Hud", h'),
          ("Meta", meta'),
          ("Model", ab')
        ]

svgStyle :: ChartSvgStyle
svgStyle =
  defaultChartSvgStyle
    & #sizex .~ 900
    & #sizey .~ 600
    & #chartAspect .~ 1.5
    & #outerPad .~ Nothing
    & #chartFrame .~ Nothing
    & #orig .~ Nothing

hudConfig :: HudConfig
hudConfig =
  defaultHudConfig
    & #hudCanvas .~ Nothing
    & #hudTitles
      .~ [ defaultTitle "Training Data",
           (defaultTitle "Profit in $10,0000s" :: Title Double)
             & #place .~ PlaceRight
             & #style . #size .~ 0.05,
           (defaultTitle "Population of City in 10,000s" :: Title Double)
             & #place .~ PlaceBottom
             & #style . #size .~ 0.05
         ]
    & #hudAxes
      .~ [ defaultAxisConfig
             & #abar .~ Just b,
           defaultAxisConfig
             & #abar .~ Just b'
             & #place .~ PlaceLeft
         ]
  where
    b =
      (defaultBar :: Bar Double)
        & #wid .~ (0.01 :: Double)
        & #buff .~ 0.01
        & #rstyle .~ rs
    b' =
      (defaultBar :: Bar Double)
        & #wid .~ (0.01 :: Double)
        & #buff .~ (0.01 :: Double)
        & #rstyle .~ rs
    rs = RectStyle 0 grey 0 (PixelRGB8 105 48 157) 0.5

glyphStyle :: GlyphStyle
glyphStyle =
  defaultGlyphStyle
    & #color .~ PixelRGB8 217 124 32
    & #borderOpacity .~ 1
    & #shape .~ SquareGlyph
    & #borderSize .~ 0.002

modelStyle :: LineStyle
modelStyle =
  defaultLineStyle
    & #color .~ PixelRGB8 173 33 23
    & #width .~ 0.005

gdStyle :: LineStyle
gdStyle =
  defaultLineStyle
    & #color .~ PixelRGB8 50 33 173
    & #width .~ 0.005

modelTitle :: AB Double -> Title Double
modelTitle ab =
  defaultTitle (modelText ab)
    & (#place .~ PlaceBottom :: Title Double -> Title Double)
    & #anchor .~ AnchorStart
    & #style . #color .~ PixelRGB8 173 33 23
    & #style . #size .~ 0.05

gdTitle :: Converge [Double] -> Title Double
gdTitle ab =
  defaultTitle (gdText ab)
    & #style . #color .~ PixelRGB8 50 33 173
    & #style . #size .~ 0.05
    & (#place .~ PlaceBottom :: Title Double -> Title Double)
    & #anchor .~ AnchorStart

gdLine :: Range Double -> Converge [Double] -> [Spot Double]
gdLine _ Nada = []
gdLine r (Unstable (x : y : _)) = abLine r (AB x y)
gdLine _ (Unstable _) = []
gdLine r (Convergent (x : y : _)) = abLine r (AB x y)
gdLine _ (Convergent _) = []
gdLine _ Divergent = []

gdText :: Converge [Double] -> Text
gdText Nada = "No Data"
gdText Divergent = "Divergent"
gdText (Unstable (x : y : _)) = modelText (AB x y) <> " (Unstable)"
gdText (Unstable _) = "Can't happen"
gdText (Convergent (x : y : _)) = modelText (AB x y) <> " (Converged)"
gdText (Convergent _) = "Can't happen"

testPage :: Text -> [(Text, Html ())] -> Page
testPage title' sections =
  bootstrapPage
    <> bridgePage
    & #htmlHeader .~ title_ "mlh charts"
    & #htmlBody
      .~ b_
        "container"
        ( mconcat
            [ b_ "row" (h1_ (toHtml title')),
              b_ "row" $ mconcat $ (\(t, h) -> b_ "col" (h2_ (toHtml t) <> with div_ [id_ t] h)) <$> sections
            ]
        )

main :: IO ()
main = do
  ex1' <- ex1
  scotty 3000 $ do
    middleware $ staticPolicy (noDots >-> addBase "other")
    middleware
      ( midChart
          ( repMain
              svgStyle
              glyphStyle
              hudConfig
              modelStyle
              gdStyle
              (Meta 1000 0.0001 0.01)
              ex1'
          )
      )
    servePageWith
      "/chart/ex1"
      defaultPageConfig
      ( testPage
          "exercise 1"
          [ ("input", mempty),
            ("output", mempty)
          ]
      )

-- backprop
type Model p a b = p -> a -> b

type ModelG p a b =
  forall z.
  Reifies z W =>
  BVar z p ->
  BVar z a ->
  BVar z b

linReg :: ModelG [Double] [Double] Double
linReg bs xs = sum (zipWith (*) (sequenceVar bs) (auto 1 : sequenceVar xs))

sqerr :: (Backprop p, Backprop b, Num b) => ModelG p a b -> a -> b -> p -> p
sqerr f x targ = gradBP $ \p -> (f p (auto x) - auto targ) ^ (2 :: Int)

sqerrb :: (Backprop p, Backprop b, Num b) => ModelG p a b -> a -> b -> p -> (b, p)
sqerrb f x targ = backprop $ \p -> (f p (auto x) - auto targ) ^ (2 :: Int)

sqerre :: (Num b) => ModelG p a b -> a -> b -> p -> b
sqerre f x targ = evalBP $ \p -> (f p (auto x) - auto targ) ^ (2 :: Int)

sqerrs :: (Foldable f, Functor f, Backprop p, Backprop b, Num b, Floating p) => ModelG [p] [a] b -> f ([a], b) -> [p] -> [p]
sqerrs f xs p = (/ fromIntegral (length xs)) <$> foldl' (zipWith (+)) (repeat 0) ((\(x, targ) -> sqerr f x targ p) <$> xs)

sgd :: (Backprop b, Backprop p, Num b, Num p) => [p] -> ModelG [p] [a] b -> [p] -> [([a], b)] -> [p]
sgd r f = foldl' (\p (x, y) -> zipWith (-) p (zipWith (*) r (sqerr f x y p)))

data Converge a = Nada | Unstable a | Convergent a | Divergent deriving (Eq, Show)

converge :: Int -> (a -> a -> Bool) -> (a -> Bool) -> [a] -> Converge a
converge _ _ _ [] = Nada
converge n p d (x : xs) = go 0 p d x xs
  where
    go :: Int -> (a -> a -> Bool) -> (a -> Bool) -> a -> [a] -> Converge a
    go _ _ d' x' [] = bool (Unstable x') Divergent (d' x')
    go k p' d' x0 (x1 : xs')
      | d' x1 = Divergent
      | k > n = Unstable x1
      | p' x0 x1 = Convergent x1
      | otherwise = go (k + 1) p' d' x1 xs'

batchgd ::
  (Backprop b, Backprop p, Num b, Floating p) =>
  Int ->
  ([p] -> [p] -> Bool) ->
  ([p] -> Bool) ->
  p ->
  ModelG [p] [a] b ->
  [p] ->
  [([a], b)] ->
  Converge [p]
batchgd n pr d r f p0 xs' =
  converge n pr d $
    iterate (\ps -> zipWith (-) ps ((r *) <$> sqerrs f xs' ps)) p0

linBatch :: Meta Double -> [Double] -> [([Double], Double)] -> Converge [Double]
linBatch m =
  batchgd
    (m ^. #maxn)
    (\p p' -> (< (m ^. #tol)) $ sum $ zipWith (\x y -> abs (x - y)) p p')
    (any isNaN)
    (m ^. #rate)
    linReg

pixel' :: (Point Double -> Double) -> AB (Range Double) -> [Chart Double]
pixel' f (AB (Range amin amax) (Range bmin bmax)) =
  (\(r, c) -> Chart (RectA (RectStyle 0 black 0 c 1)) [SpotRect r])
    <$> pixelate f (toRect $ SR amin amax bmin bmax) (Point 100 100) blue grey

fsqerr :: [([Double], Double)] -> Point Double -> Double
fsqerr xs (Point a b) = sqrt $ sum $ (\(x, y) -> sqerre linReg x y [a, b]) <$> xs

pixel'' :: [([Double], Double)] -> [Chart Double]
pixel'' xs = pixel' (fsqerr xs) (AB (Range -5 0) (Range 0 2))
