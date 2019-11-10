---
pagetitle: fmeme
---

[fmeme](https://github.com/tonyday567/fmeme) example
===

> {-# OPTIONS_GHC -Wall #-}
> {-# LANGUAGE NoImplicitPrelude #-}
> {-# LANGUAGE OverloadedStrings #-}
> {-# LANGUAGE DataKinds #-}
> {-# LANGUAGE DeriveGeneric #-}
> {-# LANGUAGE ScopedTypeVariables #-}
> {-# LANGUAGE TypeOperators #-}
> {-# LANGUAGE FlexibleInstances #-}
> {-# LANGUAGE RankNTypes #-}
> {-# LANGUAGE FlexibleContexts #-}
> {-# LANGUAGE EmptyDataDeriving #-}

[libraries](https://www.stackage.org/)
---

- [protolude](https://www.stackage.org/package/protolude)
- [readme-lhs](https://www.hackage.org/package/readme-lhs)
- [moo](https://www.hackage.org/package/moo)

> import Protolude hiding ((<>))
> import Readme.Lhs
> import FMeme.Funks
> import Moo.GeneticAlgorithm
> import Numeric.Backprop
> import Data.Sv
> import qualified Data.Sv.Decode as D



code
---

Concepts

- https://en.wikipedia.org/wiki/Memetic_algorithm
- neighbourhood
- climber
- exhaustive methods
  - depth first
  - breadth first
  - pruning
    exhaustive O(exp) >>> pruning O(x^n)

- tsp
  - http://users.cs.cf.ac.uk/C.L.Mumford/howard/FarthestInsertion.html
  - https://en.wikipedia.org/wiki/Christofides_algorithm
  - https://github.com/boechat107/tsp_furthest_insertion_haskell/blob/master/second_version/KDtree.hs
  - https://github.com/timerg/HaskellForTSP/blob/master/src/TSP/NN.hs
  - https://en.wikipedia.org/wiki/2-opt


- [hoogle](https://www.stackage.org/package/hoogle)

Machine Learning definition
===

From coursera
---

a computer program is said to learn from experience E with respect to some task T and some performance measure P, if its performance on T, as measured by P, improves with experience E. ~ Tom Mitchell

> data Experience f e = Experience { unexperience :: (Foldable f) => f e}
> data Performance a = Performance { unperformance :: a } deriving (Eq, Ord)
> data Task e a = Task { untask :: e -> Performance a}
>
> test :: e -> Task e a -> Performance a
> test e (Task t) = t e
>
> data Learn f e a = Learn { unlearn :: (Foldable f) => Experience f e -> Task e a -> Task e a}
> data Step e a = Step { unstep :: e -> Task e a -> Task e a }
>
> learn :: Step e a -> Learn f e a
> learn (Step s) = Learn $ \(Experience e) task -> foldr s task e
>
> isGoodLearn
>   :: (Ord a, Foldable f)
>   => Step e a
>   -> Experience f e
>   -> Task e a
>   -> e
>   -> Bool
> isGoodLearn s es task0 e0 =
>   test e0 task' > test e0 task0
>   where
>     (Learn l) = learn s
>     task' = l es task0

> data LearningType = Supervised | Unsupervised | Reinforcement | RecommenderSystems
>

Support Vector Machine

For Supervised Learning, Experience is organized as Features and the Result, and called a Training Set.

> type TrainingSet f e a = Experience f (e, a)
> data Features a fs = Features { unfeatures :: a -> fs }


> data PredictionType a c = Regression { unregress :: (Ord a, Num a) => a } | Classification { unclassify :: (Eq c) => c }

> 

> data HypothesisType = Linear
> 
> data Hypothesis f a =
>   Hypothesis { hypothesisType :: HypothesisType, alpha :: a, betas :: f a }
>

> guess :: (Num a) => Hypothesis [] a -> [a] -> a
> guess (Hypothesis Linear a bs) xs = a + sum (zipWith (+) bs xs)

TODO: switch to sized-vector, vec or whatnot


conal elliot
---

https://www.youtube.com/watch?v=Ns3DxUeCvRg

- function optimisation - "best" element of a set
  - differentiation
  - gradient descent
- find objective function from a set (meta-function optimisationx)
- objective function defined using input/output pairs

important elements of an api
  - function representation
  - can we differentiate (implementation detail)
  - binary sequential, parallel conditional composition
  - not multi-dimensional arrays

Values
- precision
- simplicity
- generality



> cost :: (Num a, Traversable f) => Hypothesis [] a -> f ([a], a) -> a
> cost h@(Hypothesis Linear _ _) es = sum $ fmap ((^(2::Int)) . (\(xs,y) -> guess h xs - y)) es
>

> type Model p a b = p -> a -> b
> type ModelG p a b = forall z. Reifies z W
>                => BVar z p
>                -> BVar z a
>                -> BVar z b

> linReg :: ModelG [Double] [Double] Double
> linReg bs xs = sum (zipWith (+) (sequenceVar bs) ((auto 1):sequenceVar xs))

> sqerr :: (Backprop p, Backprop b, Num b) => ModelG p a b -> a -> b -> p -> p
> sqerr f x targ = gradBP $ \p -> (f p (auto x) - auto targ) ^ (2 :: Int)
>

> sgd :: (Backprop b, Backprop p, Num p, Num b) => p -> ModelG p a b -> p -> [(a,b)] -> p
> sgd r f = foldl' $ \p (x,y) -> p - r * sqerr f x y p
>
>
>
> sqerrs :: (Backprop p, Backprop b, Num b) => ModelG p a b -> [(a,b)] -> p -> p
> sqerrs f xs = gradBP $ \p -> sum $ (\(x,targ) -> (f p (auto x) - auto targ) ^ (2 :: Int)) <$> xs
>

> batchgd :: (Backprop b, Backprop p, Num p, Num b) => p -> ModelG p a b -> p -> [(a,b)] -> p
> batchgd r f = foldl' $ \p (x,y) -> p - r * sqerr f x y p




reading data
---

> ex1File :: FilePath
> ex1File = "app/ex1data1.txt"

> opts :: ParseOptions
> opts = ParseOptions (fromIntegral $ fromEnum ' ') Unheaded

> data Ex1Data = Ex1Data { v1 :: Double, v2 :: Double } deriving (Show, Eq)

> ex1Decoder :: Decode' ByteString Ex1Data
> ex1Decoder = Ex1Data <$> D.double <*> D.double
>

> ex1 :: IO (DecodeValidation ByteString [Ex1Data])
> ex1 = parseDecodeFromFile ex1Decoder opts ex1File

```{.output .example}

```


θj:=θj−α∂∂θjJ(θ0,θ1)


> main :: IO ()
> main = do
>   let n = 10
>   let answer = product [1..n::Integer]
>   void $ runOutput ("app/example.lhs", LHS) ("readme.md", GitHubMarkdown) $ do
>     pure () -- output "example" (show answer)

