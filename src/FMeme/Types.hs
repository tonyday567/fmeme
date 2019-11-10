
-- functions for testing
module FMeme.Types where

import Control.Foldl as L
import Data.Vector as V
import Data.Functor.Identity

newtype HyperHeuristic f g h a b = HyperHeuristic { unHH :: f (Heuristic g h a) -> f (Heuristic g h b) }

data HeuristicType = Constructive | Perturbative

-- any approach to problem solving or *self-discovery* that employs a practical method, not guaranteed to be optimal, perfect, or rational, but instead sufficient for reaching an immediate goal.
newtype Heuristic f g h a = Heuristic { unH :: f (Params h a) -> g (Params h a) }

newtype Params f a = Params { unParams :: f a }

newtype Funk f a b = Funk { unFunk :: Params f a -> b }

type Neighbourhood = Heuristic Identity
type Peturbation = Heuristic Identity Identity
type Mutation = Heuristic Identity Identity
type Crossover f = Heuristic f Identity
type Population f g a = f (Params g a)
type Individual g a = Population Identity g a






