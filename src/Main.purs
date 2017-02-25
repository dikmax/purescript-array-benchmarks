module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Data.Array ((..))
import Data.Foldable (foldMap, foldr)
import Data.Monoid.Additive (Additive(..))
import Data.Monoid.Multiplicative (Multiplicative(..))
import Data.Newtype (ala)
import Test.QuickCheck.Arbitrary (arbitrary)
import Test.QuickCheck.Gen (vectorOf)
import Benchotron.Core (Benchmark, BenchEffects, benchFn, mkBenchmark)
import Benchotron.UI.Console (runSuite)

foreign import concatDefault :: forall a. Array (Array a) -> Array a
foreign import concatApply :: forall a. Array (Array a) -> Array a
foreign import concatCall :: forall a. Array (Array a) -> Array a
foreign import concatChunks :: forall a. Array (Array a) -> Array a

benchConcat :: Benchmark
benchConcat = mkBenchmark
  { slug: "concat"
  , title: "Finding the concat of an arrays"
  , sizes: (1..10) <#> (_ * 100)
  , sizeInterpretation: "Number of arrays"
  , inputsPerSize: 1
  , gen: \n -> vectorOf n (vectorOf n arbitrary)
  , functions: [ benchFn "default" (concatDefault :: Array (Array Number) -> Array Number)
               -- , benchFn "concat.apply (unsafe)" (concatApply :: Array (Array Number) -> Array Number)
               -- , benchFn "concat" (concatCall :: Array (Array Number) -> Array Number)
               , benchFn "concat chunks" (concatChunks :: Array (Array Number) -> Array Number)
               ]
  }

benchConcat2 :: Benchmark
benchConcat2 = mkBenchmark
  { slug: "concat2"
  , title: "Finding the concat of an arrays"
  , sizes: (1..20) <#> (_ * 10)
  , sizeInterpretation: "Number of arrays"
  , inputsPerSize: 1
  , gen: \n -> vectorOf n (vectorOf n arbitrary)
  , functions: [ benchFn "default" (concatDefault :: Array (Array Number) -> Array Number)
               -- , benchFn "concat.apply (unsafe)" (concatApply :: Array (Array Number) -> Array Number)
               -- , benchFn "concat" (concatCall :: Array (Array Number) -> Array Number)
               , benchFn "concat chunks" (concatChunks :: Array (Array Number) -> Array Number)
               ]
  }

benchConcatSmallOfLarge :: Benchmark
benchConcatSmallOfLarge = mkBenchmark
  { slug: "concat-small"
  , title: "Finding the concat of an arrays"
  , sizes: (1..10)
  , sizeInterpretation: "Number of arrays"
  , inputsPerSize: 1
  , gen: \n -> vectorOf n (vectorOf 100000 arbitrary)
  , functions: [ benchFn "default" (concatDefault :: Array (Array Number) -> Array Number)
               -- , benchFn "concat.apply (unsafe)" (concatApply :: Array (Array Number) -> Array Number)
               -- , benchFn "concat" (concatCall :: Array (Array Number) -> Array Number)
               , benchFn "concat chunks" (concatChunks :: Array (Array Number) -> Array Number)
               ]
  }

benchConcatLargeOfSmall :: Benchmark
benchConcatLargeOfSmall = mkBenchmark
  { slug: "concat-large"
  , title: "Finding the concat of an arrays"
  , sizes: (1..10) <#> (_ * 100000)
  , sizeInterpretation: "Number of arrays"
  , inputsPerSize: 1
  , gen: \n -> vectorOf n (vectorOf 1 arbitrary)
  , functions: [ benchFn "default" (concatDefault :: Array (Array Number) -> Array Number)
               -- , benchFn "concat.apply (unsafe)" (concatApply :: Array (Array Number) -> Array Number)
               -- , benchFn "concat" (concatCall :: Array (Array Number) -> Array Number)
               , benchFn "concat chunks" (concatChunks :: Array (Array Number) -> Array Number)
               ]
  }

benchConcatLargeOfSmall2 :: Benchmark
benchConcatLargeOfSmall2 = mkBenchmark
  { slug: "concat-large2"
  , title: "Finding the concat of an arrays"
  , sizes: (1..10) <#> (_ * 100000)
  , sizeInterpretation: "Number of arrays"
  , inputsPerSize: 1
  , gen: \n -> vectorOf n (vectorOf 10 arbitrary)
  , functions: [ benchFn "default" (concatDefault :: Array (Array Number) -> Array Number)
               -- , benchFn "concat.apply (unsafe)" (concatApply :: Array (Array Number) -> Array Number)
               -- , benchFn "concat" (concatCall :: Array (Array Number) -> Array Number)
               , benchFn "concat chunks" (concatChunks :: Array (Array Number) -> Array Number)
               ]
  }

main :: forall eff. Eff (BenchEffects eff) Unit
main = runSuite [ benchConcat
                , benchConcat2
                , benchConcatSmallOfLarge
                , benchConcatLargeOfSmall
                , benchConcatLargeOfSmall2
                ]
