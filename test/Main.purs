module Test.Main where

import Prelude

import Data.Set as Set
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import StringRewriting.KnuthBendix (Equation(..))
import FunctorialDataMigration.Core.Signature (Signature, signatureIsWellFormed)
import FunctorialDataMigration.Core.SignatureMapping (SignatureMapping, mappingIsWellFormed)
import Test.Assert (assert, assertFalse)
import Test.Spec (it, describe)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

emptySignature :: Signature
emptySignature = { nodes         : Set.empty
                 , edges         : Set.empty
                 , pathEquations : Set.empty
                 }

-- |      f
-- |   a ---> b
-- |   |      |
-- | g |      | h
-- |   \/     \/
-- |   c ---> d
-- |      i
-- |
freeSquare :: Signature
freeSquare =
  { nodes : Set.fromFoldable ["a", "b", "c", "d"]
  , edges : Set.fromFoldable [ {source: "a", target: "b"}
                             , {source: "b", target: "d"}
                             , {source: "a", target: "c"}
                             , {source: "c", target: "d"}
                             ]
  , pathEquations : Set.empty
  }

-- |       f'
-- |    a' ---> b'
-- |    |       |
-- | g' |   =   | h'
-- |    \/      \/
-- |    c' ---> d'
-- |        i'
-- |
-- | with f' ; h' = g' ; i'
-- |
commutativeSquare :: Signature
commutativeSquare =
  { nodes : Set.fromFoldable ["a'", "b'", "c'", "d'"]
  , edges : Set.fromFoldable [ {source: "a'", target: "b'"}
                             , {source: "b'", target: "d'"}
                             , {source: "a'", target: "c'"}
                             , {source: "c'", target: "d'"}
                             ]
  , pathEquations : Set.singleton
      $ Equation [ {source: "a'", target: "b'"}, {source: "b'", target: "d'"} ]
                 [ {source: "a'", target: "c'"}, {source: "c'", target: "d'"} ]
  }

-- | The bogus square has a path equation including an edge
-- | from a to d which does not exist in the signature's edge set
bogusSquare :: Signature
bogusSquare =
  { nodes : Set.fromFoldable ["a", "b", "c", "d"]
  , edges : Set.fromFoldable [ {source: "a", target: "b"}
                             , {source: "b", target: "d"}
                             , {source: "a", target: "c"}
                             , {source: "c", target: "d"}
                             ]
  , pathEquations : Set.singleton
    (Equation [ {source: "a", target: "b"}, {source: "b", target: "d"} ]
              [ {source: "a", target: "d"} ])
  }

-- | A signature with an edge that doesn't have corresponding nodes
invalidEdgeSig :: Signature
invalidEdgeSig =
  { nodes : Set.singleton "a"
  , edges : Set.singleton $ { source : "a", target : "b" }
  , pathEquations : Set.empty
  }

-- | This mapping from the free square to the commutative square is
-- | well-formed as the node- and edge-functions are well-formed
-- | and the mapping preserves path equivalences.
-- |
-- | As the free square has no path equivalences, they are trivially preserved.
freeToCommMapping :: SignatureMapping
freeToCommMapping =
  { source : freeSquare
  , target : commutativeSquare
  , nodeFunction : Set.fromFoldable
    [ Tuple "a" "a'"
    , Tuple "b" "b'"
    , Tuple "c" "c'"
    , Tuple "d" "d'"
    ]
  , edgeFunction : Set.fromFoldable
    [ Tuple {source: "a", target: "b"}
            [ {source: "a'", target: "b'"} ]
    , Tuple {source: "b", target: "d"}
            [ {source: "b'", target: "d'"} ]
    , Tuple {source: "a", target: "c"}
            [ {source: "a'", target: "c'"} ]
    , Tuple {source: "c", target: "d"}
            [ {source: "c'", target: "d'"} ]
    ]
  }

-- | This mapping violates the functor law of source-target preservation for
-- | morphisms. For a morphism in the domain $f: a -> b$ that is mapped to a
-- | morphisms in the codomain $F(f)$ by a functor $F$, then $F(f) must have
-- | source and target given by $F(a) -> F(b)$.
freeToCommMappingViolatingSourceTargetPreservation :: SignatureMapping
freeToCommMappingViolatingSourceTargetPreservation =
  { source : freeSquare
  , target : commutativeSquare
  , nodeFunction : Set.fromFoldable
    [ Tuple "a" "a'"
    , Tuple "b" "b'"
    , Tuple "c" "a'"
    , Tuple "d" "b'"
    ]
  , edgeFunction : Set.fromFoldable
    [ Tuple {source: "a", target: "b"}
            [ {source: "a'", target: "b'"} ]
    , Tuple {source: "b", target: "d"}
            [ {source: "b'", target: "d'"} ]
    , Tuple {source: "a", target: "c"}
            [ {source: "a'", target: "c'"} ]
    , Tuple {source: "c", target: "d"}
            [ {source: "c'", target: "d'"} ]
    ]
  }

-- | This mapping from the commutative square to the free square is not well-formed
-- | as the morphisms of the commutative square do not obey their equation after
-- | being mapped to the free square.
-- |
-- | For this mapping to correspond to a valid functor, the following equation
-- | has to hold:
-- |
-- |     $f ; h = F(f') ; F(h') = F(f' ; h') = F(g' ; i') = F(g') ; F(i') = g ; h$
-- |
-- | But this does not for the free square, where $f ; h \neq g ; h$
commToFreeMapping :: SignatureMapping
commToFreeMapping =
  { source : commutativeSquare
  , target : freeSquare
  , nodeFunction : Set.fromFoldable
    [ Tuple "a'" "a"
    , Tuple "b'" "b"
    , Tuple "c'" "c"
    , Tuple "d'" "d"
    ]
  , edgeFunction : Set.fromFoldable
    [ Tuple {source: "a'", target: "b'"}
            [ {source: "a", target: "b"} ]
    , Tuple {source: "b'", target: "d'"}
            [ {source: "b", target: "d"} ]
    , Tuple {source: "a'", target: "c'"}
            [ {source: "a", target: "c"} ]
    , Tuple {source: "c'", target: "d'"}
            [ {source: "c", target: "d"} ]
    ]
  }

-- | This mapping from the commutative square to the free square *is* well-formed
-- | as by mapping all commutativeSquare objects to a single freeSquare object and
-- | all commutativeSquare edges to the identity morphism of the freeSquare object,
-- | the path equations are trivially preserved.
-- |
-- | The mappings from the edges fo the commutative square to the identity morphism on
-- | the "a" object of the free square are implicit.
commToFreeMappingTrivial :: SignatureMapping
commToFreeMappingTrivial =
  { source : commutativeSquare
  , target : freeSquare
  , nodeFunction : Set.fromFoldable
    [ Tuple "a'" "a"
    , Tuple "b'" "a"
    , Tuple "c'" "a"
    , Tuple "d'" "a"
    ]
  , edgeFunction : Set.empty
  }

main :: Effect Unit
main = do
  launchAff_ do
    runSpec [ consoleReporter ] do
      describe "Well-formedness of Signatures" do
        it "emptySignature is well-formed" $ liftEffect $
          assert $ signatureIsWellFormed emptySignature
        it "freeSquare is well-formed" $ liftEffect $
          assert $ signatureIsWellFormed freeSquare
        it "commutativeSquare is well-formed" $ liftEffect $
          assert $ signatureIsWellFormed commutativeSquare
        it "bogusSquare is not well-formed" $ liftEffect $
          assertFalse $ signatureIsWellFormed bogusSquare
        it "invalidEdgeSig is not well-formed" $ liftEffect $
          assertFalse $ signatureIsWellFormed invalidEdgeSig
      describe "Well-formedness of SignatureMappings" do
        it "freeToCommMapping is well-formed" $ liftEffect $
          assert $ mappingIsWellFormed freeToCommMapping
        it "freeToCommMappingViolatingSourceTargetPreservation is not well-formed" $ liftEffect $
          assertFalse $ mappingIsWellFormed freeToCommMappingViolatingSourceTargetPreservation
        it "commToFreeMapping is not well-formed" $ liftEffect $
          assertFalse $ mappingIsWellFormed commToFreeMapping
        it "commToFreeMappingTrivial is well-formed" $ liftEffect $
          assert $ mappingIsWellFormed commToFreeMappingTrivial
