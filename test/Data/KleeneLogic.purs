module Test.Data.KleeneLogic where

import Prelude (Unit, bind, discard, ($), (==))
import Control.Monad.Aff (Aff, attempt)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Random (RANDOM)
import Data.KleeneLogic (KleeneLogic, kleeneNot)
import Data.Either (isLeft)
import Data.Lattice (meet, bottom)
import Data.Lattice.Verify (verify, verifyBoundedLattice, verifyLattice)
import Test.QuickCheck (class Testable)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.QuickCheck (QCRunnerEffects, quickCheck)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (run)
import Type.Proxy (Proxy(..))

quickCheckFail ∷ ∀ t e. Testable t ⇒ t → Aff (random ∷ RANDOM | e) Unit
quickCheckFail a = do
  result ← attempt $ quickCheck a
  isLeft result `shouldEqual` true

pseudoComplement :: KleeneLogic -> Boolean
pseudoComplement a = a `meet` (kleeneNot a) == bottom

p ∷ Proxy KleeneLogic
p = Proxy

main ∷ Eff (QCRunnerEffects (exception ∷ EXCEPTION)) Unit
main = run [consoleReporter] do
  describe "Lattice KleeneLogic" do
    it "should satisfy the Lattice laws" do
      quickCheck $ verify p verifyLattice
  describe "BoundedLattice KleeneLogic" do
    it "should satisfy the BoundedLattice laws" do
      quickCheck $ verify p verifyBoundedLattice
  describe "Not HeytingAlgebra KleeneLogic" do
    it "should not satisfy the HeytingAlgebra laws (ie. pseudoComplement)" do
      quickCheckFail pseudoComplement
