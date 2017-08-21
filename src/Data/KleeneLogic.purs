module Data.KleeneLogic where

import Prelude (class Eq, class Show, ($))
import Data.Lattice
import Data.NonEmpty ((:|))
import Test.QuickCheck.Arbitrary (class Arbitrary, class Coarbitrary)
import Test.QuickCheck.Gen (elements, perturbGen)

data KleeneLogic = True | False | Unknown

kleeneConj ∷ KleeneLogic → KleeneLogic → KleeneLogic
kleeneConj False _ = False
kleeneConj _ False = False
kleeneConj Unknown True = Unknown
kleeneConj True Unknown = Unknown
kleeneConj Unknown Unknown = Unknown
kleeneConj True True = True

kleeneDisj ∷ KleeneLogic → KleeneLogic → KleeneLogic
kleeneDisj True _ = True
kleeneDisj _ True = True
kleeneDisj Unknown False = Unknown
kleeneDisj False Unknown = Unknown
kleeneDisj Unknown Unknown = Unknown
kleeneDisj False False = False

kleeneNot ∷ KleeneLogic → KleeneLogic
kleeneNot False = True
kleeneNot True = False
kleeneNot Unknown = Unknown

kleeneImpl ∷ KleeneLogic → KleeneLogic → KleeneLogic
kleeneImpl False _ = True
kleeneImpl True True = True
kleeneImpl True Unknown = Unknown
kleeneImpl True False = False
kleeneImpl Unknown True = True
kleeneImpl Unknown Unknown = Unknown
kleeneImpl Unknown False = Unknown

instance showKleeneLogic ∷ Show KleeneLogic where
  show True = "True"
  show False = "False"
  show Unknown = "Unknown"

instance eqKleeneLogic ∷ Eq KleeneLogic where
  eq True True = true
  eq False False = true
  eq Unknown Unknown = true
  eq _ _ = false

instance joinSemilatticeKleeneLogic ∷ JoinSemilattice KleeneLogic where
  join = kleeneDisj

instance meetSemilatticeKleeneLogic ∷ MeetSemilattice KleeneLogic where
  meet = kleeneConj

instance bJoinSemilatticeKleeneLogic ∷ BoundedJoinSemilattice KleeneLogic where
  bottom = False

instance bMeetSemilatticeKleeneLogic ∷ BoundedMeetSemilattice KleeneLogic where
  top = True

instance latticeKleeneLogic ∷ Lattice KleeneLogic

instance boundedLatticeKleeneLogic ∷ BoundedLattice KleeneLogic

instance arbitraryKleeneLogic :: Arbitrary KleeneLogic where
  arbitrary = elements $ Unknown :| [True, False]

instance coarbitraryKleeneLogic :: Coarbitrary KleeneLogic where
  coarbitrary True    = perturbGen 1.0
  coarbitrary False   = perturbGen 2.0
  coarbitrary Unknown = perturbGen 3.0

