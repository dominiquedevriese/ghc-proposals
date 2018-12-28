Notes on reStructuredText - delete this section before submitting
==================================================================

The proposals are submitted in reStructuredText format.  To get inline code, enclose text in double backticks, ``like this``.  To get block code, use a double colon and indent by at least one space

::

 like this
 and

 this too

To get hyperlinks, use backticks, angle brackets, and an underscore `like this <http://www.haskell.org/>`_.


Explicit Dictionary Applications
================================

.. proposal-number:: Leave blank. This will be filled in when the proposal is
                     accepted.
.. trac-ticket:: Leave blank. This will eventually be filled with the Trac
                 ticket number which will track the progress of the
                 implementation of the feature.
.. implemented:: Leave blank. This will be filled in with the first GHC version which
                 implements the described feature.
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/0>`_.
            **After creating the pull request, edit this file again, update the
            number in the link, and delete this bold sentence.**
.. sectnum::
.. contents::

This proposed GHC extension adds a form of explicit dictionary applications without compromising global instance uniqueness and uniqueness.
It is based on the paper [Coherent Dictionary Applications for Haskell][https://dl.acm.org/citation.cfm?id=3242752] by Winant and Devriese.
The proposal consists of a number of interconnected components that we explain below.

Motivation
------------
In many situations, it would be useful to be able to instantiate a type class constraint with a custom implementation of the type class.
The obvious example that comes to mind is the many *By methods in the Prelude.
Consider, for example, the nub method in the Prelude.

::

  nub :: Eq a => [a] -> [a]

In this case, Prelude offers the nub function for all types a for which the ``Eq`` type class is instantiated.
The function filters duplicates from a list:

  nub ["abc", "def", "aBc", "abc"] = ["abc", "def", "aBc"]

Workaround: xyzBy
`````````````````

However, often we want to use such a function with a non-standard notion of equality.
In this case, we can use an alternative function offered by the Prelude:

::

  nubBy :: (a -> a -> Bool) -> [a] -> [a] -> Bool
  nubBy ((==) `on` map toLower) ["abc", "def", "aBc", "abc"] = ["abc", "def", "aBc"]

However, this only works because the Prelude exposes two interfaces to the same method, implementing one in terms of the other.
Expecting all library authors to do this for all methods with type class constraints is hardly realistic, if only because of the overhead of implementing both versions for all functions.

In situations where library authors didn't have the foresight to provide a ``nubBy`` version of a function with a constraint ``nub``, there exist a number of workarounds.

Workaround: Newtype wrappers
````````````````````````````
One approach is to define a newtype wrapper:

::

  newtype StringCI = MkStringCI { unStringCI :: String }
  instance Eq StringCI where
    (==) = (==) `on` (map toLower . unStringCI)

This works if the alternative instance we want to give can be defined as a top-level instance.
Imagine that we want to use nub with equality-modulo-k in a function that takes k as an argument.

::

  newtype IntModK = MkIntModK { unIntModK :: Int }
  instance Eq IntModK where
    (==) = (==) `on` (`mod` k) -- k is not in scope here?
    
  f :: Int -> [Int] -> [Int]
  f k = map unIntModK . nub . map MkIntModK

In this case, we cannot give the instance we want to give because the instance
for our newtype must necessarily be toplevel (like all instances) and cannot
mention local values like ``k``.

Workaround: Reflection
``````````````````````
For this problem too, there are solutions, particularly the
[reflection][http://hackage.haskell.org/package/reflection] library, based on
the `implicit configurations <https://dl.acm.org/citation.cfm?id=1017481>` paper
by Kiselyov and Shan.

::

  newtype IntMod s = MkIntMod { unIntMod :: proxy s -> Int }
  instance Reifies s Int => Eq (IntMod s) where
    (==) = (==) `on` (`mod` reflect (Proxy @ s))

  f :: Int -> [Int] -> [Int]
  f k = reify k $ \ ps -> map (unIntMod ps) . nub . map (MkIntMod . const)

While this works for our example, it comes with quite some technical complexity (phantom type variable s, infrastructure like Reifies, reify, reflect etc.).
Additionally, it becomes a bit annoying to use in more complex situations (but let's not go into this to avoid derailing the discussion).

Dictionary applications
```````````````````````
Our proposal is more direct: we propose to allow explicit dictionary applications that look as follows:

::

  mkEqDict :: (a -> a -> Bool) -> Eq.Dict a
  mkEqDict eq = Eq.Dict eq (\ x y -> not (eq x y))

  f :: Int -> [Int] -> [Int]
  f k = nub @ (mkEqDict ((==) `on` (`mod` k)))

Coherence
`````````
However, naively adding dictionary applications is dangerous for two reasons. The first is illustrated below:

::

  twoEqs :: (Eq a, Eq a) => a -> a -> Bool
  twoEqs = (==)

  coherenceProblem = (twoEqs @ mkEqDict (\ _ _ -> True)) 1 2

In this case, we instantiate one ``Eq a`` instance of a function that takes two.
However, inside ``twoEqs``, it depends on details of the constraint solver which one will be used, and so does the result of ``coherenceProblem``.

Global instance uniqueness (GIU)
````````````````````````````````
The second problem is that some libraries rely on a property called `global instance uniqueness <http://blog.ezyang.com/2014/07/type-classes-confluence-coherence-global-uniqueness/>`_.
An example from the paper by Winant and Devriese is the following:

::

  insert :: Ord a ⇒ a → Set a → Set a
  empty :: Set a
  reverseOrd :: Ord a ⇒ Ord.Dict a
  reverseOrd = Ord.Dict { compare = flip compare }

  insert @ {reverseOrd} 1 (insert 1 (insert 2 empty)) = fromList [1, 2, 1]

What happens here is that the Data.Set API relies on the fact that if `insert` is used multiple times on the same tree, it will always happen with the same `Ord` instance.
By violating this assumption (as above), we can break the library's invariants, as demonstrated above (the set produced above contains the value `1` twice, which should never happen).

Proposed Change Specification
-----------------------------
The proposal consists of a number of related additions that enable explicit dictionary applications, but only under some restrictions that preserve coherence and GIU.  

All of the below modifications are enabled by the language extension flag DictionaryApplications.
The flag only has a local effect, restricted to the source file(s) for which it is enabled.

Dictionary types
````````````````

For every type class definition like the following:

::

  class C1 x1s, C2 x2s, ..., Cn xns, OtherCs => C x1 ... xn where
    m1 :: T1
    m2 :: T2
    ..
    mn :: Tn

In the above, C1 through Cn are type classes (possibly the same as C) and OtherCs are non-type-class constraints.

We now also expose a datatype ``C.Dict``.
If OtherCs is empty, then the type is equivalent to the following data type definition:

::

  data C.Dict x1 ... xn = C.Dict {
       parent1 :: C1.Dict x1s
     , parent2 :: C2.Dict x2s
     , ...
     , parentn :: Cn.Dict xns
     , _m1 :: T1
     , ...
     , _mn :: Tn
    }

If OtherCs is not empty, then initially, we propose to not expose `C.Dict`, although in principle, we could probably generate the GADT-equivalent of the above, with OtherCs as a constraint for the constructor C.Dict.

The names ``_m1`` through ``_mn`` are not exposed because the existing methods ``m1`` can be used instead, using a dictionary application.
Note: see below about naming choices in general, and ``C.Dict``, ``parentX``, ``m1`` in particular.

Dictionary applications
```````````````````````
We add a new form of expression of the form ``e_1 @{e_2 as C taus'}``.
Note: in the current protype implementation, this is written as ``e1 ((e2))``, but this is not intended as a long-term choice.

It is well-typed iff
- Typing rules:
  - e_2 is of type C.Dict taus
  - The polymorphic type of e_1 is explicit, i.e. e_1 is either
    - an expression with an explicit type signature
    - the name of a variable that has been previously given a type signature
  - e_1 is of type forall as. Cs => tau
  - one of the constraints in Cs is C taus', the remainder is Crest
  - taus = taus' phi for some substitution phi
  - Crest phi is generated as a wanted constraint
  - The dictionary application as a whole has type `tau phi` is generated as a wanted constraint.
- GIU condition:
  - for one of the type variables a in C taus',
  - tau depends on a at role representational
- Coherence condition:
  - For all of the constraints Ct in Cs that mention the type variable a
  - Ct is a class constraint C' taucs (i.e. not an equality constraint or anything else)
  - a type class instance for `(C' taucs)[a \mapsto Newtype a]` would be legal, particularly:
    - it does not overlap with any of the instances registered for the type class C'
    - it respects the functional dependencies conditions if C' has fundeps.
  - Additionally, for all constraints C' taucs in Crest that mention the type variable a (i.e. Cs except for the constraint being instantiated)
    - (C' taucs) depends on a at role representational

Note: the part ``as C taus'`` in the new syntax should probably be made optional, as it can normally be inferred from the type of e_2.

[NOT part of this proposal] Dictionary Instances
````````````````````````````````````````````````

Winant and Devriese also proposed new syntax for declarations of the form ``instance Cs => C taus = e``, which defines an instance by a given instance expression (of type ``C.Dict taus``) instead of using a ``where`` block.  

Although dictionary instances are very useful (e.g.\ as a more general alternative to the DerivingVia extension, for elegantly deriving parent instances from child dictionaries etc.), it is not part of this proposal.
The reason for this is that it creates an issue that didn't exist before: partiality in instance definitions.
Concretely, it becomes possible to write

::

  instance Eq MyType1 = error "Err..."

and also

::

  instance Eq MyType2 = veryLongCalculation

It is unclear how we should deal with this. The prototype implementation accepts all dictionary instances and simply inlines the expression (untouched) whenever the instance is used during constraint resolution.
In other words, the `Eq MyType1` instance would be inlined wherever MyType1 values are checked for equality and the error would be generated at runtime when the instance is used.
Similarly, the `Eq MyType2` instance would be inlined and the very long calculation would be performed once for every use of the instance.

An alternative might be to perform a kind of termination check + normalization as part of the instance declaration, but this comes with quite a number of design choices itself, and it is unclear whether this is desirable.
Because of these remaining questions, we propose to treat this idea as separate from this proposal and perhaps revisit it in the future, once DictionaryApplications has reached maturity. 

Newtype Translation
```````````````````
To understand the coherence criterion and GIU criterion we propose, it is useful to consider the following "newtype translation".
It is important to understand that this is only a *theoretical* translation, that will never be executed in reality (the real implementation is much simpler because it can be made part of the regular dictionary translation in GHC).
Still, the newtype translation is conceptually important, because it demonstrates how dictionary applications are conceptually equivalent to an application of coercions and it explains where the proposed criteria come from.
The newtype translation is explained in the paper by Winant and Devriese, but we re-explain the translation here to capture all the information about DictionaryApplications in one place.

Consider the following code:

::

  trivialEq = Eq.Dict (\ _ _ -> True) (\ _ _ -> False)

  doSomething :: Eq a, Show a, Monoid b => a -> b
  doSomething = _

  test :: (Show c, Monoid b) => c -> b
  test = doSomething @{trivialEq as Eq a}

The newtype translation of test looks as follows:

  newtype Wrapper a = Wrap { unWrap :: a }

  instance Eq (Wrapper a) = trivialEq
  instance Show a => Show (Wrapper a) = coerce (showDict :: Show.Dict a)
  
  test :: forall c. (Show c, Monoid b) => c -> b
  test = coerce doSomething'
    where doSomething' :: (Show c, Monoid b) => Wrapper c -> cool
          doSomething' = doSomething @(Wrapper c)

Our proposed validity criteria are exactly the criteria required to make the above translated code legal (modulo the use of a dictionary instance).
Specifically, the two coerces in the above translation are legal under the two role requirements in our proposed criteria for the dictionary application.
Also, the instances in the translation are legal under the conditions in our proposed criteria for the dictionary application.

Relation to Winant and Devriese's paper
```````````````````````````````````````
It is worth pointing out that this proposal builds on the feature proposed by Winant and Devriese in their paper presented at the Haskell Symposium 2018.
However, an important difference is that the coherence criterion we propose here is more practical but (probably) more restrictive.
The criterion proposed here is essentially inspired more directly by the newtype translation.

Alternatives
------------
See discussion about the reflection package above.

Unresolved Questions
--------------------

Syntax choices
``````````````
The above mentioned choices concerning syntax and naming are preliminary.
- ``e1 @{e2 as tau}`` (dictionary applications)

- ``instance C taus = e`` (dictionary instances)

Naming choices
``````````````
- `C.Dict` (the type of the dictionaries):

  The name `Dict` is not reserved, so this clashes with current syntax for type Dict in module C.
  Perhaps this is acceptable because it is only a problem in modules that enable DictionaryApplications and because it is only for this particular name.

  Alternatives?

- `C.Dict` (the dictionary constructor):

  Same remarks.

- `parent1` ... `parentn`:

  These are obviously not the most meaningful of names, but it is not clear how we could do better.
  Perhaps we could add syntax to class declarations for naming parent constraint dictionaries?

Import/Export rules
```````````````````
When are the new `C.Dict` and `parent1`...`parentn` names exported by a module?


Implementation Plan
-------------------

A proof of concept implementation was implemented by Thomas Winant as `a fork of GHC <ssh://git@dnetcode.cs.kuleuven.be:2222/explicit-dictionaries-ghc.git>`_.
It is usable as is, but quite a long way from ready.
Specifically, it does not implement the coherence criterion proposed here, nor the theoretical one used in the paper, but a different one that is not sufficient.
Additionally, when the GIU criterion is violated, the prototype implementation generates warnings, not errors.

Based on the experience with the prototype implementation, we do not expect it to be a very costly implementation.
Specifically, there is
* no interaction at all with the constraint solver
* little interaction with the role infrastructure (suitable methods for checking roles are available).

Thomas Winant, who has implemented the prototype implementation as part of his PhD, is now working at `Well-Typed <https://www.well-typed.com/>`_ on different projects.
As such, we are looking for a volunteer to bring the prototype implementation to maturity.
