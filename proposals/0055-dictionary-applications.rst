Explicit Dictionary Applications
================================

    TODOs

    * Add missing pieces from paper, in particular practical implementation choices:
      - ``... -> Constraint`` vs. ``... -> Type``
      - More on the import/export policy
      - ``getDict``
      - Default method implementations mechanism to fill in missing fields? ``MINIMAL`` pragma?
      - Expose the dictionaries of ``Coercible`` and ``HEq``?
      - ...
    * Go over everything again, especially the new coherence criterion

.. proposal-number::
.. trac-ticket::
.. implemented::
.. highlight:: haskell
.. header:: This proposal is `discussed at this pull request <https://github.com/ghc-proposals/ghc-proposals/pull/0>`_.
            **After creating the pull request, edit this file again, update the
            number in the link, and delete this bold sentence.**
.. sectnum::
.. contents::

This proposed GHC extension adds a form of explicit dictionary application without compromising on *global uniqueness of instances* and *coherence*.
By exposing the *dictionary record* that is generated behind the scenes for each type class, we give users the ability to create, pass around, and modify instances, making them truly first-class.
Together with the ability to *safely* explicitly apply such a dictionary to a function with a corresponding type class constraint, users will be able to use custom instances, overriding the default instance selection process.
The goal is to be backwards compatible: users should be able to pass dictionaries for existing type classes without modifying any libraries and library authors can rest safe that the new extension cannot be used to break their invariants.

This proposal is based on the paper `Coherent Explicit Dictionary Application for Haskell <https://dl.acm.org/citation.cfm?id=3242752>`_ by Winant and Devriese.
The main difference with the paper is a new criterion for coherence, that is practically implementable and still safe.
The proposal consists of a number of interconnected components that we explain below.

Motivation
------------

    TODO multiple instances, the ``Semigroup`` example?

In many situations, it would be useful to be able to instantiate a type class constraint with a custom implementation of the type class.
The obvious example are the many ``*By`` methods in the Prelude.
Consider, for example, the ``nub`` method in the Prelude.

::

  nub :: Eq a => [a] -> [a]

In this case, the Prelude offers the nub function for all types ``a`` that have an instance of the ``Eq`` type class.
The function filters duplicates from a list:

::

  >> nub ["abc", "def", "aBc", "abc"]
  ["abc", "def", "aBc"]

However, often we want to use such a function with a non-standard notion of equality.
For example, we may want to use a case-insensitive notion of equality when applying ``nub`` on the list ``["abc", "def", "aBc", "abc"]``.

Workaround: -By variants
````````````````````````

A first workaround is to use an alternative function offered by the Prelude:

::

  nubBy :: (a -> a -> Bool) -> [a] -> [a] -> Bool

  >> nubBy ((==) `on` map toLower) ["abc", "def", "aBc", "abc"]
  ["abc", "def", "aBc"]

However, this only works because the Prelude exposes two interfaces to the same method, implementing one in terms of the other.
Expecting all library authors to do this for all methods with type class constraints is hardly realistic, if only because of the overhead of implementing both versions for all functions.

In situations where library authors didn't have the foresight to provide a ``-By`` variant of a function with a constraint ``Eq`` (or ``Ord``), there exist a number of workarounds.

Workaround: Newtype wrappers
````````````````````````````
One approach is to define a newtype wrapper:

::

  newtype StringCI = MkStringCI { unStringCI :: String }
  instance Eq StringCI where
    (==) = (==) `on` (map toLower . unStringCI)

This works if the alternative instance we want to give can be defined as a top-level instance.
Imagine that we want to use nub with equality-modulo-``k`` in a function that takes ``k`` as an argument.

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
`reflection <http://hackage.haskell.org/package/reflection>`_ library, based on
the `implicit configurations <https://dl.acm.org/citation.cfm?id=1017481>`_ paper
by Kiselyov and Shan.

::

  newtype IntMod s = MkIntMod { unIntMod :: proxy s -> Int }
  instance Reifies s Int => Eq (IntMod s) where
    (==) = (==) `on` (`mod` reflect (Proxy @ s))

  f :: Int -> [Int] -> [Int]
  f k = reify k $ \ ps -> map (unIntMod ps) . nub . map (MkIntMod . const)

While this works for our example, it comes with quite some technical complexity (phantom type variable ``s``, infrastructure like ``Reifies``, ``reify``, ``reflect``, etc.).
Additionally, it becomes a bit annoying to use in more complex situations (e.g.\ instantiating multiple instances), but let's not go into this to avoid derailing the discussion.

Dictionary applications
```````````````````````
Our proposal is more direct: we propose to allow explicit dictionary applications that look as follows:

::

  mkEqDict :: (a -> a -> Bool) -> Eq.Dict a
  mkEqDict eq = Eq.Dict
    { (==) = eq
    , (/=) = \x y -> not (eq x y)
    }

  f :: Int -> [Int] -> [Int]
  f k = nub @{mkEqDict ((==) `on` (`mod` k))}

Coherence
`````````
However, naively adding dictionary applications is dangerous for two reasons. The first is illustrated below:

::

  twoEqs :: (Eq a, Eq a) => a -> a -> Bool
  twoEqs = (==)

  coherenceProblem = twoEqs @{mkEqDict (\ _ _ -> True)} 1 2

In this case, we instantiate one ``Eq a`` instance of a function that takes two.
However, inside ``twoEqs``, the instance/dictionary for ``Eq a`` used for the equality depends on the implementation of the constraint solver, and so does thus the result of ``coherenceProblem``.

Global uniqueness of instances (GUI)
````````````````````````````````````
The second problem is that some libraries rely on a property called `global uniqueness of instances <http://blog.ezyang.com/2014/07/type-classes-confluence-coherence-global-uniqueness/>`_.
An example from the paper by Winant and Devriese is the following:

::

  insert :: Ord a => a -> Set a -> Set a
  empty :: Set a
  reverseOrd :: Ord a => Ord.Dict a
  reverseOrd = Ord.Dict { compare = flip compare }

  >> insert @{reverseOrd} 1 (insert 1 (insert 2 empty))
  fromList [1, 2, 1]

What happens here is that the ``Data.Set`` API relies on the fact that if ``insert`` is used multiple times on the same binary search-tree, it will always happen with the same ``Ord`` instance.
By violating this assumption (as above), we can break the library's invariants, as demonstrated above (the set produced above contains the value ``1`` twice, which should never happen).

Proposed Change Specification
-----------------------------
The proposal consists of a number of related additions that enable explicit dictionary application, but only under some restrictions that preserve coherence and GUI.

All of the below modifications are enabled by the language extension flag ``-XDictionaryApplications``.
The flag only has a local effect, restricted to the source file(s) for which it is enabled.
This includes exposing the dictionaries, both for classes defined in the current file and any imported modules.

Exposing dictionaries
`````````````````````

For every type class definition like the following:

::

  class (C1 x1s, C2 x2s, ..., Cn xns, OtherCs) => C x1 ... xn where
    m1 :: t1
    m2 :: t2
    ..
    mn :: tn

In the above, ``C1`` through ``Cn`` are type classes (possibly the same as ``C``) and ``OtherCs`` are non-type-class constraints.

We now also expose a datatype ``C.Dict``.
If ``OtherCs`` is empty, then the type is equivalent to the following data type definition:

::

  data C.Dict x1 ... xn = C.Dict
    { parent1 :: C1.Dict x1s
    , parent2 :: C2.Dict x2s
    , ...
    , parentn :: Cn.Dict xns
    , m1 :: t1
    , ...
    , mn :: tn
    }

If ``OtherCs`` is not empty, then initially, we propose to not expose ``C.Dict``, although in principle, we could perhaps generate the GADT-equivalent of the above, with ``OtherCs`` as a constraint for the constructor ``C.Dict``.

The names ``m1`` through ``mn`` are not exposed as accessors, as they would conflict with the type class methods.
To access these fields, one can use pattern-matching (they are exposed as field names), or use the existing methods in combination with explicit dictionary application.

Note: see below for a discussion about naming choices in general, ``C.Dict`` and ``parentX`` in particular.

Explicit dictionary application
```````````````````````````````
We add a new expression of the form ``e_1 @{e_2 as C τs'}``.
Note: in the current protype implementation, this is written as ``e1 ((e2))`` and ``e2 ((e2 :: C τs'))``, but this is not intended as a long-term choice.

Note: is it necessary to require the programmer to explicitly name the type class C?
Alternatively, we could require the type of `e_2`` to be syntactically of the form ``C.Dict ...`` and require them to use a type annotation if it isn't.
    TODO ascii typing rules?

It is well-typed iff:

* Typing rules:

  - ``e_2`` is of type ``C.Dict τs``
  - The polymorphic type of ``e_1`` is explicit, i.e. ``e_1`` is either:

    + an expression with an explicit type signature
    + the name of a variable that has been previously given a type signature

  - ``e_1`` is of type ``forall as. Cs => τ``
  - one of the constraints in ``Cs`` is ``C τ'``, the remainder is ``C_rest``
  - ``τs`` = ``θ(τs')`` for some substitution θ
  - ``θ(C_rest)`` are emitted as wanted constraints
  - The dictionary application as a whole has type `θ(τ)` and emitted as a wanted constraint (TODO equality constraint).

* for one of the type variables ``a`` in ``C τs'`` (i.e. check all free variables until we find one for which all of the following hold...),

* global uniqueness condition:

  - For two fresh type variables a_1 and a_2, we have that

::

      Coercible a_1 a_2 ||-  Coercible ([a |-> a_1] τ) ([a |-> a_2] τ)

* Coherence condition:

  - For all of the constraints ``Ct`` in ``Cs`` that mention the type variable ``a``
  - ``Ct`` is a class constraint ``C' τcs`` (i.e. not an equality constraint or anything else)
  - a type class instance for `[a -> Newtype a](C' τcs)` would be legal, particularly:

    + it does not overlap with any of the instances registered for the type class ``C'``
    + it respects the functional dependency conditions if ``C'`` has functional dependencies.

  - Additionally, for all constraints ``C' τcs`` in ``C_rest`` that mention the type variable ``a`` (i.e. ``Cs`` except for the constraint being instantiated)

    + For two fresh type variables ``a_1`` and ``a_2``, we have that

::

   Coercible a_1 a_2 ||- Coercible ([a |-> a_1] C' τcs) ([a |-> a_2] C' τcs) 

Note: the part ``as C τs'`` in the new syntax is optional, as it can usually be inferred from the type of ``e_2``.
TODO required for disambiguation in cases of multiple type classes with the same name but different type variables.

[NOT part of this proposal] Dictionary Instances
````````````````````````````````````````````````

    TODO I'm not convinced the reasons to exclude this are problematic for everyone.
    This is a very nice and powerful feature that I would find very practical and would possibly use even more than explicit dictionary application in practice.

Winant and Devriese also proposed new syntax for declarations of the form:

::

   instance Cs => C τs = e

This defines an instance by a given instance expression (of type ``C.Dict τs``) instead of having to provide an implementation for each method.

Although dictionary instances are very useful, e.g. they offer more power than the existing ``-XDerivingVia`` and ``-XDefaultSignatures`` extensions (for quickly providing instances based on some predefined instance templates, elegantly deriving parent instances from child dictionaries, etc.), it is not part of this proposal.
The reason for this is that it creates an issue that didn't exist before: partiality in instance definitions.
Concretely, it becomes possible to write

::

  instance Eq MyType1 = error "Err..."

and also

::

  instance Eq MyType2 = veryLongCalculation

It is unclear how we should deal with this. The prototype implementation accepts all dictionary instances and simply inlines the expression (untouched) whenever the instance is used during constraint resolution.
In other words, the ``Eq MyType1`` instance would be inlined wherever ``MyType1`` values are checked for equality and the error would be reported at runtime, when the instance is used.
Similarly, the ``Eq MyType2`` instance would be inlined and the very long calculation would be performed once for every use of the instance (unless common subexpression elimination (CSE) is able to optimise this).

An alternative might be to perform a kind of termination check and normalisation as part of the instance declaration, but this comes with quite a number of design choices itself, and it is unclear whether this is desirable.
Because of these remaining questions, we propose to treat this idea as separate from this proposal and perhaps revisit it in the future, once DictionaryApplications has reached maturity.


Newtype Translation
```````````````````
To understand the coherence criterion and GUI criterion we propose, it is useful to consider the following *newtype translation*.
It is important to understand that this is only a *theoretical* translation, that will never be executed in reality (the real implementation is much simpler because it can be made part of the regular dictionary translation in GHC).
Still, the newtype translation is conceptually important, because it demonstrates how dictionary applications are conceptually equivalent to an application of coercions and it explains where the proposed criteria come from.
The newtype translation is explained in the paper by Winant and Devriese, but we re-explain the translation here to capture all the information in one place.

Consider the following code:

::

  trivialEq :: Eq.Dict t
  trivialEq = Eq.Dict (\ _ _ -> True) (\ _ _ -> False)

  doSomething :: (Eq a, Show a, Monoid b) => a -> b

  test :: (Show c, Monoid b) => c -> b
  test = doSomething @{trivialEq as Eq a}

The newtype translation of ``test`` looks as follows:

::

  newtype Wrapper a = Wrap { unWrap :: a } deriving (Show, Monoid)

  instance Eq (Wrapper a) = trivialEq
  instance Show a => Show (Wrapper a) = coerce (showDict :: Show.Dict a)

  test :: forall c. (Show c, Monoid b) => c -> b
  test = coerce doSomething'
    where
      doSomething' :: (Show c, Monoid b) => Wrapper c -> cool
      doSomething' = doSomething @(Wrapper c)


TODO explain what happens in more detail + role criterion

Our proposed validity criteria are exactly the criteria required to make the above translated code legal (modulo the use of a dictionary instance).
Specifically, the two coerces in the above translation are legal under the two role requirements in our proposed criteria for the dictionary application.
Also, the instances in the translation are legal under the conditions in our proposed criteria for the dictionary application.

Relation to Winant and Devriese's paper
```````````````````````````````````````
It is worth pointing out that this proposal builds on the feature proposed by Winant and Devriese in their paper presented at the Haskell Symposium 2018.
However, an important difference is that the coherence criterion we propose here is more practical but (probably) more restrictive.
The criterion proposed here is essentially inspired more directly by the newtype translation.

Effect and Interactions
-----------------------
TODO

Costs and Drawbacks
-------------------
TODO

Alternatives
------------
See discussion about the reflection package above.

TODO positional dictionary application instead of nominal

Unresolved Questions
--------------------

Syntax choices
``````````````
The above mentioned choices concerning syntax and naming are preliminary.

- ``e1 @{e2 as tau}`` (dictionary applications)
  We chose the ``@`` as it resembles the syntax for (visible) type application.

- ``instance C taus = e`` (dictionary instances)
  TODO ``where`` block allowed? Not too confusing?

Naming choices
``````````````
- `C.Dict` (the type of the dictionaries):

  The name `Dict` is not reserved, so this clashes with current identifier for the type ``Dict`` with (module) qualifier ``C``.
  Alternatives?

- `C.Dict` (the dictionary constructor):

  Same remarks.

- `parent1` ... `parentn`:

  These are obviously not the most meaningful of names, but it is not clear how we could do better.
  Perhaps we could add syntax to class declarations for naming parent constraint dictionaries?
  TODO ``-XDuplicateRecordFields`` is necessary.

Import/Export rules
```````````````````
When are the new ``C.Dict`` and ``parent1 ... parentn`` identifiers exported by a module?

It is our goal to avoid having to modify existing code.
To accomplish this, ideally, these identifiers should be exported by default whenever the corresponding type class is exported.
How this can be accomplished without generating conflicts remains to be seen.
Optionally, module authors should have the ability to explicitly *not* export them.


Implementation Plan
-------------------

A proof of concept implementation was implemented by Thomas Winant as `a fork of GHC <https://github.com/mrBliss/ghc>`_.
It is usable as is, but quite a long way from ready.
Specifically, it does not implement the coherence criterion proposed here, nor the theoretical one used in the paper, but a different one that is not sufficient.
Additionally, when the GUI criterion is violated, the prototype implementation generates warnings, not errors.

Based on the experience with the prototype implementation, we do not expect it to be a very costly implementation.
Specifically, there is
* no interaction at all with the constraint solver
* little interaction with the role infrastructure (suitable methods for checking roles are available).

Thomas Winant, who has implemented the prototype implementation as part of his PhD, is now working at `Well-Typed <https://www.well-typed.com/>`_ on different projects.
As such, we are looking for a volunteer to bring the prototype implementation to maturity.
