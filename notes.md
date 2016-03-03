# Notes

## To Do

* Fix documentation.
* Fix exports so that users can extend classes.
* Replace ConditionalType with a rule mechanism in RTypeInference.
* Replace constant-folding with references or move it out of typr.

## Design

The guiding principle of this package is to provide an __elegant__ interface
for working with types. RTypeInference and RLLVMCompile are the primary users,
so their needs are considered first.

For the first phase of development, methods are only added as needed.

## Questions

What types do we need?

* top (unknown)
* primitives / atoms
    + number
    + text
    + function
* composites
    + array (homogenous, fixed length)
    + list (non-homogenous, variable length)
    + structure (non-homogenous, fixed length)
    + closure (structure + procedure)
* unit (void)

Thoughts on types:

* How is a list different from an array?
* A closure is just a structure that contains a single method.
* Do we need option types?
* Do we need reference types?

  These aren't used anywhere by R or C. Would they be useful for type
  inference?

---

What queries do we need to make?

* equality of types
* subtype relations
* get/set internal type(s) for composite types

Should types be able to report their size?

On one hand, this is essential for determining whether the data will fit in a
given piece of memory. On the other hand, types should be separated from
their concrete implementation.

---

How should we deal with arrays?

What about lists?

A list can have multiple elements, and each might have a diffferent type.

---

Should values (e.g., for constant folding) be stored on atomic types or in the
flow graph?

Types: 

* No need to look up the value for a type when it's needed.
* Allows checks for valid value based on type.

Flow graph:

* Values are in a central location, although value changes would still have to
  be propagated through the graph (do values change?)
* Decouples types from values.

---

Should context/semantic information be stored as attributes, in a slot on Type,
or as container classes?

Attributes:

* Easy to modify or adapt to needs.

On Type:

Container Classes:

* Only strategy that makes dispatch on context information possible.

This really depends on how we're going to use contextual information. Index and
iterator information is important because we want to be able to identify
iterative constructs and optimize accordingly. For example, if a `for` loop
generates a sequential vector solely for indexing, we could replace the vector
with an iterator that produces the appropriate value on each iteration, and
thereby reduce the memory cost of the loop.

Conditional types are probably the most important kind of contextual
information. They mark variables where slightly more information is needed to
correctly determine the type. Classical type inference algorithms do not use a
conditional type, and instead use the least upper bound (least common
supertype) of the known type information. Conditional types defer taking the
LUB for as long as possible. They also address a more practical issue: they are
a mechanism for writing rules for functions whose type signature cannot or
should not be inferred, such as many of the base R functions. So the main
concerns are:

1. We want to preserve detailed type information as long as possible, taking
   the LUB only when it's clear no new information will become available. On
   the other hand, what will the compiler do if that's the case? Should the
   compiler attempt to handle cases where complete type information is
   unavailable?

2. We need a mechanism to write type rules manually.

3. We need to address functions that aren't type stable. How should we describe
   the type signature for such a function?

---

Should the S4 class hierarchy be separated from the type lattice?

The S4 class hierarchy determines how dispatch will work in code that uses the
typr package. Adding methods that operate on the assumed type lattice is
trivial, so the organization of the S4 classes depends entirely on what we'd
like to dispatch on rather than testing for.

How should the S4 classes be organized?

RTypeInference creates types, and does not dispatch on types very often,
although it might be useful for the rule-writing mechanism. On the other hand,
the compiler needs to convert abstract types to concrete types, which will be
the main use of dispatch on types. The compiler may also enable/disable
optimizations depending on type information.


