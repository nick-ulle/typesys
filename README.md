# typesys

A general-purpose abstract type system.

This package provides an abstraction of common data types, so that they can be
queried and manipulated using a unified interface. The primary goal is to wrap
types from programming languages (especially C and LLVM), but markup languages
and data structures may also be supported. In other words, the types in this
package are general enough that they should work in many applications.

Types are arranged in a hierarchy of S4 classes which may include metadata
describing additional properties. Polymorphic types are also handled through
type metadata.

