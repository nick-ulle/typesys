# typesys

A type expression language for R written in R.

This package includes:

*   A formula-based type expression language for describing types.

*   A parser to convert the type expression language into a syntax tree of S4
    objects.

*   A collection of S4 classes that represent general-purpose types.

*   A collection of S4 classes that represent specific types in R.

*   Type environments that hold (name, type) pairs, analogous to how R's
    environments hold (name, value) pairs.

*   Additional functions for manipulating types. For example, a method to unify
    two types algebraically.

This package is infrastructure for __[RTypeInference][]__. It might be useful
if you need a formal representation for types in R or some other typed
interface.

Python's __[typing][]__ module provided some of the inspiration for
__typesys__. This package does not provide a way to annotate R expressions, but
could be used in combination with the __[types][]__ package to annotate code.


[RTypeInference]: https://github.com/duncantl/RTypeInference
[typing]: https://docs.python.org/3/library/typing.html
[types]: https://github.com/jimhester/types


## Installation

__typesys__ is unstable and under active development, so it's not yet available
on CRAN. To install, open an R prompt and run:

```r
install.packages("devtools")

devtools::install_github("nick-ulle/typesys")
```


