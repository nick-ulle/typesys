# Methods to compare terms.
#

#' @include terms.R
NULL

setMethod("!=", c("typesys::Term", "ANY"), function(e1, e2) TRUE)
setMethod("!=", c("ANY", "typesys::Term"), function(e1, e2) TRUE)
setMethod("!=", c("typesys::Term", "typesys::Term"),
  function(e1, e2) !(e1 == e2))

setMethod("==", c("typesys::Term", "ANY"), function(e1, e2) FALSE)
setMethod("==", c("ANY", "typesys::Term"), function(e1, e2) FALSE)
setMethod("==", c("typesys::Term", "typesys::Term"), function(e1, e2) FALSE)

setMethod("==", c("typesys::Variable", "typesys::Variable"),
  function(e1, e2) {
    identical(e1, e2)
  })

setMethod("==", c("typesys::Function", "typesys::Function"),
  function(e1, e2) {
    identical(e1, e2)
  })

setMethod("==", c("typesys::Constant", "typesys::Constant"),
  function(e1, e2) {
    identical(e1, e2)
  })
