#' Instantiate Variables in a Term or Constraint
#'
#' This function instantiates variables in the given term or constraint,
#' replacing them with new variables.
#'
#' @param term Term or constraint to instantiate.
#' @param ignore List of Variables to ignore when replacing.
#' @param counter Counter for generating new variables.
#' @param helper Helper object for the function.
#'
#' @export
setGeneric("instantiate",
  function(term, ignore = list(), counter = NULL,
    helper = InstantiateHelper$new(counter))
    standardGeneric("instantiate")
)


#' @export
setMethod("instantiate", signature("typesys::ImplicitInstance"),
  function(term, ignore = term@monomorphic, counter = NULL,
    helper = InstantiateHelper$new(counter))
  {
    term@t2 = instantiate(term@t2, ignore, helper = helper)

    term
  })

#' @export
setMethod("instantiate", signature("typesys::Variable"),
  function(term, ignore = list(), counter = NULL,
    helper = InstantiateHelper$new(counter))
  {
    # Check if the variable is in the monomorphic set.
    for (x in ignore)
      if (x@name == term@name)
        return (term)

    # If it isn't, create a fresh type variable or use an existing substitution.
    tvar = helper$sub[[term]]
    if (is.null(tvar)) {
      tvar = new_variable(helper)
      helper$sub[[term]] = tvar
    }

    tvar
  })


#' @export
setMethod("instantiate", signature("typesys::Composite"),
  function(term, ignore = list(), counter = NULL,
    helper = InstantiateHelper$new(counter))
  {
    term@components = lapply(term@components, instantiate, ignore = ignore,
      helper = helper)
    term
  })


#' @export
setMethod("instantiate", signature("typesys::Constant"),
  function(term, ignore = list(), counter = NULL,
    helper = InstantiateHelper$new(counter))
    term
)

# Helpers ----------------------------------------
InstantiateHelper = R6::R6Class("InstantiateHelper",
  "public" = list(
    counter = NULL,
    sub = NULL,

    initialize = function(counter = NULL, sub = Substitution()) {
      if (is.null(counter)) {
        stopifnot(require("rstatic"))
        counter = rstatic::Counter$new()
      }

      self$counter = counter
      self$sub = sub
    }
  )
)

new_variable = function(helper, prefix = "t", ...) {
  name = rstatic::next_name(helper$counter, name = prefix, ...)
  Variable(name)
}
