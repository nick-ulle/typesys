
#' @include substitution.R
NULL


#' Get Variable Names
#'
#' Methods to get the names of variables in a Term.
#'
#' Names can only be set on a Variable object.
#'
#' @param x A Term object.
#' @return A character vector of variable names.
#'
#' @rdname names.typesys
#' @export
setMethod("names", signature("typesys::Variable"), function(x) x@name)


#' @rdname names.typesys
#' @export
setMethod("names", signature("typesys::Constant"), function(x) character(0))


#' @rdname names.typesys
#' @export
setMethod("names", signature("typesys::Composite"),
  function(x) {
    unlist(lapply(x@components, names), FALSE, FALSE)
  })


#' @rdname names.typesys
#' @export
setMethod("names<-", signature("typesys::Variable"),
  function(x, value) {
    x@name = value
    validObject(x)

    x
  })


# TODO: Document me.
#' @export
setMethod("names", "typesys::Substitution", function(x) names(x@map))
