#' Variable Names
#'
#' Functions to get the variable names in a Term. Names can only be set on a
#' Variable.
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

# TODO: Document me.
#' @export
setMethod("names", "typesys::Substitution", function(x) names(x@map))


#' @rdname names.typesys
#' @export
setMethod("names<-", signature("typesys::Variable"),
  function(x, value) {
    x@name = value
    validObject(x)

    x
  })
