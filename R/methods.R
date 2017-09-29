#' @include types.R
NULL

#' @export
setMethod("same_type", signature(x = "typesys::Type"),
  function(x, y) is(x, class(y))
)


#' @export
setMethod("same_type", signature(x = "typesys::CompositeType"),
  function(x, y) {
    same_class = is(x, class(y))
    if (!same_class)
      return(FALSE)

    types_x = element_type_all(x)
    types_y = element_type_all(y)
    if (length(types_x) != length(types_y))
      return(FALSE)

    all(mapply(same_type, types_x, types_y))
  }
)


#' @export
setMethod("element_type", signature(self = "typesys::Type"),
  function(self) self
)


#' @export
setMethod("element_type", signature(self = "typesys::CompositeType"),
  function(self) stop("composite types may contain multiple element types.")
)
    

#' @export
setMethod("element_type_all", signature(self = "typesys::Type"),
  function(self) list(self)
)


#' @export
setMethod("element_type_all", signature(self = "typesys::CompositeType"),
  function(self) self@types
)


#' @export
setMethod("length", signature(x = "typesys::Type"),
  function(x) 1L
)


#' @export
setMethod("length", signature(x = "typesys::CompositeType"),
  function(x) length(x@types)
)


# Functions
# =========

#' Add Context to Type
#'
#' @export
add_context = function(object, context) {
  object@contexts = c(object@contexts, context)
  validObject(object)
  return(object)
}


#' Check If Type Has Context
#'
#' @export
has_context = function(object, context) {
  context %in% object@contexts
}


#' @export
setMethod("element_type", signature(self = "typesys::ArrayType"),
  function(self) self@types[[1]]
)


#' @export
setMethod("element_type<-", signature(self = "typesys::ArrayType"),
  function(self, value) {
    self@types = list(value)
    validObject(self)
    return(self)
  }
)


#' @export
setMethod("length", signature(x = "typesys::ArrayType"),
  function(x) {
    Reduce(function(a, b) {
      if (is.numeric(b)) b * a
      else NA
    }, x@dimension)
  }
)


#' @export
setMethod("dim", signature(x = "typesys::ArrayType"),
  function(x) x@dimension
)
