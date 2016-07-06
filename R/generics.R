# Description:
#   S4 generics for type classes.

#' Convert a Type to a Character String
#'
#' This function converts a Type to a character string.
#'
#' @export
setGeneric("to_string",
  function(x, ...) standardGeneric("to_string"),
  valueClass = "character"
)


#' Test Types for Equality
#'
#' Test whether two types are the same, including any contained types.
#'
#' @export
setGeneric("same_type",
  function(x, y) standardGeneric("same_type"),
  valueClass = "logical"
)


# FIXME: How is RTypeInference using `types()` and does it really need an
# method to extract just one type?
#' Extract Contained Type
#' 
#' For an atomic type or a container type with homogeneous elements, return the
#' underlying element type. For a continer type with heterogeneous elements,
#' throw an error.
#'
#' @export
setGeneric("element_type",
  function(self) standardGeneric("element_type"),
  valueClass = "Type"
)


#' Extract All Contained Types
#'
#' For any type, return a list of the underlying element types.
#'
#' @export
setGeneric("element_type_all",
  function(self) standardGeneric("element_type_all"),
  valueClass = "list"
)


#' Assign Contained Types
#'
#' For a container type, set the underlying element type.
#'
#' @export
setGeneric("element_type<-",
  function(self, value) standardGeneric("element_type<-"),
  valueClass = "Type"
)

