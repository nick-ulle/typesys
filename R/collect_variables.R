#' @include types.R
NULL

#' @export
setGeneric("collect_variables",
function(type) standardGeneric("collect_variables")
)

#' @export
setMethod("collect_variables", "typesys::FunctionType",
function(type) {
  arg_types = lapply(type@args, collect_variables)
  arg_types = unlist(arg_types)

  union(arg_types, collect_variables(type@return_type))
})

#' @export
setMethod("collect_variables", "typesys::TypeVar",
function(type) type@name
)

#' @export
setMethod("collect_variables", "typesys::AtomicType",
function(type) character(0)
)

#' @export
setMethod("collect_variables", "typesys::Join",
function(type) {
  arg_types = lapply(type@args, collect_variables)
  unlist(arg_types)
})
