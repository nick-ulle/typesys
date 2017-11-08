#' @include types.R
NULL

#' @export
setGeneric("collect_type_vars",
  function(type) standardGeneric("collect_type_vars")
)

#' @export
setMethod("collect_type_vars", "typesys::FunctionType",
  function(type) {
    arg_types = lapply(type@args, collect_type_vars)
    arg_types = unlist(arg_types)

    union(arg_types, collect_type_vars(type@return_type))
})

#' @export
setMethod("collect_type_vars", "typesys::TypeVar",
  function(type) type@name
)

#' @export
setMethod("collect_type_vars", "typesys::AtomicType",
  function(type) character(0)
)

