#' @include types.R
NULL

#' @export
setGeneric("compute",
  function(x) standardGeneric("compute")
)

#' @export
setMethod("compute", signature(x = "typesys::Join"),
  function(x) {
    # TODO: This should also work for vectors.
    # TODO: Only search x@args once for max type.
    if (any_is(x@args, "typesys::TypeVariable"))
      x
    else if (any_is(x@args, "typesys::ComplexType"))
      typesys::ComplexType()
    else if (any_is(x@args, "typesys::NumericType"))
      typesys::NumericType()
    else if (any_is(x@args, "typesys::IntegerType"))
      typesys::IntegerType()
    else if (any_is(x@args, "typesys::LogicalType"))
      typesys::LogicalType()
  }
)

any_is =
function(objects, class_name)
  # Test inheritance for multiple objects.
{
  any(vapply(objects, is, TRUE, class_name))
}
