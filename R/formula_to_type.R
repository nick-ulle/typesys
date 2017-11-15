#' Convert a Formula to a Type Expression
#'
#' This function makes it possible to write type expressions concisely using a
#' formula-based language.
#'
#' Literal types have the same names and capitalization as their S4 classes,
#' but with \code{typesys::} and \code{Type} excluded. So
#' \code{typesys::IntegerType} is just written as \code{Integer}. Type
#' variables must be written in lower case.
#'
#' @param x (formula) The formula to convert to a type expression.
#'
#' @export
formula_to_type = function(x) {
  UseMethod("formula_to_type")
}


#' @export
formula_to_type.formula = function(x) {
  args = formula_to_type(x[[2]])
  return_type = formula_to_type(x[[3]])
  if (!is(return_type, "typesys::Type"))
    stop("Invalid return type.")

  FunctionType(args, return_type)
}


#' @export
formula_to_type.name = function(x) {
  name = as.character(x)
  if (grepl("^[a-z]", name))
    return (TypeVar(name))

  # Check if this is a literal type.
  switch(name,
    "Complex" = ComplexType(),
    "Real"    = RealType(),
    "Integer" = IntegerType(),
    "Boolean" = BooleanType(),
    stop(sprintf("Unrecognized type '%s'.", name))
  )
}


#' @export
formula_to_type.call = function(x) {
  name = as.character(x[[1]])
  if (name == "c")
    lapply(x[-1], formula_to_type)
  else if (name == "Join")
    do.call(Join, lapply(x[-1], formula_to_type))
  else
    stop(sprintf("Unrecognized type constructor '%s'.", name))
}


#' @export
`formula_to_type.typesys::Type` = function(x) x
