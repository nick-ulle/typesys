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
  args = expr_to_type(x[[2]])
  return_type = expr_to_type(x[[3]])

  FunctionType(args, return_type)
}


expr_to_type = function(expr) {
  # NOTE: formula_to_type() could be the generic instead. Do we want to expose
  # the expr_to_type() methods to users?
  UseMethod("expr_to_type")
}

#' @export
expr_to_type.name = function(expr) {
  name = as.character(expr)
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
expr_to_type.call = function(expr) {
  name = as.character(expr[[1]])
  if (name == "c")
    lapply(expr[-1], expr_to_type)
  else
    stop(sprintf("Unrecognized type constructor '%s'.", name))
}
