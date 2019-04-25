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
#' @param quantify (logical) Quantify the type variables?
#'
#' @export
formula_to_type = function(x, quantify = FALSE) UseMethod("formula_to_type")


#' @export
formula_to_type.formula = function(x, quantify = FALSE) {
  if (length(x) == 2) {
    type = formula_to_type(x[[2]])

  } else if (length(x) == 3) {
    args = formula_to_type(x[[2]])
    return_type = formula_to_type(x[[3]])
    if (!is(return_type, "typesys::Type"))
      stop("Invalid return type.")

    type = FunctionType(args, return_type)
  }

  if (quantify) quantify(type)
  else type
}


#' @export
formula_to_type.name = function(x, quantify = FALSE) {
  name = as.character(x)
  if (grepl("^[a-z]", name))
    return (TypeVariable(name))

  # Check if this is a literal type.
  switch(name
    , "Null"        = NullType()
    , "Environment" = EnvironmentType()
    # Use "Char" instead of "Character" for the scalar string type, to avoid
    # mixups with R's "character" vectors (which are "String" here).
    , "Char"        = CharacterType()
    , "Logical"     = LogicalType()
    , "Integer"     = IntegerType()
    , "Numeric"     = NumericType()
    , "Complex"     = ComplexType()
    , "String"      = StringType()
    , "ExternalPtr" = ExternalPtrType()
    , "Raw"         = RawType()
    , "Character"   =
      stop("Unrecognized type 'Character'. Did you mean 'String'?")
    , stop(sprintf("Unrecognized type '%s'.", name))
  )
}


#' @export
formula_to_type.call = function(x, quantify = FALSE) {
  name = as.character(x[[1]])
  switch(name
    , "c"      = lapply(x[-1], formula_to_type)
    , "Join"   = do.call(Join, lapply(x[-1], formula_to_type))
    , "Array"  = ArrayType(formula_to_type(x[[2]]))
    , "Vector" = VectorType(formula_to_type(x[[2]]))
    , stop(sprintf("Unrecognized type constructor '%s'.", name))
  )
}


#' @export
`formula_to_type.typesys::Type` = function(x, quantify = FALSE) x
