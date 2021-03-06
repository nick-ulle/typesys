#' Construct R Types from Formula Notation
#'
#' This function makes it possible to write R type expressions concisely using
#' a formula-based language.
#'
#' Literal types have the same names and capitalization as their S4 classes,
#' but with \code{typesys::} excluded. So \code{typesys::RInteger} is just
#' written as \code{RInteger}. Type variables must be written in lower case.
#'
#' @param x (formula) The formula to convert to a type expression.
#'
#' @export
formula_to_type = function(x) UseMethod("formula_to_type")


#' @export
formula_to_type.formula = function(x) {
  if (length(x) == 2) {
    type = formula_to_type(x[[2]])

  } else if (length(x) == 3) {
    args = formula_to_type(x[[2]])
    return_type = formula_to_type(x[[3]])
    if (!is(return_type, "typesys::Term"))
      stop("Invalid return type.")

    type = RFunction(args, return_type = return_type)
  }

  type
}


#' @export
formula_to_type.name = function(x) {
  name = as.character(x)
  if (grepl("^[a-z]", name))
    return (Variable(name))

  # Check if this is a literal type.
  switch(name
    , "RNull"        = RNull
    , "REnvironment" = REnvironment
    # Use "Char" instead of "Character" for the scalar string type, to avoid
    # mixups with R's "character" vectors (which are "String" here).
    , "RChar"        = RChar
    , "RLogical"     = RLogical
    , "RInteger"     = RInteger
    , "RNumeric"     = RNumeric
    , "RComplex"     = RComplex
    , "RString"      = RString
    , "RExternalPtr" = RExternalPtr
    , "RRaw"         = RRaw
    , "RCharacter"   =
      stop("Unrecognized type 'RCharacter'. Did you mean 'RString'?")
    , stop(sprintf("Unrecognized type '%s'.", name))
  )
}


#' @export
formula_to_type.call = function(x) {
  name = as.character(x[[1]])
  switch(name
    , "c"      = lapply(x[-1], formula_to_type)
    #, "Join"   = do.call(Join, lapply(x[-1], formula_to_type))
    #, "Array"  = ArrayType(formula_to_type(x[[2]]))
    , "RVector" = RVector(formula_to_type(x[[2]]))
    , stop(sprintf("Unrecognized type constructor '%s'.", name))
  )
}


#' @export
`formula_to_type.typesys::Term` = function(x) x
