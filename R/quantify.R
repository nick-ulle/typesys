#' Quantify a Type Variable
#'
#' This function quantifies all type variables in the supplied type expression
#' that aren't bound in the supplied type environment.
#'
#' @param type (Type) The type expression to quantify.
#' @param tenv (TypeEnvironment) The type environment to check for bound type
#' variables. If NULL, all type variables are quantified.
#'
#' @export
quantify = function(type, tenv = NULL) {
  candidates = typesys::collect_variables(type)
  if (is.null(tenv))
    type@quantified = candidates
  else
    type@quantified = candidates[!(candidates %in% tenv)]

  type
}
