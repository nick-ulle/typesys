#' @include types.R
#' @include type_environment.R
NULL

#' @export
Substitution = function(sub = list()) {
  sub = as.list(sub)

  unique_names = unique(names(sub))
  if (length(unique_names) != length(sub))
    stop("sub must be a list with unique element names.")

  structure(sub, class = "Substitution")
}

#' Compose Substitutions
#' 
#' This function applies the second substitution to the first.
#'
#' @export
compose =
function(sub1, sub2) {
  x = structure(lapply(sub1, do_substitution, sub2), class = "Substitution")

  new = setdiff(names(sub2), names(sub1))
  x[new] = sub2[new]

  x
}

#' Apply a Substitution
#'
#' @export
setGeneric("do_substitution",
function(exp, sub) standardGeneric("do_substitution")
)

#' @export
setMethod("do_substitution", "TypeEnvironment",
function(exp, sub) {
  exp$objects = lapply(exp$objects, do_substitution, sub)
  exp
})

#' @export
setMethod("do_substitution", "typesys::FunctionType",
function(exp, sub) {
  exp@args = lapply(exp@args, do_substitution, sub)
  exp@return_type = do_substitution(exp@return_type, sub)

  exp
})

#' @export
setMethod("do_substitution", "typesys::RecordType",
function(exp, sub) {
  exp@fields = lapply(exp@fields, do_substitution, sub)

  exp
})

#' @export
setMethod("do_substitution", "typesys::Join",
function(exp, sub) {
  exp@args = lapply(exp@args, do_substitution, sub)

  simplify(exp)
})

#' @export
setMethod("do_substitution", "typesys::TypeVariable",
function(exp, sub) {
  index = match(exp@name, names(sub))
  if (is.na(index))
    exp
  else
    sub[[index]]
})

#' @export
setMethod("do_substitution", "typesys::AtomicType",
function(exp, sub) exp
)
