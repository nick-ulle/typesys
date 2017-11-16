#' Simplify a Join
#'
#' This function simplifies a join so that it does not contain any nested joins
#' and each element is unique.
#'
#' @param join (Join) The join to simplify.
#'
#' @export
simplify = function(join) {
  args = flatten_join(join)
  args = unique(args)

  if (length(args) == 1) {
    args[[1]]
  } else {
    join@args = args
    join
  }
}


# This function flattens a join (possibly nested) into a list.
setGeneric("flatten_join",
function(arg) standardGeneric("flatten_join")
)

#' @export
setMethod("flatten_join", "typesys::Join",
function(arg) {
  unlist(lapply(arg@args, flatten_join), recursive = FALSE)
})

#' @export
setMethod("flatten_join", "typesys::Type",
function(arg) {
  arg
})
