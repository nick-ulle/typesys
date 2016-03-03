# Description:
#   Deprecated conditional type for RTypeInference compatibility.

#' Infer Type
#'
#' Attempt to infer the type of a ConditionalType.
#'
#' @export
setGeneric("infer", function(self, args) {
  standardGeneric("infer")
})

setClass("ConditionalType", contains = "Type",
  slots = list(
    handler = "function"
  )
)

#' Conditional Type
#'
#' A type dependent on a condition. See UnionType.
#'
#' @details ConditionalType uses LIFO condition checking, so conditions should
#' be mutually exclusive or added in increasing order of granularity.
#' Internally, conditions are stored as an if-else statement.
#'
#' @export
ConditionalType = function(handler = function(args) {})
  # Construct a ConditionalType.
{
  # TODO: S4 validation.

  # Check that handler includes a condition list.
  if ( !("args" %in% names(formals(handler))) )
    stop("Handler must have parameter 'args'.")

  if ( body(handler)[[1]] != "{" )
    body(handler) = call("{", body(handler))

  new("ConditionalType", handler = handler)
}

#' Push Quoted Condition
#'
#' Push a quoted condition onto the stack of conditions in a ConditionalType.
#'
pushCondition_q = function(self, condition, if_type, else_type)
  # Push a new condition onto the ConditionalType.
{
  body = body(self@handler)

  # Conditions are stored in the final if-else statement.
  length = length(body)
  if_statement = body[[length]]

  if (class(if_statement) == "if") {
    # Push branch onto existing if-else statement.
    if_statement = call("if", condition, if_type, if_statement)
    body[length] = list(if_statement)

  } else if (!missing(else_type)) {
    # No if-else statement, so create one.
    if_statement = call("if", condition, if_type, else_type)
    body[length + 1] = list(if_statement)

  } else {
    stop("There was no existing else_type, so one must be supplied.")
  }

  body(self@handler) = body
  return(self)
}


#' Push Condition
#'
#' Push a condition onto the stack of conditions in a ConditionalType.
#'
#' @export
pushCondition = function(self, condition, if_type, else_type)
{
  pushCondition_q(self, substitute(condition), substitute(if_type),
    substitute(else_type))
}


#' Get Branch Types
#'
#' Get the name of the type on each branch of a ConditionalType.
#'
#' @export
getBranchTypes = function(self)
{
  body = body(self@handler)

  length = length(body)
  if_statement = body[[length]]

  if_apply(if_statement, class)
}


#' If Apply
#'
#' Map over the branches of an if-else statement.
#'
if_apply = function(statement, f, simplify = TRUE)
{
  # if <condition> <body> <else>
  answer = list( f(statement[[3]]) )

  if (length(statement) > 3) {
    answer = append(answer,
      if (class(statement[[4]]) == "if")
        if_apply(statement[[4]], f)
      else
        f(statement[[4]])
    )
  }

  if (simplify)
    simplify2array(answer)
  else answer
}


#' @export
setMethod("infer",
  list(self = "ConditionalType"),
  function(self, args)
  {
    self@handler(args)
  }
)
