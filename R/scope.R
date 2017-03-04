
#' Scope
#'
#' This R6 class represents a scope within a program. In particular, a scope
#' can have a parent scope, a list of contained names, and a return value (to
#' model functions).
#'
#' @export
Scope = function(parent = NULL) {
  scope = .Scope$new()
  scope$parent = parent

  return(scope)
}

.Scope = R6::R6Class("typesys::Scope",
  # public ----------
  public = list(
    parent = NULL,
    stop = FALSE,

    # FIXME: Merge with `set()` if possible.
    set_l = function(l, force = FALSE) {
      Map(self$set, names(l), l, force)
    },

    set = function(name, type, force = FALSE) {
      name = as.character(name)
      index = match(name, names(private$names))

      if (is.na(index))
        private$names[[name]] = type
      else if (force)
        private$names[[index]] = type
      else
        stop(sprintf("name '%s' already contains '%s'", name, format(type)))

      # FIXME:
      #else if ( force || is(private$names[[index]], "UnknownType") )
      #  variable_types[[index]] <<- type

      #else
      #  warning(
      #    sprintf("'%s' has type %s, not updating as %s.",
      #      name, class(variable_types[[index]]), class(type))
      #  )
    },

    get = function(name) {
      # FIXME:
      if (missing(name))
        return(private$names)

      name = as.character(name)
      index = match(name, names(private$names))

      if(is.na(index))
        UnknownType()
      else
        private$names[[index]]
    }

    #set_return = function(type) {
    #  private$value = type
    #},
    #
    #get_return = function() {
    #  private$value
    #}
  ),
  # private ----------
  private = list(
    names = list()
    #value = NA
  )
)

setOldClass("typesys::Scope")
setClassUnion("typesys::MaybeScope", c("typesys::Scope", "NULL"))

