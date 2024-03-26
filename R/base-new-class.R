#' Ramanome class
#'
#' This class represents the Ramanome data.
#'
#' @slot datasets A list of datasets
#' @slot meta.data A data frame for meta data
#' @slot reductions A list of reductions
#' @slot interested.bands A list of interested bands
#'
#' @importFrom hyperSpec nrow
#' @importFrom methods setClass
#' @export
#' @return 
Ramanome <- setClass(
  Class = 'Ramanome',
  slots = c(
    datasets = 'list',
    meta.data = 'data.frame',
    reductions = 'list',
    interested.bands = 'list'
  )
)

#' Show method for Ramanome class
#'
#' This method shows information about the Ramanome object.
#'
#' @param object The Ramanome object
#'
#' @importFrom methods setMethod
#' @export
#' @return 
setMethod(
  f = "show",
  signature = "Ramanome",
  definition = function(object) {
    num.spec <- hyperSpec::nrow(object@datasets$raw.data)
    num.wave <- length(object@wavenumber)
    num.group <- length(unique(object@meta.data$group))
    cat(class(x = object), "Object", "\n")
    cat(
      num.wave,
      'Raman features across',
      num.spec,
      'samples within',
      num.group,
      ifelse(test = num.group == 1, yes = 'group', no = 'groups'),
      "\n"
    )
    cat('\n')
  }
)
