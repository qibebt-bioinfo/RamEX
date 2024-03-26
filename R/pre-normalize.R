#' Pre-process Raman data by normalization
#'
#' This function applies normalization to the Raman data based on the specified method and updates the Raman object.
#'
#' @param object A Ramanome object.
#' @param method The normalization method to use: "CH", "max", "specific", or "area".
#' @param wave The specific wavenumber to use for the "specific" normalization method.
#'
#' @return The updated Ramanome object with the normalized Raman data.
#'
#' @export pre.normalize 
pre.normalize <- function(object, method, wave = NULL) {
  dataset <- get.nearest.dataset(object)
  if (method == 'CH') {
    range <- object@wavenumber > 2850 & object@wavenumber < 3000
    value <- apply(dataset[, range], 1, max)
  } else if (method == 'max') {
    value <- apply(dataset, 1, max)
  } else if (method == 'specific') {
    if (is.null(wave)) {
      stop('Error! Please input the interested wavenumber!')
    }
    loc <- which.min(abs(object@wavenumber - wave))
    value <- dataset[, loc]
  } else if (method == 'area') {
    value <- rowSums(dataset)
  }
  object@datasets$normalized.data <- dataset / value
  return(object)
}
