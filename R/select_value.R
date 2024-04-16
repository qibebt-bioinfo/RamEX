#' Get the nearest dataset from a Ramanome object
#'
#' This function retrieves the nearest dataset from a Ramanome object.
#'
#' @param object A Ramanome object.
#'
#' @return The nearest dataset.
#' @export
get.nearest.dataset <- function(object) {
  dataset <- tail(names(object@datasets), 1)
  dataset <- object@datasets[dataset][[1]]
  return(dataset)
}

#' Select a single Raman intensity value for a given wavenumber
#'
#' This function selects a single Raman intensity value for a given wavenumber from a Ramanome object.
#'
#' @param object A Ramanome object.
#' @param wave The wavenumber for which to retrieve the intensity value.
#'
#' @return The Raman intensity value for the given wavenumber.
select.value <- function(object, wave) {
  loc <- which.min(abs(object@wavenumber - wave))
  dataset <- get.nearest.dataset(object)
  return(dataset[, loc])
}

#' Select a range of Raman intensity values for a given wavenumber range
#'
#' This function selects a range of Raman intensity values for a given wavenumber range from a Ramanome object.
#'
#' @param object A Ramanome object.
#' @param waves A vector with two values representing the lower and upper bounds of the wavenumber range.
#'
#' @return The Raman intensity values within the given wavenumber range.
select.band <- function(object, waves) {
  locs <- object@wavenumber <= waves[2] & object@wavenumber >= waves[1]
  dataset <- get.nearest.dataset(object)
  return(rowSums(dataset[, locs]))
}

#' Confirm the selected wavenumber or Raman band
#'
#' This function confirms the selected wavenumber or Raman band and returns the corresponding name.
#'
#' @param waves A vector with one or two values representing the wavenumber or wavenumber range.
#'

confirm.name <- function(waves) {
  if (length(waves) == 1) {
    name <- as.character(waves)
  } else if (length(waves) == 2) {
    name <- paste(waves[1], waves[2], sep = '~')
  } else {
    stop('Error! Please input a wavenumber or a Raman band!')
  }
  return(name)
}

#' Confirm the selected wavenumber or Raman band
#'
#' This function confirms the selected wavenumber or Raman band and returns the corresponding name.
#' @param object A Ramanome object.
#' @param waves The selected waves.
#'

confirm.select <- function(object, waves) {
  if (length(waves) == 1) {
    values <- select.value(object, waves)
  } else if (length(waves) == 2) {
    values <- select.band(object, waves)
  }else { print('Error! Please input a wavenumber or a Raman band!') }
  return(values)
}


#' Calculate Raman intensities for selected wavenumbers or Raman bands
#'
#' This function calculates the Raman intensities for the selected wavenumbers or Raman bands and updates the Ramanome object.
#'
#' @param object A Ramanome object.
#' @param wavenumber A list of wavenumbers or wavenumber ranges.
#'
#' @return The updated Ramanome object.
#' @export
#' @examples
#' data(Ramanome)
#' raman <- intensity(Ramanome, wavenumber = list(600, c(800, 1000)))
intensity <- function(object, wavenumber) {
  wavenumber <- as.list(wavenumber)
  a <- lapply(wavenumber, function(x) confirm.select(object, x))
  name <- lapply(wavenumber, confirm.name)
  object@interested.bands <- append(object@interested.bands, a)
  names(object@interested.bands) <- c(names(object@interested.bands), unlist(name))
  return(object)
}
