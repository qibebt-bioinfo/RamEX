#' Cut spectral data based on cutoff values
#'
#' This function cuts the spectral data based on cutoff values for the wavenumber range.
#'
#' @param single_spec A data frame containing the spectral data.
#' @param cutoff A numeric vector with two values representing the lower and upper bound cutoff values.
#'
#' @return The subset of spectral data within the specified cutoff range.
#'
#' @import data.table

cut.spec <- function(single_spec, cutoff) {
  return(single_spec[single_spec$V1 < cutoff[2] & single_spec$V1 > cutoff[1]])
}

read.single <- function(cell_path,xout){
  spec <- fread(cell_path, header = FALSE, sep = "\t")
  wave <- spec$V1
  inten <- spec$V2
  fn  <- stats::splinefun(wave, inten, "natural")
  return(fn(xout))
}

#' Read spectral data from files
#'
#' This function reads spectral data from files and returns a Ramanome object.
#'
#' @param data_path The path to the directory containing the spectral files.
#' @param group.index The index of the group information in the file names. Default is 1.
#' @param group.levels The levels of the group variable. Default is NULL.
#' @param cutoff A numeric vector with two values representing the lower and upper bound cutoff values for the wavenumber range. Default is c(500, 3150).
#' @param interpolation logical, should the wavelength axis be interpolated?
#'
#' @return A Ramanome object containing the spectral data, wavenumber information, and meta data.
#'
#' @import data.table
#' @import rlist
#' @import stringr
#' @export
read.spec <- function (data_path, group.index = 1, group.levels = NULL, cutoff = c(500,3150), interpolation = FALSE){
  filenames <- as.matrix(list.files(data_path, pattern = "*.txt", full.names = TRUE,
                                    include.dirs = T, recursive = T))
  if(interpolation){
    wavenumber <- seq(cutoff[1],cutoff[2])
    data_mat <- t(apply(filenames, 1, function(x)read.single(x,wavenumber)))
  } else{
    wavenumber <- cut.spec(fread(filenames[1], header = FALSE, sep = "\t"), cutoff)$V1
    data_mat <- t(apply(filenames, 1, function(x)cut.spec(fread(x, header = FALSE, sep = "\t"), cutoff)$V2))
  }
  colnames(data_mat) <- wavenumber
  group <- stringr::str_split(basename(filenames), pattern = "_",
                              simplify = T)[, group.index]
  if (is.null(group.levels)) {
    group.levels <- unique(group)
  }
  group <- factor(group, levels = group.levels)
  meta.data <- data.frame(group = group, filenames = basename(filenames))
  Ramanome <- new("Ramanome", datasets = list(raw.data = data_mat),
                  wavenumber = wavenumber, meta.data = meta.data)
  show(Ramanome)
  return(Ramanome)
}