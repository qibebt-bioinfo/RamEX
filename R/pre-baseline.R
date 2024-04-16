#' Preprocess baseline
#'
#' This function performs baseline pre-processing on the input object.
#'
#' @param object The input object
#' @param order The order of the polynomial used in the baseline calculation (default: 1)
#'
#' @return The modified object with preprocessed baseline data
#'
#' @importFrom hyperSpec spc.fit.poly.below
#' @importFrom hyperSpec spc.fit.poly
#' @export pre.baseline.polyfit
pre.baseline.polyfit <- function(object, order = 1) {
  wavenumber <- object@wavenumber
  spc2hs <- new("hyperSpec", spc = get.nearest.dataset(object), wavelength = wavenumber)

  data_hyperSpec <- spc2hs
  wave_max <- max(data_hyperSpec@wavelength)
  wave_min <- min(data_hyperSpec@wavelength)

  hyperspec <- data_hyperSpec

  if (wave_max > 3050 & wave_min < 600) {
    hyperspec_1 <- hyperspec[, , floor(wave_min) ~ 1790] - hyperSpec::spc.fit.poly.below(
      hyperspec[, , floor(wave_min) ~ 1790],
      hyperspec[, , floor(wave_min) ~ 1790],
      poly.order = 1
    )
    hyperspec_2 <- hyperspec[, , 1790 ~ floor(wave_max)] - hyperSpec::spc.fit.poly(
      hyperspec[, , c(1790 ~ 2065, 2300 ~ 2633, 2783, floor(wave_max))],
      hyperspec[, , 1790 ~ floor(wave_max)],
      poly.order = 6
    )
    hyperspec_baseline <- cbind(hyperspec_1, hyperspec_2)
    print(paste0("The output data contains ", length(hyperspec_baseline), " spectra"))
  }
  else if (wave_max > 1750 & wave_min < 600) {
    hyperspec_baseline <- hyperspec[, , 550 ~ 1750] - hyperSpec::spc.fit.poly.below(
      hyperspec[, , 550 ~ 1750],
      hyperspec[, , 550 ~ 1750],
      poly.order = order
    )
    print(paste0("The output data contains ", length(hyperspec_baseline), " spectra"))
  }
  else if (wave_max > 3050 & wave_min < 1800 & wave_min > 600) {
    hyperspec_baseline <- hyperspec[, , 1800 ~ 3050] - hyperSpec::spc.fit.poly(
      hyperspec[, , c(1800 ~ 2065, 2300 ~ 2633, 2783, 3050)],
      hyperspec[, , 1800 ~ 3050],
      poly.order = order
    )
    print(paste0("The output data contains ", length(hyperspec_baseline), " spectra"))
  }
  else if (wave_max < 1750 & wave_min > 600) {
    print("The spc is too small to baseline")
  }
  else {
    print("Your spc data is invalid!")
  }

  data_hyperSpec_baseline <- hyperspec_baseline
  data <- data_hyperSpec_baseline$spc[, !duplicated(colnames(data_hyperSpec_baseline$spc))]
  object@datasets$baseline.data <- data
  object@wavenumber <- as.numeric(colnames(data))

  return(object)
}

polyfit <- function (spectra, t, degree = 4, tol = 0.001, rep = 100)
{
  dimnames(spectra) <- NULL
  np <- dim(spectra)
  baseline <- matrix(0, np[1], np[2])
  if (missing(t) || (t == FALSE))
    t <- 1:np[2]
  polx <- cbind(1/sqrt(np[2]), stats::poly(t, degree = degree))
  for (i in 1:np[1]) {
    ywork <- yold <- yorig <- spectra[i, ]
    nrep <- 0
    repeat {
      nrep <- nrep + 1
      ypred <- polx %*% crossprod(polx, yold)
      ywork <- pmin(yorig, ypred)
      crit <- sum(abs((ywork - yold)/yold), na.rm = TRUE)
      if (crit < tol || nrep > rep)
        break
      yold <- ywork
    }
    baseline[i, ] <- ypred
  }
  list(baseline = baseline, corrected = spectra - baseline)
}


grow_bubble <- function(spectrum, alignment = "center") {
  xaxis <- 0:(length(spectrum)-1)
  # Adjusting bubble parameter based on alignment
  if (alignment == "left") {
    # half bubble right
    width <- 2 * length(spectrum)
    middle <- 1
  } else if (alignment == "right") {
    # half bubble left
    width <- 2 * length(spectrum)
    middle <- length(spectrum)
  } else {
    # Centered bubble
    width <- length(spectrum)
    middle <- length(spectrum) / 2
  }

  squared_arc <- (width / 2) ^ 2 - (xaxis - middle) ^ 2  # squared half circle
  # squared_arc[squared_arc < 0] <- 0
  bubble <- sqrt(squared_arc) - width
  # find new intersection
  touching_point <- which.min(spectrum - bubble)

  # grow bubble until touching
  bubble <- bubble + min(spectrum - bubble)

  return(list(bubble=bubble, relative_touching_point=touching_point))
}

keep_largest <- function(baseline, bubble) {
  for (i in 1:length(baseline)) {
    if (baseline[i] < bubble[i]) {
      baseline[i] <- bubble[i]
    }
  }
  return(baseline)
}

bubbleloop <- function(spectrum, baseline, min_bubble_widths) {
  range_cue <- list(c(1, length(spectrum)))
  i <- 1
  while (i <= length(range_cue)) {
    left_bound <- range_cue[[i]][1]
    right_bound <- range_cue[[i]][2]

    i <- i + 1

    if (right_bound == left_bound) {
      next
    }

    if (is.numeric(min_bubble_widths)) {
      min_bubble_width <- min_bubble_widths
    } else {
      min_bubble_width <- min_bubble_widths[(left_bound + right_bound) %/% 2]
    }
    if (left_bound == 1 & right_bound != length(spectrum)) {
      alignment <- "left"
    } else if (left_bound != 1 & right_bound == length(spectrum)) {
      alignment <- "right"
    } else {
      if ((right_bound - left_bound) < min_bubble_width) {
        next
      }
      alignment <- "center"
    }
    bubble <- grow_bubble(spectrum[left_bound:right_bound], alignment)$bubble
    relative_touching_point <- grow_bubble(spectrum[(left_bound:right_bound)], alignment)$relative_touching_point
    touching_point <- relative_touching_point + left_bound -1
    baseline[left_bound:right_bound] <- keep_largest(baseline[left_bound:right_bound], bubble)
    if (touching_point == left_bound) {
      range_cue <- c(range_cue, list(c(touching_point+1, right_bound)))
    } else if (touching_point == right_bound) {
      range_cue <- c(range_cue, list(c(left_bound, touching_point-1)))
    } else {
      range_cue <- c(range_cue, list(c(left_bound, touching_point - 1)), list(c(touching_point, right_bound)))
    }
  }
  return(baseline)
}

bubblefill <- function(spectrum, min_bubble_widths = 200, fit_order = 1) {
  xaxis <- as.numeric(colnames(spectrum))
  band_1 <- which(xaxis<1800)
  spectrum_ <- spectrum
  # smin <- min(spectrum_)
  # spectrum_ <- spectrum_ - smin
  # scale <- max(spectrum_) / length(spectrum)
  # spectrum_ <- spectrum_ / scale
  baseline <- rep(0, length(spectrum))
  baseline <- bubbleloop(spectrum_, baseline, min_bubble_widths)
  # baseline <- baseline * scale + poly_fit + smin
  if (!is.numeric(min_bubble_widths)) {
    filter_width <- max(pmin(min_bubble_widths))
  } else {
    filter_width <- max(min_bubble_widths)
  }
  # baseline <- prospectr::savitzkyGolay(baseline, m = 0, p = 3, w = round(2 * (filter_width %/% 4) + 3), delta.wav = 0)
  raman <- spectrum - baseline
  return(raman)
}

#' Remove baseline by bubble
#'
#' This function performs baseline pre-processing on the input object.
#'
#' @param object Ramanome object
#'
#' @return The baseline corrected spectra with bubble method
#' @export pre.baseline.bubble
pre.baseline.bubble <- function(object){
  band_1 <- which(object@wavenumber < 1800)
  band_2 <- which(object@wavenumber > 1800 & object@wavenumber < 2700)
  band_3 <- which(object@wavenumber > 2700)
  data_matrix <- get.nearest.dataset(object)
  data_matrix[,band_1] <- t(apply(data_matrix[,band_1], 1, bubblefill))
  data_matrix[,band_2] <- polyfit(data_matrix[,band_2], degree = 1, tol = 0.001, rep = 100)$corrected
  data_matrix[,band_3] <- polyfit(data_matrix[,band_3], degree = 1, tol = 0.001, rep = 100)$corrected
  data_matrix[, band_2] <- data_matrix[, band_2] - (data_matrix[,band_2[1]]-data_matrix[,band_2[1]-1])
  data_matrix[, band_3] <- data_matrix[, band_3] - (data_matrix[,band_3[1]]-data_matrix[,band_3[1]-1])
  object@datasets$baseline.data <- data_matrix
  return(object)
}
