#' Pre-process Raman data by smoothing with Savitzky-Golay filtering
#'
#' This function applies Savitzky-Golay filtering to smooth the Raman data and updates the Raman object.
#'
#' @param object A Ramanome object.
#' @param m The order of the polynomial to fit. Default is 0.
#' @param p The degree of the polynomial to fit. Default is 5.
#' @param w The width of the smoothing window. Default is 11.
#' @param delta.wav The wavelength difference between adjacent data points. Default is 2.
#'
#' @return The updated Ramanome object with the smoothed Raman data.
#' @export pre.smooth
pre.smooth <- function(object, m = 0, p = 5, w = 11, delta.wav = 2) {
  pred.data <- get.nearest.dataset(object)
  pred.data <- cbind(
    pred.data[, 1:((w - 1) / 2)],
    prospectr::savitzkyGolay(pred.data, m = m, p = p, w = w, delta.wav = delta.wav),
    pred.data[, (ncol(pred.data) - (w - 1) / 2 + 1):ncol(pred.data)]
  )
  object@datasets$smooth.data <- pred.data
  return(object)
}
