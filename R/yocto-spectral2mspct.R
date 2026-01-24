#' Yocto-Spectral to spectra
#'
#' Convert a data frame with data imported with \code{read_yocto_spectral_csv()}
#' into a colelction of \code{raw_spct} objects-
#'
#' @param x data.frame With only one of average, minimum or maximum sensor
#'   counts.
#'
#' @export
#'
yocto_spectral2mspct <- function(x) {

  if (is.null(channels)) {
    channels <- "all"
  } else if (tolower(channels) == "xyz") {
    channels <- c("FZ", "FY", "FXL")
  } else if (channels == "wide") {
    channels <- c("VIS", "F8", "NIR")
  } else if (channels == "narrow") {
    channels <- paste("F", 1:8, sep = "")
  }
  z <- tidyr::pivot_longer(x,
                           cols = grep("time", colnames(x), invert = TRUE),
                           names_to = "channel",
                           values_to = "counts")
  z[["channel"]] <- gsub("\\.avg$", "", z[["channel"]])
  z[["w.length"]] <- AS7343_metadata()[z[["channel"]]]
  z <- z[order(z[["w.length"]])]
  z[["time"]] <- factor(as.character(z[["time"]])) # better performance?

  photobiology::subset2mspct(z, member.class = "raw_spct", idx.var = "time")
}
