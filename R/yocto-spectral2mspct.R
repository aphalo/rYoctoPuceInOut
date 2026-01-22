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
yocto_spectral2mspct <- function(x, channels = "all") {

  if (is.null(channels)) {
    channels <- "all"
  } else if (tolower(channels) == "xyz") {
    channels <- c("FZ", "FY", "FXL")
  } else if (channels == "wide") {
    channels <- c("VIS", "F8", "NIR")
  } else if (channels == "narrow") {
    channels <- paste("F", 1:8, sep = "")
  }
  tidyr::pivot_longer(x,
                      cols = !contains("time"),
                      names_to = "channel",
                      values_to = "counts") |>
    dplyr::select(-UNIX.time, -Local.time) |>
    dplyr::mutate(
      channel = gsub("\\.avg$", "", channel),
      w.length = AS7343_metadata()[channel]
    ) |>
    dplyr::arrange(time, w.length) -> z

  if (! "all" %in% channels) {
    z <- dplyr::filter(z, channel %in% channels)
  }

  photobiology::subset2mspct(z, member.class = "raw_spct", idx.var = "time")
}
