#' Yocto-Spectral to spectra
#'
#' Convert a data frame with data imported with \code{read_yocto_spectral_csv()}
#' into a colelction of \code{raw_spct} objects-
#'
#' @param df data.frame With only one of average, minimum or maximum sensor
#'   counts for the channels.
#' @param channels character vector of length one or longer. In addition to a
#'   vector of spectral sensor channel names as used in the AS7343
#'   documentation, the nicknames \code{"all"}, \code{"wide"}, \code{"narrow"}
#'   and \code{"xyz"} as vectors of length one are accepted.
#'
#' @export
#'
yocto_spectral2mspct <- function(df, channels = "all") {

  if (is.null(channels) || channels == "all") {
    channels <- names(AS7343_metadata())
  } else if (tolower(channels) == "xyz") {
    channels <- c("FZ", "FY", "FXL")
  } else if (channels == "wide") {
    channels <- c("VIS", "F8", "NIR")
  } else if (channels == "narrow") {
    channels <- paste("F", 1:8, sep = "")
  }

  channels <- intersect(channels, names(AS7343_metadata()))
  stopifnot("Invalid or missing 'channel' names!" = length(channels) >= 1L)

  # sort channels by w.length before pivoting
  channels <- channels[order(AS7343_metadata()[channels])]
  # make sure colnames are bare channel names
  colnames(df) <- gsub("\\.avg$", "", colnames(df))
  # subset columns before pivoting
  df <- df[ , c("time", channels)]
  # convert to factor before pivoting
  df[["time"]] <- factor(as.character(df[["time"]]))

  message("Creating a 'source_mspct' containing ", length(levels(df[["time"]])),
          " 'source_spct' objects.")

  z <- tidyr::pivot_longer(df,
                           cols = grep("time", colnames(df), invert = TRUE),
                           names_to = "channel",
                           values_to = "counts")
  z[["w.length"]] <- AS7343_metadata()[z[["channel"]]]

  # this is currently rather slow!
  photobiology::subset2mspct(z, member.class = "raw_spct", idx.var = "time")
}
