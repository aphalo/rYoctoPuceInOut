#' Yocto-Spectral to spectra
#'
#' Convert a data frame with data imported with \code{read_yocto_spctlog()}
#' into a collection of \code{raw_spct} objects.
#'
#' @param df data.frame With only one of average, minimum or maximum sensor
#'   counts for each spectral channels.
#' @param channels character vector of length one or longer. In addition to a
#'   vector of spectral sensor channel names as used in the AS7343
#'   documentation, the nicknames \code{"all"}, \code{"wide"}, \code{"narrow"}
#'   and \code{"xyz"} as vectors of length one are accepted.
#' @inheritDotParams photobiology::subset2mspct drop.idx ncol byrow
#'
#' @details
#' The data frame, possibly after subsetting data columns, is pivoted into
#' long form, and subsequently split by time stamp.
#' The readings at each time point are converted into _short_ spectra with at
#' most 13 wavelength values, one from each channel.
#'
#' @note Currently, function \code{\link[photobiology]{subset2mspct}()} used
#'   in the conversion into a `raw_mspct` object can be slow in the case of
#'   10000's of spectra.
#'
#' @seealso \code{\link[photobiology]{subset2mspct}()},
#'   \code{\link[photobiology:source_spct]{raw_spct}()}.
#'
#' @export
#'
#' @examples
#' nrow(yocto_spectral.df)
#' comment(yocto_spectral.df)
#' yocto_spectral.mspct <- yocto_spectral2mspct(yocto_spectral.df)
#' summary(yocto_spectral.mspct)
#' yocto_spectral.mspct[[1]]
#'
yocto_spectral2mspct <- function(df, channels = "all", ...) {

  # copying, needs to be implemented
  #
  # comment.txt <- comment(df)
  # how_measured.txt <- how_measured(df)

  if (is.null(channels) || channels == "all") {
    channels <- names(rYoctoPuceInOut::y_spectral.descriptor$peak.wl)
  } else if (tolower(channels) == "xyz") {
    channels <- c("FZ", "FY", "FXL")
  } else if (channels == "wide") {
    channels <- c("VIS", "F8", "NIR")
  } else if (channels == "narrow") {
    channels <- paste("F", 1:8, sep = "")
  }

  channels <-
    intersect(channels,
              names(rYoctoPuceInOut::y_spectral.descriptor$peak.wl))
  stopifnot("Invalid or missing 'channel' names!" = length(channels) >= 1L)

  # sort channels by w.length before pivoting
  channels <-
    channels[order(rYoctoPuceInOut::y_spectral.descriptor$peak.wl[channels])]
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
  z[["w.length"]] <-
    rYoctoPuceInOut::y_spectral.descriptor$peak.wl[z[["channel"]]]
  z <- z[order(z[["w.length"]]), ]

  # this is currently rather slow with 1000s of spectra!
  photobiology::subset2mspct(z,
                             member.class = "raw_spct",
                             idx.var = "time",
                             ...)
}

#' @title Example of imported spectral data
#'
#' @description A dataset containing data logged with a Yocto-Spectral USB
#'   module from YoctoPuce and imported with function
#'   \code{\link[rYoctoPuceInOut:read_yocto_datalog]{read_yocto_spctlog}()}.
#'
#' @docType data
#' @keywords datasets
#' @format A \code{data.frame} with 199 rows and 14 variables.
#'
#' @seealso \code{\link{yocto_spectral2mspct}()},
#'   \code{{read_yocto_datalog}()}.
#'
#' @examples
#' head(yocto_spectral.df)
#'
"yocto_spectral.df"
