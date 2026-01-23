#' Read data from YoctoPuce CSV files
#'
#' Functions able to directly read and validate data from .CSV files from
#' the dataloggers biult into YoctoPuce USB modules.
#'
#' @param cols.pattern character A string suitable as argument for
#'   \code{pattern} in a call to \code{grep()}. \code{NULL} or
#'   \code{characte(0)} retain all columns.
#' @inheritParams utils::read.table
#' @inheritParams lubridate::ymd_hms
#' @inheritDotParams utils::read.table colClasses nrows skip check.names comment.char
#'
#' @details The dataloggers implemented in different USB modules from
#' YoctoPuce return .CSV files with a consistent format, that varies only
#' in the number of data columns and their names. Function
#' \code{read_yocto_logger()} reads any of these files preserving column
#' names. The UTC times are converted into \code{POSIXct} values and added
#' under column \code{time}.
#'
#' Warnings are issued if something unexpected is encountered in the sequence
#' of UNIX time stamps in the file.
#'
#' In modules with multiple channels, the user can enable and disable logging
#' on a channel by channel basis. Thus, the number of data columns can vary,
#' making it necessary to match columns by name when replacing some of the
#' default names by shorter or more informative ones. Currently, the
#' replacement names are hard coded.
#'
#' @export
#'
#' @examples
#' yocto_meteo.file <-
#'   system.file("extdata", "yocto-meteo-snm.csv",
#'             package = "AS7343", mustWork = TRUE)
#'
#' read_yocto_logger_csv(yocto_meteo.file)
#' read_yocto_logger_csv(yocto_meteo.file, cols.pattern = "avg")
#' read_yocto_logger_csv(yocto_meteo.file, cols.pattern = "min|max")
#' read_yocto_logger_csv(yocto_meteo.file, cols.pattern = "temperature")
#'
#' yocto_spectral.file <-
#'   system.file("extdata", "yocto-spectral-LED.csv",
#'             package = "AS7343", mustWork = TRUE)
#'
#' read_yocto_spectral_csv(yocto_spectral.file)
#' read_yocto_spectral_csv(yocto_spectral.file, cols.rename = FALSE)
#' read_yocto_spectral_csv(yocto_spectral.file, cols.pattern = "Channel1\\.")
#'
#' AS7343_metadata()
#'
read_yocto_logger_csv <- function(file,
                                  cols.pattern = NULL,
                                  dec = ".", sep = ";", tz = "UTC", ...) {

  data.df <- utils::read.csv(file,
                             quote = "", na.strings = "", strip.white = FALSE,
                             blank.lines.skip = TRUE,
                             dec = dec, sep = sep,
                             ...)

  # add column with POSIXct
  data.df[["time"]] <- lubridate::ymd_hms(data.df[["Local.time"]], tz = tz)

  # sanity checks for time
  message("Period: ",
          paste(strftime(range(data.df[["time"]]), "%Y-%m-%d %H:%M:%S"),
                collapse = " to ", sep = ""))
  if (length(unique(diff(data.df[["UNIX.time"]]))) > 1L) {
    message("Found gaps, time steps range from ",
            paste(lubridate::as.duration(
              range(unique(diff(data.df[["UNIX.time"]])))),
                  collapse = " to "))
  }
  if (anyNA(data.df[["UNIX.time"]])) {
    warning("Found missing time stamps! Deleting ", sum(is.na(data.df[["UNIX.time"]])))
    data.df <- data.df[!is.na(data.df[["UNIX.time"]]), ]
  }
  if (is.unsorted(data.df[["UNIX.time"]], na.rm = TRUE, strictly = TRUE)) {
    warning("Found unsorted time stamps! Likely clock was reset midway.")
  }
  if (length(unique(data.df[["UNIX.time"]])) < nrow(data.df)) {
    warning("Found duplicated time stamps! Likely clock was reset midway.")
  }

  # select columns
  if (length(cols.pattern)) {
    data.cols <- grep(cols.pattern, x = colnames(data.df), value = TRUE)
    data.df <- data.df[ , c("time", data.cols)]
  }

  # add comment with file and import data
  comment(data.df) <-
    paste("File \"", basename(file), "\" created on ",
          strftime(file.info(file)[["ctime"]], "%Y-%m-%d %H:%M:%S"),
          " was imported on ",
          strftime(lubridate::now(), "%Y-%m-%d %H:%M:%S"),
          " using TZ = \"", tz, "\"",
          sep = "")

  data.df
}

#' @rdname read_yocto_logger_csv
#'
#' @param cols.rename logical Flag, if \code{TRUE} use channel names from
#'   sensor IC specification as column names, and if \code{FALSE} keep the
#'   names used in the imported file.
#'
#' @export
#'
read_yocto_spectral_csv <- function(file,
                                    cols.pattern = "avg",
                                    cols.rename = TRUE,
                                    dec = ".", sep = ";", tz = "UTC", ...) {

  data.df <- read_yocto_logger_csv(file,
                                   cols.pattern = cols.pattern,
                                   dec = dec, sep = sep, tz = tz, ...)

  # get sensor metadata
  metadata <- AS7343_metadata()

  # find columns with data
  datacol.idxs <- which(!grepl("time", colnames(data.df)))
  datacol.names <- colnames(data.df)[datacol.idxs]
  channel.idxs <-
    as.integer(gsub("spectralChannel|\\.min|\\.avg|\\.max", "", datacol.names))

  if (cols.rename) {
    datacol.names <- paste(names(metadata[channel.idxs]),
                           gsub("spectralChannel[0-9]*", "", datacol.names),
                           sep = "")
    colnames(data.df)[datacol.idxs] <- datacol.names
  }

  attr(data.df, "yocto.properties") <-
    list(data.channels = names(metadata)[unique(channel.idxs)],
         data.w.lengths = unname(metadata)[unique(channel.idxs)],
         sensor.type = "AS743",
         module.type = "Yocto-Spectral",
         sensor.channels = names(metadata),
         sensor.w.lengths = unname(metadata),
         s.irrad.unit = "arbitrary units")

  data.df
}

#' @rdname read_yocto_logger_csv
#'
#' @export
#'
AS7343_metadata <- function() {
  chn.names.AS7343 <-
    c("F1", "F2", "FZ", "F3", "F4", "FY", "F5", "FXL", "F6", "F7", "F8", "NIR", "VIS")

  chn.wls.AS7343 <-
    c(405, 425, 450, 475, 515, 555, 550, 600, 640, 690, 745, 855, 620)
  names(chn.wls.AS7343) <- chn.names.AS7343
  chn.wls.AS7343
}
