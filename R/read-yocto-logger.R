#' Read data from YoctoPuce CSV files
#'
#' Functions able to directly read and validate data from .CSV files created by
#' the data loggers built into YoctoPuce USB modules.
#'
#' @param settings.file character Path to a JSON file containing the module
#'   settings used to acquire the data.
#' @param cols.pattern character A string suitable as argument for
#'   \code{pattern} in a call to \code{grep()} on column headings in the
#'   imported CSV file. \code{NULL} or \code{characte(0)} retain all data
#'   columns, while \code{NA} returns all data and time columns.
#' @param cols.logical.names logical Use logical names from module
#'   metadata instead of column heading in CSV file. Require readable
#'   \code{settings.file}.
#' @param nacols.rm logical If \code{TRUE} delete columns that contain only
#'   \code{NA} values.
#' @param geocode	data.frame Containing columns \code{lon} and \code{lat} used
#'   to set attribute \code{"where.measured"}.
#' @param label	character Additional text to be appended to the default
#'   value used to set attribute \code{"comment"}.
#' @inheritParams utils::read.table
#' @inheritParams lubridate::ymd_hms
#' @inheritDotParams utils::read.table colClasses nrows skip check.names comment.char
#'
#' @details The dataloggers implemented in different USB modules from
#' YoctoPuce return .CSV files with a consistent format, that varies only
#' in the number of data columns and their names. Function
#' \code{read_yocto_logger()} reads any of these files preserving column
#' names. The UTC times are converted into \code{POSIXct} values and added
#' under column \code{time}. A subset of columns can be requested by
#' passing a regular expression to be used in a call to \code{grep()} on the
#' column names. If \code{nacols.rm = TRUE} is passed, columns containing only
#' \code{NA} values are deleted, as these columns in most cases correspond
#' to logger channels that are not in use.
#'
#' Warnings are issued if something unexpected, such as dates several years
#' into the past, unsorted or repeated time stamps
#' are encountered in the sequence of UNIX time stamps in the file. These
#' discrepancies can occur when the modules' or the YoctoHub clocks do not
#' acquire a valid time at start-up. Inconsistent time steps are reported
#' through a message, as these are the result of a stop followed by restart
#' of logging. During such a pause it is possible that module settings could
#' have been changed.
#'
#' If the name of a JSON file as saved from the YoctoPuce module is passed as
#' an argument to parameter \code{settings.file} its contents parse into an
#' R list will be saved to attribute \code{yoctopuce.settings} in the returned
#' data frame.
#'
#' Function \code{read_yocto_spectral_csv()} is a wrapper on
#' \code{read_yocto_logger()} that renames columns using the names used in
#' the data sheet for the AS7343 sensor.
#'
#' In modules with multiple channels, the user can enable and disable logging
#' on a channel by channel basis. Thus, the number of data columns can vary,
#' making it necessary to match columns by name when replacing some of the
#' default names by shorter or more informative ones.
#'
#' @section Warning!: The units and basis of expression, and in several cases
#' even the physical quantity measured cannot be decoded from the CSV files.
#' The values in the returned data frame are those read from the file, and
#' the read values have to be interpreted based on the settings and, possibly,
#' scripts used during data acquisition by the USB module. These settings
#' if available in a JSON file downloaded from the module can be stored in
#' attribute \code{yoctopuce.settings} of the data frame returned.
#'
#' @return A data frame with \code{POSIXct} time stamps in column \code{time}
#'   and data in a variable number of columns containing values converted by
#'   \code{\link[utils:read.table]{read.csv}()}.
#'
#' @references
#' Documentation for each YocotoPuce USB module is available at
#' \url{https://www.yoctopuce.com/}.
#'
#' @export
#'
#' @examples
#' # Yocto-Meteo module
#'
#' yocto_meteo.file <-
#'   system.file("extdata", "yocto-meteo-snm.csv",
#'             package = "rYoctoPuceInOut", mustWork = TRUE)
#' yocto_meteo_json.file <-
#'   system.file("extdata", "METEOMK2-19A230.json",
#'             package = "rYoctoPuceInOut", mustWork = TRUE)
#'
#' read_yocto_logger_csv(yocto_meteo.file) |> head(n = 5)
#' read_yocto_logger_csv(yocto_meteo.file, cols.pattern = "avg") |>
#'   head(n = 5)
#' read_yocto_logger_csv(yocto_meteo.file, cols.pattern = "min|max") |>
#'   head(n = 5)
#' read_yocto_logger_csv(yocto_meteo.file, cols.pattern = "temperature") |>
#'   head(n = 5)
#' read_yocto_logger_csv(yocto_meteo.file, nrows = 4L)
#'
#' # metadata from JSON file
#' data.df <- read_yocto_logger_csv(yocto_meteo.file, yocto_meteo_json.file)
#' how_measured(data.df)
#'
#' # Yocto-Millivolt-Rx module
#'
#' yocto_mv.file <-
#'   system.file("extdata", "yocto-millivolt-Rx.csv",
#'             package = "rYoctoPuceInOut", mustWork = TRUE)
#'
#' read_yocto_logger_csv(yocto_mv.file) |> head(n = 5)
#'
#' # Yocto-0-10V-Rx module
#'
#' yocto_v.file <-
#'   system.file("extdata", "yocto-0-10V-Rx.csv",
#'             package = "rYoctoPuceInOut", mustWork = TRUE)
#'
#' read_yocto_logger_csv(yocto_v.file) |> head(n = 5)
#' read_yocto_logger_csv(yocto_v.file, cols.pattern = "avg") |> head(n = 5)
#' read_yocto_logger_csv(yocto_v.file, cols.pattern = "min|max") |> head(n = 5)
#' read_yocto_logger_csv(yocto_v.file, cols.pattern = "Sensor2") |> head(n = 5)
#'
#' # Yocto-Serial module
#'
#' yocto_serial.file <-
#'   system.file("extdata", "yocto-serial.csv",
#'             package = "rYoctoPuceInOut", mustWork = TRUE)
#' yocto_serial_settings.file <-
#'   system.file("extdata", "YSERIAL1-EAD24.json",
#'             package = "rYoctoPuceInOut", mustWork = TRUE)
#'
#' read_yocto_logger_csv(yocto_serial.file) |> head(n = 5)
#' read_yocto_logger_csv(yocto_serial.file, nacols.rm = FALSE) |> head(n = 2)
#' read_yocto_logger_csv(yocto_serial.file, cols.pattern = "avg") |> head(n = 5)
#' read_yocto_logger_csv(yocto_serial.file, cols.pattern = "Sensor1$") |>
#'   head(n = 5)
#'
#' # metadata from JSON file
#' data.tb <-
#'   read_yocto_logger_csv(yocto_serial.file,
#'                         yocto_serial_settings.file,
#'                         cols.logical.names = TRUE)
#' how_measured(data.tb)
#' colnames(data.tb)
#'
#' # Yocto-I2C module
#'
#' yocto_i2c.file <-
#'   system.file("extdata", "yocto-i2c-tsl2591.csv",
#'             package = "rYoctoPuceInOut", mustWork = TRUE)
#'
#' read_yocto_logger_csv(yocto_i2c.file) |> head(n = 5)
#' read_yocto_logger_csv(yocto_i2c.file, cols.pattern = "avg") |>
#'   head(n = 5)
#' read_yocto_logger_csv(yocto_i2c.file, cols.pattern = "min|max") |>
#'   head(n = 5)
#' read_yocto_logger_csv(yocto_i2c.file, cols.pattern = "Sensor1[.]") |>
#'   head(n = 5)
#' read_yocto_logger_csv(yocto_i2c.file, cols.pattern = "Sensor[1-3][.]") |>
#'   head(n = 5)
#' read_yocto_logger_csv(yocto_i2c.file, cols.pattern = "avg", nacols.rm = TRUE) |>
#'   head(n = 5)
#' read_yocto_logger_csv(yocto_i2c.file, cols.pattern = "Sensor[1-3][.]avg") |>
#'   head(n = 5)
#'
#' # Yocto-CO2 module
#'
#' yocto_CO2.file <-
#'   system.file("extdata", "yocto-CO2-V1.csv",
#'             package = "rYoctoPuceInOut", mustWork = TRUE)
#'
#' read_yocto_logger_csv(yocto_CO2.file) |> head(n = 5)
#' read_yocto_logger_csv(yocto_CO2.file, cols.pattern = "avg") |> head(n = 5)
#' read_yocto_logger_csv(yocto_CO2.file, cols.pattern = "min|max") |> head(n = 5)
#'
#' # Yocto-Spectral module
#'
#' yocto_spectral.file <-
#'   system.file("extdata", "yocto-spectral-LED.csv",
#'             package = "rYoctoPuceInOut", mustWork = TRUE)
#' yocto_spectral_json.file <-
#'   system.file("extdata", "SPECTRL1-2CF3B6.json",
#'             package = "rYoctoPuceInOut", mustWork = TRUE)
#'
#' read_yocto_spectral_csv(yocto_spectral.file) |> head(n = 5)
#' read_yocto_spectral_csv(yocto_spectral.file, cols.pattern = "Channel1\\.") |>
#'   head(n = 5)
#' read_yocto_spectral_csv(yocto_spectral.file, cols.rename = FALSE) |>
#'   head(n = 5)
#'
#' # metadata from JSON file
#' spct.df <- read_yocto_spectral_csv(yocto_spectral.file,
#'                                    yocto_spectral_json.file) |> head(n = 5)
#' how_measured(spct.df)
#'
#' AS7343_metadata()
#'
read_yocto_logger_csv <- function(file,
                                  settings.file = NULL,
                                  geocode = NULL,
                                  label = NULL,
                                  cols.pattern = NULL,
                                  cols.logical.names = FALSE,
                                  nacols.rm = TRUE,
                                  dec = ".", sep = ";", tz = "UTC",
                                  ...) {

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

  # remove NA columns
  keepcols <- character()
  if (nacols.rm) {
    for (c in colnames(data.df)) {
      if (!all(is.na(data.df[[c]]))) {
        keepcols <- c(keepcols, c)
      }
    }
    data.df <- data.df[ , keepcols, drop = FALSE]
  }

  # select columns using grep() for regex pattern matching on names
  if (!length(cols.pattern) || !is.na(cols.pattern)) {
    if (length(cols.pattern)) {
      if (is.na(cols.pattern)) {
        return(data.df)
      }
      data.cols <- grep(cols.pattern, x = colnames(data.df), value = TRUE)
    } else {
      data.cols <- grep("time", x = colnames(data.df), value = TRUE, invert = TRUE)
    }
    if (!length(data.cols)) {
      warning("Returning only time values!! ",
              "Selection pattern '", cols.pattern,
              "' did not match any data column",
              ifelse(nacols.rm, " with non-missing data. ", ". "))
    }
    data.df <- data.df[ , c("time", data.cols), drop = FALSE]
  }

  # add comment attr with file and import data
  comment.txt <-
    paste("Data from a YoctoPuce module.\nFile \"",
          basename(file), "\" created on ",
          strftime(file.info(file)[["ctime"]], "%Y-%m-%d %H:%M:%S"),
          " was imported on ",
          strftime(lubridate::now(), "%Y-%m-%d %H:%M:%S"),
          " using TZ = \"", tz, "\"",
          sep = "")
  comment(data.df) <- paste(comment.txt, label, sep = "\n")

  # add where.measured attribute
  if (SunCalcMeeus::is_valid_geocode(geocode)) {
    where_measured(data.df) <- geocode
  }

  # add yocotopuce.settings and how.measured attr with module metadata
  if (length(settings.file)) {
    settings.list <- jsonlite::read_json(settings.file,
                                         simplifyVector = FALSE,
                                         flatten = FALSE)

    if (!length(settings.list[["api"]])) {
      stop("File '", settings.file, "' parsing failure.")
    }
    # rename columns to logical names
    if (cols.logical.names) {
      channel.names <- grepv("genericSensor", names(settings.list$api))
      logical.names <- character(length(channel.names))
      for (i in seq_along(channel.names)) {
        logical.names[i] <- settings.list[["api"]][[i]][["logicalName"]]
        if (logical.names[i] == "") {
          logical.names[i] <- channel.names[i]
        }
      }
      col.names <- colnames(data.df)
      for (n in col.names) {
        if (n %in% names(logical.names)) {
          gsub(n, logical.names[n], col.names)
        }
      }
      colnames(data.df) <- col.names
    }

    attr(data.df, "yocto.module.settings") <- settings.list
    how_measured(data.df) <-
      paste("USB module ",
            ifelse(settings.list$api$module$logicalName != "",
                   paste("named '",
                         settings.list$api$module$logicalName,
                         "' type '", sep = ""),
                   "type '"),
            settings.list$api$module$productName, "', s.n. '",
            settings.list$api$module$serialNumber, "' with firmware '",
            settings.list$api$module$firmwareRelease, "'.", sep = "")
  }

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
                                    settings.file = NULL,
                                    cols.pattern = "avg",
                                    cols.rename = TRUE,
                                    dec = ".", sep = ";", tz = "UTC", ...) {

  data.df <- read_yocto_logger_csv(file,
                                   settings.file = settings.file,
                                   cols.pattern = cols.pattern,
                                   cols.logical.names = FALSE,
                                   nacols.rm = FALSE,
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
