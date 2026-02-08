#' Read data from YoctoPuce CSV files
#'
#' Functions able to directly read and validate data from .CSV files created by
#' the data loggers built into YoctoPuce USB modules, and metadata extracted
#' from a JSON file with the USB module settings.
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
#' @param module.descriptor list A matadata descriptor of the YocotoPuce USB
#'   module to be added as an attribute to the returned object.
#' @inheritParams utils::read.table
#' @inheritParams lubridate::ymd_hms
#' @inheritDotParams utils::read.table colClasses nrows skip check.names comment.char
#'
#' @details The dataloggers implemented in different USB modules from
#' YoctoPuce return .CSV files with a consistent format, that varies only
#' in the number of data columns and their names. Function
#' \code{read_yocto_datalog()} reads any of these files preserving column
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
#' an argument to parameter \code{settings.file} its contents parsed into an
#' R list are saved to attribute \code{"yocto.module.settings"} in the returned
#' data frame. The metadata also make it possible to set column names to the
#' logical names set in the module. The logical name of the module and the
#' units are also extracted if available, formatted and saved in the
#' \code{"how.measured"} attribute.
#'
#' A comment attribute is always set with information about the imported file(s)
#' and the import time and package.
#'
#' Function \code{read_yocto_spctlog()} is a wrapper on
#' \code{read_yocto_datalog()} that renames columns using the names used in
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
#' the read values have to be interpreted based on the module settings which,
#' scripts used during data acquisition by the USB module. These settings
#' if available in a JSON file downloaded from the module, can be stored in
#' attribute \code{"yocto.module.settings"} of the data frame returned.
#'
#' I have access to several different USB modules from YoctoPuce, but not to
#' all of them. There are currently some modules that are not fully supported,
#' and a few others that "should" work but have not been yet tested.
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
#' library(photobiology)
#' # Yocto-Meteo module
#'
#' yocto_meteo.file <-
#'   system.file("extdata", "yocto-meteo-snm.csv",
#'               package = "rYoctoPuceInOut", mustWork = TRUE)
#' yocto_meteo_json.file <-
#'   system.file("extdata", "METEOMK2-19A230.json",
#'               package = "rYoctoPuceInOut", mustWork = TRUE)
#'
#' meteo1.df <- read_yocto_datalog(yocto_meteo.file)
#' head(meteo1.df, n = 5)
#' cat(comment(meteo1.df))
#' cat(how_measured(meteo1.df))
#'
#' read_yocto_datalog(yocto_meteo.file, cols.pattern = "avg") |>
#'   head(n = 5)
#' read_yocto_datalog(yocto_meteo.file, cols.pattern = "min|max") |>
#'   head(n = 5)
#' read_yocto_datalog(yocto_meteo.file, cols.pattern = "temperature") |>
#'   head(n = 5)
#' read_yocto_datalog(yocto_meteo.file, nrows = 4L)
#'
#' # metadata from JSON file
#' meteo2.df <- read_yocto_datalog(yocto_meteo.file, yocto_meteo_json.file)
#' head(meteo2.df, n = 5)
#' cat(comment(meteo2.df))
#' cat(how_measured(meteo2.df))
#'
#' meteo3.df <- read_yocto_datalog(yocto_meteo.file,
#'                                    yocto_meteo_json.file,
#'                                    cols.logical.names = TRUE)
#' head(meteo3.df, n = 5)
#' cat(comment(meteo3.df))
#' cat(how_measured(meteo3.df))
#' str(attr(meteo3.df, "yocto.module.settings"), max.level = 2)
#'
#' # Yocto-Spectral module
#'
#' yocto_spectral.file <-
#'   system.file("extdata", "yocto-spectral-LED.csv",
#'               package = "rYoctoPuceInOut", mustWork = TRUE)
#' yocto_spectral_json.file <-
#'   system.file("extdata", "SPECTRL1-2CF3B6.json",
#'               package = "rYoctoPuceInOut", mustWork = TRUE)
#'
#' read_yocto_spctlog(yocto_spectral.file) |> head(n = 5)
#' read_yocto_spctlog(yocto_spectral.file, cols.pattern = "Channel1\\.") |>
#'   head(n = 5)
#' read_yocto_spctlog(yocto_spectral.file, cols.rename = FALSE) |>
#'   head(n = 5)
#'
#' # metadata from JSON file
#' spectral.df <-
#'   read_yocto_spctlog(yocto_spectral.file,
#'                           yocto_spectral_json.file)
#' cat(how_measured(spectral.df))
#' cat(comment(spectral.df))
#'
read_yocto_datalog <- function(file,
                               settings.file = NULL,
                               geocode = NULL,
                               label = NULL,
                               cols.pattern = NULL,
                               cols.logical.names = FALSE,
                               nacols.rm = TRUE,
                               dec = ".", sep = ";", tz = "UTC",
                               module.descriptor = NULL,
                               ...) {

  data.df <- utils::read.csv(file,
                             quote = "", na.strings = "",
                             strip.white = FALSE,
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
              signif(range(unique(diff(data.df[["UNIX.time"]]))), digits = 3)),
                  collapse = " to "))
  }
  if (anyNA(data.df[["UNIX.time"]])) {
    warning("Found missing time stamps! Deleting ", sum(is.na(data.df[["UNIX.time"]])))
    data.df <- data.df[!is.na(data.df[["UNIX.time"]]), ]
  }
  if (length(unique(data.df[["UNIX.time"]])) < nrow(data.df)) {
    warning("Found duplicated time stamps! Likely clock was reset midway.")
  }
  if (is.unsorted(data.df[["UNIX.time"]], na.rm = TRUE, strictly = FALSE)) {
    warning("Found unsorted time stamps! Likely clock was reset midway.")
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

  # drop columns matching "time": "UNIX time", "Local time", "time"
  data.cols <- grep("time", colnames(data.df), value = TRUE, invert = TRUE)
  if (length(cols.pattern)) {
    if (is.na(cols.pattern)) {
      # return all columns as is
      return(data.df)
    }
    # select columns using grep() for regex pattern
    data.cols <- grep(cols.pattern, x = data.cols, value = TRUE)
  }
  if (!length(data.cols)) {
    warning("Returning only time values!! ",
            "Selection pattern '", cols.pattern,
            "' did not match any data column",
            ifelse(nacols.rm, " with non-missing data. ", ". "))
  }
  data.df <- data.df[ , c("time", data.cols), drop = FALSE]

  # add comment attr with file and import data
  if (length(settings.file)) {
    settings.file.text <-
      paste("Metadata file \"",
            basename(settings.file), "\" created on ",
            strftime(file.info(settings.file)[["ctime"]],
                     "%Y-%m-%d %H:%M:%S.\n"),
            sep = "")
  } else {
    settings.file.text <- character(0L)
  }

  comment.txt <-
    paste("Data logged by a YoctoPuce USB module.\nData file \"",
          basename(file), "\" created on ",
          strftime(file.info(file)[["ctime"]], "%Y-%m-%d %H:%M:%S.\n"),
          settings.file.text,
          "Imported on ",
          strftime(lubridate::now(), "%Y-%m-%d %H:%M:%S"),
          " using TZ = \"", tz, "\" with 'rYoctoPuceInOut' (== ",
          utils::packageVersion("rYoctoPuceInOut"), ").",
          sep = "")
  comment(data.df) <- paste(comment.txt, label, sep = "\n")

  # add where.measured attribute
  if (SunCalcMeeus::is_valid_geocode(geocode)) {
    photobiology::where_measured(data.df) <- geocode
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
    known.channel.names <-
      c("genericSensor", # Yocto-Serial, Yocto-RS232, Yocto-RS485, Yocto-SDI12, Yocto-I2C, Yocto-SPI, Yocto-Millivolt-Rx, Yocto-0-10V-Rx, Yocto-4-20mA-Rx
        "spectralChannel", # Yocto-Spectral
        "temperature", "humidity", "pressure", # Yocto-Meteo, Yocto-CO2 (V2)
        "carbonDioxide", # Yocto-CO2 (V1, V2)
        "lightSensor", # Yocto-Light (V1, V2, V3, V4, V5)
        "voc", "tvoc", # Yocto-VOC
        # to add Yocto-3D, Yocto-Inclinometer
        # to add Yocto-GPS
        "altitude", # Yocto-Altimeter
        "proximity", # Yocto-Proximity
        "rangeFinder", # Yocto-RangeFinder
        "current", # Yocto-Amp
        "weighScale", "multiCellWeighScale", # Yocto-Bridge, Yocto-MaxiBridge
        # to add Yocto-MaxiMicroVolt-Rx
        "voltage", "current", "power" # Yocto-Volt, Yocto-Watt
      )
    channel.names <-
      grep(paste(known.channel.names, collapse = "|"),
           names(settings.list$api), value = TRUE)

    # keep channel names for which there are data columns
    channel.names <-
      intersect(channel.names,
                unique(gsub("\\.avg|\\.min|\\.max", "", data.cols)))

    logical.names <- character(length(channel.names))
    channel.units <- character(length(channel.names))
    for (i in seq_along(channel.names)) {
      logical.names[i] <-
        settings.list[["api"]][[channel.names[i]]][["logicalName"]]
      if (logical.names[i] == "") {
        logical.names[i] <- channel.names[i]
      }
      if ("unit" %in% names(settings.list[["api"]][[channel.names[i]]])) {
        channel.units[i] <-settings.list[["api"]][[channel.names[i]]][["unit"]]
      }
    }
    names(logical.names) <- channel.names

    if (cols.logical.names) {
      # multiple columns can contain data from the same channel
      # with names ending in .avg, .min, .max
      col.names <- colnames(data.df)
      for (n in channel.names) {
        col.names <- gsub(n, logical.names[n], col.names)
      }
      colnames(data.df) <- col.names
      names(channel.units) <- logical.names
    } else {
      names(channel.units) <- channel.names
    }

    attr(data.df, "yocto.module.settings") <- settings.list
    if (!is.null(module.descriptor) && is.list(module.descriptor)) {
      attr(data.df, "yocto.module.descriptor") <- module.descriptor
    }
    if (length(channel.units) && any(channel.units != "")) {
      channel.units.txt <-
        paste("Units: ",
              paste(names(channel.units), ": ", channel.units,
                    collapse = ", ", sep = ""), ".",
              sep = "")
      channel.units.txt <-
        ifelse(channel.units.txt == "", "NA", channel.units.txt)
    } else {
      channel.units.txt <- "Units: not available."
    }
    how_measured.txt <-
      paste("YoctoPuce USB module ",
            ifelse(settings.list$api$module$logicalName != "",
                   paste("named '",
                         settings.list$api$module$logicalName,
                         "' type '", sep = ""),
                   "type '"),
            settings.list$api$module$productName, "',\ns.n. '",
            settings.list$api$module$serialNumber, "' with firmware '",
            settings.list$api$module$firmwareRelease, "'.",
            "\n", channel.units.txt, sep = "")
  } else {
    how_measured.txt <- "YoctoPuce USB module type 'unknown'."
  }
  photobiology::how_measured(data.df) <- how_measured.txt

  data.df

}

#' @rdname read_yocto_datalog
#'
#' @param cols.rename logical Flag, if \code{TRUE} use channel names from
#'   sensor IC specification as column names, and if \code{FALSE} keep the
#'   names used in the imported file.
#'
#' @export
#'
read_yocto_spctlog <- function(file,
                                settings.file = NULL,
                                cols.pattern = "avg",
                                cols.rename = TRUE,
                                dec = ".", sep = ";", tz = "UTC",
                                ...) {

  data.df <- read_yocto_datalog(file,
                               settings.file = settings.file,
                               cols.pattern = cols.pattern,
                               cols.logical.names = FALSE,
                               nacols.rm = FALSE,
                               dec = dec, sep = sep, tz = tz,
                               module.descriptor =
                                 rYoctoPuceInOut::y_spectral.descriptor,
                               ...)

  # get sensor metadata
  metadata <- rYoctoPuceInOut::y_spectral.descriptor$peak.wl

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

  data.df
}
