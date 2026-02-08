#' Compute measured quantities
#'
#' Compute derived quantities from data logged by a YoctoPuce USB module.
#'
#' @param x data.frame as returned by \code{read_yocto_datalog()} or
#'   \code{read_yocto_spctlog()}.
#' @param calibrations list A nested named list with calibration coefficients
#'   or function definitions.
#' @param save.calibrations logical Flag indicating if the calibration object
#'   is to be stored as an arribute in the returned object.
#'
#' @details Function \code{calc_calibrated_qtys()} computes derived quantities
#'   from data logged by YoctoPuce USB module previously and imported into R by
#'   means of function \code{\link{read_yocto_datalog}()} or
#'   function \code{read_yocto_spctlog()}.
#'
#'   Multiple approaches to calibration are supported, one based on multipliers
#'   applied to data for one or more channels and combined by summation. This
#'   approach is suitable when the calibration function is obtained by fitting a
#'   multiple regression linear model (i.e., with no interaction terms). In this
#'   case the calibration object is a \code{list} where \code{numeric} values
#'   are stored. The name of the members of the calibration object must match
#'   the names of the columns of data frame \code{x}.
#'
#'   The second more flexible approach, relies on user-defined functions stored
#'   in the calibration object instead of \code{numeric} values. In this case
#'   the \code{function} definitions can replace one of more of the
#'   \code{numeric} values, allowing more complex conversion computations than a
#'   simple multiplication factor. These functions should accept as input a
#'   single \code{numeric} vector, i.e., the raw values data in one column of
#'   \code{x}, i.e., data acquired from a single sensor channel and return a
#'   numeric vector of calibrated values. In this case, as when using
#'   \code{numeric} multipliers, the values computed from different channels for
#'   a single output are added up.
#'
#'   A final variation is a calibration based on a single function that accepts
#'   as input data frame \code{x} in whole and returns a vector of calibrated
#'   values of the same length as rows in \code{x}. This approach is the most
#'   flexible and is agnostic about the class of the data in \code{x} columns or
#'   returned values, i.e., can apply transformations to non-numeric data.
#'
#'   All three approaches can be combined in a single calibration object, with
#'   different approaches used for the different derived quantities. This allows
#'   using the most suitable/convenient approach for the computation of each
#'   derived quantity.
#'
#'   The number of quantities in the returned data frame and their names are
#'   determined by the named values stored in \code{calibrations}.
#'
#'   Functions \code{read_yocto_datalog()} and
#'   \code{read_yocto_spctlog()} store metadata as attributes in the data
#'   frames they return. These attributes are copied to the data frames of
#'   calibrated data, adding a trace of the application of calibrations.
#'
#' @section Calibration object: The coefficients or functions are stored in a
#'   tree-like named list of named lists. The names in the root list are names
#'   of the derived quantities while the terminal branches are named for the
#'   columns in \code{x}, columns each containing raw data for a single channel
#'   of a YoctoPuce USB module.
#'
#'   As the names of the columns in \code{x} not only depend on the module type
#'   but also on module settings and possible renaming during import into R,
#'   \emph{calibration objects are data workflow specific}.
#'
#' @return A data frame with column \code{"time"} with the times from
#'   \code{x$time} and one column of calibrated data for each primary branch in
#'   \code{calibrations}. The \code{calibrations} object is stored as attribute
#'   \code{"yocto.module.calibrations"} together with those attributes supported
#'   by the R for photobiology suite or comment present in \code{x}.
#'
#' @export
#'
#' @examples
#' # example calibration list based on parallel measurements of sunlight
#' # with all branches except one relying on numeric multipliers applied to
#' # a single sensor channel, and one channel relying on the sum of scaled
#' # values from two sensor channels.
#'
#' s01.cal <-
#'   list(module.type = "YoctoSpectral",
#'        module.sn = "SPECTRL1-2CF3B6",
#'        module.name = "spectrl-01",
#'        Q_PAR = list(VIS.avg = 1180 / 9296),
#'        Q_ePAR = list(VIS.avg = 1377 / 9296),
#'        Q_e = list(F8.avg = (1377 - 1180) / 3422),
#'        Q_FarRed = list(F8.avg = 197.87 / 3422),
#'        Q_Red = list(F6.avg = 265.15 / 5340 * (5340 / (5340 + 6462)),
#'                             F7.avg = 265.15 / 6462 * (6462 / (5340 + 6462))))
#'
#' # path to example logged data from a Yocto-Spectral USN module
#' yocto_spectral.file <-
#'   system.file("extdata", "yocto-spectral-LED.csv",
#'               package = "rYoctoPuceInOut", mustWork = TRUE)
#' yocto_spectral_json.file <-
#'   system.file("extdata", "SPECTRL1-2CF3B6.json",
#'               package = "rYoctoPuceInOut", mustWork = TRUE)
#'
#' # read data and apply calibration
#' read_yocto_spctlog(yocto_spectral.file) |>
#'   calc_calibrated_qtys(s01.cal)
#'
#' read_yocto_spctlog(yocto_spectral.file, yocto_spectral_json.file) |>
#'   calc_calibrated_qtys(s01.cal)
#'
calc_calibrated_qtys <- function(x,
                                 calibrations,
                                 save.calibrations = TRUE) {
  calibrations.name <- substitute(calibrations)
  calibrations.name <-
    if (is.name(calibrations.name))
      as.character(calibrations.name) else "anonymous"

  z <- list(time = x[["time"]])
  qty.names <- grep("^module",
                    names(calibrations),
                    value = TRUE, invert = TRUE)
  for (qty.name in qty.names) {
     z[[qty.name]] <- 0
     for (channel.name in names(calibrations[[qty.name]])) {
       stopifnot("Bad channel name found in 'calibrations'!" =
                   channel.name %in% colnames(x))
       cal.info <- calibrations[[qty.name]][[channel.name]]
       if (is.numeric(cal.info)) {
         z[[qty.name]] <- z[[qty.name]] + x[[channel.name]] * cal.info
       } else if (is.function(cal.info)) {
         if (channel.name %in% c("x", "all")) {
           z[[qty.name]] <- z[[qty.name]] + cal.info(x)
         } else {
           z[[qty.name]] <- z[[qty.name]] + cal.info(x[[channel.name]])
         }
       }
     }
  }
  stopifnot("Assertion failed! Not all quantities in 'calibrations' were computed." =
              ncol(z) == length(calibrations) + 1L)
  z <- as.data.frame(z)
  if (save.calibrations) {
    attr(z, "yocto.module.calibrations") <- calibrations
  }
  photobiology::how_measured(z) <-
    paste(gsub("unknown", calibrations[["module.type"]],
               photobiology::how_measured(x)),
          "\nCalibration: '", calibrations.name, "' (",
          calibrations[["module.sn"]], ").", sep = "")
  photobiology::where_measured(z) <- photobiology::where_measured(x)
  photobiology::when_measured(z) <- photobiology::when_measured(x)
  photobiology::what_measured(z) <- photobiology::when_measured(x)
  comment(z) <-
    paste("Values computed by 'calc_calibrated_qtys()' using calibration '",
          calibrations.name, "'.\n",
          comment(x), sep = "")
  z
}
