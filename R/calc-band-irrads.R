#' Compute selected irradiances
#'
#' Starting from data logged in a Yocto-Spectral USB module and its
#' calibration, compute photon irradiances for wavebands of interest in
#' plant production as well as in research on plant photobiology.
#'
#' @param x data.frame as returned by \code{read_yocto_spectral_csv()}.
#' @param calibrations list A nested named list with calibration coefficients.
#'
#' @details The calibration is additive, and can make use of readings from one
#' or more AS7343 channels, each with a separate multiplier coefficient or
#' a calibration function.
#' The coefficients or functions are stored in a named list of named lists.
#' The names in the outer list are names of
#' the irradiance quantities while the terminal branches are named for the
#' AS7343 channels and contain numeric multiplier coefficients. If a function,
#' the branch can be named also is named \code{"x"}, in which case the
#' function is passed as its first argument the \code{x} data frame in whole.
#' In other cases, similarly as for numeric coefficients just data for a single
#' channel is passed to the function.
#'
#' The number of quantities and their names are determined by the values
#' stored in \code{calibrations}.
#'
#' @return A data frame with column \code{"time"} with the times from
#'   \code{x$time} and one column of irradiances for each primary branch
#'   in \code{calibrations}.
#'
#' @export
#'
#' @examples
#' # example calibration list based on parallel measurements of sunlight
#' s01.cal <-
#'   list(module.type = "YoctoSpectral",
#'        module.sn = "SPECTRL1-2CF3B6",
#'        module.name = "spectrl-01",
#'        Q_PAR = list(VIS.avg = 1180 / 9296),
#'        Q_ePAR = list(VIS.avg = 1377 / 9296),
#'        Q_e = list(F8.avg = (1377 - 1180) / 3422),
#'        Q_FarRed.Smith20 = list(F8.avg = 72.4 / 3422),
#'        Q_Red.Smith20 = list(FXL.avg = 88.1 / 5263),
#'        Q_FarRed.Sellaro = list(F8.avg = 197.87 / 3422),
#'        Q_Red.Sellaro = list(F6.avg = 265.15 / 5340 * (5340 / (5340 + 6462)),
#'                             F7.avg = 265.15 / 6462 * (6462 / (5340 + 6462))),
#'        Q_Red.Sellaro2 = list(FXL.avg = 265.15 / 5263),
#'        Q_Green.Sellaro = list(FY.avg = 289.3 / 5660),
#'        Q_Blue.Sellaro = list(FZ.avg = 235.9 / 3223),
#'        Q_UVA1.CIE = list(F1.avg = 72.16 / 1439))
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
#' read_yocto_spectral_csv(yocto_spectral.file) |>
#'   calc_waveband_irrads(s01.cal)
#'
#' read_yocto_spectral_csv(yocto_spectral.file, yocto_spectral_json.file) |>
#'   calc_waveband_irrads(s01.cal)
#'
calc_waveband_irrads <- function(x, calibrations) {
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
  photobiology::how_measured(z) <-
    paste(gsub("unknown", calibrations[["module.type"]],
               photobiology::how_measured(x)),
          "\nCalibration: '", calibrations.name, "' (",
          calibrations[["module.sn"]], ").", sep = "")
  photobiology::where_measured(z) <- photobiology::where_measured(x)
  photobiology::when_measured(z) <- photobiology::when_measured(x)
  photobiology::what_measured(z) <- photobiology::when_measured(x)
  comment(z) <-
    paste("Irradiances computed by 'calc_waveband_irrads()' using calibration '",
          calibrations.name, "'.\n",
          comment(x), sep = "")
  z
}
