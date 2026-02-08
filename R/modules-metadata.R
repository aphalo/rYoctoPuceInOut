#' Metadata for YoctoPuce modules
#'
#' Metadata for sensors used in YoctoPuce USB modules. The metadata were
#' retrieved from module specifications or the data sheet for the sensor used in
#' the modules. As such, they are \emph{typical} or \emph{nominal} values.
#' Individual sensors' properties are dependent on manufacturing tolerances and
#' on temperature. For accurate measurements sensors need to be individually
#' calibrated keeping in mind that sensors need recalibration as their
#' properties can also change with time.
#'
#' @docType data
#' @keywords datasets
#'
#' @seealso
#' Package \code{\link[photobiologySensors]{photobiologySensors-package}}
#' provides spectral response data for numerous light sensors relevant to
#' light measurements relevant to plants and vegetation.
#'
#' @examples
#' names(y_spectral.descriptor)
#' y_spectral.descriptor$peak.wl
#' y_spectral.descriptor$HMFW
#'
"y_spectral.descriptor"
