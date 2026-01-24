#' Simulate spectral sensor response
#'
#' @param source.spct source_spct object Light source spectral irradiance.
#' @param sensor.mspct responce_spct or response_mspct object Light sensor
#'   spectral responsiveness, one or more channels.
#' @param range numeric vector of length two Range of wavelengths to use
#'   (nanometres, nm)
#' @inheritParams photobiology::response
#' @inheritParams photobiology::normalize
#'
#' @details Compute sensor response spectrum by convolution of light source
#'   emission spectrum or illumination spectrum and the responsiveness spectrum
#'   of a sensor with one or more channels. Return the integral over wavelengths
#'   for each sensor channel.
#'
#' @export
#'
#' @import photobiology
#'
#' @examples
#' library(photobiologySensors)
#'
#' simul_response(sun.spct, sensors.mspct$ams_AS7343, norm = "undo")
#' simul_response(sun.spct, ccd.spct)
#' simul_response(sun.spct, ccd.spct, range = c(400, 700))
#' simul_response(sun.spct, ccd.spct, unit.out = "photon")
#'
#' simul_AS7343(sun.spct, unit.out = "photon")
#'
simul_response <-
  function(source.spct,
           sensor.mspct,
           norm = "skip",
           range = NULL,
           unit.out = getOption("photobiology.radiation.unit",
                                default = "energy"),
           time.unit = NULL,
           scale.factor = 1) {

    if (!is.null(range)) {
      range <- range(range)
    }
    if (photobiology::is.response_spct(sensor.mspct)) {
      sensor.mspct <- photobiology::subset2mspct(sensor.mspct)
    }
    sensor.mspct <-
      photobiology::normalise(sensor.mspct, range = range, norm = norm)
    stopifnot("'source.spct' wrong class" =
                photobiology::is.source_spct(source.spct))
    stopifnot("'sensor.mspct' wrong class" =
                photobiology::is.response_mspct(sensor.mspct))
    source.spct <- photobiology::trim_wl(source.spct, range = range, fill = 0)
    sensor.mspct <- photobiology::trim_wl(sensor.mspct, range = range, fill = 0)

    channel.responses.mspct <- photobiology::response_mspct()

    for (ch in names(sensor.mspct)) {
      channel.responses.mspct[[ch]] <- sensor.mspct[[ch]] * source.spct
    }

    z <-
      photobiology::response(spct = channel.responses.mspct,
                             w.band = range,
                             unit.out = unit.out,
                             quantity = "total",
                             time.unit = time.unit,
                             scale.factor = scale.factor)

    photobiology::what_measured(z) <-
      paste("illumination: ", photobiology::what_measured(source.spct),
            "\nsensor: ", photobiology::what_measured(sensor.mspct), sep = "")
    z
  }

#' @rdname simul_response
#'
#' @export
#'
simul_AS7343 <- function(source.spct,
                         range = NULL,
                         unit.out = getOption("photobiology.radiation.unit",
                                              default = "energy"),
                         time.unit = NULL,
                         scale.factor = 1) {
  ams_AS7343.spct <- photobiologySensors::sensors.mspct[["ams_AS7343"]]
  ams_AS7343.spct <- photobiology::normalise(ams_AS7343.spct, norm = "undo")
  simul_response(source.spct = source.spct,
                 sensor.mspct = ams_AS7343.spct,
                 range = range(source.spct),
                 unit.out = unit.out,
                 time.unit = time.unit,
                 scale.factor = scale.factor)
}
