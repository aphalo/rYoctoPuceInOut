
# rYoctoPuceInOut 0.0.2

Improve naming. Support as many YoctoPuce USB modules as possible with each 
function. Move function of wider usefulness to 'photobiology'. 

_The interface is taking shape, but possibly subject to both minor and minor
changes in the near future._

- Replace `simul_AS7343()` by `photobiology::simul_sensor_response()`.
- Rename `read_yocto_logger_csv()` into `read_yocto_datalog()`.
- Rename `read_yocto_spectral_csv()` into `read_yocto_spectlog()`.
- Rename `calc_waveband_irrads()` into `calc_calibrated_qtys()`.
- Metadata are now stored as `list` objects as package data, such as 
`y_spectral.descriptor` and stored in attribute `yocto.module.descriptor`.
- Add unit tests and fix minor bugs.

# rYoctoPuceInOut 0.0.1-1

_Initial release. Very unstable!_

- Implement generic import of logged data in function `read_yocto_logger_csv()`.
- Implement import of data logged by *YoctoSpectral* USB modules in function
`read_yocto_spectral_csv()`, with function `AS7343_metadata()` returning
metadata for the *AS7343* sensor integrated circuit used in the *YoctoSpectral* 
module.
- Implement function `yocto_spectral2mspct()` for conversion of the data
frame returned by `read_yocto_spectral_csv()` into a `source_mspct` collection
of `source_spct` objects. 
- Implement function `simul_AS7343()` for simulating the response of each
channel of the *AS7343* sensor integrated circuit based on irradiance
spectra.
