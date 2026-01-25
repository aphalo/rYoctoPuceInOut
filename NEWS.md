
# rYoctoPuceInOut 0.0.1-1

Initial release. 

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
