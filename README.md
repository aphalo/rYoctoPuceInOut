
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rYoctoPuceInOut

<!-- badges: start -->

<!-- badges: end -->

The goal of ‘rYoctoPuceInOut’ is to facilitate the import of data saved
by the built-in logger of various USB modules made by YoctoPuce. File
import functions as well as auxiliary functions that easy the use of the
logged data are provided. These additional functions currently target
the AS7343 digital spectral sensor from AMS-Osram used in the
*YoctoSpectral* module.

The simulations of channels responses of the AS7343 makes possible
*in-silico* simulations of the retrieval specific quantities of interest
such as PAR, ePAR, UVA:PAR, R:FR and B:G ratios.

The package includes CSV files with data logged by a selection of
different USB USB modules from *YoctoPuce*.

Package ‘rYoctoPuceInOut’ is an extension of the R for photobiology
suite of R packages.

## Installation

You can install the development version of AS7343 with:

``` r
remotes::install_github("aphalo/AS7343")
packageVersion("rYoctoPuceInOut")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(AS7343)
#> Loading required package: photobiology
#> Loading required package: SunCalcMeeus
#> Documentation at https://docs.r4photobiology.info/
simul_AS7343(sun.spct, 
             unit.out = "photon",
             scale.factor = 1e-6)
#> # A tibble: 13 × 2
#>    spct.idx `R[/q]_range.280.800`
#>    <fct>                    <dbl>
#>  1 F1                       1.06 
#>  2 F2                       1.27 
#>  3 F3                       3.14 
#>  4 F4                       3.83 
#>  5 F5                       3.66 
#>  6 F6                       4.46 
#>  7 F7                       4.61 
#>  8 F8                       3.25 
#>  9 FXL                      7.40 
#> 10 FY                       9.63 
#> 11 FZ                       4.32 
#> 12 NIR                      0.155
#> 13 VIS                     13.7
```

``` r
yocto_meteo.file <-
  system.file("extdata", "yocto-meteo-snm.csv",
              package = "AS7343", mustWork = TRUE)

head(read_yocto_logger_csv(yocto_meteo.file), 5)
#> Period: 2025-08-07 19:32:05 to 2025-08-08 01:23:00
#> Found gaps, time steps range from 55s to 60s (~1 minutes)
#>                  time humidity.min humidity.avg humidity.max pressure.min
#> 1 2025-08-07 16:32:05       53.310       53.310       53.310     1012.712
#> 2 2025-08-07 16:33:05       53.043       53.352       53.715     1012.472
#> 3 2025-08-07 16:34:05       53.188       53.269       53.394     1012.398
#> 4 2025-08-07 16:35:05       53.219       53.482       53.684     1012.426
#> 5 2025-08-07 16:36:05       53.631       53.794       54.005     1012.413
#>   pressure.avg pressure.max temperature.min temperature.avg temperature.max
#> 1     1012.716     1012.717          23.495          23.495          23.495
#> 2     1012.548     1012.712          23.495          23.503          23.506
#> 3     1012.434     1012.477          23.495          23.501          23.506
#> 4     1012.467     1012.525          23.495          23.502          23.506
#> 5     1012.468     1012.525          23.506          23.510          23.517
head(read_yocto_logger_csv(yocto_meteo.file, 
                           cols.pattern = "avg"), 5)
#> Period: 2025-08-07 19:32:05 to 2025-08-08 01:23:00
#> Found gaps, time steps range from 55s to 60s (~1 minutes)
#>                  time humidity.avg pressure.avg temperature.avg
#> 1 2025-08-07 16:32:05       53.310     1012.716          23.495
#> 2 2025-08-07 16:33:05       53.352     1012.548          23.503
#> 3 2025-08-07 16:34:05       53.269     1012.434          23.501
#> 4 2025-08-07 16:35:05       53.482     1012.467          23.502
#> 5 2025-08-07 16:36:05       53.794     1012.468          23.510
head(read_yocto_logger_csv(yocto_meteo.file,
                           cols.pattern = "temperature"), 5)
#> Period: 2025-08-07 19:32:05 to 2025-08-08 01:23:00
#> Found gaps, time steps range from 55s to 60s (~1 minutes)
#>                  time temperature.min temperature.avg temperature.max
#> 1 2025-08-07 16:32:05          23.495          23.495          23.495
#> 2 2025-08-07 16:33:05          23.495          23.503          23.506
#> 3 2025-08-07 16:34:05          23.495          23.501          23.506
#> 4 2025-08-07 16:35:05          23.495          23.502          23.506
#> 5 2025-08-07 16:36:05          23.506          23.510          23.517
```

``` r
yocto_spectral.file <-
   system.file("extdata", "yocto-spectral-LED.csv",
             package = "AS7343", mustWork = TRUE)

head(read_yocto_spectral_csv(yocto_spectral.file,
                             cols.pattern = "avg"), 5)
#> Period: 2025-12-03 21:54:00 to 2025-12-04 03:33:00
#> Found gaps, time steps range from 2s to 16114s (~4.48 hours)
#>                  time     F1.avg     F2.avg      FZ.avg    F3.avg      F4.avg
#> 1 2025-12-03 19:54:00  0.0000000  0.0000000  0.01923449  0.000000  0.01892148
#> 2 2025-12-03 19:55:00 14.8170959 13.2602612 10.70854803 14.034536 14.68704875
#> 3 2025-12-03 19:56:00  0.2099664  0.6277985  1.43448786  1.286988  1.30558184
#>        FY.avg     F5.avg     FXL.avg      F6.avg     F7.avg   F8.avg   NIR.avg
#> 1  0.08539738  0.0000000  0.09671708  0.08235444  0.0681717  0.00000 0.0000000
#> 2 14.28618803 17.9928947 19.16909935 17.43903218 68.5390143 69.80591 3.4036647
#> 3  0.45975569  0.2496803  0.66015027  1.58963228 59.3480445 57.34682 0.7206823
#>      VIS.avg
#> 1  0.0813861
#> 2 37.2297163
#> 3 50.1754135
#>  [ reached 'max' / getOption("max.print") -- omitted 2 rows ]
```
