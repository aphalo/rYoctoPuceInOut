
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rYoctoPuceInOut

<!-- badges: start -->

[![R Universe
version](https://aphalo.r-universe.dev/badges/rYoctoPuceInOut)](https://aphalo.r-universe.dev/rYoctoPuceInOut)
[![R-CMD-check](https://github.com/aphalo/rYoctoPuceInOut/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/aphalo/rYoctoPuceInOut/actions/workflows/R-CMD-check.yaml)
[![Documentation](https://img.shields.io/badge/documentation-rYoctoPuceInOut-informational.svg)](https://docs.r4photobiology.info/rYoctoPuceInOut/)
<!-- badges: end -->

## Warning

Package ‘rYoctoPuceInOut’ is at a very early stage of development.
Function interfaces and object names are still subject to changes likely
to break code written for the current version of ‘rYoctoPuceInOut’ (==
0.0.1.9001).

## Purpose

The goal of ‘rYoctoPuceInOut’ is to facilitate the import of data saved
by the built-in logger of various USB modules made by
[YoctoPuce](https://www.yoctopuce.com/). File import functions as well
as auxiliary functions that easy the use of the logged data by
applicaton of calibrations are provided, or conversion into objects
following the expectations of the **R for Photobiology** suite of R
packages.

The package includes CSV files with data logged by a selection of
different USB USB modules from *YoctoPuce*. It also includes JSON files
with settings for the same modules.

Package ‘rYoctoPuceInOut’ is an extension of the R for photobiology
suite of R packages.

## Installation

Package ‘rYoctoPuceInOut’ is not yet published through CRAN. Currently,
it is easiest to install it from R-Universe. It is also possible to
directly install it from the sources at the GitHUb repository.

In recent versions of R an option can be set to make the author’s
repository at R-Universe. Once the option is set installing this package
works similarly as from CRAN.

``` r
repos <- getOption("repos", default = list())
repos[["r4photobiology"]] <- "https://aphalo.r-universe.dev"
options(repos = repos)
```

``` r
install.packages(c('rOmniDriver', 'ooacquire'))
```

Without setting the option, it is also possible to pass the URL of the
my R-Universe repository in the call to `install.packages()` together
with the CRAN URL to ensure that dependencies are installed.

``` r
install.packages(c('rOmniDriver', 'ooacquire'), 
                 repos = c('https://aphalo.r-universe.dev', 
                           'https://cloud.r-project.org'))
```

You can install the same version of ‘rYoctoPuceInOut’ with:

``` r
remotes::install_github("aphalo/rYoctoPuceInOut")
```

To check the currently installed version use:

``` r
packageVersion("rYoctoPuceInOut")
#> [1] '0.0.1.9001'
```

## Documentation and examples

Documentation includes one vignettes in addition to help pages. The
examples in the vignettes and help pages use example files downloaded
from different YoctoPuce modules. These CSV (data) and JSON (settings)
files can be found in folder `extdata`.

## Examples

This is a basic example which shows you how to solve a common problem:

``` r
library(rYoctoPuceInOut)

yocto_meteo.file <-
  system.file("extdata", "yocto-meteo-snm.csv",
              package = "rYoctoPuceInOut", mustWork = TRUE)

head(read_yocto_datalog(yocto_meteo.file), 5)
#> Period: 2025-08-07 19:32:05 to 2025-08-07 22:50:00
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
head(read_yocto_datalog(yocto_meteo.file, 
                           cols.pattern = "avg"), 5)
#> Period: 2025-08-07 19:32:05 to 2025-08-07 22:50:00
#> Found gaps, time steps range from 55s to 60s (~1 minutes)
#>                  time humidity.avg pressure.avg temperature.avg
#> 1 2025-08-07 16:32:05       53.310     1012.716          23.495
#> 2 2025-08-07 16:33:05       53.352     1012.548          23.503
#> 3 2025-08-07 16:34:05       53.269     1012.434          23.501
#> 4 2025-08-07 16:35:05       53.482     1012.467          23.502
#> 5 2025-08-07 16:36:05       53.794     1012.468          23.510
head(read_yocto_datalog(yocto_meteo.file,
                           cols.pattern = "temperature"), 5)
#> Period: 2025-08-07 19:32:05 to 2025-08-07 22:50:00
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
             package = "rYoctoPuceInOut", mustWork = TRUE)

head(read_yocto_spctlog(yocto_spectral.file,
                             cols.pattern = "avg"), 5)
#> Period: 2025-12-03 21:54:00 to 2025-12-03 22:52:14
#> Found gaps, time steps range from 10s to 60s (~1 minutes)
#>                  time      F1.avg      F2.avg      FZ.avg      F3.avg
#> 1 2025-12-03 19:54:00  0.00000000  0.00000000  0.01923449  0.00000000
#> 2 2025-12-03 19:55:00 14.81709593 13.26026119 10.70854803 14.03453598
#> 3 2025-12-03 19:56:00  0.20996641  0.62779851  1.43448786  1.28698813
#> 4 2025-12-03 19:57:00  0.01586413  0.01212687  0.01720980  0.01690788
#> 5 2025-12-03 19:58:00  0.01586413  0.01212687  0.01619746  0.01690788
#>        F4.avg      FY.avg      F5.avg     FXL.avg      F6.avg      F7.avg
#> 1  0.01892148  0.08539738  0.00000000  0.09671708  0.08235444  0.06817170
#> 2 14.68704875 14.28618803 17.99289470 19.16909935 17.43903218 68.53901431
#> 3  1.30558184  0.45975569  0.24968026  0.66015027  1.58963228 59.34804452
#> 4  0.01593387  0.01985986  0.01890010  0.02136773  0.02010981  0.02340223
#> 5  0.01493801  0.01787387  0.01691062  0.01799387  0.01436415  0.01933227
#>        F8.avg     NIR.avg     VIS.avg
#> 1  0.00000000 0.000000000  0.08138610
#> 2 69.80591451 3.403664712 37.22971629
#> 3 57.34681909 0.720682303 50.17541353
#> 4  0.02057654 0.003291578  0.02034652
#> 5  0.01943340 0.003464819  0.01592337
```

## Documentation

HTML documentation for this package is available at
(<https://docs.r4photobiology.info/rYoctoPuceInOut/>), including a *User
Guide* and a description of the algorithms.

## Folder Structure

The folder structure of the Git repository is shown below. Folders
`.github`, `data-raw` and `articles` are not included in the built
package. Folder `data-raw` contains raw data and scripts mostly thought
as useful for future use. Folder `tests` contains files implementing
unit tests.

    rYoctoPuceInOut
    ├── .github 
    ├── data
    ├── data-raw
    ├─┬ inst
    │ └── extdata
    ├── man 
    ├── R
    ├─┬ tests
    │ └── testthat
    └─┬ vignettes
      └── articles

## Contributing

Please report bugs and request new features at
(<https://github.com/aphalo/rYoctoPuceInOut/issues>). Pull requests are
welcome at (<https://github.com/aphalo/rYoctoPuceInOut>).

## Citation

If you use ‘rYoctoPuceInOut’ to produce scientific or commercial
publications, acknowledge this by citing the package according to:

``` r
citation("rYoctoPuceInOut")
#> To cite package 'rYoctoPuceInOut' in publications use:
#> 
#>   Aphalo P (2026). _rYoctoPuceInOut: Logged Data File Import for
#>   YoctoPuce USB Modules_. R package version 0.0.1.9001,
#>   <https://docs.r4photobiology.info/rYoctoPuceInOut/>.
#> 
#> A BibTeX entry for LaTeX users is
#> 
#>   @Manual{,
#>     title = {rYoctoPuceInOut: Logged Data File Import for YoctoPuce USB Modules},
#>     author = {Pedro J. Aphalo},
#>     year = {2026},
#>     note = {R package version 0.0.1.9001},
#>     url = {https://docs.r4photobiology.info/rYoctoPuceInOut/},
#>   }
```

## License

© 2025-2026 Pedro J. Aphalo (<pedro.aphalo@helsinki.fi>). This software
carries no warranty of any kind even if carefully designed and coded.
The open source code is available for inspection.
