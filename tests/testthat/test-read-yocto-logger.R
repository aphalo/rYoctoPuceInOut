## thorough test with Yocto-Meteo data

yocto_meteo.file <-
  system.file("extdata", "yocto-meteo-snm.csv",
              package = "rYoctoPuceInOut", mustWork = TRUE)
yocto_meteo_json.file <-
  system.file("extdata", "METEOMK2-19A230.json",
              package = "rYoctoPuceInOut", mustWork = TRUE)

test_that("Yocto-Meteo CSV file is read correctly", {
  expect_no_error(meteo1.df <- read_yocto_logger_csv(yocto_meteo.file))
  expect_false(anyNA(meteo1.df))
  expect_s3_class(meteo1.df, "data.frame")
  expect_equal(dim(meteo1.df), c(199, 10))
  expect_named(meteo1.df, c("time", "humidity.min", "humidity.avg",
                            "humidity.max", "pressure.min", "pressure.avg",
                            "pressure.max", "temperature.min",
                            "temperature.avg", "temperature.max"))
  expect_equal(length(comment(meteo1.df)), 1L)
  expect_true(grepl("YoctoPuce USB module type 'unknown'", how_measured(meteo1.df)))
})

test_that("columns from Yocto-Meteo CSV file are read correctly", {
  expect_no_error(
    meteo2.df <- read_yocto_logger_csv(yocto_meteo.file, cols.pattern = "\\.avg$"))
  expect_false(anyNA(meteo2.df))
  expect_s3_class(meteo2.df, "data.frame")
  expect_equal(dim(meteo2.df), c(199, 4))
  expect_named(meteo2.df,
               c("time", "humidity.avg", "pressure.avg", "temperature.avg"))
  expect_equal(length(comment(meteo2.df)), 1L)
  expect_true(grepl("YoctoPuce USB module type 'unknown'", how_measured(meteo2.df)))

  # no columns from CSV file removed
  expect_no_error(meteo2NA.df <-
                    read_yocto_logger_csv(yocto_meteo.file, cols.pattern = NA))
  expect_equal(dim(meteo2NA.df), c(199, 12))
  # check names of columns
  expect_named(meteo2NA.df,
               c("UNIX.time", "Local.time",
                 "humidity.min", "humidity.avg",
                 "humidity.max", "pressure.min", "pressure.avg",
                 "pressure.max", "temperature.min",
                 "temperature.avg", "temperature.max", "time")
               )

})

test_that("Yocto-Meteo CSV and JSON files are read correctly", {
  expect_no_error(
    meteo3.df <- read_yocto_logger_csv(yocto_meteo.file, yocto_meteo_json.file))
  expect_silent(settings <- attr(meteo3.df, "yocto.module.settings", exact = TRUE))
  expect_false(anyNA(meteo3.df))
  expect_s3_class(meteo3.df, "data.frame")
  expect_equal(dim(meteo3.df), c(199, 10))
  expect_named(meteo3.df,
               c("time", "humidity.min", "humidity.avg",
                 "humidity.max", "pressure.min", "pressure.avg",
                 "pressure.max", "temperature.min",
                 "temperature.avg", "temperature.max"))
  expect_equal(length(comment(meteo3.df)), 1L)
  expect_true(grepl("METEOMK2-19A230", how_measured(meteo3.df)))

  expect_silent(
    settings_meteo3 <- attr(meteo3.df, "yocto.module.settings", exact = TRUE))
  expect_named(settings_meteo3,  c("api", "extras", "files"))
  expect_named(settings_meteo3[["api"]],
               c( "module", "humidity", "pressure", "temperature", "dataLogger"))
})

test_that("columns from Yocto-Meteo CSV and JSON files are named correctly", {
  expect_no_error(meteo4.df <-
                    read_yocto_logger_csv(yocto_meteo.file,
                                          yocto_meteo_json.file,
                                          cols.logical.names = TRUE))
  expect_false(anyNA(meteo4.df))
  expect_s3_class(meteo4.df, "data.frame")
  expect_equal(dim(meteo4.df), c(199, 10))
  expect_named(meteo4.df, c("time", "C2_humidity.min", "C2_humidity.avg",
                            "C2_humidity.max", "C2_pressure.min",
                            "C2_pressure.avg", "C2_pressure.max",
                            "C2_temperature.min", "C2_temperature.avg",
                            "C2_temperature.max"))
  expect_equal(length(comment(meteo4.df)), 1L)
  expect_true(grepl("METEOMK2-19A230", how_measured(meteo4.df)))
})

## test with Yocto-Spectral data

yocto_spectral.file <-
  system.file("extdata", "yocto-spectral-LED.csv",
              package = "rYoctoPuceInOut", mustWork = TRUE)
yocto_spectral_json.file <-
  system.file("extdata", "SPECTRL1-2CF3B6.json",
              package = "rYoctoPuceInOut", mustWork = TRUE)

test_that("Yocto-Spectral CSV file is read correctly", {
  expect_no_error(spectral1.df <- read_yocto_logger_csv(yocto_spectral.file))
  expect_false(anyNA(spectral1.df))
  expect_s3_class(spectral1.df, "data.frame")
  expect_equal(dim(spectral1.df), c(199, 40))
  # check names of first 5 columns
  expect_equal(names(spectral1.df)[1:7],
               c("time", "spectralChannel1.min", "spectralChannel1.avg",
                 "spectralChannel1.max", "spectralChannel2.min",
                 "spectralChannel2.avg", "spectralChannel2.max"))
  expect_equal(length(comment(spectral1.df)), 1L)
  expect_true(grepl("YoctoPuce USB module type 'unknown'", how_measured(spectral1.df)))
})

test_that("columns from Yocto-Spectral CSV file are read correctly", {
  expect_warning(read_yocto_logger_csv(yocto_spectral.file, cols.pattern = "bad$"))

  expect_no_error(spectral2.df <-
                    read_yocto_logger_csv(yocto_spectral.file, cols.pattern = "\\.avg$"))
  expect_false(anyNA(spectral2.df))
  expect_s3_class(spectral2.df, "data.frame")
  expect_equal(dim(spectral2.df), c(199, 14))
  # check names of first 5 columns
  expect_equal(names(spectral2.df),
               c("time", paste("spectralChannel", 1:13, ".avg", sep = "")))
  expect_equal(length(comment(spectral2.df)), 1L)
  expect_true(grepl("YoctoPuce USB module type 'unknown'", how_measured(spectral2.df)))
})

test_that("columns from Yocto-Spectral CSV and JSON files are named correctly", {
  expect_no_error(spectral4.df <-
                    read_yocto_logger_csv(yocto_spectral.file,
                                          yocto_spectral_json.file,
                                          cols.pattern = "\\.avg$",
                                          cols.logical.names = TRUE))
  expect_false(anyNA(spectral4.df))
  expect_s3_class(spectral4.df, "data.frame")
  expect_equal(dim(spectral4.df), c(199, 14))
  expect_named(spectral4.df, c("time", "F1.avg", "F2.avg", "FZ.avg", "F3.avg",
                               "F4.avg", "FY.avg", "F5.avg", "FXL.avg",
                               "F6.avg", "F10.avg", "F11.avg", "F12.avg", "F13.avg"))
  expect_equal(length(comment(spectral4.df)), 1L)
  expect_true(grepl("SPECTRL1-2CF3B6", how_measured(spectral4.df)))
})

## test with Yocto-Bridge data

yocto_bridge.file <-
  system.file("extdata", "yocto-bridge-with-weighing-platform.csv",
              package = "rYoctoPuceInOut", mustWork = TRUE)
yocto_bridge_json.file <-
  system.file("extdata", "YWBRIDG1-12683B.json",
              package = "rYoctoPuceInOut", mustWork = TRUE)

test_that("Yocto-bridge CSV file is read correctly", {
  expect_no_error(bridge1.df <- read_yocto_logger_csv(yocto_bridge.file))
  expect_false(anyNA(bridge1.df))
  expect_s3_class(bridge1.df, "data.frame")
  expect_equal(dim(bridge1.df), c(53, 4))
  # check names of first 5 columns
  expect_equal(names(bridge1.df),
               c("time", "weighScale1.min", "weighScale1.avg", "weighScale1.max"))
  expect_equal(length(comment(bridge1.df)), 1L)
  expect_true(grepl("YoctoPuce USB module type 'unknown'", how_measured(bridge1.df)))
})

test_that("columns from Yocto-bridge CSV file are read correctly", {
  expect_warning(read_yocto_logger_csv(yocto_bridge.file, cols.pattern = "bad$"))

  expect_no_error(bridge2.df <-
                    read_yocto_logger_csv(yocto_bridge.file, cols.pattern = "\\.avg$"))
  expect_false(anyNA(bridge2.df))
  expect_s3_class(bridge2.df, "data.frame")
  expect_equal(dim(bridge2.df), c(53, 2))
  # check names of first 5 columns
  expect_equal(names(bridge2.df),
               c("time", "weighScale1.avg"))
  expect_equal(length(comment(bridge2.df)), 1L)
  expect_true(grepl("YoctoPuce USB module type 'unknown'", how_measured(bridge2.df)))
})

test_that("columns from Yocto-Bridge CSV and JSON files are named correctly", {
  expect_no_error(bridge4.df <-
                    read_yocto_logger_csv(yocto_bridge.file,
                                          yocto_bridge_json.file,
                                          cols.pattern = "\\.avg$",
                                          cols.logical.names = TRUE))
  expect_false(anyNA(bridge4.df))
  expect_s3_class(bridge4.df, "data.frame")
  expect_equal(dim(bridge4.df), c(53, 2))
  expect_named(bridge4.df, c("time", "C3_DS_weight.avg"))
  expect_equal(length(comment(bridge4.df)), 1L)
  expect_true(grepl("YWBRIDG1-12683B", how_measured(bridge4.df)))
})

## test with YoctoCO2 V1 data

yocto_CO2.file <-
  system.file("extdata", "yocto-CO2-V1.csv",
              package = "rYoctoPuceInOut", mustWork = TRUE)
yocto_CO2_json.file <-
  system.file("extdata", "YCO2MK01-39F22.json",
              package = "rYoctoPuceInOut", mustWork = TRUE)

test_that("Yocto-CO2 CSV file is read correctly", {
  expect_no_error(co21.df <- read_yocto_logger_csv(yocto_CO2.file))
  expect_false(anyNA(co21.df))
  expect_s3_class(co21.df, "data.frame")
  expect_equal(dim(co21.df), c(199, 4))
  # check names of first 5 columns
  expect_equal(names(co21.df),
               c("time", "carbonDioxide.min",
                 "carbonDioxide.avg", "carbonDioxide.max"))
  expect_equal(length(comment(co21.df)), 1L)
  expect_true(grepl("YoctoPuce USB module type 'unknown'", how_measured(co21.df)))
})

test_that("columns from Yocto-CO2 CSV file are read correctly", {
  expect_warning(read_yocto_logger_csv(yocto_CO2.file, cols.pattern = "bad$"))

  expect_no_error(co22.df <-
                    read_yocto_logger_csv(yocto_CO2.file, cols.pattern = "\\.avg$"))
  expect_false(anyNA(co22.df))
  expect_s3_class(co22.df, "data.frame")
  expect_equal(dim(co22.df), c(199, 2))
  # check names of first 5 columns
  expect_equal(names(co22.df),
               c("time", "carbonDioxide.avg"))
  expect_equal(length(comment(co22.df)), 1L)
  expect_true(grepl("YoctoPuce USB module type 'unknown'", how_measured(co22.df)))
})

test_that("columns from Yocto-CO2 CSV and JSON files are named correctly", {
  expect_no_error(co24.df <-
                    read_yocto_logger_csv(yocto_CO2.file,
                                          yocto_CO2_json.file,
                                          cols.pattern = "\\.avg$",
                                          cols.logical.names = TRUE))
  expect_false(anyNA(co24.df))
  expect_s3_class(co24.df, "data.frame")
  expect_equal(dim(co24.df), c(199, 2))
  expect_named(co24.df, c("time", "carbonDioxide.avg"))
  expect_equal(length(comment(co24.df)), 1L)
  expect_true(grepl("YCO2MK01-39F22", how_measured(co24.df)))
})

## test with Yocto-I2C data

yocto_i2c.file <-
  system.file("extdata", "yocto-i2c-tsl2591.csv",
              package = "rYoctoPuceInOut", mustWork = TRUE)


## test with Yocto-I2C data

yocto_i2c.file <-
  system.file("extdata", "yocto-i2c-tsl2591.csv",
              package = "rYoctoPuceInOut", mustWork = TRUE)
yocto_i2c_settings.file <-
  system.file("extdata", "YI2CMK01-1214E2.json",
              package = "rYoctoPuceInOut", mustWork = TRUE)

# test with Yocto-SPI data



## test with Yocto-Serial data

yocto_serial.file <-
  system.file("extdata", "yocto-serial.csv",
              package = "rYoctoPuceInOut", mustWork = TRUE)
yocto_serial_settings.file <-
  system.file("extdata", "YSERIAL1-EAD24.json",
              package = "rYoctoPuceInOut", mustWork = TRUE)

## test with Yocto-Serial data

yocto_serial.file <-
  system.file("extdata", "yocto-serial.csv",
              package = "rYoctoPuceInOut", mustWork = TRUE)
yocto_serial_settings.file <-
  system.file("extdata", "YSERIAL1-EAD24.json",
              package = "rYoctoPuceInOut", mustWork = TRUE)

# test with Yocto-0-10V-Rx data

yocto_v010.file <-
  system.file("extdata", "yocto-0-10V-Rx.csv",
              package = "rYoctoPuceInOut", mustWork = TRUE)
yocto_v010_json.file <-
  system.file("extdata", "RX010V01-1219AD.json",
              package = "rYoctoPuceInOut", mustWork = TRUE)

# test with Yocto-Millivolt-Rx data

yocto_mv.file <-
  system.file("extdata", "yocto-millivolt-Rx.csv",
              package = "rYoctoPuceInOut", mustWork = TRUE)
yocto_mv_json.file <-
  system.file("extdata", "RXMVOLT1-3EEE1.json",
              package = "rYoctoPuceInOut", mustWork = TRUE)



