test_that("calibrations are applied correctly", {
  # example calibration list based on parallel measurements of sunlight
  s01.cal <-
    list(module.type = "YoctoSpectral",
         module.sn = "SPECTRL1-2CF3B6",
         module.name = "spectrl-01",
         Q_PAR = list(VIS.avg = 1180 / 9296),
         Q_ePAR = list(VIS.avg = 1377 / 9296),
         Q_FarRed.Sellaro = list(F8.avg = 197.87 / 3422),
         Q_Red.Sellaro = list(F6.avg = 265.15 / 5340 * (5340 / (5340 + 6462)),
                              F7.avg = 265.15 / 6462 * (6462 / (5340 + 6462))),
         Q_Green.Sellaro = list(FY.avg = 289.3 / 5660),
         Q_Blue.Sellaro = list(FZ.avg = 235.9 / 3223))

  # path to example logged data from a Yocto-Spectral USN module
  yocto_spectral.file <-
    system.file("extdata", "yocto-spectral-LED.csv",
                package = "rYoctoPuceInOut", mustWork = TRUE)
  yocto_spectral_json.file <-
    system.file("extdata", "SPECTRL1-2CF3B6.json",
                package = "rYoctoPuceInOut", mustWork = TRUE)

  # read data and apply calibration
  yocto_data.df <- head(read_yocto_spctlog(yocto_spectral.file), 5)

  irrads.df <- calc_calibrated_qtys(yocto_data.df, s01.cal)
  expect_s3_class(irrads.df, "data.frame")
  expect_equal(dim(irrads.df), c(5, 7))
  expect_named(irrads.df,
               c("time", "Q_PAR", "Q_ePAR", "Q_FarRed.Sellaro",
                 "Q_Red.Sellaro", "Q_Green.Sellaro", "Q_Blue.Sellaro"))
})
