library(photobiologySensors)

# Nominal values from data sheet
chn.names.AS7343 <-
  c("F1", "F2", "FZ", "F3", "F4", "FY", "F5", "FXL", "F6", "F7", "F8", "NIR", "VIS")
chn.wls.AS7343 <-
  c(405, 425, 450, 475, 515, 555, 550, 600, 640, 690, 745, 855, 620)
names(chn.wls.AS7343) <- chn.names.AS7343

wl.order <- order(chn.wls.AS7343)
names.ordered <- chn.names.AS7343[wl.order]
center.wls <- chn.wls.AS7343

sensor.spct <- normalize(sensors.mspct$ams_AS7343, norm = "undo")

peaks.df <- peaks(sensor.spct, span = NULL)
wls.peaks <- peaks.df$w.length
names(wls.peaks) <- peaks.df$channel
wls.peaks <- wls.peaks[chn.names.AS7343] |> round(1)

wls.HM.df <- wls_at_target(sensor.spct)
wls.HM <- wls.HM.df$w.length
names(wls.HM) <- wls.HM.df$channel

wls.HM.left <- wls.HM[c(TRUE, FALSE)][chn.names.AS7343] |> round(1)
wls.HM.right <- wls.HM[c(FALSE, TRUE)][chn.names.AS7343] |> round(1)
HMFW <- wls.HM.right - wls.HM.left

wls.10.df <- wls_at_target(sensor.spct, target = "0.1max")
wls.10 <- wls.10.df$w.length
names(wls.10) <- wls.10.df$channel

wls.10.left <- wls.10[c(TRUE, FALSE)][chn.names.AS7343] |> round(1)
wls.10.right <- wls.10[c(FALSE, TRUE)][chn.names.AS7343] |> round(1)
TMFW <- wls.10.right - wls.10.left

wls.05.df <- wls_at_target(sensor.spct, target = "0.05max")
wls.05 <- wls.05.df$w.length
names(wls.05) <- wls.05.df$channel

wls.05.left <- wls.05[c(TRUE, FALSE)][chn.names.AS7343] |> round(1)
wls.05.right <- wls.05[c(FALSE, TRUE)][chn.names.AS7343] |> round(1)
FMFW <- wls.05.right - wls.05.left

y_spectral.descriptor <-
  list(sensor.descriptor = attr(sensors.mspct$ams_AS7343, "sensor.properties"),
       channels = chn.names.AS7343,
       center.wl = center.wls,
       peak.wl = wls.peaks,
       wl.HM.left = wls.HM.left,
       wl.HM.right = wls.HM.right,
       HMFW = wls.HM.right - wls.HM.left,
       wl.10.left = wls.10.left,
       wl.10.right = wls.10.right,
       TMFW = wls.10.right - wls.10.left
  )

save(y_spectral.descriptor, file = "./data/modules-metadata.rda")
