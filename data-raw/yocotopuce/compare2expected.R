library(photobiologyLEDs)
library(photobiologySensors)
library(AS7343)
library(ggspectra)

yocto_spectral.file <-
   system.file("extdata", "yocto-spectral-LED.csv",
               package = "AS7343", mustWork = TRUE)
observed.tb <- read_yocto_spctlog(yocto_spectral.file)
observed.mspct <- yocto_spectral2mspct(observed.tb)

autoplot(observed.mspct[[161]])
observed.spct <- observed.mspct[[161]]

q36.spct <- leds.mspct$Nichia_unknown_757
what_measured(q36.spct)
comment(q36.spct)
q36.spct <- normalize(q36.spct, norm = "undo")

expected <- simul_AS7343(q36.spct)

expected.vec <- expected[[2]]
names(expected.vec) <- expected[[1]]

# compute calibration factors
k <- expected.vec[observed.spct$channel] / observed.spct$counts

observed.spct$counts * k
