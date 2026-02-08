library(photobiology)
library(photobiologyInOut)
library(ggspectra)
library(dplyr)
library(lubridate)
library(AS7343)

## Data Lumitronix Q-36 LED array Auto-gain test
# times are out of sync!!

Q36_01.tb <-
  read_yocto_spctlog(file = "data-raw/yocotopuce/spctrl-01-Lumitronix-Q36-150mm.csv",
                          tz = "Europe/Helsinki")
Q36_01.tb <- subset(Q36_01.tb, select = !grepl("min|max", names(Q36_01.tb))) |>
  subset(month(time) == 10)

time.s01.at.max <- with(Q36_01.tb, time[VIS.avg == (max(VIS.avg))])

Q36_02.tb <-
  read_yocto_spctlog(file = "data-raw/yocotopuce/spctrl-02-Lumitronix-Q36-150mm.csv",
                          tz = "Europe/Helsinki")
Q36_02.tb <- subset(Q36_02.tb, select = !grepl("min|max", names(Q36_02.tb))) |>
  subset(month(time) == 10)

time.s02.at.max <- with(Q36_02.tb, time[VIS.avg == (max(VIS.avg))])


time.s02.at.max - time.s01.at.max

Q36_01.tb$time <- Q36_01.tb$time + (time.s02.at.max - time.s01.at.max)

ggplot(Q36_01.tb, aes(time)) +
  geom_point(aes(y = VIS.avg)) +
  geom_point(aes(y = VIS.avg), colour = "red") +
  geom_line(aes(y = VIS.avg)) +
  geom_line(aes(y = VIS.avg), colour = "red")

ggplot(Q36_02.tb, aes(time)) +
  geom_point(aes(y = VIS.avg)) +
  geom_point(aes(y = VIS.avg), colour = "red") +
  geom_line(aes(y = VIS.avg)) +
  geom_line(aes(y = VIS.avg), colour = "red")

Q36.mspct <- yocto_spectral2mspct(Q36_01.tb, "narrow")

autoplot(Q36.mspct[[30]], geom = "col", range = c(350, 900))
autoplot(Q36.mspct[[30]], geom = c("point", "line"), range = c(350, 900))

Q36.mspct <- yocto_spectral2mspct(Q36_01.tb, "wide")

autoplot(Q36.mspct[[30]], geom = "col", range = c(250, 1000))

Q36.mspct <- yocto_spectral2mspct(Q36_01.tb, "xyz")

autoplot(Q36.mspct[[30]], geom = "col", range = c(250, 1000))

Q36.mspct <- yocto_spectral2mspct(Q36_01.tb, "all")

autoplot(Q36.mspct[[30]], geom = "col")
autoplot(Q36.mspct[[30]], geom = c("point", "line"), range = c(350, 900))

