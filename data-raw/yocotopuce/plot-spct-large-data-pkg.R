library(ggspectra)
library(dplyr)
library(lubridate)
library(AS7343)

## Data Lumitronix Q-36 LED array Auto-gain test
# times are out of sync!!

Q36_01.tb <-
  read_yocto_spectral_csv(file = "data-raw/yocotopuce/spectrl-01-2025-12-02.csv",
                          tz = "Europe/Helsinki")

Q36_02.tb <-
  read_yocto_spectral_csv(file = "data-raw/yocotopuce/spectrl-02-2025-12-02.csv",
                          tz = "Europe/Helsinki")

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

ggplot(Q36_01.tb, aes(time)) +
  geom_point(aes(y = NIR.avg)) +
  geom_point(aes(y = NIR.avg), colour = "red") +
  geom_line(aes(y = NIR.avg)) +
  geom_line(aes(y = NIR.avg), colour = "red")

ggplot(Q36_01.tb, aes(time)) +
  geom_point(aes(y = F1.avg)) +
  geom_point(aes(y = F1.avg), colour = "red") +
  geom_line(aes(y = F1.avg)) +
  geom_line(aes(y = F1.avg), colour = "red")

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

