library(ggspectra)
library(dplyr)
library(lubridate)
library(rYoctoPuceInOut)

## Data Lumitronix Q-36 LED array Auto-gain test
# times are out of sync!!

data1.df <-
  read_yocto_spctlog(file = "data-raw/yocotopuce/spectrl-01-2025-12-02.csv",
                          tz = "Europe/Helsinki")

data2.df <-
  read_yocto_spctlog(file = "data-raw/yocotopuce/spectrl-02-2025-12-02.csv",
                          tz = "Europe/Helsinki")

ggplot(data1.df, aes(time)) +
  geom_point(aes(y = VIS.avg)) +
  geom_point(aes(y = VIS.avg), colour = "red") +
  geom_line(aes(y = VIS.avg)) +
  geom_line(aes(y = VIS.avg), colour = "red")

ggplot(data2.df, aes(time)) +
  geom_point(aes(y = VIS.avg)) +
  geom_point(aes(y = VIS.avg), colour = "red") +
  geom_line(aes(y = VIS.avg)) +
  geom_line(aes(y = VIS.avg), colour = "red")

ggplot(data1.df, aes(time)) +
  geom_point(aes(y = NIR.avg)) +
  geom_point(aes(y = NIR.avg), colour = "red") +
  geom_line(aes(y = NIR.avg)) +
  geom_line(aes(y = NIR.avg), colour = "red")

ggplot(data1.df, aes(time)) +
  geom_point(aes(y = F1.avg)) +
  geom_point(aes(y = F1.avg), colour = "red") +
  geom_line(aes(y = F1.avg)) +
  geom_line(aes(y = F1.avg), colour = "red")

data1_all.mspct <- yocto_spectral2mspct(data1.df)

autoplot(data1_all.mspct[[30]], geom = "col", range = c(350, 900))
autoplot(data1_all.mspct[[30]], geom = c("point", "line"), range = c(350, 900))

data1_narrow.mspct <- yocto_spectral2mspct(data1.df, "narrow")

autoplot(data1_narrow.mspct[[30]], geom = "col", range = c(350, 900))
autoplot(data1_narrow.mspct[[30]], geom = c("point", "line"), range = c(350, 900))

data1_wide.mspct <- yocto_spectral2mspct(data1.df, "wide")

autoplot(data1_wide.mspct[[30]], geom = "col", range = c(250, 1000))

data1_xyz.mspct <- yocto_spectral2mspct(data1.df, "xyz")

autoplot(data1_xyz.mspct[[30]], geom = "col", range = c(250, 1000))
