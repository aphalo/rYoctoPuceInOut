library(photobiology)
library(photobiologyInOut)
library(ggpp)
library(dplyr)
library(lubridate)

chn.names.AS7343 <-
  c("F1", "F2", "FZ", "F3", "F4", "FY", "F5", "FXL", "F6", "F7", "F8", "NIR", "VIS")

chn.wls.AS7343 <-
  c(405, 425, 450, 475, 515, 555, 550, 600, 640, 690, 745, 855, 600)
names(chn.wls.AS7343) <- chn.names.AS7343

## Data sun through window?

simo_01.tb <- read.csv(file = "data-raw/yocotopuce/data-spectrl-01-simo.csv", sep = ";")
simo_01.tb <- subset(simo_01.tb, select = !grepl("min|max", names(simo_01.tb)))
names(simo_01.tb) <- gsub("spectralChannel", "ch", names(simo_01.tb))
names(simo_01.tb)[-(1:2)] <- paste("s01", names(simo_01.tb)[-(1:2)], sep = "_")
names(simo_01.tb)

simo_02.tb <- read.csv(file = "data-raw/yocotopuce/data-spectrl-01-simo.csv", sep = ";")
simo_02.tb <- subset(simo_02.tb, select = !grepl("min|max", names(simo_02.tb)))
names(simo_02.tb) <- gsub("spectralChannel", "ch", names(simo_02.tb))
names(simo_02.tb)[-(1:2)] <- paste("s02", names(simo_02.tb)[-(1:2)], sep = "_")
names(simo_02.tb)

simo.tb <- full_join(simo_01.tb, simo_02.tb)

## Data Lumitronix Q-36 LED array Auto-gain test
# times are out of sync!!

Q36_01.tb <-
  read.csv(file = "data-raw/yocotopuce/spctrl-01-Lumitronix-Q36-150mm.csv", sep = ";")
Q36_01.tb <- subset(Q36_01.tb, select = !grepl("min|max", names(Q36_01.tb)))
names(Q36_01.tb) <- gsub("spectralChannel", "ch", names(Q36_01.tb))
names(Q36_01.tb)[-(1:2)] <- paste("s01", names(Q36_01.tb)[-(1:2)], sep = "_")
names(Q36_01.tb)
Q36_01.tb |>
  mutate(time.EET = with_tz(ymd_hms(Local.time, tz = "UTC"),
                            tzone = "Europe/Helsinki")) |>
  subset(month(time.EET) == 10) -> Q36_01.tb

time.s01.at.max <- with(Q36_01.tb, time.EET[s01_ch12.avg == (max(s01_ch12.avg))])

Q36_02.tb <- read.csv(file = "data-raw/yocotopuce/spctrl-02-Lumitronix-Q36-150mm.csv", sep = ";")
Q36_02.tb <- subset(Q36_02.tb, select = !grepl("min|max", names(Q36_02.tb)))
names(Q36_02.tb) <- gsub("spectralChannel", "ch", names(Q36_02.tb))
names(Q36_02.tb)[-(1:2)] <- paste("s02", names(Q36_02.tb)[-(1:2)], sep = "_")
names(Q36_02.tb)
Q36_02.tb |>
  mutate(time.EET = with_tz(ymd_hms(Local.time, tz = "UTC"),
                            tzone = "Europe/Helsinki")) |>
  subset(month(time.EET) == 10) -> Q36_02.tb

time.s02.at.max <- with(Q36_02.tb, time.EET[s02_ch12.avg == (max(s02_ch12.avg))])

time.s02.at.max - time.s01.at.max

Q36_01.tb$time.EET <- Q36_01.tb$time.EET + (time.s02.at.max - time.s01.at.max)

Q36.tb <- full_join(Q36_01.tb, Q36_02.tb, by = "time.EET")

ggplot(Q36.tb, aes(time.EET)) +
  geom_point(aes(y = s01_ch12.avg)) +
  geom_point(aes(y = s02_ch12.avg), colour = "red") +
  geom_line(aes(y = s01_ch12.avg)) +
  geom_line(aes(y = s02_ch12.avg), colour = "red")

Q36.tb |>
  select(time.EET, s01_ch12.avg)

Q36.tb |>
  select(time.EET, s02_ch12.avg)
