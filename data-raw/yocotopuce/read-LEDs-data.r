# channel by channel comparison of two different Yocto-Spectral
# one possibly with the factory calibration overwriten by mistake

library(dplyr)
library(ggplot2)
library(ggpmisc)
library(smatr)
library(lubridate)

file_names <- list.files("data-raw/yocotopuce/", pattern = "2025-12-03")

spectrl_01.tb <-
  read.csv(paste("data-raw/yocotopuce/", file_names[1], sep = ""), sep = ";") |>
  mutate(time = ymd_hms(Local.time, tz = "Europe/Helsinki"))
spectrl_02.tb <-
  read.csv(paste("data-raw/yocotopuce/", file_names[2], sep = ""), sep = ";") |>
  mutate(time = ymd_hms(Local.time, tz = "Europe/Helsinki"))

colnames(spectrl_01.tb)
head(spectrl_01.tb$time, 30)
head(spectrl_02.tb$time, 30)

spectrl_s01_s02.tb <-
  full_join(spectrl_01.tb, spectrl_02.tb, by = "Local.time")

ggplot(subset(spectrl_s01_s02.tb, spectralChannel13.avg.x < 500),
        aes(spectralChannel13.avg.x, spectralChannel13.avg.y)) +
  geom_point() +
  stat_poly_line(method = "sma", se = FALSE) +
  stat_poly_eq(mapping = use_label("eq", "R2", "n", "AIC"), method = "sma")

ggplot(subset(spectrl_s01_s02.tb, spectralChannel12.avg.x < 500),
       aes(spectralChannel12.avg.x, spectralChannel12.avg.y)) +
  geom_point() +
  stat_poly_line(method = "sma", se = FALSE) +
  stat_poly_eq(mapping = use_label("eq", "R2", "n", "AIC"), method = "sma")

ggplot(subset(spectrl_s01_s02.tb, spectralChannel11.avg.x < 500),
       aes(spectralChannel11.avg.x, spectralChannel11.avg.y)) +
  geom_point() +
  stat_poly_line(method = "sma", se = FALSE) +
  stat_poly_eq(mapping = use_label("eq", "R2", "n", "AIC"), method = "sma")

ggplot(subset(spectrl_s01_s02.tb, spectralChannel10.avg.x < 500),
       aes(spectralChannel10.avg.x, spectralChannel10.avg.y)) +
  geom_point() +
  stat_poly_line(method = "sma", se = FALSE) +
  stat_poly_eq(mapping = use_label("eq", "R2", "n", "AIC"), method = "sma")

ggplot(subset(spectrl_s01_s02.tb, spectralChannel9.avg.x < 500),
       aes(spectralChannel9.avg.x, spectralChannel9.avg.y)) +
  geom_point() +
  stat_poly_line(method = "sma", se = FALSE) +
  stat_poly_eq(mapping = use_label("eq", "R2", "n", "AIC"), method = "sma")

ggplot(subset(spectrl_s01_s02.tb, spectralChannel8.avg.x < 500),
       aes(spectralChannel8.avg.x, spectralChannel8.avg.y)) +
  geom_point() +
  stat_poly_line(method = "sma", se = FALSE) +
  stat_poly_eq(mapping = use_label("eq", "R2", "n", "AIC"), method = "sma")

ggplot(subset(spectrl_s01_s02.tb, spectralChannel7.avg.x < 500),
       aes(spectralChannel7.avg.x, spectralChannel7.avg.y)) +
  geom_point() +
  stat_poly_line(method = "sma", se = FALSE) +
  stat_poly_eq(mapping = use_label("eq", "R2", "n", "AIC"), method = "sma")

ggplot(subset(spectrl_s01_s02.tb, spectralChannel6.avg.x < 500),
       aes(spectralChannel6.avg.x, spectralChannel6.avg.y)) +
  geom_point() +
  stat_poly_line(method = "sma", se = FALSE) +
  stat_poly_eq(mapping = use_label("eq", "R2", "n", "AIC"), method = "sma")

ggplot(subset(spectrl_s01_s02.tb, spectralChannel5.avg.x < 500),
       aes(spectralChannel5.avg.x, spectralChannel5.avg.y)) +
  geom_point() +
  stat_poly_line(method = "sma", se = FALSE) +
  stat_poly_eq(mapping = use_label("eq", "R2", "n", "AIC"), method = "sma")

ggplot(subset(spectrl_s01_s02.tb, spectralChannel4.avg.x < 500),
       aes(spectralChannel4.avg.x, spectralChannel4.avg.y)) +
  geom_point() +
  stat_poly_line(method = "sma", se = FALSE) +
  stat_poly_eq(mapping = use_label("eq", "R2", "n", "AIC"), method = "sma")

ggplot(subset(spectrl_s01_s02.tb, spectralChannel3.avg.x < 500),
       aes(spectralChannel3.avg.x, spectralChannel3.avg.y)) +
  geom_point() +
  stat_poly_line(method = "sma", se = FALSE) +
  stat_poly_eq(mapping = use_label("eq", "R2", "n", "AIC"), method = "sma")

ggplot(subset(spectrl_s01_s02.tb, spectralChannel2.avg.x < 500),
       aes(spectralChannel2.avg.x, spectralChannel2.avg.y)) +
  geom_point() +
  stat_poly_line(method = "sma", se = FALSE) +
  stat_poly_eq(mapping = use_label("eq", "R2", "n", "AIC"), method = "sma")

ggplot(subset(spectrl_s01_s02.tb, spectralChannel1.avg.x < 500),
       aes(spectralChannel1.avg.x, spectralChannel1.avg.y)) +
  geom_point() +
  stat_poly_line(method = "sma", se = FALSE) +
  stat_poly_eq(mapping = use_label("eq", "R2", "n", "AIC"), method = "sma")

## time series

ggplot(spectrl_01.tb, aes(time, spectralChannel1.avg)) +
         geom_point(alpha = 0.25) +
  stat_panel_counts()

ggplot(spectrl_02.tb, aes(time, spectralChannel2.avg)) +
  geom_point(alpha = 0.25) +
  stat_panel_counts()

ggplot(spectrl_02.tb, aes(time, spectralChannel3.avg)) +
  geom_point(alpha = 0.25) +
  stat_panel_counts()

ggplot(spectrl_02.tb, aes(time, spectralChannel4.avg)) +
  geom_point(alpha = 0.25) +
  stat_panel_counts()

ggplot(spectrl_02.tb, aes(time, spectralChannel5.avg)) +
  geom_point(alpha = 0.25) +
  stat_panel_counts()

ggplot(spectrl_02.tb, aes(time, spectralChannel7.avg)) +
  geom_point(alpha = 0.25) +
  stat_panel_counts()

# VIS
ggplot(spectrl_02.tb, aes(time, spectralChannel13.avg)) +
  geom_point(alpha = 0.25) +
  stat_panel_counts()

ggplot(subset(spectrl_02.tb,
              time > ymd_h("2025-12-01 20", tz = "Europe/Helsinki") &
                time < ymd_h("2025-12-02 03", tz = "Europe/Helsinki")),
       aes(time, spectralChannel13.avg)) +
  geom_point(alpha = 0.25) +
  geom_line() +
  stat_panel_counts()

ggplot(subset(spectrl_02.tb,
              time > ymd_h("2025-12-01 20", tz = "Europe/Helsinki") &
                time < ymd_h("2025-12-02 03", tz = "Europe/Helsinki")),
       aes(time, spectralChannel13.avg)) +
  geom_point(alpha = 0.25) +
  geom_line() +
  stat_panel_counts()

# NIR
ggplot(spectrl_02.tb, aes(time, spectralChannel12.avg)) +
  geom_point(alpha = 0.25) +
  stat_panel_counts()

ggplot(subset(spectrl_02.tb,
              time > ymd_h("2025-12-01 20", tz = "Europe/Helsinki") &
                time < ymd_h("2025-12-02 03", tz = "Europe/Helsinki")),
       aes(time, spectralChannel12.avg)) +
  geom_point(alpha = 0.25) +
  geom_line() +
  stat_panel_counts()

ggplot(subset(spectrl_02.tb,
              time > ymd_h("2025-12-01 20", tz = "Europe/Helsinki") &
                time < ymd_h("2025-12-02 03", tz = "Europe/Helsinki")),
       aes(time, spectralChannel12.avg)) +
  geom_point(alpha = 0.25) +
  geom_line() +
  stat_panel_counts()
