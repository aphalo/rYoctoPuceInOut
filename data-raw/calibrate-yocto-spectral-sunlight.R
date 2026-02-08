library(photobiology)
library(photobiologyWavebands)
library(photobiologyPlants)
library(ggspectra)
library(rYoctoPuceInOut)
library(dplyr)

# calibration of YoctoSpectral sensors

file.paths <- list.files(path = "data-raw/acq-irrad-2025-08-14/",
                         pattern = "spct\\.Rda",
                         full.names = TRUE)

for (f in file.paths) {
  load(f)
}

rm(list = ls(pattern = "raw_mspct$"))

all.mspct <- collect2mspct()

all.mspct <- subset2mspct(all.mspct)

time_1240.spct <- all.mspct$sun_cosine_002.spct.time.08

maya_irrads.tb <-
  q_irrad(all.mspct,
          c(list(PAR(), PAR("ePAR"), VIS(), NIR(),
                 Red("Sellaro"), Far_red("Sellaro")),
            Plant_bands()),
          scale.factor = 1e6,
          attr2tb = c("when.measured"))

time_1240.spct <- all.mspct$sun_cosine_002.spct.time.08
irrads_1240 <- q_irrad(time_1240.spct,
                       c(list(PAR(), PAR("ePAR"), VIS(), NIR(),
                              Red("Sellaro"), Far_red("Sellaro")),
                         Plant_bands()),
                       scale.factor = 1e6)

AS7343_simulated.df <-
  simul_AS7343(time_1240.spct, unit.out = "photon", scale.factor = 1e-6)

colnames(AS7343_simulated.df) <- c("channel", "q.response")
AS7343_simulated.df$q.response.rel <-
  AS7343_simulated.df$q.response / AS7343_simulated.df$q.response[[13]]

yocto_s01_raw.tb <-
  read_yocto_spctlog("data-raw/yocotopuce/viikki-field-s01-2025-08-14.csv")
yocto_s02_raw.tb <-
  read_yocto_spctlog("data-raw/yocotopuce/viikki-field-s02-2025-08-14.csv")

yocto_s01_measured <- t(yocto_s01_raw.tb[37, -1])
yocto_s02_measured <- t(yocto_s02_raw.tb[37, -1])

AS7343_observed.df <- as.data.frame(cbind(yocto_s01_measured, yocto_s02_measured))
AS7343_observed.df$channel <- gsub("\\.avg$", "", rownames(AS7343_observed.df))
colnames(AS7343_observed.df)[1:2] <- c("s01.raw", "s02.raw")
AS7343_observed.df$s01.raw.rel <-
  AS7343_observed.df$s01.raw / AS7343_observed.df$s01.raw[[13]]
AS7343_observed.df$s02.raw.rel <-
  AS7343_observed.df$s02.raw / AS7343_observed.df$s02.raw[[13]]

AS7343_data.df <-
  full_join(AS7343_simulated.df, AS7343_observed.df)

# PAR
PAR_k_s01_VIS <- 1180 / 9296
PAR_k_s02_VIS <- 1180 / 10117

# correction for computing the difference between PAR and ePAR
# which in sunlight is nearly identical to FR with Sellaro's defintion
e_k_s01__F8 <- (1377 - 1180) / 3422
e_k_s02__F8 <- (1377 - 1180) / 3390

# ePAR
ePAR_k_s01_VIS <- 1377 / 9296
ePAR_k_s02_VIS <- 1377 / 10117

# Smith 20
FR_k_s01_F8 <- 72.4 / 3422
FR_k_s02_F8 <- 72.4 / 3390

R_k_s01_F6.F7 <- 88.1 / (5340 + 6462)
R_k_s02_F6.F7 <- 88.1 / (5869 + 6735)

# Sellaro
FR_k_s01_F8 <- 197.87 / 3422
FR_k_s02_F8 <- 197.87 / 3390

R_k_s01_F6.F7 <- 265.15 / (5340 + 6462)
R_k_s02_F6.F7 <- 265.15 / (5869 + 6735)

R_k_s01_F6.FXL <- 265.15 / 5263
R_k_s02_F6.FXL <- 265.15 / 5811

G_k_s01_FY <- 289.3 / 5660
G_k_s02_FY <- 289.3 / 6254

B_k_s01_FZ <- 235.9 / 3223
B_k_s02_FZ <- 235.9 / 3412

# CIE
UVA1_k_s01_F1 <- 72.16 / 1439
UVA1_k_s01_F1 <- 72.16 / 1692

AS7343_metadata()
