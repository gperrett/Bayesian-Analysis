library(tidyverse)
library(haven)
library(mirt)

set.seed(12345)
options(scipen = 999)

trust <- read_sav("W35_May18/ATP W35.sav")
trust <- na_if(trust, 99)

trust <- trust %>% select(
  "TC5_W35", # should they be more or less regulated
  "TC6A_W35", #support gender
  "TC6B_W35", # support a ideology
  "SNSA_W35", #use facebook 
  "SNSB_W35", # twitter
  "SNSE_W35", # insta
  "SM6A_W35", #bullying v support
  "F_SEX_FINAL", #sex
  "F_CREGION_FINAL", # region
  "F_EDUCCAT_FINAL", # education 
  "F_RACECMB_RECRUITMENT", #race
  "F_AGECAT_FINAL", # age
  "F_INCOME_FINAL", # income
  "F_IDEO_FINAL", # ideology
  "SM6A_W35",
  "SM6B_W35",
contains("SM"),
-contains("SM3"),
 "WEIGHT_W35"
 )

trust <- drop_na(trust)

instrument <- trust %>% select(contains("SM"), 
                               -contains("SM6"),
                                  -contains("SM3"))

instrument <- instrument %>% select(1:16, 23:27, 33:43, everything())


instrument_irt_12 <- mirt(instrument, model = 12, itemtype = "gpcm", method = "QMCEM", 
                          technical = list(NCYCLES = 1000))

instrument_irt_11 <- mirt(instrument, model = 11, itemtype = "gpcm", method = "QMCEM", 
                          technical = list(NCYCLES = 1000))

instrument_irt_10 <- mirt(instrument, model = 10, itemtype = "gpcm", method = "QMCEM", 
                          technical = list(NCYCLES = 1200))

instrument_irt_9 <- mirt(instrument, model = 9, itemtype = "gpcm", method = "QMCEM", 
                          technical = list(NCYCLES = 1200))





#instrument_irt_10 <- mirt(instrument, model = 10, method = "QMCEM")

summary(instrument_irt_10, rotate = "varimax")

trust <- cbind(trust, fscores(instrument_irt_9, QMC = T))
analysis <- trust %>% select(1:14,47:56)
write_csv(analysis, "pre_processed_data.csv")



