library(tidyverse)
library(haven)
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
contains("SM3"),
 "WEIGHT_W35"
 )

trust <- drop_na(trust)

instrument <- trust %>% 
select(contains("SM"), 
       -contains("SM6"),
       -contains("SM8"), 
       -contains("SM10"),
       -contains("SM11"),
       -contains("SM3"))

fit <- factanal(instrument, 9,
                rotation = "varimax", 
                scores = "regression")

print(fit, digits=2, cutoff=.3, sort=TRUE)

trust <- cbind(trust, fit$scores)

analysis <- trust %>% 
  select(1:14, 
         contains("SM6"),
         contains("SM8"), 
         contains("SM10"),
         contains("SM11"),
         contains("SM3"),
         contains("SM7"),
         contains("Factor"), 
        "WEIGHT_W35")


write_csv(analysis, "data/pre_processed_data.csv")
fit
