library(tidyverse)


data <- read_csv("data/pre_processed_data.csv")

# limit analysis to only users of facebook twitter or instagram
users <- data %>% 
  filter(SNSA_W35 == 1| 
         SNSB_W35 == 1| 
         SNSE_W35 == 1)

users <- users %>% 
  rename(sex = F_SEX_FINAL,
         region = F_CREGION_FINAL,
         race = F_RACECMB_RECRUITMENT,
         income = F_INCOME_FINAL,
         age = F_AGECAT_FINAL, 
         ideaology = F_IDEO_FINAL,
         education = F_EDUCCAT_FINAL,
         weight = WEIGHT_W35, 
         regulation = TC5_W35)

users <- users %>% 
  mutate(regulation = 
           if_else(regulation == 1, 
                   "More Regulation", 
                   "Enough Regulation"),
         bullying = if_else(SM6A_W35 == 1, 1, 0),
         support = if_else(SM6A_W35 == 2, 1, 0),
         deception = if_else(SM6B_W35 == 1, 1, 0),
         correct_misinformation = if_else(SM6B_W35 == 2, 1, 0),
         accurate_pic = if_else(SM3_W35 == 1, 1, 0),
         `censorship very likely` = if_else(SM7_W35 == 1, 1, 0),
         `censorship not very likely` = if_else(SM7_W35  == 3, 1, 0),
         `censorship not at all likely` = if_else(SM7_W35 == 1, 1, 0),
         accept_selective_look = if_else(SM8A_W35 == 1, 1, 0),
         selective_vote_reminder = if_else(SM8B_W35 == 1, 1, 0),
         selective_emotion = if_else(SM8C_W35 == 1, 1, 0),
         male = if_else(sex == 2, 1, 0),
         some_college = if_else(education == 2, 1, 0),
         college_grads = if_else(education == 1, 1, 0),
         black = if_else(race == 2, 1, 0),
         asian = if_else(race == 3, 1, 0),
         mixed_race = if_else(race == 4,1, 0),
         other = if_else(race == 5 | race == 9, 1, 0),
         `below 10k` = if_else(income == 1, 1, 0),
         `10k - 20k` = if_else(income == 2, 1, 0),
         `20k - 30k` = if_else(income == 3, 1, 0),
         `30k - 40k` = if_else(income == 4, 1, 0),
         `40k - 50k` = if_else(income == 5, 1, 0),
         `75k - 100k` = if_else(income == 7, 1, 0),
         `100k - 150k` = if_else(income == 8, 1, 0),
         `150k and above` = if_else(income == 9, 1, 0),
         very_conservative = if_else(ideaology == 1, 1, 0),
         conservative = if_else(ideaology == 2, 1, 0),
         liberal = if_else(ideaology == 4, 1, 0),
         very_liberal = if_else(ideaology == 5, 1, 0),
         `18 - 29` = if_else(age == 1, 1, 0),
         `50 - 64` = if_else(age == 3,1, 0),
         `65 and above` = if_else(age == 4,1, 0)
         )


model_data <- users %>% select(weight, 
                               regulation, 
                               contains("F"),
                               37:69, 
                               region)

write_csv(model_data, "data/model_data.csv")


