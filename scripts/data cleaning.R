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
         income = F_INCOME_RECODE_FINAL,
         age = F_AGECAT_FINAL, 
         ideology = F_IDEO_FINAL,
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
         censorship_very_likely = if_else(SM7_W35 == 1, 1, 0),
         censorship_not_very_likely = if_else(SM7_W35  == 3, 1, 0),
         censorship_not_at_all_likely = if_else(SM7_W35 == 1, 1, 0),
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
         below_30K = if_else(income == 3, 1, 0),
         above_75K = if_else(income == 1, 1, 0),
         very_conservative = if_else(ideology == 1, 1, 0),
         conservative = if_else(ideology == 2, 1, 0),
         liberal = if_else(ideology == 4, 1, 0),
         very_liberal = if_else(ideology == 5, 1, 0),
         age_18_29 = if_else(age == 1, 1, 0),
         age_50_64 = if_else(age == 3,1, 0),
         age_65_up = if_else(age == 4,1, 0)
         )


model_data <- users %>% select(weight, 
                               regulation, 
                               contains("F"),
                               27:63, 
                               region, ideology, age, race, income, education,SM6A_W35, SM6B_W35)


model_data <- model_data %>% mutate(`political identity` = 
                                      case_when(ideology == 1 ~ "very conservative", 
                                                ideology == 2 ~ "conservative", 
                                                ideology == 3 ~ "moderate", 
                                                ideology == 4 ~ "liberal", 
                                                ideology == 5 ~ "very liberal"))

model_data <- model_data %>% rename(Negative_Affect = Factor9, 
                                    Charged_Content = Factor1,
                                    Political_Action = Factor7)

model_data <- model_data %>% mutate(Negative_Affect = Negative_Affect*-1,
                                    Charged_Content = Charged_Content*-1)


model_data$Negative_Affect <- scale(model_data$Negative_Affect, scale = F)
model_data$Charged_Content <- scale(model_data$Charged_Content, scale = F)

write_csv(model_data, "data/model_data.csv")


