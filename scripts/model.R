library(tidyverse)
library(bayesplot)
library(rstan)
library(rstanarm)
library(ggthemes)

model_data <- read_csv("data/model_data.csv")
model_data <- model_data %>%
  mutate(regulation = if_else(regulation == "More Regulation", 1, 0))

# Set Priors --------------------------------------------------------------

# stan with defaults

default_prior <- stan_glm(regulation ~ male + 
                            some_college + college_grads + 
                            black + asian + mixed_race + other + 
                            `below 10k` + `10k - 20k` + `20k - 30k` + 
                            `30k - 40k` + `40k - 50k` + `75k - 100k` + 
                            `100k - 150k` + `150k and above` + 
                            very_conservative + conservative + 
                            liberal + very_liberal + 
                            `18 - 29` + `50 - 64` + `65 and above`, 
                          model_data, family = binomial, weights = weight,
                          prior_PD = T)
prior_summary(default_prior)

prior_check <- posterior_predict(default_prior)


# my priors
my_prior <- stan_glm(regulation ~ male + 
                            some_college + college_grads + 
                            black + asian + mixed_race + other + 
                            `below 10k` + `10k - 20k` + `20k - 30k` + 
                            `30k - 40k` + `40k - 50k` + `75k - 100k` + 
                            `100k - 150k` + `150k and above` + 
                            very_conservative + conservative + 
                            liberal + very_liberal + 
                            `18 - 29` + `50 - 64` + `65 and above`, 
                          model_data, family = binomial, weights = weight,
                          prior_intercept = normal(0, .5, autoscale = F),
                          prior = normal(0, 1, autoscale = F),
                          prior_PD = T)

my_prior_check <- posterior_predict(my_prior)
my_prior_means <- rowMeans(my_prior_check)
default_prior_means <- rowMeans(prior_check)
prior_compare <- tibble(
  prior = my_prior_means, 
  `default prior` = default_prior_means)

prior_compare %>% pivot_longer(cols = 1:2) %>% 
  ggplot(aes(value, fill = name)) + 
  geom_density(alpha = .5) + 
  theme_fivethirtyeight()


# Model 1: Demographics ---------------------------------------------------

base_model <- stan_glm(regulation ~ male + 
                       some_college + college_grads + 
                       black + asian + mixed_race + other + 
                       `below 10k` + `10k - 20k` + `20k - 30k` + 
                       `30k - 40k` + `40k - 50k` + `75k - 100k` + 
                       `100k - 150k` + `150k and above` + 
                       very_conservative + conservative + 
                       liberal + very_liberal + 
                       `18 - 29` + `50 - 64` + `65 and above`, 
                     model_data, family = binomial, weights = weight,
                     prior_intercept = normal(0, .5, autoscale = F),
                     prior = normal(0, 1, autoscale = F), 
                     seed = 2, cores = 4)

base_post <- posterior_predict(base_model)
prior_compare$posterior <- rowMeans(base_post)

prior_compare %>% 
  select(-`default prior`) %>% 
  pivot_longer(cols = 1:2) %>% 
  ggplot(aes(value, fill = name)) + 
  geom_density(alpha = .7) + 
  theme_fivethirtyeight()
  


# Model 2: Adding Experiance ----------------------------------------------

model_2 <- stan_glm(regulation ~ male + 
                         some_college + college_grads + 
                         black + asian + mixed_race + other + 
                         `below 10k` + `10k - 20k` + `20k - 30k` + 
                         `30k - 40k` + `40k - 50k` + `75k - 100k` + 
                         `100k - 150k` + `150k and above` + 
                         very_conservative + conservative + 
                         liberal + very_liberal + 
                         `18 - 29` + `50 - 64` + `65 and above` + 
                      F1 + F2 + F3 + F4 + F5 + F6 + F7 + F10 + F11 + F12, 
                       model_data, family = binomial, weights = weight,
                       prior_intercept = normal(0, .5, autoscale = F),
                       prior = normal(0, 1, autoscale = F), 
                       seed = 2, cores = 4)
launch_shinystan(model_2)
