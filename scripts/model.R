library(tidyverse)
library(bayesplot)
library(rstan)
library(rstanarm)
library(projpred)
library(ggthemes)
library(loo)
library(tidybayes)

model_data <- read_csv("data/model_data.csv")
model_data <- model_data %>%
  mutate(regulation = if_else(regulation == "More Regulation", 1, 0))



# Set Priors --------------------------------------------------------------

# stan with defaults

default_prior <- stan_glm(regulation ~  
                            very_conservative + conservative + 
                            liberal + very_liberal, 
                          model_data, 
                          family = binomial,
                          prior_PD = T)

prior_summary(default_prior)

prior_check <- posterior_predict(default_prior)



# Fit 1 -------------------------------------------------------------------

# my priors
prior_1 <- stan_glm(regulation ~  
                       very_conservative + conservative + 
                       liberal + very_liberal, 
                          model_data, family = binomial,
                          prior_intercept = normal(0, .33, autoscale = F),
                          prior = normal(c(-1, -.25, .25, 1), .33, autoscale = F),
                          seed = 2, prior_PD = T)

# Model 1
M1 <- stan_glm(
  regulation ~
    very_conservative + conservative +
    liberal + very_liberal,
  model_data,
  family = binomial,
  prior_intercept = normal(0, .33, autoscale = F),
  prior = normal(c(-1,-.25, .25, 1), .33, autoscale = F),
  seed = 2, cores = 4
)


# draw from posterior
posterior <- as.matrix(ux_model)
prior <- as.matrix(prior_ux)

#plot 
tibble(
  posterior =
    c(posterior[, 1],
      posterior[, 2],
      posterior[, 3],
      posterior[, 4],
      posterior[, 5]),
  prior = c(prior[, 1],
            prior[, 2],
            prior[, 3],
            prior[, 4],
            prior[, 5]),
  parameter = c(
    rep("intercept", 4000),
    rep("very_conservative", 4000),
    rep("conservative", 4000),
    rep("liberal", 4000),
    rep("very liberal", 4000)
  )
) %>%
  pivot_longer(cols = 1:2) %>%
  ggplot(aes(value, fill = name)) +
  geom_density(alpha = .5) +
  facet_grid(parameter ~ .) +
  theme_fivethirtyeight()


# Add age  ----------------------------------------------------------------

my_prior <- normal(c(-1,-.25, .25, 1, rep(0, 3)), 
                   c(rep(.33, 4), rep(1.5, 3)), autoscale = F)
M2 <- stan_glm(
  regulation ~
    very_conservative + conservative + liberal + very_liberal + 
    age_18_29 + age_50_64 + age_65_up,
  model_data,
  family = binomial,
  prior_intercept = normal(0, .33, autoscale = F),
  prior =my_prior,
  seed = 2, cores = 4
)

M2_loo <- loo(M2)

# Add race ----------------------------------------------------------------

my_prior <- normal(c(-1,-.25, .25, 1, rep(0, 8)), 
                   c(rep(.33, 4), rep(1.5, 8)), autoscale = F)
M3 <- stan_glm(
  regulation ~
    very_conservative + conservative + liberal + very_liberal +
    age_18_29 + age_50_64 + age_65_up + 
    black + asian + mixed_race + other,
  model_data,
  family = binomial,
  prior_intercept = normal(0, .33, autoscale = F),
  prior = my_prior,
  seed = 2,
  cores = 4
)

M3_loo <- loo(M3)



# Other Demographic models ------------------------------------------------

my_prior <- normal(c(-1,-.25, .25, 1, rep(0, 8)), 
                   c(rep(.33, 4), rep(1.5, 8)), autoscale = F)
M4 <- stan_glm(
  regulation ~
    factor(ideaology) + factor(age) + factor(race) + factor(income),
  model_data,
  family = binomial,
  prior_intercept = normal(0, .33, autoscale = F),
  prior =my_prior,
  seed = 2, cores = 4
)

income <- loo(M4)

M5 <- stan_glm(
  regulation ~
    factor(ideaology) + factor(age) + factor(race) + factor(education),
  model_data,
  family = binomial,
  prior_intercept = normal(0, .33, autoscale = F),
  prior =my_prior,
  seed = 2, cores = 4
)

education <- loo(M5)

M6 <- stan_glm(regulation ~ 
                         very_conservative + conservative + 
                         liberal + very_liberal + 
                         age_18_29 + age_50_64 + age_65_up + 
                         male,
                       model_data, family = binomial,
                       prior_intercept = normal(0, .5, autoscale = F),
                       prior = normal(0, 1, autoscale = F), 
                       seed = 2, cores = 4)

gender<- loo(M6)


# Social Variable ---------------------------------------------------------

with_social <- stan_glm(regulation ~ 
                   very_conservative + conservative + 
                   liberal + very_liberal + 
                   age_18_29 + age_50_64 + age_65_up + 
                   Factor1 +  Factor7 + Factor9 + 
                     deception + correct_misinformation + 
                     black + asian + mixed_race + other, 
                 model_data, family = binomial,
                 prior_intercept = normal(0, .5, autoscale = F),
                 prior = normal(0, 1.5, autoscale = F), 
                 seed = 2, cores = 4)

launch_shinystan(with_social)

loo_social <- loo(with_social)


loo_compare(loo_social, loo_demographic)




person.pred <- rowMeans(post)                                    
plot(density(person.pred))



  

pivot_longer(cols = 1:2) %>% 
  ggplot(aes(value,fill = name)) +
  geom_density(alpha = .7)



plot_title <- ggtitle("Posterior distributions",
                      "with medians and 80% intervals")
mcmc_areas(posterior,
           pars = c("cyl", "drat", "am", "wt"),
           prob = 0.8) + plot_title

posterior <- as.matrix(with_social)
dim(posterior)
color_scheme_set("red")
mcmc_dens(posterior, pars = c("(Intercept)", "very_conservative"))

my_prior <- normal()

MLM <- stan_glmer(regulation ~ Factor1 + Factor9 + Factor7 + 
                     deception + correct_misinformation + 
                     age_18_29 + age_50_64 + age_65_up + 
                     black + asian + mixed_race + other + 
                     (Factor1 + Factor9 + Factor7 +
                      deception + correct_misinformation|ideaology), 
                   family = binomial("logit"), model_data,
                   prior_intercept = normal(0, .33),
                   QR = TRUE, adapt_delta = .98, chains = 4, cores = 4)
loo_MLM <- loo(MLM)
loo_FE <-loo(with_social)
loo_compare(loo_FE, loo_MLM)
