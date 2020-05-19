data('df_binom', package = 'projpred')

analysis_df <- model_data %>% select(2:44)
weights <- model_data$weight
# fit the full model
n <- nrow(analysis_df)
D <- ncol(analysis_df) - 1
p0 <- 12 # prior guess for the number of relevant variables
sigma <- 2 # approximate plug-in value for observation information (Piironen and Vehtari, 2017b)
tau0 <- p0/(D-p0) * sigma/sqrt(n)
prior_coeff <- hs(global_scale = tau0, slab_scale = 1)

fit <- stan_glm(regulation ~ male + 
                  some_college + college_grads + 
                  black + asian + mixed_race + other + 
                  `below 10k` + `10k - 20k` + `20k - 30k` + 
                  `30k - 40k` + `40k - 50k` + `75k - 100k` + 
                  `100k - 150k` + `150k and above` + 
                  very_conservative + conservative + 
                  liberal + very_liberal + 
                  `18 - 29` + `50 - 64` + `65 and above`,
                  family=binomial(), data= analysis_df, weights = weights,
                prior_intercept = normal(0, .5, autoscale = F),
                prior = normal(0, 1, autoscale = F)
                , seed=12, chains = 2)

vs <- varsel(fit, method='forward')
vs$vind
names(model_data)
