setwd("~/Desktop/AQMSSII/assignment8")

library(sf)
library(spData)
library(spdep)
library(spatialreg)
library(ggplot2)
library(modelsummary)
data(world)

### PART 1
## 1.1 Setup and OLS baseline 

# a) 
world = world[!is.na(world$gdpPercap) & !is.na(world$lifeExp), ]
world = world[world$continent != "Antarctica", ]
world$log_gdp = log(world$gdpPercap)
nrow(world)

# 160 observations. Log-transforming GDP per capita redcues the right-skew by 
# compressing the upper tail, making the relationship between GDP and life expectancy 
# more linear. 


# b) 
ols_fit = lm(lifeExp ~ log_gdp, data = world)
summary(ols_fit)

# The coefficient is 5.5403 - positive and statistically significant (p < 0.001). 
# It means that a one-unit increase in log GDP per capita is associated with higher
# life expectancy by approximately 5.54 years. The R^2 shows that GDP per capita
# accounts for a lot of the relationship. 


# c) 

world$ols_resid = residuals(ols_fit)

ggplot(world) +
  geom_sf(aes(fill = ols_resid), color = "white", linewidth = 0.2) +
  scale_fill_gradient2(low = "#2166ac", mid = "white", high = "#d6604d",
                       midpoint = 0, name = "OLS residual") +
  theme_void() +
  labs(title = "OLS residuals: life expectancy ~ log GDP per capita")

ggsave("old_residual_map.pdf", width = 10, height = 5)

# The residual map reveals that Sub-Saharan Africa shows a concentration of negative 
# residuals - countries with lower life expectancy than the model predicts given their 
# income level, likely due to disease burden. Western Europe and parts of East Asia 
# display positive residuals, indicating that these regions achieve higher life expectancy 
# than income alone predicts. This geographic pattern in the residuals suggests 
# spatial autocorrelation. 


## 1.2 Spatial weights matrix 

# a) 
nb = poly2nb(world, queen = TRUE)
listw = nb2listw(nb, style = "W", zero.policy = TRUE)
summary(nb)

# 16 countries have no neighbours in the continguity matrix. These are island nations
# that share no land boundary or common border with any other polygon. Queen contiguity 
# requires at least one shared point, islands have none so they are isolated nodes 
# in the wieghts graph. zero.policy = TRUE allows these units to remain in the analysis. 

# b) 
moran.test(world$ols_resid, listw = listw, zero.policy = TRUE)

# The Moran's I statistic is positve and the p-value < 0.05, indicating statiscally 
# significant positive spatial autocorrelation in the OLS residuals. Countries close 
# to each other tend ot have similar residuals - either both overestimated or both 
# underestimated - which violates the OLS assumption of indpendent errros. Ignoring 
# this pattern yields inefficeint estimates and invalid standard errors. 

## 1.3 Lagrange multiplier tests

lm_tests = lm.LMtests(ols_fit, listw = listw,
                      test = c("LMerr", "LMlag", "RLMerr", "RLMlag"),
                      zero.policy = TRUE)

summary(lm_tests)

# a) 
# LMerr tests whether there is spatial dpeendence in error term. LMlag tests whether
# a spatially lagged dependent variabel belongs in the model. Only LMerr is
# significant (p<0.05). The p-value for RSerr is highly significant (p < 0.001) 
# where as the p-value for RSlag is 0.8040, suggesting no evidence of spillover effect. The SLM is perferred  

# b) 
# The robust tests (RLMerr, RLMlag) each control for the presence of the other type
# of spatial dependence. If RLMerr is more significant than RLMlag, the evidence 
# favours the SEM. If RLMlag dominates, the SLM is preferred. The adjRSerr is significant
# but the adjRSlag is not, so the SEM is preferred. 


## 1.4 Spatial Error Model (SEM) 

# a) 
sem_fit = errorsarlm(lifeExp ~ log_gdp, data = world,
                     listw = listw, zero.policy = TRUE)
summary(sem_fit)

modelsummary(list("OLS" = ols_fit, 
                  "SEM" = sem_fit),
             stars = TRUE) 


# The coefficient on log_gdp from the SEM is 3.958, meaning for every 1-unit increase
# in the log of GDP, life expectancy is predicted to increase by approx 3.96 years. 
# The coefficient on the OLS model is 5.540. The OLS model overestimates the correlation. 
# The lambda is 0.763, which is positive and significant, suggesting the SEM captures
# genuine spatial dependence. 

# b) 

# In the SEM lambda is the spatial autocorrelation coefficient that measures the 
# spatial dependence in the error term. If lambda is positive then the residuals
# are geographically clustered meaning similar errors are clustered together. If 
# lambda = 0 there is no spatial autocrorrelation.If lambda is negative, then dissimlar
# values cluster. 

# c) 

world$sem_resid = residuals(sem_fit)
moran.test(world$sem_resid, listw = listw, zero.policy = TRUE)

# The SEM reduces the spatial autocorrelation in the residuals. The test statistic 
# in the Moran's I is much closer to zero (-0.0859).This suggests the spatial error  
# correction has absorbed most of the geographic clustering that OLS left behind 
# its residuals. 

## 1.5 Distance-based weights: an alternative neighborhood

# a) 
coords = st_centroid(st_geometry(world))
nb_dist = dnearneigh(coords, d1 = 0, d2 = 300)
summary(nb_dist)

# 114 countries now have no neigbours, which is higher than estimated by the queen 
# contiguity matrix from 1.2a. This may be because distance based weights rely on 
# an arbitrary threshold which might isolate countries which are far away from 
# other countries. In this case the threshold is that the centroids must be within 
# 300km of each other, which is arbitrarily small and will leave a lot of countries 
# isolated.

#b) 
listw_dist = nb2listw(nb_dist, style = "W", zero.policy = TRUE)

sem_dist = errorsarlm(lifeExp ~ log_gdp, data = world,
                      listw = listw_dist, zero.policy = TRUE)
summary(sem_dist)

# Lambda = 0.424, p-value = 0.000129. The lambda is significantly closer to zero. 
# This demonstrates that spatial models are very sensitive to the definition of 
# neighbourhood. 

# c) 

moran.test(world$sem_resid, listw = listw_dist, zero.policy = TRUE)

# At -0.021, the Moran's I statistic is closer to zero than the result from 1.4c. 
# This suggests there is less spatial autocorrelation than originally estimated. 
# Since the p-value is greater than 0.05 (0.49) this result is not statistically 
# significant and no meaningful evidence of spatial autocorrelation was found. 


### PART 2: SPATIAL LAG MODEL AND MODEL COMPARISON 

## 2.1 SLM 

# a) 

slm_model <- lagsarlm(lifeExp ~ log_gdp, data = world,
                      listw = listw, zero.policy = TRUE)

summary(slm_model)


# The rho = -0.004 and its p-value is 0.805. The coefficient on log_gdp is 5.548.

# b) 
# The rho is very close to zero and the p-value is greater than 0.1, meaning it is 
# not statistically significant.This suggests the outcome variable is not spatially
# lagged. If rho is larger than 0, this would mean that a change in a country's life 
# expectancy has an effect on its neighbours life expectancy.


# c) 
# The coefficient on log GDP in the SLM is not the marginal effect because changes 
# in one unit's GDP affect not only its own life expectancy, but propagate through 
# neighbouring units via the spatial lag. From the reduced form, the matrix captures 
# these feedback effects - a change in x includences y, then spills over to neighbours
# and feeds back again. The total effect of GDP includes both direct effects and 
# indirect effects, so the raw coefficient alone does not represent the marginal impact. 


## 2.2 Direct and Indirect Effects 

set.seed(123)

impacts_slm <- impacts(
  slm_model,
  listw = listw,
  R = 500   
)

summary(impacts_slm)

# The direct effect is 5.548, the indirect effect is -0.023 making the total effect 
# 5.524. The total effect is slightly lower than than the raw log_gdp from the 
# SLM output (5.548), but slightly higher than the OLS coefficient (5.5403). 

# b) 
# The indirect effect captures how a change in GDP in one country affects life 
# expectancy in other countries through spatial spillovers. If log GDP increases 
# by one unit in one country, the indirect effect measures the resulting change 
# in life expectancy in neighbouring countries after accounting the full spatial 
# feedback process. This reflects how improvements in one country can diffuse 
# across borders and influence outcomes (in this case, the effect is slightly negative).

# c) 
# My results show that the total effect is actually slightly smaller than the direct
# effect. Typically the total effect is often larger due to positive spatial 
# spillovers, but in this case, the estimated spatial parameter is close to zero 
# and slightly negative. As a result the indirect effect is small and negative, 
# making the total effect slightly smaller than the direct effect. More generally, 
# as p increases and becomes positive, spillovers strengthen and the total effect 
# exceeds the direct effect. 

## 2.3 Model Comparison 

# a) 
AIC(ols_fit, sem_fit, slm_model)

# The AIC for the OLS model is 965.99, for the SEM model is 894.70 and for the SLM 
# model is 967.93, making the SEM the model with the lowest score and best fit. 
# This is consistent with the LM-test-based model choice in 1.3b which suggested 
# using the SEM model.

# b) 
# The Moran's I test on the OLS residuals indicates statistically significant 
# positive spatial autocorrelation, suggesting that residuals are geographically 
# clustered rather than randomly distributed. Based on the LM test, the SEM was 
# selected over the SLM, as the robust LM test showed evidence of spatial dependence
# in the error terms but not the spatial lag. The estimated coefficient on log GDP 
# differs across models, with OLS producing a larger estimate (5.54) than the SEM 
# (3.96), indicating that OLS overstates the relationship by failing to account 
# for spatially correlated omitted variables, while the SLM estimate (5.55) is very 
# similar to OLS. The SLM results show the spatial autoregressive parameter is close 
# to zero and statistically insignificant, implying no meaningful evidence of life 
# expectancy spillovers across countries. this is reinforced by the negligible indirect
# effect estimated from the impact analysis. Overall the results suggest spatial 
# dependence arises from unobserved regional factors rather than direct diffusion 
# of outcomes. One limitation of using queen contiguity at the country level is 
# that it only captures shared land borders and ignores other factors which may 
# generate spatial dependence. 




