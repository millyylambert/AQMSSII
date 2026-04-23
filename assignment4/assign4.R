library(tidyverse)
library(dplyr)
library(broom)
library(ggplot2)
library(modelsummary)
library(marginaleffects)
library(readstata13)
setwd("~/Desktop/AQMSSII/assignment4")

### PART 1

# 1.1 Setup and Data Exploration 

# a) 
corruption <- read.dta13("~/Desktop/AQMSSII/data/corruption.dta")

# b) 

corruption %>%
  summarise(
    ti_cpi_na  = sum(is.na(ti_cpi)),
    undp_gdp_na = sum(is.na(undp_gdp))
  ) # counting how many NAs there are

# There are no missing values on ti_cpi or undp_gdp. 170 observations. 

# c) 
summary(corruption$ti_cpi)
sd(corruption$ti_cpi)

summary(corruption$undp_gdp)
sd(corruption$undp_gdp)

# 1.2 Exploratory Visualization 

# a) 

ggplot(corruption, 
       aes(x = undp_gdp, y = ti_cpi)) + 
  geom_point() + 
  geom_smooth(method = "lm") + 
  labs(x = "GDP per capita (PPP)", y = "Corruption Perceptions Index")

# b) 

# The relationship between corruption perceptions and GDP is positive, but non-linear. 
# There is a cluster around the low gdp. 

# c) 

ggplot(corruption, aes(x = log(undp_gdp), y = ti_cpi)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "log(GDP per capita)", y = "Corruption Perceptions Index")

# The log transformation spreads out the lower income countries and compresses the 
# upper tail, producing a more linear relationship. 

## 1.3 Bivariate Regression 

# a) 
m1 = lm(ti_cpi ~ undp_gdp, data = corruption)

# b) 
tidy(m1)

coef(m1)["undp_gdp"] * 10000
# The coefficient on undp_gdp gives the predicted change in the corruption index
# for a one dollar increase in GDP per capita. For a $10,000 increase, multiply the 
# coefficient [0.000173] by 10,000. 

# c) 
q25 <- quantile(corruption$undp_gdp, 0.25)
q75 <- quantile(corruption$undp_gdp, 0.75)
c(q25, q75)

predictions(m1, newdata = datagrid(undp_gdp = c(q25, q75)))

## 1.4 Non-linear specifications 

# a) 
m2 = lm(ti_cpi ~ log(undp_gdp), data = corruption)

# b)
tidy(m2)

# In a level-log model, a 1% increase in GDP per capita is associated with a 
# change of β1/100 in the corruption index.For a doubling of GDP (log(2) ≈ 0.693

coef(m2)["log(undp_gdp)"] * log(2)

# c) 
m3 = lm(ti_cpi ~ undp_gdp + I(undp_gdp^2), data = corruption)
tidy(m3)

# d) 
r2 = c("Level-Level" = summary(m1)$r.squared,
       "Level-Log" = summary(m2)$r.squared,
       "Quadratic" = summary(m3)$r.squared)
r2

# The log specification fits the data best, consistent with the scatter plots 
# showing a concave relationship. A non-linearspecification is appropriate because 
# the marginal return to additional GDP diminishes at higher income levels, moving 
# from $1,000 to $5,000 matters more for governance quality than moving from $25,000 
# to $29,000.

# 1.5 

# a) 
avg_slopes(m2, variables = "undp_gdp")

# b) 

# The AME differs from the raw coefficient on log (undp_gdp) because the marginal 
# effect of GDP in a level-logmodel depends on the level of GDP: ∂y/∂x = β/x. 
# The AME averages this over all observed values. It tells us theaverage predicted 
# change in the corruption index for a one-dollar increase in GDP across all countries 
# in the sample. 

# c) 
slopes(m3, variables = "undp_gdp", newdata = datagrid(undp_gdp = c(2000, 10000, 30000)))

# The marginal effect of GDP on corruption diminishes as countries become richer. 
# At low GDP levels, an additionaldollar of income has a larger predicted effect 
# on corruption than at high GDP levels. This is consistent with theconcave shape 
# of the relationship. 

## 1.6 Prediction plots 

# a) 
p1 = plot_predictions(m2, condition = "undp_gdp")
p1

ggsave("pred_plot_m2.png", p1, width = 6, height = 4)

# b)
p2 = plot_predictions(m3, condition = "undp_gdp")
p2

ggsave("pred_plot_m3.png", p2, width = 6, height = 4)

# c) 
# Both models tell a similar story: corruption decreases sharply with initial 
# increases in GDP and then levels off athigher income levels. The log model 
# produces a smoother curve, while the quadratic model can curve back upwardat 
# very high GDP values (a feature of the parabolic functional form that may not 
# be substantively meaningful)

## 1.7 Residual Diagnostics 

# a) 
m1_aug = augment(m1)
ggplot(m1_aug, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Fitted values", y = "Residuals", title = "Residuals vs Fitted: Level-Level (m1)")

# The residual plot shows a clear curved pattern, indicating that the linear 
# specification misses the non-linearrelationship. The spread of residuals also 
# appears to increase with fitted values, suggesting heteroskedasticity.

# b) 
m2_aug = augment(m2)
ggplot(m2_aug, aes(x = .fitted, y = .resid)) +
  geom_point() +geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Fitted values", y = "Residuals", title = "Residuals vs Fitted: Level-Log (m2)")

# The log transformation substantially improves the residual pattern. The curvature 
# is reduced, though someheteroskedasticity may remain.

# c)
n = nrow(corruption)
threshold = 4 / n
cooks_d = cooks.distance(m2)
influential = which(cooks_d > threshold)
corruption$cname[influential]

plot(m2, which = 4)

# d) 
# Influential observations should not be removed automatically. They may represent 
# genuine cases (e.g., verywealthy or very corrupt countries) rather than data errors. 
# A recommended robustness check would be to re-estimate the model excluding these 
# observations and compare the coefficients. If the results are similar, the o
# riginal estimates are robust.

## 1.8 Tabels 

modelsummary(
  list("Level-Level" = m1, 
       "Level-Log" = m2, "Quadratic" = m3),
  vcov = "robust",stars = TRUE,
  gof_map = c("r.squared", "nobs"),
  output = "markdown")

# b) 

# The level-log model (m2) is the preferred specification. It has the highest R2, 
# produces the best residual diagnostics, and its functional form has a clear substantive 
# interpretation: the relationship between wealth and corruption is one of diminishing 
# returns. The log transformation also avoids the quadratic model’s problem of 
# an eventual sign reversal at extreme values.

### PART 2

## 2.1 Data Exploration 

# a) 
inf_mortal <- read.dta13("~/Downloads/infantmortality.dta") 
summary(inf_mortal)

# b) 

ggplot(inf_mortal, aes(x = infant)) +
  geom_histogram(bins = 30) +
  labs(title = "Histogram of Infant Mortality",
       x = "Infant Mortality (per 1,000 births)",
       y = "Frequency")

ggplot(inf_mortal, aes(x = income)) +
  geom_histogram(bins = 30) +
  labs(title = "Histogram of GDP per Capita",
       x = "GDP per Capita",
       y = "Frequency")

# Both histograms for infant mortality and income are right-skewed

# c) 

ggplot(inf_mortal, aes(x = income, y = infant, color = region)) +
  geom_point() +
  labs(title = "Infant Mortality vs GDP per Capita",
       x = "GDP per Capita",
       y = "Infant Mortality (per 1,000 births)",
       color = "Region") +
  theme_minimal()

# d) 

ggplot(inf_mortal, aes(x = log(income), y = log(infant), color = region)) +
  geom_point() +
  labs(title = "Log Infant Mortality vs Log GDP per Capita",
       x = "Log GDP per Capita",
       y = "Infant Mortality (per 1,000 births)",
       color = "Region") +
  theme_minimal()

# The log-log relationship looks a lot more linear, suggesting the relationship 
# between income and infant mortality is proportional. As percentage GDP increases, 
# percentange infnat mortality falls. 


## 2.2 Comparing Specifications

# a) 
model1 = lm(infant ~ income, data = inf_mortal)
model1

# b) 
model2 = lm(log(infant) ~ log(income), data = inf_mortal)
model2

# c) 

# In model 1, a $1000 increase is associated with a -20.91 percentage decrease in 
# infant mortality. In model 2, a 1% increase in income is associated with a 0.5118%
# decrease in infant mortality. 

# d)

plot(fitted(model1), resid(model1),
     xlab = "Fitted Values",
     ylab = "Residuals",
     main = "Residuals vs Fitted (Linear Model)")

abline(h = 0)

plot(fitted(model2), resid(model2),
     xlab = "Fitted Values",
     ylab = "Residuals",
     main = "Residuals vs Fitted (Log-Log Model)")

abline(h = 0)

# The log-log model has a better residual pattern as the residuals are more evenly 
# scattered around zero with a consistent spread. In the linear model many of the 
# residual values are clustered on the left. 

## 2.3 Multiple Regression with Controls 

# a) 
model3 = lm(log(infant) ~ log(income) + region + oil, data = inf_mortal)

# b) 
model3

# The coefficient on log(income) is -0.34, so when controlling for region and oil status
# a 1% increase in income is associated with a 0.34% decrease in infant mortality. 
# The coefficient has decreased compared to the previous log model with no controls, 
# suggesting that some of that negative relationship was confounded by regional/oil
# related factors. 

# c) 

# Africa is used as the reference category in the model, so negative coefficients 
# for other regions (Americas, Asia and Europe) indicate that infant mortality rates 
# are lower in these regions. 

# d) 

avg_slopes(model3)

# AME of income is -0.00159, indicating that, on average, a $1 increase in income 
# reduces infant mortality by 0.00159 per 1,000 births.

## 2.4 Interaction: oil status and income 

# a) 
model4 = lm(log(infant) ~ log(income) * oil + region, data = inf_mortal)

# b) 
avg_slopes(model4, variables = "income", by = "oil")

# c) 

# For countries that do not export oil, a $1 increase in income is associated with a 
# 0.00209 decrease in infant mortality (per 1,000 births). For oil-exporting countries, 
# the marginal effect of income is positive a $1 increase in income is associated 
# with a 0.00111 increase in infant mortality. This may indicate that wealth from 
# oil is not translated into social spending on healthcare, or perhaps oil wealth is
# highly concentrated and does not affect (or worsens) healthcare for the general population. 

# d) 
plot_slopes(model4, variables = "income", condition = "oil")

ggsave("income_marginal_effect_by_oil.png", width = 6, height = 4)

## 2.5 Predicted Values for specific scenarios

# a) 

preds <- predictions(m3,
            newdata = datagrid(
              income = c(1000, 20000, 10000),
              region = c("Africa", "Europe", "Americas"),
              oil = c("no", "no", "yes")))

preds$estimate <- exp(preds$estimate)

preds  

# b) 

# These predicted values (73.5 per 1000 for every regions) are not plausible. Rather, 
# we would expect mortality rates to vary widely across regions and oil statuses.

## 2.6 Visualization 

plot_predictions(model3, condition = c("income", "region")) +
                 labs(
                   title = "Predicted Infant Mortality by Income and Region",
                   x = "Income per Capita (USD)",
                   y = "Predicted Infant Mortality (per 1,000 live births)",
                   color = "Region"
                 ) +
                   theme_minimal() +
  guides(fill = "none")  

ggsave("infant_mortality_by_GDP.png", width = 6, height = 4)

# This plot shows that as income per capita increases, the infant mortality rate
# declines in all regions. At every level, Africa has the highest infant mortality 
# rate and Europe has the lowest. 

## 2.7 Diagnostics and Robust Inference 

# a) 

plot(fitted(model3), resid(model3),
     xlab = "Fitted Values",
     ylab = "Residuals",
     main = "Residuals vs Fitted Model 3")

abline(h = 0)

# The plot does not suggest heteroskedasticity, but rather homoskedasticity because
# the residuals are evenly scattered and there is no systematic increase or decrease. 

# b) 

modelsummary(
  list("Level" = model1, 
       "Log-Log" = model2,
       "Controls" = model3, 
       "Interaction" = model4),
  vcov = "robust",stars = TRUE,
  gof_map = c("r.squared", "nobs"))

# c) 

modelsummary(
  list("Controls (default)" = model3,
       "Controls (robust)"  = model3),
  vcov = list(NULL, "robust"),
  stars = TRUE,
  gof_map = c("r.squared", "nobs")
)

# The values of the robust SEs are generally slightly higher than the default SEs. 
# The robust SE for oil-exporting countries is much higher than the default and it 
# is no longer statistically significant, meaning that countries which export oil 
# have more variable outcomes for infant mortality so we cannot confidently say that 
# being an oil-exporting country has an effect on infant mortality rates - robust SE's 
# fix overconfidence. 

