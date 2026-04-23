setwd("~/Desktop/AQMSSII/assignment9")
library(carData)
library(MASS)
library(nnet)
library(pscl)
library(AER)
library(marginaleffects)
library(ggplot2)
data(BEPS)

### PART 1: ORDINAL, MULTINOMIAL AND COUNT OUTCOMES

## 1.1 Ordered logit: perceptions of the national economy

# a)
table(BEPS$economic.cond.national)
BEPS$econ_ord = factor(BEPS$economic.cond.national, ordered = TRUE)

# The third category (stayed about the same) is most common with 607 responses. 
# The extremes 1 (got much worse) and 5 (got much better) were much rarer with 37 
# and 82 responses respectively. OLS treats the numeric values 1-5 as equally spaced, 
# implying the difference between "got much worse" and "got a little worse" is 
# identical to "stayed the same" and "got a little better". For likert-scale survey 
# questions where the meanings are up for interpretation by respondents, this is 
# certainly wrong. Ordered logit avoids this assumption by estimating threshold 
# parameters that let the data determine the spacing of the latent scale. 


# b) 
m_ologit = polr(econ_ord ~ age + gender + Europe + political.knowledge,
                data = BEPS, Hess = TRUE)
summary(m_ologit)

# The raw coefficient on Europe is -0.122693, meaning there is a positive association 
# after applying the sign reversal. Respondents with stronger pro-EU attitudes tend to 
# pereive national economic conditions as having improved. 

# c) 
avg_slopes(m_ologit)

# For Europe, AMEs on the lower categories (1 and 2) are negative, while they are 
# positive for the higher categories (4 and 5), consistent with a positive association
# between pro-EU sentiment and more optimistic economic assessments. The AME's for 
# any given predictor must sum to zero across the five categories because probabilities 
# are constrained to sum to one. 

# d) 
predictions(m_ologit, newdata = datagrid(gender = c("female", "male")))

# For both the most pestimistic category (1) and the most optimistic (5) there are 
# very modest differences across male and female respondents. At the lower level 
# they are both around 0.02 and at the upper, for females the probability is 0.0421 
# and for males its 0.0506.

## 1.2 Multinomial logit: vote choice 

# a) 
BEPS$vote = relevel(BEPS$vote, ref = "Conservative")
m_mlogit = multinom(vote ~ economic.cond.national + Blair + Hague +
                      Kennedy + Europe, data = BEPS, trace = FALSE)
summary(m_mlogit)


# b) 
avg_slopes(m_mlogit)


# c) 
# IIA: odds ratio between any two alternatives is unaffected by the presence or 
# absence of other alternatives. IIA fails when two alternatives are close substitutes 
# adn removing one simply shifts probability to the other. IIA is a moderate concern 
# for British party cohhice, IIA is a moderate concern: Labour and the Lib Dems are 
# both centre left parties, so some voters may treat them as substitutes. 

## 1.3 Poisson regression: publication counts 
data(bioChemists)

# a) 
summary(bioChemists$art)
var(bioChemists$art)

ggplot(bioChemists, aes(x = art)) +
  geom_histogram(binwidth = 2, fill = "seagreen", color = "white") +
  theme_minimal() +
  labs(title = "Publications in last 3 years of PhD",
       x = "Number of articles", y = "Count")

# The distribution of article publicatino is right-skewed with a mode at zero and 
# a long upper tail. The mean is around 1.69 while the variance is approx. 3.71, 
# which is roughly twice the mean. Under the Poisson assumption, the variance should 
# equal the mean. A ratio susbtantially above 1 indicates overdispersion. This 
# pattern is a first signal that a standard Poisson model may underestimate 
# uncertainty and produce anti-conservative standard errors.

# b) 
m_pois = glm(art ~ fem + mar + kid5 + phd + ment,
             data = bioChemists, family = poisson)
summary(m_pois)

exp(coef(m_pois)["ment"])

# The incidence rate ratio for men is 1.026; each additional articles published 
# by the mentor is associated with a multiplicative increase in expected student 
# articles by that factor, holding all else constant. the effect is modest but 
# positive, suggesting that more productive mentors slightly boost student output. 
# The residual deviance is substantially larger than the residual degrees of freedom
# (their ratio is well above 2), which is another clear diagnostic signal of overdipersion
# - the poisson model does nt adequately capture the variation in publication counts. 

# c) 
dispersiontest(m_pois)

# The dispersion test rejects the null hypothesis of equidispersion (p<0.001). The 
# estimated dispersion parameter is above 1, confirming that the variance in art 
# exceeds its mean. This means the Poisson SEs are too small and the model 
# underestimates uncertainty, inflates test statistics, and produces p-values that 
# are misleadingly small. A model that explicitly accounts for overdispersion is needed
# e.g.. negative bionomial. 


## 1.4 Negative bionomial regression

# a) 
m_nb = glm.nb(art ~ fem + mar + kid5 + phd + ment,
              data = bioChemists)
summary(m_nb)

# the coefficient on ment is similar ot the Poisson estimate, indicating that the 
# point estimate is reasonably stable. The key difference is in the SEs. The negative 
# binomial model produces larger, more honest uncertainty estimates. The estimated 
# overdispersion parameter theta quantifies how much the variance exceeds the Poisson 
# prediction; a smaller theta means more severe overdispersion. Here theta is moderate
# indicating meaninful but not extreme extra-Poisson variation. 

# b) 
AIC(m_pois)
AIC(m_nb)

# The negative bionomial AIC is substantially lower than the Poisson AIC, despite the 
# NB model having one additional parameter (theta). Under AIC, the improvement in 
# fit more than compensates for the added complexity. This confirms that overdispersion
# is a genuine feature of the data, not noise, and that the negative binomial is the more 
# appropriate model for these publication counts. 

# c) 
predictions(m_nb, newdata = datagrid(fem = c("Men", "Women")))

# The predicted number of articles for men exceeds that for the women, holding marital 
# status, number of young children, PhD prestige, and mentor productivity constant
# at their sample means. The confidence intervals provide information on whether 
# this gender gap is statistically distinguishable: if the intervals do not overlap, 
# the difference is significant at conventional levels. The gap reflects a persistent
# within-group gender difference in publication productivity that is not simply an 
# artefact of other observable characteristics. 

# d) 
# The Poisson model is not adequate for this dataset. The variance-to-mean ratio of 
# art is roughly double, the residual deviance far exceeds the degrees of freedom, 
# and the formal disperstiontest() rejects equidispersion with a p-value well below 
# 0.001. The negative binomial model, which adds a dispersion parameter to accommodate
# this extra variation, achieves a substantially lower AIC and produces more reliable SEs. 
# The mentor's productivity has a positive and significant effect, with an IRR slightly 
# above 1. Each additoinal mentor article is assocaited with a modest multiplicative 
# increase in expected student articles, suggesting that working with a productive 
# mentor confers a real boost. Gender and number of young children are both negative 
# and significant: women publish fewer articles and each additional child under 5
# is associated with reduced output. PhD program prestige and marital status are not 
# statistically significant. Together, the results point to early-career productivity
# being shaped by mentor environment, gender, and family demands - patterns consistent 
# with literature on PhD student outcomes in STEM fields.



### PART 2: Survival Analysis 
library(survival)
library(broom)
library(ggplot2)
library(marginaleffects)

lung <- survival::lung

## 2.1. Kaplan Meier survival curves

nrow(lung) # 228 total observations
lung$dead <- lung$status - 1
sum(lung$status == 2) # 165 deaths
sum(lung$status == 1) # 63 censored

sum(lung$status == 1) / nrow(lung) # 0.276, 27% censored. 

# Roughly 27% of the patients are censored, which is substantial enough to warrant 
# methods accounting for censoring, such as survival analysis.

# b) 

km_fit <- survfit(Surv(time, dead) ~ 1, data = lung)
summary(km_fit)

print(km_fit)

# The median survival time in 310 days, meaning that half of the patients are expected
# to survive at least 310 after the start of the observation. At day 310, we would 
# expect half the patients to have died.

# c) 

km_sex <- survfit(Surv(time, dead) ~ sex, data = lung)
km_df <- broom::tidy(km_sex)
head(km_df)

km_df$sex <- ifelse(km_df$strata == "sex=1", "Male", "Female")

ggplot(km_df, aes(x = time, y = estimate, color = sex, fill = sex)) +  
  geom_step(size = 1) +  
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), 
              alpha = 0.2, color = NA) +  
  labs(x = "Time (days)",    
       y = "Survival probability",    
       color = "Sex",    
       fill = "Sex",    
       title = "Kaplan-Meier Survival Curves by Sex") +
  theme_minimal()

ggsave("km_sex_plot.pdf", width = 7, height = 5)

logrank <- survdiff(Surv(time, dead) ~ sex, data = lung)
pval <-  1 - pchisq(logrank$chisq, df = length(logrank$n) - 1)
pval

# The KM curve shows that women survive longer than men. The confidence intervals
# overlap somewhat, particularly near the beginning, but the curve diverge as time 
# progresses. The p-value is 0.0013, so we reject the null hypothesis that survival 
# functions are the same across sexes. 


## 2.2 Cox proportional hazards model 

# a) 
cox_model <- coxph(Surv(time, dead) ~ age + sex + ph.ecog, data = lung)
summary(cox_model)

# The estimated hazard ratio for sex is 0.5754, suggesting that women have a 42% lower 
# risk of death at any given time compared to men (1 - 0.5754). This is statically 
# significant (p = 0.001) meaning this difference is unlikely to be due to chance.


# b) 

# The hazard ratio of ph.ecog is 1.59, meaning that a one unit increase in ECOG 
# performance, moving towards worsening physical health is associated with a 
# 59% higher risk of death. 

# c) 
cox.zph(cox_model)

# The p-values for age, sex and ph.ecog are 0.66, and 0.15 respectively, and the 
# global p-value is 0.22. Since all the p-values are greater than 0.05, there is 
# no statistically significant evidence that the proportional hazards assumption 
# is violated for any covariate or globally. The proportional hazards assumption 
# holds - the hazard ratios are constant over time. 

# d) 

# The KM analysis revealed a statistically significant survival advantage for women 
# compared to men (logrank test p = 0.0013). In the Cox proportional hazards model, 
# sex and ECOG performance status were significant predictors; women had 42% lower
# hazard of death than men (HR = 0.58, p = 0.001), and each unit increase in ECOG 
# performance decline was associated with a 59% higher hazard (HR = 1.59). The 
# proportional hazards assumption was satisfied globally (p = 0.22), validating 
# model interference. These results suggest that sex and physical health status 
# are key modifiable and non-modifiable predictors of survival in lung cancer patients. 

