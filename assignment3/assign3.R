library(tidyverse)
library(broom)
setwd("~/Desktop/AQMSSII/assignment3")
anes <- read_csv("https://raw.githubusercontent.com/franvillamil/AQM2/refs/heads/master/datasets/anes/anes_timeseries_2020.csv")

### PART 1 

## 1.1 

# a)
df = anes %>% 
  transmute(
    voted = ifelse(V202109x < 0, NA, V202109x), 
    age = ifelse(V201507x < 0, NA, V201507x),
    female = case_when(
      V201600 == 2 ~ 1,
      V201600 == 1 ~ 0, 
      TRUE ~ NA_real_), 
    education = case_when(
      V201511x == 1 ~ 10, V201511x == 2 ~ 12, V201511x == 3 ~ 14, 
      V201511x == 4~ 16, V201511x == 5~ 20, TRUE ~ NA_real_), 
    income = ifelse(V201617x < 0 , NA, V201617x), 
    party_id = ifelse(V201231x < 0, NA, V201231x)
    )
 
# b)
summary(df)
df = df %>% filter(!is.na(voted))
df = df %>% filter(!is.na(age))
df = df %>% filter(!is.na(female))
df = df %>% filter(!is.na(education))
df = df %>% filter(!is.na(income))
df = df %>% filter(!is.na(party_id))

# c) 
mean(df$voted) # Voter turnout = 0.86
summary(df)

## 1.2 Exploratory Visualization

turnout_by_edu <- df %>%
  group_by(education) %>%
  summarise(turnout_rate = mean(voted, na.rm = TRUE)
  )

ggplot(turnout_by_edu, aes(x = factor(education), y = turnout_rate)) +
  geom_col() +
  labs(
    x = "Education level",
    y = "Turnout rate",
    title = "Voter Turnout by Education Level"
  ) 

# 1.3 Linear Probability model 

# a) 
lpm = lm(voted ~ age + education + income + female, data = df)

# b) 
tidy(lpm)

# d) 
preds_lpm = predict(lpm) 
sum(preds_lpm < 0)
sum(preds_lpm > 1)
range(preds_lpm)

# 1.4 Logisitic regression 

# a)
logit = glm(voted ~ age + education + income + female, 
            family = binomial, data = df)

nd = data.frame(
  age = c(25, 50), 
  education = c(10, 10), 
  income = rep(20, 2),
  female = rep(1, 2)
)

predict(logit, newdata = nd)


# b) 
tidy(logit)

# c) 
exp(coef(logit))

# d) 
preds_logit = predict(logit, type = "response")
range(preds_logit)

## 1.5 comparing LPM and Logit

# a) 
library(marginaleffects)
library(modelsummary)
avg_slopes(logit)

# c) 
modelsummary(list("LPM" = lpm, "Logit" = logit), 
             vcov = list("robust", NULL))

## 1.6 Predicted Probabilities 

# a) 
p1 = plot_predictions(logit, condition = "education")
ggsave("pred_prob_education.png", p1, width = 6, height = 4)

p2 = plot_predictions(logit, condition = c("age", "female"))
p2

ggsave("pred_prob_age_gender.png", p2, width = 6, height = 4)

## 1.7 Presenting Results 

p3 = modelplot(list("LPM" = lpm, "Logit" = logit),vcov = list("robust", NULL))
p3

ggsave("coefplot_lpm_logit.png", p3, width = 6, height = 4)

### PART 2

## 2.2 Data Preparation

# a)
star <- read_csv("https://raw.githubusercontent.com/franvillamil/AQM2/refs/heads/master/datasets/star/star.csv")

star$classtype_f <- factor(
  star$classtype,
  levels = c(1, 2, 3),
  labels = c("small", "regular", "regular + aide")
) # factor variable for classtype 

star$race_f <- factor(
  star$race,
  levels = c(1, 2, 3, 4, 5, 6), 
  labels = c("White", "Black", "Asian", "Hispanic", "Native American", "Other")
) # factor variable for race 

# b)
star$small <- as.integer(star$classtype_f == "small") # binary variable for small class

# c)
sum(!is.na(star$hsgrad)) # 3047 non-missing observations

# d)
star %>%
  group_by(classtype_f) %>%
  summarise(
    grad_rate = mean(hsgrad, na.rm = TRUE),
    n = n()
  )
# Students in regular + aide classes exhibit the highest graduation rate (84%), 
# closely followed by small classes (83.6%), with regular classes exhibiting the 
# lowest graduation rate (82.5%). Overall the differences between class type are 
# very small. 

# 2.2 LPM and Logit

# a)
star <- star %>%
  mutate(small = ifelse(classtype_f == "small", 1, 0))

lpm1 = lm(hsgrad ~ small, data = star)
summary(lpm1)

# b) 
logit1 = glm(hsgrad ~ small, family = binomial, data = star)

# c)
# The LPM estimates that students in small classes are 0.38 percentage points 
# more likely to graduate from high school than students in non-small classes.

# d)
logit1 <- glm(hsgrad ~ small, data = star, family = binomial)
avg_slopes(logit1)

# Logit AME is 0.00375, which is identical to the LPM coefficient, indicating 
# that both models imply a very small difference in graduation probability between
# small and non-small classes.


## 2.3 Adding Controls 

# a)
lpm2 = lm(hsgrad ~ small + race + yearssmall, data = star)
logit2 = glm(hsgrad ~ small + race + yearssmall, family = binomial, data = star)
lpm2
logit2

# b)

# After controlling for race and years spent in a small class, the coefficient 
# for the controlled LPM is -0.073 and the controlled logit coefficient is -0.537.
# Both the sign and the magnitude change, which is consistent with successful 
# randomization which ensures balance in the bivariate estimation. 

# c)
avg_slopes(logit2)

# The average marginal effect of years spent in a small class is 0.027, implying 
# that each year spent in a small class is associated with a 2.7 pp increase in 
# the probability of graduating from high school. 

## 2.4 Predicted probabilities 

# a)

library(marginaleffects)

preds <- predictions(logit2, newdata = datagrid(race_f = c("White", "Black"),
    small = c(1, 0),
    yearssmall = c(3, 0))
)

preds

# For a white student with 3 years in a small class, the coefficient is 0.864, with 
# a CI - [0.840 0.886] meaning they have an 86.4% chance of graduating high school. 
# For a black student with zero years spent in a small class, the coefficient is
# 0.856 with a CI = [0.900, 0.941], so the predicted graduation probability slightly lower 
# at 85.6%, 
 

# b) 
p <- plot <- plot_predictions(logit2, condition = c("yearssmall", "small"))
p

ggsave(filename = "predicted_grad_probs.png", plot = p, width = 7, height = 5,
  dpi = 300)

# The plot shows that graduation probabilities rise with years spent in small classes,
# however at any given outcome, students in non-small classes have higher predicted 
# outcomes. 

## 2.5 Interactions 

logit3 = glm(hsgrad ~ small * race_f + yearssmall, family = binomial, data = star)
logit3

# The marginal effect of small classes varies across racial groups, but reliable 
# patterns are only observable for Black and Other students due to low sample sizes 
# for other groups. Black students experience a slightly smaller increase in
# graduation probability from small classes compared to White students, while 
# other students show an effect closer to zero. 

# b) 
avg_slopes(logit3, variables = "small", by = "race_f")

# c) 

# The AMEs show that the effect of small class sizes on graduation probabilities 
# varies across racial groups. For white students, being in a small class reduces 
# the probability of graduating by around 7.7 pp. For black students, the probability 
# is reduced by 10.3 pp. These are both statistically significant. For Asian 
# students, small class sizes have a positive impact, but this is not statistically 
# significant. For Native American and other students, there is essentially no effect. 
 
## 2.6 Presenting results and discussion 

# a)
models <- list(
  "LPM Bivariate" = lpm1,
  "LPM Controlled" = lpm2,
  "Logit Bivariate" = logit1,
  "Logit Controlled" = logit2
)
  vcov_list <- list(
    "LPM Bivariate" = "HC1",
    "LPM Controlled" = "HC1",
    "Logit Bivariate" = "HC0",
    "Logit Controlled" = "HC0")  # robust SE for LPMs only
 
modelsummary(models, vcov = vcov_list, stars = TRUE, output = "gt") 

# b)
modelplot(models, vcov = vcov_list) + 
  ggtitle("Effect of Small Classes on HS Graduation by Model") +
  theme_minimal()

# c) 

# The STAR data suggest that being in a small class has a modest negative effect 
# on high school graduation. While the bivariate models show a small positive effect, 
# the  controlled models show that once for race and years in a small class is accounted 
# for, the coefficient on small becomes negative (-0.073 in the LPM). The results 
# suggest that it is the compound effect of being in a small class (yearssmall)
# that increases students probability of graduating high school - this is consistent across 
# both the logit and LPM models. There is also evidence that students are randomly 
# assigned to small classes, ensuring the treatment effect is not confounded by 
# unobserved factors (e.g. parental income, students natural abilities).