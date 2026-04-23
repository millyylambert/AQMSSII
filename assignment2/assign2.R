library(tidyverse)
setwd("~/Desktop/AQMSSII/assignment2")

## Part 1: QoG dataset
qog <- read.csv("~/Desktop/AQMSSII/data/qog_std_cs.csv")
nrow(qog)
names(qog)

## 1.1
df <- qog %>%
  
  # Renaming variables 
  select(cname, epi_epi, wdi_wip, wbgi_gee, cpds_lg)

df <- df %>%
  rename(
    country = cname,
    epi = epi_epi,
    women_parl = wdi_wip,
    gov_eff = wbgi_gee,
    green_seats = cpds_lg
  )
names(df)
summary(df) # 194 countries 

  # Drop missing variables 
df <- df %>% 
  drop_na()
nrow(df) # 36 countries remain
summary(df)

## 1.2
  
  # Scatter plotting the data
ggplot(df, aes(x = women_parl, y = epi)) + 
  geom_point() +
  geom_smooth(method = lm, se = TRUE) +
  labs (
    x = "Share of Women in Parliament",
    y = "Environmental Protection Index Score",
    title = "Women in Parliament vs EPI"
  ) +
  theme_minimal()

# The scatter plot shows a positive relationship between share of women in 
# parliament and EPI score. The points are very dispersed, but the linear trend 
# suggests that increasing the share of women in parliament is associated with 
# better environmental performance. 


## 1.3 

  # Bivariate regression 
model_1 <- lm(epi ~ women_parl, data = df)
tidy(model_1)

# The intercept is 58.6, meaning this is the predicted EPI value when 
# women_parl = 0. The coefficient is 0.194 meaning for each percentage point 
# increase in women in parliament, the EPI score increases by 0.194 percentage 
# points, on average. 

  # Predicted difference 
predicted_diff <- (quantile(df$women_parl, 0.75) - quantile(df$women_parl, 0.25)) * coef(model_1)["women_parl"]
predicted_diff

# The predicted difference is 2.99, which means moving from the 1st quartile of 
# women in parliament to the 3rd quartile increases the EPI score by almost 3 points. 

## 1.4 

  # Multiple regression of EPI on women_parl controlling for gov_eff

model_2 <- lm(epi ~ women_parl + gov_eff, data = df)
tidy(model_2)
  
# In the multiple regression model, the coeffcient for women_parl dropped to 
# 0.075 after controlling for government efficeincy. This suggests that some of 
# the relationship between women in parliament and EPI is explained by government 
# effectiveness. Countires with more women in parliament also tend to have more 
# effective governments which contributes to higher EPI scores. 

## 1.5 

# β₁ (Bivariate) = 0.194
# β₁ (Multiple) = 0.075
# β₂ (Multiple) = 4.22

  # Auxiliary model 
aux_model <- lm(gov_eff ~ women_parl, data = df)
summary(aux_model)
# δ = 0.028

  # Verifying the OVB model 
0.0751 + 4.22 * coef(aux_model)["women_parl"]

# The OVB formula matches the bivariate coefficient confirming that the positive
# association in the bivariate model was partly due to omitted variable bias. 

## 1.6
library(modelsummary)
modelsummary(model_2)

# SEs = 0.092 (women_parl) and 1.685 (gov_eff)

# Robust SEs
modelsummary(model_2, vcov = "robust")

# SEs = 0.132 (women_parl) and 1.864 (gov_eff)

# Using robust standard errors the SE's for both women_parl and gov_eff
# increased slightly reflecting potential heteroskedasticity in the data. The 
# substantive conclusion that women in parliament have minimal effect on EPI 
# after controlling for government effectiveness does not change. 

# 1.7

modelsummary(list("Bivariate" = model_1, "Multiple" = model_2),
             vcov = "robust",
             )

coef_plot <- modelplot(list("Bivariate" = model_1, "Multiple" = model_2),
                       vcov = "robust",
                       dodge_size = 0.5) +
  ggtitle("Comparison of Bivariate vs Multiple Regression Coefficients") +
  theme_minimal() +
  coord_flip()


coef_plot

ggsave("regression_coeff_plot.png", plot = coef_plot, width = 8, height = 5, dpi = 300)



## Part 2: STAR dataset 

## 2.1 Data Preparation 
star <- read.csv("https://github.com/franvillamil/AQM2/raw/refs/heads/master/datasets/star/star.csv")

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

star$small <- as.integer(star$classtype_f == "small") # binary variable for small class

nrow(star) # number of observations = 6325
sum(!is.na(star$g4reading)) # non-missing observations for g4reading = 2353
sum(!is.na(star$g4math)) # non-missing observations for g4math = 2395

## 2.2 Comparing groups 

star %>%
  group_by(classtype_f) %>%
  summarise(
    mean_g4reading = mean(g4reading, na.rm = TRUE)
  ) %>%
  arrange(desc(mean_g4reading)) # Mean 4th grade reading level for each class type
# Small classes score the highest on average [723]

m_reading <- star %>%
  lm(g4reading ~ small, data = .) # Bivariate Regression for g4reading 

summary(m_reading) 
# Regression coefficient is 3.10 meaning small classes score 3.10 points higher 
# on average in Grade 4 reading than non-small classes. 

star %>%
  filter(classtype_f %in% c("small", "regular + aide")) %>%
  group_by(classtype_f) %>%
  summarise(mean_g4reading = mean(g4reading, na.rm = TRUE))
# small = 723, regular + aide = 721, which matches results from part a.

m_math <- star %>%
  lm(g4math ~ small, data = .) # Bivariate Regression for g4math

summary(m_math) 
# Coefficient for g4math is 0.59, so small classes score on average 0.59 points 
# higher in math compared to the non-small class group. The effect is similar, 
# but less pronounced in math compared to reading. 

## 2.3 Adding Controls 

model_reading_controls <- star %>%
  lm(g4reading ~ small + race_f + yearssmall, data = .)

summary(model_reading_controls) # Multiple Regression of g4reading
# Holding race and years spent in a small class constant, students in small 
# classes score 4 points lower in 4th grade reading than students in regular or 
# regular + aide classes. The coeffcient on small has become negative compared 
# to the positive +3.10 coefficient on the bivariate, suggesting the bivariate 
# difference was picking up correlated factors, such as cumulative exposure to 
# small classes. 

# The coefficient on yearssmall is 2.17, so holding class type and race constant,
# each additional year spent in a small class is associated with an increase of 
# approx 2.17 points in 4th grade reading scores. Any positive effect of small 
# classes operates through cumulative exposure, rather than being in a small 
# class in the current year. 

## 2.4 Interactions 

m_interaction <- lm(g4reading ~ small * race_f + yearssmall, data = star) # Interaction model 

library(broom)
tidy(m_interaction)

# For white students, being in a small class is associated with a 5.3 point lower 
# reading score relative to white students in regular or regular + aide classes. 
# Adding the interaction term [+6.97] for black students, the small class effect 
# is +1.65, so slightly positive but not statistically significant. 

# The interaction term suggests the effect of small class sizes differs across 
# racial groups. However, none of the estimated effects are statistically 
# significant, and sever are imprecise due to small smample sizes for some race 
# groups. I would say there is limited support that the effect of small classes 
# differs meaningfully by race. 

## 2.5 Presenting the data

library(sandwich)

# Table 
models <- list(
  "Bivariate"   = m_reading,
  "Controls"    = model_reading_controls,
  "Interaction" = m_interaction
)

tab <- modelsummary(
  models,
  vcov = "HC1",
  stars = TRUE,
  output = "gt"
)

# Plot 
coef_plot <- modelplot(
  models,
  vcov = "HC1",
  coef_keep = "small" # Plot only small coefficient
) +
  theme_minimal() +
  labs(
    title = "Effect of Small Class Size on 4th Grade Reading Scores",
    x = "Coefficient estimate",
    y = ""
  )

print(coef_plot)

library(gt)

gtsave(tab, "reading_models.png")
ggsave("reading_coefficients.png",plot = coef_plot, width = 8, height = 6, dpi = 300)  # Save the plot


## 2.6 
# The STAR data suggest being in a small class size has a positive effect on 4th 
# grade reading and math scores, although the effect is modest. The magnitude
# varies when controls and interactions are included. The bivariate models show 
# that small classes appear to score higher than regular or regular + aide classes. 
# However, once race and yearsmall (cumulative exposure) are included, the direct 
# effect is smaller and sometimes negative. Cumulative exposure is an important 
# mechanism for improving student outcomes. Students were randomly assigned to 
# small classes, controlling for potential confounders such as socioeconomic status.
# There are limitations however, for example, some racial groups have very few 
# students in small classes, reducing the effective sample size. 
