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
# Regression coefficient is 3.10 meaning small classes score 3.10 points higher on average in Grade 4 reading than non-small classes. 

star %>%
  filter(classtype_f %in% c("small", "regular + aide")) %>%
  group_by(classtype_f) %>%
  summarise(mean_g4reading = mean(g4reading, na.rm = TRUE))
# small = 723, regular + aide = 721, which matches results from part a.

m_math <- star %>%
  lm(g4math ~ small, data = .) # Bivariate Regression for g4math

summary(m_math) 
# Coefficient for g4math is 0.59, so small classes score on average 0.59 points higher in math compared to the non-small class group. The effect is similar, but less pronounced in math compared to reading. 
