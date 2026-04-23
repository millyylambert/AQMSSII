setwd("~/Desktop/AQMSSII/assignment6")
library(dplyr)
library(tidyr)
library(ggplot2)
library(fixest)
library(modelsummary)
library(did)

df <- read.csv("https://raw.githubusercontent.com/franvillamil/AQM2/refs/heads/master/datasets/other/minwage.csv")

### PART 1: MINIMUM WAGE

## Data setup and exploration 

# a) 
df = df %>% mutate(NJ = ifelse(location != "PA", 1, 0))
table(df$NJ)

# There are more restaurants in NJ than PA.

df %>%
  group_by(NJ) %>%
  summarise(mean_wage_before = mean(wageBefore, na.rm = TRUE),
            mean_wage_after = mean(wageAfter, na.rm = TRUE))

# The average wage before was roughly equal in both NJ (4.61) and PA (4.65), however 
# after the policy change the average wage in NJ increased to (5.08) and the average wage 
# in PA very slightly decreased, to (4.61). This confirms that the policy raised wages 
# in the treated state. 

# b) 
means = df %>%
  group_by(NJ) %>%
  summarise(before = mean(fullBefore, na.rm = TRUE),
            after = mean(fullAfter, na.rm = TRUE),
            change = after - before)
means

nj_change = means$change[means$NJ == 1]
pa_change = means$change[means$NJ == 0]
did_est = nj_change - pa_change
cat("DiD estimate:", round(did_est, 3), "\n")

# c) 

df_long = df %>%
  mutate(id = row_number()) %>%
  pivot_longer(cols = c(fullBefore, fullAfter),
               names_to = "period",
               values_to = "full_emp") %>%
  mutate(post = ifelse(period == "fullAfter", 1, 0),
         NJ = ifelse(location != "PA", 1, 0))

nrow(df_long)
nrow(df)

## 1.2 DiD Regression

# a) 
m_did = feols(full_emp ~ post * NJ, data = df_long, cluster = ~id)
modelsummary(m_did, stars = TRUE)


# The coeffcient on post x NJ is 2.927, which is the same from 1.1b) 

# b) 
m_did_fe = feols(full_emp ~ post * NJ | chain, data = df_long, cluster = ~id)
modelsummary(list("DiD" = m_did, "DiD + Chain FE" = m_did_fe), 
             stars = TRUE)

# The DiD estimate did not change when chain FEs were added. Chain FEs absorb 
# baseline differences in staffing levels across fast-food chains, but since chain 
# type is roughly balanced across states, controlling for it has little impact on the 
# DiD coefficient. 

# c) 
# The parallel trends assumption requires that, absent the NK minimum wage increase
# employement trends in NJ and PA fast-food chains would have been the same from 
# Feb to NOv 1992. Both states share similar economic conditions and the two surveys 
# were cose in tiem, limiting opportuniies for divering trends. A violation would occur 
# if NJ experienced and independent shock during this period. 

## 1.3 Wages as a validation check 

# a) 
df_long_wage = df %>%
  mutate(id = row_number()) %>%
  pivot_longer(cols = c(wageBefore, wageAfter),
               names_to = "period",
               values_to = "wage") %>%
  mutate(post = ifelse(period == "wageAfter", 1, 0),
         NJ = ifelse(location != "PA", 1, 0))

m_wage = feols(wage ~ post * NJ, data = df_long_wage, cluster = ~id)

modelsummary(m_wage, stars = TRUE)

# The interaction coefficient is positive and significant, meaning wages in NJ rose 
# substantially compared to PA after the policy change - this is the effect one would 
# expect. 

# b) 
# The wage DiD serves as a manipulation check. If wages had no riseen in NJ after 
# the minimum wage increase, it would be unclear whether the study is truly estimating 
# the effect of a minimum wage change at all - the law may not have been binding or 
# firms were already paying above the new minimum. 

## PART 2: Staggered DiD with the did package 

install.packages("did")
library(did)
data(mpdta)

## 2.1 Data structure and visualisation 

# a) 
length(unique(mpdta$countyreal))
# 500 counties 

length(unique(mpdta$first.treat))
# 4 cohorts, 1 control and 3 treatment cohorts. 

table(mpdta$first.treat)
# Staggered treatment means treated units received the treatment at different times, 
# meaning the counties treated later may act as controls for earlier-treated counties
# hence you cannot just compare treated vs untreated. 

# b) 

mpdta_avg = mpdta %>%
  mutate(cohort = factor(first.treat,levels = c(0, 2004, 2006, 2007),
                         labels = c("Never treated", "Adopted 2004",
                                    "Adopted 2006", "Adopted 2007"))) %>%
  group_by(year, cohort) %>%
  summarise(mean_lemp = mean(lemp, na.rm = TRUE))

ggplot(mpdta_avg, aes(x = year, y = mean_lemp, color = cohort)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(x = "Year", y = "Log teen employment", color = "Treatment cohort")

ggsave("log_teen_employment.png")

# Before treatment all cohorts follow mildly declining or flat trends. The 2004 
# cohort is in particularly sharp decline before (and after) treatment, which may be
# problematic for the parallel trends assumption. The 2006 
# cohort witnessed marginal increases in employment starting in 2004 and lasting
# until 2006 when they received treatment, after which there is a small decline again. 

## 2.2 Naive TWFE vs Callaway-Santanna 

# a) 

mpdta$treated_post <- ifelse(mpdta$first.treat > 0 & mpdta$year >= mpdta$first.treat, 1, 0)
table(mpdta$treated_post)

twfe_model <- feols(log(lemp) ~ treated_post | countyreal + year, data = mpdta)
summary(twfe_model)

# The coefficient on treated_post is -0.006 implying that treatment is associated 
# with a 0.6% decrease in teen employment. The model pools all treated cohorts together
# into a single treatment indicator, implicitly assuming that the treatment effect 
# is consistent over time i.e. counties treated at different times are assumed to 
# experience the same effect. 

# b) 

out <- att_gt(
  yname = "lemp",
  gname = "first.treat",
  idname = "countyreal",
  tname = "year",
  xformla = ~1,
  data = mpdta,
  est_method = "reg"
)

summary(out)

agg_overall <- aggte(out, type = "simple")
summary(agg_overall)

# The ATT estimated using the Callaway-Santanna model is -0.04. This represents 
# the average effect of treatment across all cohorts and time periods, using 
# never-treated counties as the control group. Treated groups have 4% lower employment. 
# This is larger than the naive TWFE model estimate, suggesting the TWFE underestimates 
# the true treatment effect. likely because it homogenizes the treatment effects. 

es <- aggte(out, type = "dynamic")
summary(es)
ggdid(es)

ggsave("event_study_plot.png", width = 6, height = 4)

# The pre-treatment leads are hardly distinguishable from zero, meaning the pre-treated
# counties were evolving similarly which supports the parallel trends assumption. 
# The post-treatment estimates are all below zero, telling us the minimum wage policy
# consistently reduces teen employment across all post-treatment periods. 

## 2.3 

# a) 
cs_model_boot <- att_gt(
  yname = "lemp",             
  tname = "year",            
  idname = "countyreal",     
  gname = "first.treat",     
  data = mpdta,
  panel = TRUE,
  bstrap = TRUE,             
  cband = TRUE               
)

agg_dynamic_boot <- aggte(cs_model_boot, type = "dynamic")
summary(agg_dynamic_boot)

pre_coef <- agg_dynamic_boot$att[agg_dynamic_boot$egt < 0]
pre_se   <- agg_dynamic_boot$se[agg_dynamic_boot$egt < 0]

wald_stat <- sum((pre_coef / pre_se)^2)
df <- length(pre_coef)
p_value <- 1 - pchisq(wald_stat, df)

p_value 

# The pre-test evaluates whether all pre-treatment (event-time < 0) ATT estimates
# are jointly zero. The p-value is 0.08, which is larger than 0.05 so we fail to 
# reject the null hypothesis, suggesting the pre-treatment trends are statistically 
# indistinguishable from zero, consistent with the parallel trends assumption. 

# b) 

ggdid(cs_model_boot) +
  theme_minimal() +
  labs(
    title = "Group-Time ATT Estimates (Event Study)",
    x = "Event Time",
    y = "ATT(g,t)"
  )

ggsave("group_time_ATT_plot.png", width = 8, height = 5)

# The pre-treatment estimates are all indistinguishable from zero for the 2006 and 
# 2007 cohorts, supporting the parallel trends assumption before treatment. 

# c) 
# Even if the pre-test does not reject the null, we cannot be certain that the 
# parallel trends hold in the post-treatment period. The pre-test checks for differences
# in the pre-treatment period.

## 2.4 Comparing control group specifications

# a) 

cs_model_notyet <- att_gt(
  yname = "lemp",
  tname = "year",
  idname = "countyreal",
  gname = "first.treat",
  data = mpdta,
  panel = TRUE,
  bstrap = TRUE,
  cband = TRUE,
  control_group = "notyettreated" 
)

agg_overall_nyt <- aggte(cs_model_notyet, type = "simple")
summary(agg_overall_nyt)

# The overall ATT with not-yet-treated controls is -0.0398, which is very similar 
# (both sign and magnitude) to the ATT from 2.2b (-0.04).  

# b) 

agg_dynamic_nyt <- aggte(cs_model_notyet, type = "dynamic")

ggdid(cs_model_notyet) +
  theme_minimal() +
  labs(
    title = "Event-Study ATT Estimates (Not-Yet-Treated Controls)",
    x = "Event Time",
    y = "ATT(g,t)"
  )

ggsave("event_study_notyet.png", width = 8, height = 5)

# Comparing the pre-treatment estimates to the never-treated event study, the pre-
# trends are similarly close to zero, and the post-treatment patters are similar 
# suggesting that the choice of control group does not substantially change the 
# conclusions. 

# c) 
# Using never-treated counties as controls avoids the assumption that future-treated 
# units are unaffected by the anticipation of their treatment, making the identification 
# cleaner. However, it reduces the number of control units, which can reduce precision. 
# Never-treated is preferred when the assumption about future-treatment anticipation is 
# questionable or there are plenty of never-treated units. Not-yet-treated is useful 
# when never-treated are scarce. 

## 2.5 Discussion 

# a) 

# The naive TWFE can produce misleading results in staggered DiD setting because 
# it uses already-treated units as controls for later-treated units, thereby comparing
# the outcomes of treated units to other units which have also been treated, but at 
# different times. The earlier-treated units' outcomes may already reflect treatment 
# effects which is problematic for causality. 

# b) 

# The TWFE estimate (-0.006) in 2.2a is much smaller than the Callaway-Santanna 
# estimate in 2.2b (-0.04). Given the event-study results from question 2.2c, the 
# pre-treatment trends appear appear flat which supports the parallel trends 
# assumption. The Callaway-Santanna estimator is more credible because it avoids 
# the flawed comparison present in TWFE and properly accounts for staggered treatment 
# timing, thereby providing a more reliable estimate of the treatment effect. 



