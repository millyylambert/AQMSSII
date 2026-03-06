setwd("~/Desktop/AQMSSII/assignment5")
library(dplyr)
library(ggplot2)
library(fixest)
library(modelsummary)
library(readstata13)

### PART 1: Presidential Approval 

## 1.1 Setup and data exploration 

# a) 
df <- read.csv("https://raw.githubusercontent.com/franvillamil/AQM2/refs/heads/master/datasets/presidential_approval/presidential_approval.csv")
length(unique(df$State))
length(unique(df$Year))
table(table(df$State))

# b) 
summary(df$PresApprov)
summary(df$UnemPct)

df_sub = df %>%
  filter(State %in% c("California", "Texas", "NewYork"))

ggplot(df_sub, aes(x = Year, y = PresApprov, color = State)) +
  geom_line() +
  theme_minimal() +
  labs(x = "Year", y = "Presidential approval (%)", color = "State")

# c) 

ggplot(df, aes(x = UnemPct, y = PresApprov)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm") +
  theme_minimal() +
  labs(x = "Unemployment rate (%)", y = "Presidential approval (%)")

## 1.2 Pooled OLS 

# a) 
m_pooled = lm(PresApprov ~ UnemPct, data = df)
summary(m_pooled)

# b) 

m_pooled2 = lm(PresApprov ~ UnemPct + South, data = df)
summary(m_pooled2)

# Controlling for Southern state status changes the coefficeiint on UnemPct modestly, 
# suggesting the bivariate OLS estimate was not strongly confounded by the North-South 
# distinction: southern states differ systematically from the rest in their approval levels, 
# but this is not strongly correlated with the unemployment approval association in this
# pooled specification. 

# c)
# Pooled OLS is problematic for panel data because it ingores unobservable differences across 
# states that may be correlated with unemployment. States with hitorically weaker economies 
# may have higher structural unemployment and different political cultures that shape 
# baseline approval. States with large unionised labour forces may have both higher unemployment 
# sensitivity and different approval baselines. 

## 1.3 Entity fixed effects 

# a) 
m_fe = feols(PresApprov ~ UnemPct | State, data = df)

modelsummary(list("Pooled OLS" = m_pooled, "State FE" = m_fe), 
             vcov = ~State,
             stars = TRUE, 
             gof_map = c("r.squared", "nobs"))

# The coefficient on UnemPct changes relative to pooled OLS. The state fixed effects 
# model compares approval witin the same state across different years, removing the 
# influence of any time-invariant state characteristics. 

# b) 
# State fixed effects absorb all time-invariant differences across states - including 
# geography, politicla culture, long-run eocnomic structure, and regional identity. 
# This is why South drops from the model: it does not vary within a state over time, so its 
# effect is indistinguishable from the state-specifc intercept. Any time-invariant 
# variable is collinear with the set of state dummies and cannot be estimated separately. 



## 1.4 Two-way fixed effects 

# a) 
m_twfe = feols(PresApprov ~ UnemPct | State + Year, data = df)

# b) 
modelsummary(
  list("Pooled OLS" = m_pooled, "State FE" = m_fe, "Two-Way FE" = m_twfe),
  vcov = ~State,
  stars = TRUE,
  gof_map = c("r.squared", "nobs"))

# c) 
# Year fixed effects control for common time shocks, such as economic cycles, presidential scandals, 
# wars, or any other event that affects the DV in all locations of interest simultaneously.
# if the coefficienton UnemPct changes noticeably after adding year FEs, it suggests tha tcommon time 
# trends were party driving the relationship. 


### PART 2: Teaching Evaluations

## 2.1 Data exploration 

# a) 
evals <- read.dta13("~/Desktop/AQMSSII/data/teaching_evals.dta")
n_distinct(evals$InstrID) # 48 
n_distinct(evals$CourseID) # 254

n_distinct(evals$InstrID)/n_distinct(evals$CourseID)

# b) 

ggplot(df, aes(x = UnemPct, y = PresApprov)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm") +
  theme_minimal() +
  labs(x = "Average Course Evaluation", y = "Students receiving A/A- (%)")
  

## 2.2 Pooled OLS baseline 

# a) 
m1 = lm(Eval ~ Apct + Enrollment + Required, data = evals)
modelsummary(m1)

# The coefficient on Apct is 0.359, meaning a 1 pp increase in the share of A grades 
# is associated with a 0.359 pp increase in evaluation scores, holidng oter factors 
# constant. 

# b) 

# The OLS estimate may be biased due to ommited variables which affect both the 
# percentage of A grades and the teaching evaluations. One example is teaching 
# quality; better teachers may receive higher evaluation scores and their students 
# may achieve more top marks because they learn more. This would lead to an upward
# bias. Likewise, course difficulty could ahve an unobserved impact. Courses which are 
# harder or invovle a bigger workload may receive less A grades as well as lower 
# evaluations, leading to a downward bias. 

## 2.3 Fixed effects models  

# a) 
model_instr = feols(Eval ~ Apct + Enrollment + Required | InstrID, data = evals)
model_twfe = feols(Eval ~ Apct + Enrollment + Required | InstrID + Year, data = evals)

# b) 
modelsummary(
  list("Pooled OLS" = m1, "Instructor FE" = model_instr, "Two-Way FE" = model_twfe),
  vcov = ~InstrID,
  stars = TRUE,
  gof_map = c("r.squared", "nobs"))

# c) 
# The Apct coefficient in the instructor-FE model is 0.306, meaning that a 1 pp
# increase in the share of A grades is associated with a 0.306 pp increase in the 
# evaluation score. The instructr fixed effect controls for time-invariant characteristics 
# of the instructor, such as teaching ability and personality. The FE coefficient 
# is smaller than the pooled OLS estimate, suggesting the pooled estimate is upward 
# biased. This implies that instructors who give more A grades also have unobserved 
# characteristics that lead to hgiher evaluation scores, such as being more able 
# or more approachable to the students. 

## 2.4 Random effects and the Hausman test

# a) 
library(plm)
pdata = pdata.frame(evals, index = c("InstrID", "CourseID"))
m_re = plm(Eval ~ Apct + Enrollment + Required,
           data = pdata, model = "random")


# b) 
m_fe_plm = plm(Eval ~ Apct + Enrollment + Required,
               data = pdata, model = "within")
phtest(m_fe_plm, m_re)


# c) 

# The null hypothesis is that the unobserved instructor effects are uncorrelated 
# with the regressors and the alternative hypothesis is that these unobserved effects  
# are correlated with the regressors, in which case the random effects estimator 
# would be inconcsistent and FE should be preferred. The p-value is 0.178, so we 
# fail to reject the null hypothesis and the random effects should be preferred. .



                                                                                                       