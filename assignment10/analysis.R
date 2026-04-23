setwd("~/Desktop/AQMSSII/assignment10")

# Assigment 10: Computing 

library(tidyverse)
library(modelsummary)
library(ggplot2)
library(haven)

# 1. Load the Data 
df <- read_csv("https://raw.githubusercontent.com/franvillamil/AQM2/refs/heads/master/datasets/presidential_approval/presidential_approval.csv")

glimpse(df)

# 2. Models

m1 <- lm(PresApprov ~ UnemPct + NEGPCT, data = df)
m2 <- lm(PresApprov ~ UnemPct + NEGPCT + PARTY + ELECTP + South, data = df)

dir.create("tab", showWarnings = FALSE)
dir.create("img", showWarnings = FALSE)

options(modelsummary_factory_latex = "kableExtra")

modelsummary(
  list("Economic" = m1, "Full Model" = m2),
  stars = TRUE,
  gof_map = c("nobs", "r.squared", "adj.r.squared"),
  coef_rename = c(
    "UnemPct"        = "Unemployment (%)",
    "NEGPCT"         = "Negative news (%)",
    "PARTY2"         = "Republican president",
    "ELECTP2"        = "Second term",
    "South"          = "Southern state",
    "(Intercept)"    = "Intercept"
  ),
  output = "tab/reg_table.tex"
)


fig <- ggplot(df, aes(x = UnemPct, y = PresApprov)) +
  geom_point(aes(color = NAME), alpha = 0.5, size = 1.5) +
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  labs(
    title   = "Unemployment and Presidential Approval",
    x       = "Unemployment Rate (%)",
    y       = "Presidential Approval (%)",
    color   = "President",
    caption = "State-level observations, 1980–present"
  ) +
  theme_bw()

ggsave("img/scatter_approval.pdf", plot = fig, width = 7, height = 5)
