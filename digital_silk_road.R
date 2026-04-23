setwd("~/Desktop/Political Economy/Final Paper ")
library(tidyverse)
library("readxl")
library(devtools)

### Digital Silk Road 

## AidData 
aid_dta <- read_excel("~/Desktop/Political Economy/Final Paper /Global-chinese-development-finance-dataset.xlsx")
unique(aid_dta$`Sector Name`)

dsr_dta <- aid_dta %>%
  filter(`Sector Name` == "COMMUNICATIONS")

length(unique(dsr_dta$Recipient)) # There are 115 unique countries receiving "communications" assistance.

range(dsr_dta$`Commitment Year`) # 2000-2021

dsr_dta %>%
  count(`Recipient`) %>%
  summarise(mean_obs = mean(n), 
            min_obs = min(n), 
            max_obs = max(n))

complete_panel <- expand.grid(
  Recipient = unique(dsr_dta$Recipient),
  Year = 2000:2021)

dsr_aggregated <- dsr_dta %>%
  group_by(Recipient, `Commitment Year`) %>%
  summarise(total_investment = sum(`Amount (Constant USD 2021)`, 
                                   na.rm = TRUE)) %>%
  rename(Year = `Commitment Year`)

dsr_panel <- complete_panel %>%
  left_join(dsr_aggregated, by = c("Recipient", "Year")) %>%
  mutate(total_investment = replace_na(total_investment, 0))

summary(dsr_panel$total_investment)

## V-dem
library(vdemdata)
vdem <- vdemdata::vdem


