setwd("~/Desktop/Political Economy/Final Paper ")

#### 1. SET UP AND DATA ========================================================================
library(tidyverse)  
library(fixest)  
library(modelsummary)

## WDI 
library(WDI)

wdi_raw <- WDI(
  country = "all",
  indicator = c(
    gdppc = "NY.GDP.PCAP.CD",
    trade = "NE.TRD.GNFS.ZS",
    oil = "NY.GDP.PETR.RT.ZS",
    minerals = "NY.GDP.MINR.RT.ZS"
  ),
  start = 2000,
  end = 2022
)

wdi_clean <- wdi_raw %>%
  select(iso3c, year, gdppc, trade, oil, minerals)
  
glimpse(wdi_clean)


## IV: VDEM 
library(vdemdata)
vdem <- vdemdata::vdem

vdem_clean <- vdem %>%  # Clean Vdem
  select(
    country_name,
    country_text_id,    # ISO3 country code
    year,
    v2xcl_prpty,        # property rights (main IV)
    v2x_rule,           # rule of law (robustness)
    v2x_corr            # corruption (robustness)
  ) %>% 
  filter(year >= 2000 & year <= 2022)

glimpse(vdem_clean)


## DV: AidData Chinese Development Finance
aid_data <- read_csv("~/Desktop/Political Economy/Final Paper /Aiddata-chinese-development-finance-dataset.csv")

aid_raw <- aid_data %>%   # Selecting variables 
  select(
  iso3c = `Recipient ISO-3`,
  country = Recipient,
  region = `Recipient Region`,
  year = `Commitment Year`,
  sector = `Sector Name`,
  amount = `Amount (Constant USD 2021)`
) 

aid_raw %>%
  summarise(across(everything(), ~ mean(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "missing_share") %>%
  arrange(desc(missing_share))

# 39% of amount figures are missing, and about 1.3% are missing iso3 codes. The Iso 
# codes can safely be dropped without affecting the analysis too much. 

# They are all regional, multi-country projects, as they cannot consistently be 
# mapped onto country-level covariates.

aid_clean <- aid_raw %>%  # Clean AID
  filter(
    !is.na(iso3c),
    !is.na(year),
    !is.na(amount))

# Observations with missing financial values (39%) were excluded, as the analysis 
# focuses on the magnitude of flows. This approach is consistent with existing 
# literature using AidData.

aid_cy <- aid_clean %>% # aggregating to country-year
  group_by(iso3c, country, region, year) %>%
  summarise(
    aid_total = sum(amount, na.rm = TRUE),
    .groups = "drop"
  )
 

## Merging Data ========================================================================
regression_data <- aid_cy %>% # Merging data sets
  left_join(wdi_clean, by = c("iso3c", "year")) %>%
  left_join(vdem_clean, by = c("iso3c" = "country_text_id", "year"))


setdiff(unique(regression_data$iso3c), unique(vdem_clean$country_text_id))

# ISO-3 codes that are in my regression sample that are not in V-Dem. Countries
# include Antigua and Barbuda, Micronesia, Kiribati, Nauru, etc. I will restrict
# sample to overlap. 

regression_data_clean <- regression_data %>%
  filter(!is.na(v2xcl_prpty)) %>%
  drop_na(gdppc, trade, oil, minerals, aid_total) 

# Estimation is restricted to country-years with non-missing V-Dem institutional 

regression_data_clean <- regression_data_clean %>%
  arrange(iso3c, year) %>%
  group_by(iso3c) %>%
  mutate(
    log_aid         = log1p(aid_total),
    log_gdppc       = log(gdppc),
    nat_resources   = oil + minerals,
    v2xcl_prpty_lag = lag(v2xcl_prpty, 1),
    v2x_corr_lag    = lag(v2x_corr, 1)
  ) %>%
  ungroup()

# Lagged institutional variables, logged GDP and Aid. 

# The lagged specification reduces the sample by 118 observations, which is the 
# loss of one observation per country for the first year of the sample.

### 2. Diagnostics ========================================================================

# Correlation check
regression_data_clean %>%
  select(v2xcl_prpty, v2x_rule, v2x_corr) %>%
  cor(use = "complete.obs")

# v2x_rule and v2x_corr are highly collinear, meaning rule of law and corruption are
# highly correlated. This warrants them being treated as a single robustness dimension. 
# Interestingly, property rights and corruption are only weakly correlated (-0.33). 
# Since they appear to capture meaningfully different aspects of institutional quality
# they will be retained as the robustness checks. Property rights captures the quality of
# economic institutions, whereas the corruption index would seem to better capture 
# the efficacy of governance/political institutions. China seemingly responds differently 
# to economic institutions versus governance quality.

# Descriptive statistics 

regression_data_clean %>%
  select(
    log_aid, v2xcl_prpty_lag, v2x_corr_lag,
    log_gdppc, trade, nat_resources
  ) %>%
  datasummary_skim(output = "markdown") # Logs

# How many unique countries in sample?
n_distinct(regression_data_clean$iso3c) # 118

# How many years?
range(regression_data_clean$year) # 2000-2021 (2022 dripped due to lag)

# Top 10 recipients by total investment
aid_cy %>%
  group_by(country) %>%
  summarise(total = sum(aid_total, na.rm = TRUE) / 1e9) %>%
  arrange(desc(total)) %>%
  slice_head(n = 10)

# Which regions are most represented?
aid_cy %>%
  distinct(iso3c, region) %>%
  count(region) %>%
  arrange(desc(n))

# Most projects take place in Africa (53) with America second place with 30. The 
# fewest projects occur in the Middle East (9).

### Models ========================================================================

model_main <- feols(
  log_aid ~ v2xcl_prpty_lag + log_gdppc + trade + nat_resources | iso3c + year,
  data = regression_data_clean,
  cluster = "iso3c"
)   # Property rights 

modelsummary(model_main, stars = TRUE, statistic = "({std.error})")   


model_corr <- feols(
  log_aid ~ v2x_corr_lag + log_gdppc + trade + nat_resources | iso3c + year,
  data = regression_data_clean,
  cluster = "iso3c"
)   # Corruption

modelsummary(model_corr, stars = TRUE)

modelsummary(
  list("Property Rights (Lagged)" = model_main,
       "Corruption (Lagged)"      = model_corr),
  stars = TRUE,
  statistic = "({std.error})",
  coef_rename = c(
    v2xcl_prpty_lag = "Property Rights (t-1)",
    v2x_corr_lag    = "Corruption (t-1)",
    log_gdppc       = "Log GDP per capita",
    trade           = "Trade Openness",
    nat_resources   = "Natural Resources"
  ))


### 3. Visualizations ===============================================================

# Plot 1: Distribution of DV
ggplot(regression_data_clean, aes(x = log_aid)) +
  geom_histogram(bins = 40, fill = "steelblue", colour = "white") +
  labs(
    title = "Distribution of Log Chinese BRI Investment",
    x     = "Log Investment (Constant USD 2021)",
    y     = "Count"
  ) +
  theme_minimal()

# Plot 2: Property Rights vs Investment
ggplot(regression_data_clean, aes(x = v2xcl_prpty_lag, y = log_aid)) +
  geom_point(alpha = 0.2, colour = "steelblue") +
  geom_smooth(method = "lm", colour = "firebrick", se = TRUE) +
  labs(
    title = "Property Rights and Chinese BRI Investment",
    x     = "Property Rights Index, t-1 (V-Dem)",
    y     = "Log BRI Investment"
  ) +
  theme_minimal()

# Plot 3: BRI investment over time
regression_data_clean %>%
  group_by(year) %>%
  summarise(total_aid = sum(aid_total, na.rm = TRUE) / 1e9) %>%
  ggplot(aes(x = year, y = total_aid)) +
  geom_line(colour = "steelblue", linewidth = 1) +
  geom_vline(xintercept = 2013, linetype = "dashed", colour = "firebrick") +
  annotate("text", x = 2013.3, y = Inf, vjust = 2,
           label = "BRI Launch (2013)", colour = "firebrick", hjust = 0) +
  labs(
    title = "Chinese Development Finance Over Time",
    x     = "Year",
    y     = "Total Investment (USD Billions)"
  ) +
  theme_minimal()

# Plot 4: Coefficient plot 
modelplot(
  list("Property Rights" = model_main,
       "Corruption"      = model_corr),
  coef_omit = "Intercept"
) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(title = "Effect of Institutional Quality on BRI Investment") +
  theme_minimal()
