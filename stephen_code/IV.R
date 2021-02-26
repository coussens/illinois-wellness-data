library(estimatr)
library(tidyverse)
claims <- read_dta("C:/Users/stephen/Documents/Git/illinois-wellness/data/stata/claims.dta")

claims <- claims %>% rename(z = treat, d = hra_c_yr1) %>%
  mutate(d = replace_na(d, 0))

claims_iv <- iv_robust(spend_0816_0717 ~ d|z, data = claims, weights = covg_0816_0717, se_type = "HC1")
summary(claims_iv)

claims_fs <- lm_robust(d ~ z, data = claims, weights = covg_0816_0717, se_type = "HC1")
summary(claims_fs)


part <- read_dta("C:/Users/stephen/Documents/Git/illinois-wellness/data/stata/participation.dta")

part <- part %>% mutate(z = treat) %>%
  mutate(d = replace_na(hra_c_yr2, 0), y = replace_na(activity_s_c_yr2, 0))

part_fs <- lm_robust(d ~ z, data = part,  se_type = "HC1")
summary(part_fs)

part_iv <- iv_robust(y ~ d|z, data = part, se_type = "HC1")
summary(part_iv)