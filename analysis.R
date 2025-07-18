library(tidyverse)
library(lubridate)

# Load Data
unrate <- read_csv("data/UNRATE.csv") # you could copy and replace folders with your own data
youth_unrate <- read_csv("data/unemploymentrate16-24.csv") 
ratio <- read_csv("data/unemploymentrate16-24overunemploymentrateunrate.csv")

# Prepare Panel Data
merged_df <- youth_unrate %>%
  rename(youth_rate = value) %>%
  left_join(unrate, by = "date") %>%
  rename(total_rate = value)

panel_df <- merged_df %>%
  pivot_longer(cols = c("youth_rate", "total_rate"), names_to = "group", values_to = "rate") %>%
  mutate(treatment = ifelse(group == "youth_rate", 1, 0),
         post_2008 = ifelse(year(date) >= 2008, 1, 0),
         did_2008 = treatment * post_2008,
         post_covid = ifelse(year(date) >= 2020, 1, 0),
         did_covid = treatment * post_covid)

# DiD Models
model_2008 <- lm(rate ~ treatment + post_2008 + did_2008, data = panel_df)
model_covid <- lm(rate ~ treatment + post_covid + did_covid, data = panel_df)

summary(model_2008)
summary(model_covid)
