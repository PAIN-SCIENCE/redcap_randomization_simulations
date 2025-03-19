library(tidyverse)
library(ggplot2)
library(dplyr)
library(tidyr)

#############
# SAMPLE 1 #
############
n <- 100
record_id <- 1:n
sex <- sample(c("Male", "Female"), n, replace = TRUE, prob = c(0.40, 0.60))
age <- rnorm(n, mean = 67.4, sd = 9)
short_mac <- rnorm(n, mean = 6, sd = 3)

# Restrict age and short_mac to defined ranges
age <- pmax(50, pmin(90, round(age)))
short_mac <- pmax(4, pmin(20, round(short_mac)))

df <- data.frame(
  record_id = record_id,
  age = age,
  sex = sex,
  short_mac = short_mac
)

# Quick histogram check for short_mac
df %>% 
  ggplot(aes(x = short_mac)) + 
  geom_histogram(bins = 10)

# Create age_in_range variable:
# 1: ages ≤ 63, 64-72, ≥ 73
df <- df %>% 
  mutate(age_in_range = cut(age, breaks = c(50, 64, 73, Inf),
                            right = FALSE, labels = FALSE))

# Create sexatbirth variable: 1 = Female, 2 = Male
df <- df %>% 
  mutate(sexatbirth = if_else(sex == "Female", 1, 2))

# Create sum_pain_intensity variable based on short_mac:
# 1: 4-7, 2: 8-11, 3: 12-20
df <- df %>% 
  mutate(sum_pain_intensity = cut(short_mac, breaks = c(4, 8, 12, 21),
                                  right = FALSE, labels = FALSE))

# Optional: view tables of the new coded variables
table(df$age_in_range)
table(df$sexatbirth)
table(df$sum_pain_intensity)

# Save to CSV
write_csv(df, "~/Documents/1_REJOICE_sim.csv")

#############
# SAMPLE 2 #
############
n <- 100
record_id <- 1:n
sex <- sample(c("Male", "Female"), n, replace = TRUE, prob = c(0.40, 0.60))
age <- rnorm(n, mean = 67.4, sd = 9)
short_mac <- rnorm(n, mean = 7, sd = 3)

# Restrict age and short_mac to defined ranges
age <- pmax(50, pmin(90, round(age)))
short_mac <- pmax(4, pmin(20, round(short_mac)))

df <- data.frame(
  record_id = record_id,
  age = age,
  sex = sex,
  short_mac = short_mac
)

# Quick histogram check for short_mac
df %>% 
  ggplot(aes(x = short_mac)) + 
  geom_histogram(bins = 10)

# Create age_in_range variable:
# 1: ages ≤ 63, 64-72, ≥ 73
df <- df %>% 
  mutate(age_in_range = cut(age, breaks = c(50, 64, 73, Inf),
                            right = FALSE, labels = FALSE))

# Create sexatbirth variable: 1 = Female, 2 = Male
df <- df %>% 
  mutate(sexatbirth = if_else(sex == "Female", 1, 2))

# Create sum_pain_intensity variable based on short_mac:
# 1: 4-7, 2: 8-11, 3: 12-20
df <- df %>% 
  mutate(sum_pain_intensity = cut(short_mac, breaks = c(4, 8, 12, 21),
                                  right = FALSE, labels = FALSE))

# Optional: view tables of the new coded variables
table(df$age_in_range)
table(df$sexatbirth)
table(df$sum_pain_intensity)

# Save to CSV
write_csv(df, "~/Documents/2_REJOICE_sim.csv")

#############
# SAMPLE 3 #
############
n <- 100
record_id <- 1:n
sex <- sample(c("Male", "Female"), n, replace = TRUE, prob = c(0.40, 0.60))
age <- rnorm(n, mean = 67.4, sd = 9)
short_mac <- rnorm(n, mean = 8, sd = 3)

# Restrict age and short_mac to defined ranges
age <- pmax(50, pmin(90, round(age)))
short_mac <- pmax(4, pmin(20, round(short_mac)))

df <- data.frame(
  record_id = record_id,
  age = age,
  sex = sex,
  short_mac = short_mac
)

# Quick histogram check for short_mac
df %>% 
  ggplot(aes(x = short_mac)) + 
  geom_histogram(bins = 10)

# Create age_in_range variable:
# 1: ages ≤ 63, 64-72, ≥ 73
df <- df %>% 
  mutate(age_in_range = cut(age, breaks = c(50, 64, 73, Inf),
                            right = FALSE, labels = FALSE))

# Create sexatbirth variable: 1 = Female, 2 = Male
df <- df %>% 
  mutate(sexatbirth = if_else(sex == "Female", 1, 2))

# Create sum_pain_intensity variable based on short_mac:
# 1: 4-7, 2: 8-11, 3: 12-20
df <- df %>% 
  mutate(sum_pain_intensity = cut(short_mac, breaks = c(4, 8, 12, 21),
                                  right = FALSE, labels = FALSE))

# Optional: view tables of the new coded variables
table(df$age_in_range)
table(df$sexatbirth)
table(df$sum_pain_intensity)

# Save to CSV
write_csv(df, "~/Documents/3_REJOICE_sim.csv")

