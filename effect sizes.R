# Load necessary libraries
library(dplyr)
library(ggplot2)
library(tidyr)
library(effsize)
library(ggpubr)

# Read in the three CSV files
file1 <- read.csv("~/Documents/1_REJOICE_sim_randomized.csv")
file2 <- read.csv("~/Documents/2_REJOICE_sim_randomized.csv")
file3 <- read.csv("~/Documents/3_REJOICE_sim_randomized.csv")

# Combine all files into one dataframe with an extra column to indicate the sample
df <- bind_rows(
  file1 %>% mutate(sample = "Sample 1"),
  file2 %>% mutate(sample = "Sample 2"),
  file3 %>% mutate(sample = "Sample 3")
)

# Check data structure and summary
summary(df)

# Ensure 'randomized_condition' and 'sample' are factors
df$randomized_condition <- factor(df$randomized_condition)
df$sample <- factor(df$sample)

# Function to calculate effect size and create a plot for each sample
calculate_and_plot_sample <- function(variable, label, df, sample_name) {
  # Filter data for the current sample
  sample_data <- df %>% filter(sample == sample_name)
  
  # Calculate means and standard deviations for each group
  summary_stats <- sample_data %>%
    group_by(randomized_condition) %>%
    summarise(
      mean_value = mean(.data[[variable]], na.rm = TRUE),
      sd_value = sd(.data[[variable]], na.rm = TRUE),
      n = n()
    )
  
  # Create plot with error bars (mean ± SD)
  p <- ggplot(summary_stats, aes(x = randomized_condition, y = mean_value, fill = randomized_condition)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.6) +
    geom_errorbar(aes(ymin = mean_value - sd_value, ymax = mean_value + sd_value), 
                  position = position_dodge(width = 0.8), width = 0.25) +
    theme_minimal() +
    labs(y = label, x = "Randomized Condition") +
    ggtitle(paste("Balance of", label, "by Randomized Condition in", sample_name)) +
    theme(legend.position = "none") +
    stat_summary(fun.data = "mean_sdl", geom = "point", shape = 3, size = 3) 
  
  # Cohen's d effect size
  cohen_d <- cohen.d(sample_data[[variable]] ~ sample_data$randomized_condition)
  
  # Pearson's r effect size
  cor_test <- cor.test(sample_data[[variable]], as.numeric(sample_data$randomized_condition))
  
  # Annotate plot with effect sizes
  p <- p + annotate("text", x = 1.5, y = max(summary_stats$mean_value + summary_stats$sd_value), 
                    label = paste("Cohen's d: ", round(cohen_d$estimate, 2), 
                                  "\nPearson's r: ", round(cor_test$estimate, 2)),
                    size = 5, hjust = 0.5, vjust = 1, color = "black")
  
  
  
  # Show the plot
  print(p)
  
  # Print summary of results
  cat(paste("\nResults for", label, "in", sample_name, ":\n"))
  cat(paste("Cohen's d: ", round(cohen_d$estimate, 2), "\n"))
  cat(paste("Pearson's r: ", round(cor_test$estimate, 2), "\n"))
  
  # Interpretation of Cohen's d
  if (abs(cohen_d$estimate) < 0.15) {
    cat("Cohen's d interpretation: Negligible effect\n")
  } else if (abs(cohen_d$estimate) < 0.40) {
    cat("Cohen's d interpretation: Small effect\n")
  } else if (abs(cohen_d$estimate) < 0.75) {
    cat("Cohen's d interpretation: Medium effect\n")
  } else {
    cat("Cohen's d interpretation: Large effect\n")
  }
  
  # Interpretation of Pearson's r
  if (abs(cor_test$estimate) < 0.10) {
    cat("Pearson's r interpretation: Negligible effect\n")
  } else if (abs(cor_test$estimate) < 0.20) {
    cat("Pearson's r interpretation: Small effect\n")
  } else if (abs(cor_test$estimate) < 0.30) {
    cat("Pearson's r interpretation: Medium effect\n")
  } else {
    cat("Pearson's r interpretation: Large effect\n")
  }
}

# Calculate and plot for age for each sample
calculate_and_plot_sample("age", "Age (years)", df, "Sample 1")
calculate_and_plot_sample("age", "Age (years)", df, "Sample 2")
calculate_and_plot_sample("age", "Age (years)", df, "Sample 3")

# Calculate and plot for sex (sexatbirth) for each sample
df$sex <- factor(df$sexatbirth, levels = c("male", "female"))
calculate_and_plot_sample("sexatbirth", "Sex (at birth)", df, "Sample 1")
calculate_and_plot_sample("sexatbirth", "Sex (at birth)", df, "Sample 2")
calculate_and_plot_sample("sexatbirth", "Sex (at birth)", df, "Sample 3")

# Calculate and plot for pain (short_mac) for each sample
calculate_and_plot_sample("short_mac", "Pain (Short Mac Scale)", df, "Sample 1")
calculate_and_plot_sample("short_mac", "Pain (Short Mac Scale)", df, "Sample 2")
calculate_and_plot_sample("short_mac", "Pain (Short Mac Scale)", df, "Sample 3")

# Calculate means for each variable by randomized_condition for each sample
# For Sample 1
sample1_stats <- file1 %>%
  group_by(randomized_condition) %>%
  summarise(
    mean_age = mean(age, na.rm = TRUE),
    mean_short_mac = mean(short_mac, na.rm = TRUE),
    mean_sex = mean(sexatbirth == "female")  # assuming "sexatbirth" is binary (male/female)
  )

# For Sample 2
sample2_stats <- file2 %>%
  group_by(randomized_condition) %>%
  summarise(
    mean_age = mean(age, na.rm = TRUE),
    mean_short_mac = mean(short_mac, na.rm = TRUE),
    mean_sex = mean(sexatbirth == "female")
  )

# For Sample 3
sample3_stats <- file3 %>%
  group_by(randomized_condition) %>%
  summarise(
    mean_age = mean(age, na.rm = TRUE),
    mean_short_mac = mean(short_mac, na.rm = TRUE),
    mean_sex = mean(sexatbirth == "female")
  )

# Print out means for each sample
cat("Sample 1 means:\n")
print(sample1_stats)

cat("\nSample 2 means:\n")
print(sample2_stats)

cat("\nSample 3 means:\n")
print(sample3_stats)

