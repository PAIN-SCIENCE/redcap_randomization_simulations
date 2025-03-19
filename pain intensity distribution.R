# Load necessary libraries
library(ggplot2)
library(dplyr)

# Function to create pain distribution
create_pain_distribution <- function(mean, std_dev, min_score, max_score, truncate_at) {
  # Generate scores from a normal distribution
  scores <- rnorm(10000, mean, std_dev)
  
  # Keep scores within the range 0-20
  scores <- scores[scores >= min_score & scores <= max_score]
  
  # Truncate scores at the lower end
  scores <- scores[scores >= truncate_at]
  
  # Count population in each score from 4-20
  score_counts <- table(cut(scores, breaks=truncate_at:max_score, right=FALSE))
  
  # Define categories
  categories <- list(
    "1" = 4:7,
    "2" = 8:11,
    "3" = 12:20
  )
  
  # Correctly count population in each range
  category_counts <- sapply(categories, function(range) sum(scores >= min(range) & scores <= max(range)))
  
  # Recalculate mean and standard deviation
  new_mean <- mean(scores)
  new_std_dev <- sd(scores)
  
  list(scores=scores, score_counts=score_counts, category_counts=category_counts, new_mean=new_mean, new_std_dev=new_std_dev)
}

# Function to plot distribution
plot_distribution <- function(scores, mean, std_dev) {
  # Create density plot
  density_plot <- ggplot(data.frame(scores), aes(x=scores)) +
    geom_density(fill="blue", alpha=0.3) +
    stat_function(fun=dnorm, args=list(mean=mean, sd=std_dev), col="red", linetype="dashed") +
    geom_vline(xintercept=mean, col="red", linetype="solid") +
    geom_vline(xintercept=mean + std_dev, col="green", linetype="dashed") +
    geom_vline(xintercept=mean - std_dev, col="green", linetype="dashed") +
    annotate("text", x=mean, y=0.05, label=paste("Mean:", round(mean, 2)), col="red", vjust=-1) +
    annotate("text", x=mean + std_dev, y=0.05, label=paste("SD:", round(std_dev, 2)), col="green", vjust=-1) +
    labs(title="Pain Score Distribution with Normal Curve", x="Pain Score", y="Density")
  
  print(density_plot)
}

# Parameters
mean <- 7
std_dev <- 4
min_score <- 0
max_score <- 20
truncate_at <- 4

# Create pain distribution
results <- create_pain_distribution(mean, std_dev, min_score, max_score, truncate_at)

# Print results
print(results$score_counts)
print(results$category_counts)
print(paste("New Mean:", round(results$new_mean, 2)))
print(paste("New Standard Deviation:", round(results$new_std_dev, 2)))

# Plot distribution
plot_distribution(results$scores, results$new_mean, results$new_std_dev)

# Check sums
print(paste("Sum of score counts:", sum(results$score_counts)))
print(paste("Sum of category counts:", sum(results$category_counts)))

# Load necessary libraries
library(ggplot2)
library(dplyr)

# Function to create pain distribution
create_pain_distribution <- function(mean, std_dev, min_score, max_score, truncate_at) {
  # Generate scores from a normal distribution
  scores <- rnorm(10000, mean, std_dev)
  
  # Round scores to the nearest integer to get single pain scores
  scores <- round(scores)
  
  # Keep scores within the range 0-20
  scores <- scores[scores >= min_score & scores <= max_score]
  
  # Count population in each score from min_score to max_score
  score_counts <- table(factor(scores, levels=min_score:max_score))
  
  # Clip values below truncate_at
  scores <- scores[scores >= truncate_at]
  
  # Count population again after truncation
  truncated_score_counts <- table(factor(scores, levels=truncate_at:max_score))
  
  # Define categories
  categories <- list(
    "1" = 4:7,
    "2" = 8:11,
    "3" = 12:20
  )
  
  # Correctly count population in each range
  category_counts <- sapply(categories, function(range) sum(scores %in% range))
  
  # Recalculate mean and standard deviation
  new_mean <- mean(scores)
  new_std_dev <- sd(scores)
  
  list(scores=scores, score_counts=score_counts, truncated_score_counts=truncated_score_counts, category_counts=category_counts, new_mean=new_mean, new_std_dev=new_std_dev)
}

# Function to plot distribution
plot_distribution <- function(scores, mean, std_dev) {
  # Create density plot
  density_plot <- ggplot(data.frame(scores), aes(x=scores)) +
    geom_density(fill="cornflowerblue", alpha=0.3) +
    stat_function(fun=dnorm, args=list(mean=mean, sd=std_dev), col="red", linetype="dashed") +
    geom_vline(xintercept=mean, col="red", linetype="solid") +
    geom_vline(xintercept=mean + std_dev, col="blue", linetype="dashed") +
    geom_vline(xintercept=mean - std_dev, col="blue", linetype="dashed") +
    annotate("text", x=mean, y=0.05, label=paste("Mean:", round(mean, 2)), col="red", vjust=-1) +
    annotate("text", x=mean + std_dev, y=0.05, label=paste("SD:", round(std_dev, 2)), col="blue", vjust=-1) +
    labs(title="Pain Score Distribution with Normal Curve", x="Pain Score", y="Density")
  
  print(density_plot)
}

# Parameters
mean <- 7
std_dev <- 4
min_score <- 0
max_score <- 20
truncate_at <- 4

# Create pain distribution
results <- create_pain_distribution(mean, std_dev, min_score, max_score, truncate_at)

# Print results
print(results$score_counts)
print(results$truncated_score_counts)
print(results$category_counts)
print(paste("New Mean:", round(results$new_mean, 2)))
print(paste("New Standard Deviation:", round(results$new_std_dev, 2)))

# Check sums
print(paste("Sum of initial score counts:", sum(results$score_counts)))
print(paste("Sum of truncated score counts:", sum(results$truncated_score_counts)))
print(paste("Sum of category counts:", sum(results$category_counts)))

# Plot distribution
plot_distribution(results$scores, results$new_mean, results$new_std_dev)

