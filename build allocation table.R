library(dplyr)
library(readr)

# Define parameters for block randomization:
block_size <- 2               # Block size (must be even for two treatments)
base_blocks_per_stratum <- 25  # Default number of blocks per stratum
extra_blocks_for_rare <- 30    # Additional blocks for rare strata
min_total_assignments <- 100   # Minimum total assignments per stratum 

#changed block_size 4 -> 2; base_blocks_per_stratum 50 -> 25; min_total_assignments 200 -> 100
#resulting N = 1860 vs. 4320
#results in more balanced age but less balanced pain (both small effects)

# Create all possible strata combinations:
strata_df <- expand.grid(
  age_in_range = 1:3,
  sexatbirth = 1:2,
  sum_pain_intensity = 1:3
)

# Define expected proportions (from original user-provided data)
age_proportions  <- c(0.318530207, 0.337691614, 0.34355275)
sex_proportions  <- c(0.603132583, 0.396867417)
pain_proportions <- c(0.444089064, 0.398203961, 0.157706975)

# Compute expected probability for each stratum:
strata_df$p_ijk <- mapply(function(a, s, p) {
  age_proportions[a] * sex_proportions[s] * pain_proportions[p]
}, 
strata_df$age_in_range, strata_df$sexatbirth, strata_df$sum_pain_intensity)

# Adjust block count for rare strata:
strata_df$block_count <- base_blocks_per_stratum + 
  ifelse(strata_df$p_ijk < 0.05, extra_blocks_for_rare, 0)  # Boost rare strata

# Ensure a minimum number of total assignments per stratum:
strata_df$total_assignments <- pmax(strata_df$block_count * block_size, min_total_assignments)

# Initialize a list to store all blocks:
allocation_list <- list()

# Generate assignments using permuted blocks:
for (i in 1:nrow(strata_df)) {
  current_stratum <- strata_df[i, ]
  
  # Generate permuted blocks:
  num_assignments <- current_stratum$total_assignments
  num_blocks <- num_assignments / block_size
  
  for (b in 1:num_blocks) {
    block_assignments <- sample(rep(c(0, 1), each = block_size / 2))
    
    # Create a data frame for this block:
    block_df <- data.frame(
      age_in_range = current_stratum$age_in_range,
      sexatbirth = current_stratum$sexatbirth,
      sum_pain_intensity = current_stratum$sum_pain_intensity,
      randomized_condition = block_assignments
    )
    
    # Append block to list:
    allocation_list[[length(allocation_list) + 1]] <- block_df
  }
}

# Combine all blocks into one allocation table:
allocation_table <- bind_rows(allocation_list)

# Add an allocation_id column using row numbers:
allocation_table <- allocation_table %>% mutate(allocation_id = row_number())

# Save the allocation table to your Documents folder:
csv_filename <- "~/Documents/randomization_table.csv"
write_csv(allocation_table, csv_filename)
cat("Final allocation table written to", csv_filename, "\n")

