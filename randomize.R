# Load necessary libraries
library(dplyr)
library(readr)

# Define file paths
sample_files <- c("~/Documents/1_REJOICE_sim.csv", 
                  "~/Documents/2_REJOICE_sim.csv", 
                  "~/Documents/3_REJOICE_sim.csv")
allocation_table_path <- "~/Documents/randomization_table_1.csv"

# Read the allocation table
original_allocation_table <- read_csv(allocation_table_path)

# Function to randomize a single sample file
randomize_file <- function(file_path, allocation_table) {
  # Read the sample file
  sample_data <- read_csv(file_path)
  
  # Initialize a vector to store randomized conditions
  randomized_conditions <- rep(NA, nrow(sample_data))  # Default to NA
  
  # Loop through each row in the sample data
  for (i in seq_len(nrow(sample_data))) {
    # Extract the stratum information
    age_in_range <- sample_data$age_in_range[i]
    sexatbirth <- sample_data$sexatbirth[i]
    sum_pain_intensity <- sample_data$sum_pain_intensity[i]
    
    # Find the corresponding stratum in the allocation table
    stratum <- allocation_table %>%
      filter(age_in_range == !!age_in_range & 
               sexatbirth == !!sexatbirth & 
               sum_pain_intensity == !!sum_pain_intensity) %>%
      arrange(allocation_id)
    
    # Assign the next available entry from the allocation table
    if (nrow(stratum) > 0) {
      randomized_conditions[i] <- stratum$randomized_condition[1]
      # Remove the assigned entry from the allocation table
      allocation_table <- allocation_table[-which(allocation_table$allocation_id == stratum$allocation_id[1]), ]
    }
  }
  
  # Add the randomized conditions to the sample data
  sample_data$randomized_condition <- randomized_conditions
  
  # Return the randomized sample data
  return(sample_data)
}

# Apply randomization to all sample files and save the results
for (sim_file in sample_files) {
  # Use a fresh copy of the allocation table for each file
  allocation_table <- original_allocation_table
  
  # Perform randomization
  df_randomized <- randomize_file(sim_file, allocation_table)
  
  # Define the output path
  out_path <- sub("REJOICE_sim", "REJOICE_sim_randomized", sim_file)
  
  # Save the new dataset to a CSV file
  write_csv(df_randomized, out_path)
  
  # Print a message indicating successful processing
  print(paste("Processed", sim_file, "and saved randomized file as", out_path))
}

