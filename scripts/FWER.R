
source("R/sources.R")
source("R/plot_functions.R")
########################
library(readxl)
library(dplyr)
library(tidyr)


# Load necessary libraries
Output_vGRF <- read_excel("vGRF_equal_1_small.xlsx")
results_vGRF <- SS_finder(Output_vGRF)
Output_KJM <- read_excel("KJM_equal_1.xlsx")
results_KJM <- SS_finder(Output_KJM)
total_result <- rbind(results_vGRF, results_KJM)



# Assuming total_result is your tibble, first convert noise_sd to numeric if needed.
total_result <- total_result %>%
  mutate(noise_sd = as.numeric(as.character(noise_sd)),
         noise_fwhm = as.numeric(as.character(noise_fwhm)))

# Pivot the tibble so that each method and its sample size appear in one row.
total_result_long <- total_result %>%
  pivot_longer(
    cols = -c(noise_sd, noise_fwhm),
    names_to = "method",
    values_to = "sample_size"
  )



total_result_long <- total_result_long %>%
  mutate(method = sub("_min_sample", "", method))

# remove sample_size = Inf
total_result_long <- total_result_long %>%
  filter(sample_size != Inf)

results_list <- list()  # To store the Input_Summary for each simulation



# Loop over each row of the long-format data
for(i in seq_len(nrow(total_result_long))) {
  
  # Extract the parameters from the current row
  current_row <- total_result_long[i, ]
  noise_sd     <- current_row$noise_sd
  noise_fwhm   <- current_row$noise_fwhm
  method       <- current_row$method
  sample_size  <- current_row$sample_size
  
  # Run your simulation for the current combination
  FWER_results <- FWER_parallel(sample_size = sample_size,
                                noise_mean = 0,       # Modify if needed
                                noise_sd = noise_sd,
                                noise_fwhm = noise_fwhm,
                                method = method,
                                n_iterations = 2500,
                                Write_file = FALSE)
  
  # Save the Input_Summary in the results list with a unique name
  results_list[[paste("Noise_SD", noise_sd, "Noise_FWHM", noise_fwhm, "Method", method, sep = "_")]] <-
    FWER_results$Input_Summary
  
  # Clean up variables no longer needed
  rm(FWER_results)
  gc()  # Invoke garbage collection after each iteration
}

# Save or inspect results_list for tracking after simulation completes
# For example, you can write results to an Excel file or analyze them further
# Write to Excel if needed
copy_results<-results_list
# copy_results has different number of columns, so we need to make them equal and then rbind the results
#rbind the results

# Ensure each data frame has the same columns and combine them
combined_results <- rbind.fill(copy_results)

write.xlsx(combined_results, "FWER_vGRF_KMJ.xlsx")

#############################################

