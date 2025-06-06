
source("R/sources.R")

# Define the input parameters
noise_sd_values <- c(40, 45, 50)
noise_fwhm_values <- seq(10, 50, by=10)
methods <- c("TWT", "IWT", "SPM","Fmax", "ERL", "IATSE")
max_sample_size <- 60
target_power <- 0.80
results_list <- list() # To store Input_Summary for each simulation

# Iterate over each combination of noise_sd and noise_fwhm
for (noise_sd in noise_sd_values) {
  for (noise_fwhm in noise_fwhm_values) {
    
    # Print current loop status
    #cat("Running simulation for Noise SD:", noise_sd, ", Noise FWHM:", noise_fwhm, "\n")
    
    
    # Initialize sample size and track methods that have met power threshold
    sample_size <- 4
    methods_to_run <- methods # Start with all methods
    
    # Continue increasing sample size until all methods reach target power or sample_size > max_sample_size
    while (sample_size <= max_sample_size && length(methods_to_run) > 0) {
      
      # Run the Power_parallel function with the current sample size
      power_results <- Power_parallel(
        data = MF_data("both"), # Replace `your_data` with the actual data variable
        sample_size = sample_size,
        noise_mean = 0, # Assuming noise_mean is set to 0, modify as needed
        noise_sd = noise_sd,
        noise_fwhm = noise_fwhm,
        signal = NULL, # Adjust signal as required
        method = methods_to_run,
        n_iterations = 2500 # Set appropriate iteration count
      )
      
      # Extract power results for each method
      power_result_summary <- power_results$Power_results
      
      # Add input summary to results list for tracking
      results_list[[paste("Noise_SD", noise_sd, "Noise_FWHM", noise_fwhm, "Sample_Size", sample_size, sep = "_")]] <- power_results$Input_Summary
      
      # Check which methods have achieved the target power
      methods_to_run <- methods_to_run[sapply(methods_to_run, function(method_name) {
        power_result_summary[[method_name]] < target_power
      })]
      
      # Increment sample size for next iteration
      sample_size <- sample_size + 1
      
      # Clean up variables and invoke garbage collection
      rm(power_results, power_result_summary)  # Remove variables no longer needed
      # Invoke garbage collection after each iteration
      gc()
    }
    # Clean up after completing a noise_fwhm simulation
    rm(methods_to_run)
    # Invoke garbage collection after completing a noise_fwhm simulation
    gc()
  }
  # Invoke garbage collection after completing a noise_sd simulation
  gc()
}

# Save or inspect results_list for tracking after simulation completes
# For example, you can write results to an Excel file or analyze them further
# Write to Excel if needed
copy_results<-results_list
# copy_results has different number of columns, so we need to make them equal and then rbind the results
#rbind the results

# Ensure each data frame has the same columns and combine them
combined_results <- rbind.fill(copy_results)

write.xlsx(combined_results, "MF.xlsx")

#############################################

