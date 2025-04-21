
source("R/sources.R")

# Define the input parameters
noise_sd_values <- c(0.8, 1, 1.2)
diff_cols <- abs (Moment_data("both")[,1]-Moment_data("both")[,2])
# do min-max on diff_cols
diff_cols <- (diff_cols-min(diff_cols))/(max(diff_cols)-min(diff_cols))
# Map [0, 1] to [a, b]
map_to_range <- function(x, a, b) {
  return(a + (b - a) * x)
}

sd1 <- map_to_range(diff_cols, 0.001, noise_sd_values[1])
sd2 <- map_to_range(diff_cols, 0.001, noise_sd_values[2])
sd3 <- map_to_range(diff_cols, 0.001, noise_sd_values[3])
# plot(sd1,type="l")
# nnn=noise_guassian_curve(10,101)
# nn=smoothed_gussian_curves(nnn,mu = 0, sig = sd1,fwhm = 25)
# matplot(nn,type="l")


noise_fwhm_values <- seq(10, 50, by=10)
methods <- c("TWT", "IWT", "SPM","Fmax", "ERL", "IATSE")
max_sample_size <- 60
target_power <- 0.80
results_list <- list() # To store Input_Summary for each simulation

# Iterate over each combination of noise_sd and noise_fwhm
for (noise_sd in list(sd1,sd2,sd3)) {
  for (noise_fwhm in noise_fwhm_values) {
    
    # Print current loop status
    #cat("Running simulation for Noise SD:", max(noise_sd), ", Noise FWHM:", noise_fwhm, "\n")
    
    
    # Initialize sample size and track methods that have met power threshold
    sample_size <- 4
    methods_to_run <- methods # Start with all methods
    
    # Continue increasing sample size until all methods reach target power or sample_size > max_sample_size
    while (sample_size <= max_sample_size && length(methods_to_run) > 0) {
      
      # Run the Power_parallel function with the current sample size
      power_results <- Power_parallel(
        data = Moment_data("both"), # Replace `your_data` with the actual data variable
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
      results_list[[paste("Noise_SD", max(noise_sd), "Noise_FWHM", noise_fwhm, "Sample_Size", sample_size, sep = "_")]] <- power_results$Input_Summary
      
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

write.xlsx(combined_results, "KJM_nonconstant.xlsx")

#############################################

