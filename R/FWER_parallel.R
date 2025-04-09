

FWER_parallel <- function(sample_size,
                       noise_mean, noise_sd, noise_fwhm,
                       method, n_iterations,
                       Write_file = FALSE,
                       file_name = "Power_Results.xlsx"){
  
  # Capture input argument names and values
  input_info <- data.frame(
    sample_size = sample_size,                # Numeric value
    noise_mean = noise_mean,                  # Numeric value
    noise_sd = noise_sd,                      # Numeric value
    noise_fwhm = noise_fwhm,                  # Numeric value
    method = paste(method, collapse = ", "),  # Store 'method' as a string of methods
    n_iterations = n_iterations               # Numeric value
  )
  
  # Initialize the method list
  method_list <- Initialize_method_list(Methods = method,
                                        Conti_size = 101,
                                        Iter_number = n_iterations)
  
  # parallelize the power calculation
  number_cores=detectCores() #number of cores
  registerDoParallel(number_cores-1) #register the number of cores
  
  # The .combine function in foreach always takes two inputs at a time:
  # First: The cumulative result of previous iterations (starting with an initial value, if provided)
  # Second: The result of the current iteration of the loop
  loop <- foreach (k = (1:n_iterations),.combine = parallel_combine_function,
                   .init = method_list) %dopar% {
                     
                     # Generate the data
                     noise1 <- noise_guassian_curve(number_of_curves = sample_size,
                                                    continuum_size = 101)
                     noise2 <- noise_guassian_curve(number_of_curves = sample_size,
                                                    continuum_size = 101)
                     noise1 <- smoothed_gussian_curves(noise1, noise_mean, noise_sd, noise_fwhm)
                     noise2 <- smoothed_gussian_curves(noise2, noise_mean, noise_sd, noise_fwhm)
                     
                     # Calculate the pvalues for each method
                     method_list <- Pvalue_calculator(method_list, noise1,
                                                      noise2)
                     
                   }
  
  
  # Stop the parallel backend
  stopImplicitCluster()
  
  # Calculate the power based on the result
  power_results <- Power_calculator(loop , n_iterations, Alpha = 0.05)
  
  # Add power results to input_info dataframe for each method
  for (method_name in names(power_results)) {
    # Add a new column with the power result for each method
    input_info[[method_name]] <- power_results[[method_name]]
  }
  
  if (Write_file == TRUE) {
    
    # Call the external function to write results to Excel
    write_results_to_excel(loop, power_results, input_info, file_name)
    
    
  }
  
  
  
  return(list(Pvalues_methods = loop, Power_results = power_results,
              Input_Summary = input_info, File = file_name))
  
}




