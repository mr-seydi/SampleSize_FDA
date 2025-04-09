
################Data with NA#########################
completed_data <- function(x, y, defined_domain=c(0,100)) {
  
  if (length(defined_domain) != 2) {
    stop("Domain must have a form like c(starting point,ending point)")
  }
  start_domain <- defined_domain[1]
  end_domain <- defined_domain[2]
  
  # Perform linear interpolation using approx() function
  xnew <- seq(start_domain, end_domain, by = 1)  # New x values with steps of 1
  interpolated <- approx(na.omit(x), na.omit(y), xout = xnew)  # Interpolate y values
  
  # Return new y values
  return(interpolated$y)
}


###################Functions###########################

sigma_to_fwhm <- function(sigma){
  return(sigma*(2 * sqrt(2 * log(2))))
}

fwhm_to_sigma <- function(fwhm){
  return(fwhm / (2 * sqrt(2 * log(2))))
}

#unsmoothed guassian noise curve
noise_guassian_curve <- function(number_of_curves, continuum_size){ 
  data <- matrix(rnorm(number_of_curves*continuum_size),
                 nrow = number_of_curves, ncol = continuum_size)
  return(data)
  
}


#apply this scaling factor to any smoothed data to ensure it has unit variance after smoothing
# from _set_scale function in https://github.com/0todd0000/power1d/blob/master/src/power1d/random.py#L35

set_scale <- function(nNodes, SD) {
  # Define a small epsilon to prevent division by zero
  eps <- .Machine$double.eps
  
  # Step 1: Define a Gaussian kernel
  t <- seq(-0.5 * (nNodes - 1), 0.5 * (nNodes - 1), length.out = nNodes)
  gf <- exp(-(t^2) / (2 * SD^2 + eps))
  
  # Step 2: Normalize the Gaussian kernel
  gf <- gf / sum(gf)
  
  # Step 3: Calculate the expected variance of the smoothed data
  # Perform FFT and compute power spectrum
  AG <- fft(gf)
  Pag <- Mod(AG)^2  # Equivalent to AG * Conj(AG)
  
  # Calculate the autocovariance by inverse FFT
  COV <- Re(fft(Pag, inverse = TRUE)) / length(Pag)
  svar <- COV[1]  # Variance of the smoothed field
  
  # Step 4: Calculate the scaling factor
  SCALE <- sqrt(1.0 / svar)
  
  return(SCALE)
}


# Computes a 1-D Gaussian (or its derivative) kernel.
# sigma: standard deviation
# order: 0 for the standard Gaussian, positive integers for derivatives
# radius: half-width of the kernel; if not given, use truncate * sigma.
# The same function as scipy.ndimage$gaussian_filter1d(x, SD, mode='wrap')#$tolist()
gaussian_kernel1d <- function(sigma, order = 0, radius) {
  if (order < 0)
    stop("order must be non-negative")
  
  # Create grid: from -radius to radius.
  x <- seq(-radius, radius)
  
  # Compute the basic Gaussian (unnormalized)
  phi_x <- exp(-0.5 * (x / sigma)^2)
  phi_x <- phi_x / sum(phi_x)  # normalize so that sum(phi_x) == 1
  
  if (order == 0) {
    return(phi_x)
  } else {
    # For derivatives we need to multiply the Gaussian by a polynomial.
    # The SciPy code computes the polynomial coefficients via a matrix method.
    # Here we mimic that process.
    expo <- 0:order  # exponents 0,1,...,order
    q <- numeric(order + 1)
    q[1] <- 1  # q[0] = 1  (R indexing: first element corresponds to exponent 0)
    
    # Build the “differentiation” matrices D (superdiagonal) and P (subdiagonal)
    D <- matrix(0, nrow = order + 1, ncol = order + 1)
    for (i in 1:order) {
      D[i, i + 1] <- expo[i + 1]
    }
    P <- matrix(0, nrow = order + 1, ncol = order + 1)
    for (i in 2:(order + 1)) {
      P[i, i - 1] <- 1 / (-sigma^2)
    }
    Q_deriv <- D + P
    # Apply the operator repeatedly (order times)
    for (i in 1:order) {
      q <- Q_deriv %*% q
    }
    # For each x value, evaluate the polynomial:
    # Compute sum_{j=0}^{order} q[j] * x^j.
    # (outer(x, expo, `^`) builds a matrix whose (i,j) entry is x[i]^(expo[j]).)
    poly_val <- as.vector(outer(x, expo, `^`) %*% q)
    return(poly_val * phi_x)
  }
}


# Performs a 1-D correlation using periodic ("wrap") boundary conditions.
# input: a numeric vector.
# weights: the 1-D kernel (assumed to have odd length).
correlate1d_wrap <- function(input, weights) {
  n <- length(input)
  k <- length(weights)
  # Assume kernel size is odd so that it is symmetric around its center:
  half <- (k - 1) / 2
  result <- numeric(n)
  
  # For each element of the output, sum over the kernel with periodic indexing.
  for (i in seq_len(n)) {
    # Offsets: from -half to +half.
    offsets <- (-half):half
    # Compute wrapped indices: in R indices run 1..n.
    idx <- ((i - 1 + offsets) %% n) + 1
    result[i] <- sum(input[idx] * weights)
  }
  return(result)
}

# The main function: a 1-D Gaussian filter with periodic (wrap) boundary conditions.
#
# Arguments:
#  - input: numeric vector to filter.
#  - sigma: standard deviation of the Gaussian.
#  - order: the order of the derivative (default 0 means no derivative).
#  - truncate: how many sigmas to include in the kernel (ignored if radius is provided).
#  - radius: if provided, the half-length of the kernel; otherwise computed as round(truncate * sigma).
#
# Note: In the SciPy version the kernel is reversed before calling the correlate1d.
# We do the same here.
gaussian_filter1d <- function(input, fwhm, order = 0, truncate = 4.0, radius = NULL) {
  sigma <- fwhm_to_sigma(fwhm)
  if (!is.numeric(input)){
    stop("input must be numeric")
  }
  
  # Determine kernel half-width (radius)
  if (is.null(radius)) {
    radius <- round(truncate * sigma)
  } else if (radius < 0 || radius != as.integer(radius)) {
    stop("radius must be a nonnegative integer")
  }
  
  # Create the kernel.
  weights <- gaussian_kernel1d(sigma, order, radius)
  # In SciPy, the kernel is reversed (because of the use of correlation rather than convolution).
  weights <- rev(weights)
  
  # Apply the filter using periodic (wrap) boundary conditions.
  output <- t(apply(input, 1, function(x) correlate1d_wrap(x, weights)))
  return(output)
}


smoothed_gussian_curves <- function(data, mu, sig, fwhm) {
  
  # Step 1: Smooth each curve in the data
  smoothed_data <- gaussian_filter1d(data, fwhm)
  
  
  # Step 2: Normalize the smoothed data to have unit variance
  nNodes <- ncol(smoothed_data)
  SD <- fwhm_to_sigma(fwhm)
  scale_factor <- set_scale(nNodes, SD)
  
  # Step 3: Scale the smoothed data
  smoothed_data_scaled <- smoothed_data * scale_factor
  
  # Step 4: Transform to have mean = mu and standard deviation = sig
  # if mu and sig was a single number
  smoothed_data_final <- smoothed_data_scaled *
    matrix(sig, nrow = dim(smoothed_data_scaled)[1],
           ncol = dim(smoothed_data_scaled)[2], byrow = TRUE) +
    matrix(mu,  nrow = dim(smoothed_data_scaled)[1],
           ncol = dim(smoothed_data_scaled)[2], byrow = TRUE)
  
  return(t(smoothed_data_final))
}


Noise_generator <- function(Sample_size, Continuum_size, Noise_mu, Noise_sig, Noise_fwhm){
  
  noise1 <- noise_guassian_curve(number_of_curves = Sample_size,
                                 continuum_size = Continuum_size)
  noise1 <- smoothed_gussian_curves(noise1, Noise_mu, Noise_sig, Noise_fwhm)
  
  sd_noise <- apply(noise1, 1, sd)
  mean_noise <- apply(noise1, 1, mean)
  
  return(list(noise1 = noise1, SD = sd_noise, Mean = mean_noise))
}

# Generating data (Baseline+noise+signal) or (Baseline+noise) or (Baseline+signal)
data_generator <- function(data,signal,noise) {
  sample_size <- dim(noise)[2]
  data_baseline <- matrix(rep(data,time=sample_size),
                          ncol = sample_size)
  
  if (missing(data)){
    signal_baseline <- matrix(rep(signal,time=sample_size),
                              ncol = sample_size)
    data_out <- signal_baseline + noise
  }
  else if (missing(signal)) {
    data_out <- data_baseline + noise
  } else if (missing(noise)) {
    signal_baseline <- matrix(rep(signal,time=sample_size),
                              ncol = sample_size)
    data_out <- data_baseline + signal_baseline
  } else {
    signal_baseline <- matrix(rep(signal,time=sample_size),
                              ncol = sample_size)
    data_out <- data_baseline + signal_baseline + noise 
  }
  return(data_out)
}



gaussian_pulse <- function(center, fwhm, continuum_size) {
  sigma = fwhm_to_sigma((fwhm/100)*continuum_size)
  x_values = seq(0, continuum_size-1, by = 1)
  dens <- dnorm(x_values, mean = center, sd = sigma)
  return(list(density_val=dens, x_values=x_values))
}

amplitude_pulse <- function(data, amp){
  scaling_factor = amp / max(data)
  y_values = scaling_factor * data
  return(y_values)
}


##############Methods functions###########

Initialize_method_list <- function(Methods, Conti_size=101, Iter_number=100){
  method_list <- list()
  for (M in Methods) {
    method_list[[M]] <- matrix(,nrow = Conti_size, ncol = 0)
  }
  return(method_list)
}

Power_data_generator <- function(Sample_size, Data,
                                 Signal, Conti_size = 101,
                                 Noise_mu, Noise_sig, Noise_fwhm){
  
  noise1 <- noise_guassian_curve(number_of_curves = Sample_size,
                                 continuum_size = Conti_size)
  noise2 <- noise_guassian_curve(number_of_curves = Sample_size,
                                 continuum_size = Conti_size)
  noise1 <- smoothed_gussian_curves(noise1, Noise_mu, Noise_sig, Noise_fwhm)
  noise2 <- smoothed_gussian_curves(noise2, Noise_mu, Noise_sig, Noise_fwhm)
  
  
  if (is.null(Signal)) {
    data1 <- data_generator(data = Data[,1], noise = noise1)
    data2 <- data_generator(data = Data[,2], noise = noise2)
  } else {
    data1 <- data_generator(data = Data, signal = Signal, noise = noise1)
    data2 <- data_generator(data = Data, noise = noise2)      
  }
  
  return(list(data1 = data1, data2 = data2))
}

Pvalue_calculator <- function(method_list, data1, data2){
  
  Methods <- names(method_list)
  
  for (M in Methods) {
    Pvalues <- Pval_method(sampel1 = t(data1), sample2 = t(data2),
                           method = M)
    #p_values dimension is continuum_size*Iter_number
    method_list[[M]] <- cbind(method_list[[M]], Pvalues)
  }
  return(method_list) #Filled in method list
}


Power_calculator <- function(Pvalues, Iter_number, Alpha){
  
  Methods <- names(Pvalues)
  
  for (M in Methods) {
    
    
    # Check if the method is either "ERL" or "IATSE"
    #Because those methods do not return p-values
    if (M %in% c("ERL", "IATSE")) {
      pvalue_less_alpha <- Pvalues[[M]]
      # For "ERL" and "IATSE", only run this code
      power <- sum(colSums(pvalue_less_alpha) > 0) / Iter_number
    } else {
      pvalue_less_alpha <- Pvalues[[M]] < Alpha
      power <- sum(colSums(pvalue_less_alpha) > 0) / Iter_number
    }
    
    Pvalues[[M]] <- power
  }
  
  return(Pvalues)
  
}


Pval_method <- function(sampel1,sample2,method) {
  if (method=="IWT") {
    pval <- IWT(sampel1,sample2)
  } else if (method=="TWT"){
    pval <- TWT(sampel1,sample2)
  } else if (method=="SPM"){
    pval <- spm(sampel1,sample2)
  } else if (method=="Fmax"){
    pval <- F_max(sampel1,sample2)
  } else if (method=="ERL"){
    pval <- ERL(sampel1,sample2)
  } else if (method=="IATSE"){
    pval <- IATSE(sampel1,sample2)
  } else {
    stop("Choose a method between options")
  }
  return(pval)
}

IWT <- function(data1,data2){
  IWT2=IWT2(data1, data2)
  pvalue_adj_IWT2=IWT2$adjusted_pval
  return(pvalue_adj_IWT2)
}

TWT <- function(data1,data2){
  TWT2=TWT2_new(data1,data2)
  pvalue_adj_TWT2=TWT2$adjusted_pval
  return(pvalue_adj_TWT2)
}

spm <- function(data1, data2){
  # spm  <- spm1d$stats$ttest2(data1, data2, equal_var=FALSE)
  # p_val <- spm1d$rft1d$f$sf((spm$z)^2, spm$df, spm$Q, spm$fwhm, withBonf=TRUE)
  p_val <- SPM(data1, data2, variance.equal = FALSE, Clevel = 0.95)
  return(p_val)
}

F_max <- function(data1, data2, B = 1000){
  
  n1 = dim(data1)[1]
  n2 = dim(data2)[1]
  
  
  group12 = factor(c(rep(1,n1),rep(2,n2)))
  data_group12 <- rbind(data1,data2)
  
  # Create a data frame that includes both data_group12 and group12
  combined_data <- data.frame(data_group12, group12)
  
  # Pass the formula with the combined data to Fmax
  Fmax_pval <- Fmax(data_group12 ~ group12, DATA = combined_data)
  return(Fmax_pval$adjusted_pval_F)
  
}




ERL <- function(data1,data2) {
  
  # t() because in Pvalue_calculator we t(data1) and t(data2)
  #since the other methods works with t() of data
  data1= t(data1)
  data2= t(data2)
  
  d <- dim(data1)[1]
  n_data <-dim(data1)[2]
  c_s <- create_curve_set(list(r=0:(d-1),obs=cbind(data1,data2)))
  
  group <- factor(rep(c(1,2),each=n_data))
  
  test <- graph.fanova(
    
    nsim = 1000,
    curve_set = c_s,
    groups = group,
    variances = "equal", #unequal variances can be used, but here we assume equal variances
    test.equality = "mean",
    typeone = "fwer",
    type = "erl",
    contrasts =TRUE,
    alpha = 0.05
  )
  
  test_result <- (test$obs>test$hi) | (test$obs<test$lo) 
  return(test_result)
  
}


IATSE <- function(data1,data2) {
  
  # t() because in Pvalue_calculator we t(data1) and t(data2)
  #since the other methods works with t() of data
  data1= t(data1)
  data2= t(data2)
  
  d <- dim(data1)[1]
  n_data <-dim(data1)[2]
  c_s <- create_curve_set(list(r=0:(d-1),obs=cbind(data1,data2)))
  
  group <- factor(rep(c(1,2),each=n_data))
  
  test <- frank.fanova(
    
    nsim = 1000,
    curve_set = c_s,
    groups = group,
    variances = "equal", #unequal variances can be used, but here we assume equal variances
    test.equality = "mean",
    typeone = "fdr",
    algorithm = "IATSE",
    contrasts =TRUE,
    alpha = 0.05
  )
  test_result <- (test$obs>test$hi) | (test$obs<test$lo) 
  return(test_result)
  
}




############## Parallel processing functions ####################
# Custom combine function to accumulate matrices across iterations
parallel_combine_function <- function(x, y) {
  for (M in names(x)) {
    # Combine matrices from the same method in both x and y
    x[[M]] <- cbind(x[[M]], y[[M]])
  }
  return(x)
}

# Handle the Excel file creation and saving for the results of parallel processing
write_results_to_excel <- function(loop, power_results, input_info, file_name) {
  # Create a new Excel workbook
  wb <- createWorkbook()
  
  # Write each method's p-values matrix to a separate sheet, named as "Method=PowerValue"
  for (method_name in names(loop)) {
    # Get the power result for this method
    power_value <- round(power_results[[method_name]], 2)
    
    # Create a sheet name like "TWT=0.08"
    sheet_name <- paste0(method_name, "=", format(power_value, nsmall = 2))
    
    # Add a new sheet
    addWorksheet(wb, sheet_name)
    
    # Write the p-values matrix to the sheet
    writeData(wb, sheet = sheet_name, loop[[method_name]])
  }
  
  # Add a final sheet for the Input_Summary
  addWorksheet(wb, "Input_Summary")
  
  # Write the input_info dataframe to the Input_Summary sheet
  writeData(wb, sheet = "Input_Summary", input_info)
  
  # Save the workbook to the specified file name
  saveWorkbook(wb, file_name, overwrite = TRUE)
}

