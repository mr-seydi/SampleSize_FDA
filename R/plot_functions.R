
# Function to calculate confidence interval for a proportion
Power_CI <- function(p_hat, n_simulations, confidence_level = 0.95) {
  # Validate inputs
  if(is.na(p_hat)) {
    return(list(
      lower_bound = NA,
      upper_bound = NA,
      margin_of_error = NA
    ))
  } else if (p_hat < 0 || p_hat > 1) {
    stop("p_hat must be between 0 and 1.")
  }
  if (n_simulations <= 0) {
    stop("n_simulations must be a positive number.")
  }
  if (confidence_level <= 0 || confidence_level >= 1) {
    stop("confidence_level must be between 0 and 1.")
  }
  
  # Calculate the standard error
  standard_error <- sqrt((p_hat * (1 - p_hat)) / n_simulations)
  
  # Determine the z-multiplier for the confidence level
  z_multiplier <- qnorm(1 - (1 - confidence_level) / 2)
  
  # Calculate the margin of error
  margin_of_error <- z_multiplier * standard_error
  
  # Calculate the confidence interval
  lower_bound <- p_hat - margin_of_error
  upper_bound <- p_hat + margin_of_error
  
  # Return the confidence interval as a list
  return(list(
    lower_bound = lower_bound,
    upper_bound = upper_bound,
    margin_of_error = margin_of_error
  ))
}

SS_finder<-function(data){
  nrow_output <- nrow(data)
  N_sim <- data$n_iterations[1]
  Output <- cbind(data,
                  TWT_upp = rep(NA, nrow_output),
                  IWT_upp = rep(NA, nrow_output),
                  SPM_upp = rep(NA, nrow_output),
                  Fmax_upp = rep(NA, nrow_output),
                  ERL_upp = rep(NA, nrow_output),
                  IATSE_upp = rep(NA, nrow_output)
                  )
  
  
  
  for (i in 1:nrow_output) {
    Output$TWT_upp[i] <- Power_CI(Output$TWT[i],
                                  n_simulations = N_sim)$upper_bound
    Output$IWT_upp[i] <- Power_CI(Output$IWT[i],
                                  n_simulations = N_sim)$upper_bound
    Output$SPM_upp[i] <- Power_CI(Output$SPM[i],
                                  n_simulations = N_sim)$upper_bound
    Output$Fmax_upp[i] <- Power_CI(Output$Fmax[i],
                                  n_simulations = N_sim)$upper_bound
    Output$ERL_upp[i] <- Power_CI(Output$ERL[i],
                                  n_simulations = N_sim)$upper_bound
    Output$IATSE_upp[i] <- Power_CI(Output$IATSE[i],
                                    n_simulations = N_sim)$upper_bound
    
  }
  

  # Define threshold
  threshold <- 0.80
  
  # Find minimum sample size for each combination of noise_sd and noise_fwhm
  results <- Output %>%
    group_by(noise_sd, noise_fwhm) %>%
    summarize(
      TWT_min_sample = min(sample_size[TWT_upp >= threshold], na.rm = TRUE),
      IWT_min_sample = min(sample_size[IWT_upp >= threshold], na.rm = TRUE),
      SPM_min_sample = min(sample_size[SPM_upp >= threshold], na.rm = TRUE),
      Fmax_min_sample = min(sample_size[Fmax_upp >= threshold], na.rm = TRUE),
      ERL_min_sample = min(sample_size[ERL_upp >= threshold], na.rm = TRUE),
      IATSE_min_sample = min(sample_size[IATSE_upp >= threshold], na.rm = TRUE),
      .groups = "drop"
    )
  # factorize noise_sd and noise_fwhm
  results$noise_sd <- factor(results$noise_sd)
  results$noise_fwhm <- factor(results$noise_fwhm)
  
  # Return the results
  return(results)
}




##################

create_heatmap <- function(data, value_column, title) {
  # Create custom labels
  data$custom_label <- ifelse(data[[value_column]] == 4, "<5", 
                              ifelse(data[[value_column]] == Inf, ">60", 
                                     as.character(data[[value_column]])))
  
  ggplot(data, aes(x = noise_fwhm, y = noise_sd, fill = !!sym(value_column))) +
    # Plot all cells using the continuous scale for values between 5 and 60
    geom_tile(color = "black") +
    # Overlay cells where value is Inf with a specific color (e.g., "red")
    geom_tile(data = subset(data, data[[value_column]] == Inf),
              aes(x = noise_fwhm, y = noise_sd), fill = "firebrick", color = "black") +
    # Display custom labels (note: color here can be adjusted as needed)
    geom_text(aes(label = custom_label), color = "#e9e9e9", size = 4,
              na.rm = TRUE, fontface = "bold") +
    # Continuous color scale for values between 5 and 60
    scale_fill_gradient(low = "olivedrab", high = "firebrick3", na.value = "white", 
                        limits = c(5, 60),
                        breaks = c(5, 20, 45, 60),
                        name = "Sample Size:  ") +
    labs(x = "Noise FWHM",
         y = "Noise SD",
         fill = "Sample Size",
         title = title) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          plot.title = element_text(hjust = 0.5)) +
    # small font size for x axis
    theme(axis.title.y = element_text(size = 8)) 
    
}


Data_plot <- function(dataset, TITLE){
  cont_size <- dim(dataset)[1]
  
  col_names <- colnames(dataset)
  # Create a data frame with the two-sample data, excluding 'Pulse'
  plot_data <- data.frame(
    x_values = rep(0:(cont_size - 1), 2),  # Repeat x_values for 2 lines
    y_values = c(dataset[, 1], dataset[, 2]),  # Combine all y-values of the two columns
    legend = factor(rep(c(col_names[1], col_names[2]), each = dim(dataset)[1]))  # Control factor levels
  )
  
  # Create a data frame for the difference (Geometry)
  diff_data <- data.frame(
    x_values = 0:(cont_size - 1),
    y_values = dataset[, 2] - dataset[, 1],
    legend = factor(rep("Difference", cont_size))  # Ensure Geometry is included in legend
  )
  
  # Combine both datasets
  combined_data <- rbind(plot_data, diff_data)
  
  # Define color and line type for all lines
  color_values <- setNames(c("tomato", "cadetblue", "black"
                             ),
                           c(col_names[1], col_names[2], "Difference"))
  linetype_values <- setNames(c("solid", "solid", "dotted"),
                              c(col_names[1], col_names[2], "Difference"))
  
  # Create the plot using ggplot
  ggplot(combined_data, aes(x = x_values, y = y_values, color = legend, linetype = legend)) +
    geom_line(linewidth = 1) +  # Plot lines for each group
    labs(title = TITLE, x = "Domain", y = "Value") +  # Labels
    scale_color_manual(values = color_values) +  # Line colors
    scale_linetype_manual(values = linetype_values) +  # Line types
    theme_minimal() +  # Use a minimal theme
    theme(plot.title = element_text(hjust = 0.5)) +  # Center the title
    theme(legend.position = "bottom") +
    theme(      
      # axis.text.x = element_text(size = 12),
      # axis.text.y = element_text(size = 12),
      # axis.title.x = element_text(size = 14),
      # axis.title.y = element_text(size = 14),
      # plot.title = element_text(size = 16),
      # legend.text = element_text(size = 12),
      legend.title = element_blank())  # Remove legend title
}




FWER_heatmap <- function(data, value_column, title) {
  
  data = data[ ,c("noise_sd","noise_fwhm",value_column)]
  # remove rows with NA values
  data <- data[complete.cases(data),]
  
  # Create custom labels
  data$custom_label <- as.character(round(data[[value_column]], 2))
  
  
  ggplot(data, aes(x = noise_fwhm, y = noise_sd, fill = !!sym(value_column))) +
    # Plot all cells using the continuous scale for values between 5 and 60
    geom_tile(color = "black") +
    # Overlay cells where value is Inf with a specific color (e.g., "red")
    # geom_tile(data = subset(data, round(data[[value_column]],2) == 0.05),
    #           aes(x = noise_fwhm, y = noise_sd), fill = "olivedrab", color = "black") +
    # Display custom labels (note: color here can be adjusted as needed)
    geom_text(aes(label = custom_label), color = "#e9e9e9", size = 4,
              na.rm = TRUE, fontface = "bold") +
    # Continuous color scale for values between 5 and 60
    scale_fill_gradientn(
      colors = c("firebrick", "olivedrab", "firebrick"),
      values = scales::rescale(c(0, 0.05, 0.1)),
      limits = c(0, 0.1),
      breaks = c(0, 0.05, 0.1),
      name = "FWER:  "
    ) +
    labs(x = "Noise FWHM",
         y = "Noise SD",
         title = title) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          plot.title = element_text(hjust = 0.5)) +
    # small font size for x axis
    theme(axis.title.y = element_text(size = 8)) 
  
}




Noise_plot <- function(Noise_curves, Title=""){
  cont_size = dim(Noise_curves)[1]
  n_curves = dim(Noise_curves)[2]
  # Create a data frame for ggplot
  plot_data <- data.frame(
    x_values = rep(0:(cont_size - 1), n_curves),
    y_values = as.vector(Noise_curves),  # Flatten the matrix
    line_group = factor(rep(1:n_curves, each = cont_size))  # Create a group for each column
  )
  
  # Generate the plot using ggplot2
  ggplot(plot_data, aes(x = x_values, y = y_values, group = line_group, color = line_group)) +
    geom_line(linewidth = 1) +  # Set line thickness
    scale_color_manual(values = colorRampPalette(c("darkblue",
                                                   "lightblue"))(n_curves),
                       labels = c(1:n_curves)) +  # Navy blue shades
    labs(title = Title, x = "Domain", y = "Value") +  # Add labels
    theme_minimal() +  # Use a minimal theme
    theme(plot.title = element_text(hjust = 0.5)) +  # Center the plot title
    theme(legend.position = "none")+
    #increase the font size of the labels and axis numbers
    theme(
      # axis.text.x = element_text(size = 12),
      # axis.text.y = element_text(size = 12),
       axis.title.x = element_text(size = 10),
      # axis.title.y = element_text(size = 14),
      # plot.title = element_text(size = 16),
      #remove legend title
      legend.title = element_blank())
}



sample_plot <- function(data_type = "baseline", Org_data = NULL, Signal_curve,
                        noise1_data, noise2_data, Title="", legend="none", xlab="Domain", ylab="Value"){
  
  if (data_type == "baseline") {
    # Generate data with and without pulse
    if(is.null(Org_data)){
      Sample1 <- data_generator(signal = Signal_curve, noise = noise1_data)
      Sample2 <- data_generator(noise = noise2_data)
    } else {
      Sample1 <- data_generator(data = Org_data, signal = Signal_curve, noise = noise1_data)
      Sample2 <- data_generator(data = Org_data, noise = noise2_data)
    }
    
    sample_label_1 <- "Group 1"
    sample_label_2 <- "Group 2"
    
    colors_plot_data <- setNames(c("tomato", "cadetblue"),
                                 c(sample_label_1, 
                                   sample_label_2))
    
  } else {
    # For two_sample case
    
    Sample1 <- data_generator(data = Org_data[, 1], noise = noise1_data)
    Sample2 <- data_generator(data = Org_data[, 2], noise = noise2_data)
    
    # Use the same labels as in pulse_plot
    sample_label_1 <- "Group 1"
    sample_label_2 <- "Group 2"
    
    colors_plot_data <- setNames(c("tomato", "cadetblue"),
                                 c(sample_label_2, 
                                   sample_label_1))
  }
  
  # Calculate mean for each group
  sample1_mean <- rowMeans(Sample1)
  sample2_mean <- rowMeans(Sample2)
  
  # Create a long format data frame for ggplot
  plot_data <- data.frame(
    x_values = rep(0:(nrow(Sample1)-1), ncol(Sample1) * 2),  # Repeat index for each column and dataset
    y_values = c(as.vector(Sample1), as.vector(Sample2)),  # Flatten both datasets
    label = factor(rep(c(sample_label_1, sample_label_2),
                       each = nrow(Sample1) * ncol(Sample1))),  # Labels from Org_data
    line_group = factor(rep(1:ncol(Sample1), each = nrow(Sample1), times = 2))  # Line group for each column
  )
  
  # Create a separate data frame for the mean lines
  mean_data <- data.frame(
    x_values = rep(0:(nrow(Sample1)-1), 2),
    y_values = c(sample1_mean, sample2_mean),
    label = factor(c(rep(sample_label_1, nrow(Sample1)), rep(sample_label_2, nrow(Sample1))), 
                   levels = c(sample_label_1, sample_label_2))
  )
  
  # Create the plot using ggplot2
  ggplot() +
    # First layer: Individual lines
    geom_line(data = plot_data, aes(x = x_values, y = y_values, group = interaction(line_group, label), color = label), linewidth = 0.5, alpha = 0.3) +
    # Second layer: Mean lines without group aesthetic
    geom_line(data = mean_data, aes(x = x_values, y = y_values, color = label), linewidth = 1.5) +
    scale_color_manual(values = colors_plot_data) +  # Set colors for both sample labels
    labs(title = Title, x = xlab, y = ylab) +  # Add labels
    theme_minimal() +  # Use a minimal theme
    theme(plot.title = element_text(hjust = 0.5)) +  # Center the plot title
    theme(legend.position = legend)+  # Move legend to bottom
    #increase the font size of the labels and axis numbers
    theme(
      # axis.text.x = element_text(size = 12),
      # axis.text.y = element_text(size = 12),
       axis.title.x = element_text(size = 10),
      # axis.title.y = element_text(size = 14),
      # plot.title = element_text(size = 16),
      #increase legends font and element size
      legend.text = element_text(size = 12),
      legend.title = element_blank())
  
} 

