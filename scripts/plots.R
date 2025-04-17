source("R/plot_functions.R")
########################
library(readxl)
library(ggplot2)
library(gridExtra)
library(grid)
library(dplyr)

##########################
#####Angle data############

# Load necessary libraries
Output_vGRF <- read_excel("/Users/more0056/Downloads/VGRF_equal_1_small.xlsx")
results_vGRF <- SS_finder(Output_vGRF)

# Create heatmaps for each column


heatmap_TWT_vGRF <- create_heatmap(results_vGRF, "TWT_min_sample", "TWT Minimum Sample Size")
heatmap_IWT_vGRF <- create_heatmap(results_vGRF, "IWT_min_sample", "IWT Minimum Sample Size")
heatmap_SPM_vGRF <- create_heatmap(results_vGRF, "SPM_min_sample", "SPM Minimum Sample Size")
heatmap_Fmax_vGRF <- create_heatmap(results_vGRF, "Fmax_min_sample", "Fmax Minimum Sample Size")
heatmap_ERL_vGRF <- create_heatmap(results_vGRF, "ERL_min_sample", "ERL Minimum Sample Size")
heatmap_IATSE_vGRF <- create_heatmap(results_vGRF, "IATSE_min_sample", "IATSE Minimum Sample Size")

# Print heatmaps
print(heatmap_TWT_vGRF)
print(heatmap_IWT_vGRF)
print(heatmap_SPM_vGRF)
print(heatmap_Fmax_vGRF)
print(heatmap_ERL_vGRF)
print(heatmap_IATSE_vGRF)
##################################
##############MF data#############
Output_KJM <- read_excel("/Users/more0056/Downloads/KJM_equal_1.xlsx")
results_KJM <- SS_finder(Output_KJM)

# Create heatmaps for each column
heatmap_TWT_KJM <- create_heatmap(results_KJM, "TWT_min_sample", "TWT Minimum Sample Size")
heatmap_IWT_KJM <- create_heatmap(results_KJM, "IWT_min_sample", "IWT Minimum Sample Size")
heatmap_SPM_KJM <- create_heatmap(results_KJM, "SPM_min_sample", "Nonparametric SPM Minimum Sample Size")
heatmap_Fmax_KJM <- create_heatmap(results_KJM, "Fmax_min_sample", "Fmax Minimum Sample Size")
heatmap_ERL_KJM <- create_heatmap(results_KJM, "ERL_min_sample", "ERL Minimum Sample Size")
heatmap_IATSE_KJM <- create_heatmap(results_KJM, "IATSE_min_sample", "IATSE Minimum Sample Size")

# Print heatmaps
print(heatmap_TWT_KJM)
print(heatmap_IWT_KJM)
print(heatmap_SPM_KJM)
print(heatmap_Fmax_KJM)
print(heatmap_ERL_KJM)
print(heatmap_IATSE_KJM)
##########################################
legend_plot <- heatmap_IATSE_KJM + theme(legend.position = "bottom") +
  # increase foint size of the legend title
  theme(legend.title = element_text(size = 14)) 
 
# Extract a common legend from one of the plots (they share the same aesthetic)
grob_legend <- ggplotGrob(legend_plot)

# Extract legend component from the gtable
legend_index <- which(sapply(grob_legend$grobs, function(x) x$name) == "guide-box")
shared_legend <- grob_legend$grobs[[legend_index]]

###########################################


# Plot all the datasets using the function
# Assume plot_heatmap is a custom function for generating heatmaps.
# Replace the function calls with your actual plot_heatmap code.
plot1 <- create_heatmap(results_vGRF, "TWT_min_sample", "") + theme(legend.position = "none", axis.title.x=element_blank(),
                                                                    axis.text.x=element_blank(),
                                                                    axis.ticks.x=element_blank()) + labs(title = NULL) 
plot2 <- create_heatmap(results_vGRF, "IWT_min_sample", "") + theme(legend.position = "none", axis.title.x=element_blank(),
                                                                    axis.text.x=element_blank(),
                                                                    axis.ticks.x=element_blank()) + labs(title = NULL) 
plot3 <- create_heatmap(results_vGRF, "SPM_min_sample", "") + theme(legend.position = "none", axis.title.x=element_blank(),
                                                                    axis.text.x=element_blank(),
                                                                    axis.ticks.x=element_blank()) + labs(title = NULL) 
plot4 <- create_heatmap(results_vGRF, "Fmax_min_sample", "") + theme(legend.position = "none", axis.title.x=element_blank(),
                                                                     axis.text.x=element_blank(),
                                                                     axis.ticks.x=element_blank()) + labs(title = NULL) 
plot5 <- create_heatmap(results_vGRF, "ERL_min_sample", "") + theme(legend.position = "none", axis.title.x=element_blank(),
                                                                    axis.text.x=element_blank(),
                                                                    axis.ticks.x=element_blank()) + labs(title = NULL) 
plot6 <- create_heatmap(results_vGRF, "IATSE_min_sample", "") + theme(legend.position = "none") + labs(title = NULL) 



plot7 <- create_heatmap(results_KJM, "TWT_min_sample", "") + theme(legend.position = "none", axis.title.x=element_blank(),
                                                                   axis.text.x=element_blank(),
                                                                   axis.ticks.x=element_blank()) + labs(title = NULL) 
plot8 <- create_heatmap(results_KJM, "IWT_min_sample", "") + theme(legend.position = "none", axis.title.x=element_blank(),
                                                                   axis.text.x=element_blank(),
                                                                   axis.ticks.x=element_blank()) + labs(title = NULL) 
plot9 <- create_heatmap(results_KJM, "SPM_min_sample", "") + theme(legend.position = "none", axis.title.x=element_blank(),
                                                                   axis.text.x=element_blank(),
                                                                   axis.ticks.x=element_blank()) + labs(title = NULL) 
plot10 <- create_heatmap(results_KJM, "Fmax_min_sample", "") + theme(legend.position = "none", axis.title.x=element_blank(),
                                                                     axis.text.x=element_blank(),
                                                                     axis.ticks.x=element_blank()) + labs(title = NULL) 
plot11 <- create_heatmap(results_KJM, "ERL_min_sample", "") + theme(legend.position = "none", axis.title.x=element_blank(),
                                                                    axis.text.x=element_blank(),
                                                                    axis.ticks.x=element_blank()) + labs(title = NULL) 
plot12 <- create_heatmap(results_KJM, "IATSE_min_sample", "") + theme(legend.position = "none") + labs(title = NULL) 

##############################
# library(grid)
# library(gridExtra)

# Convert each ggplot to a grob
p1_grob  <- ggplotGrob(plot1)
p2_grob  <- ggplotGrob(plot2)
p3_grob  <- ggplotGrob(plot3)
p4_grob  <- ggplotGrob(plot4)
p5_grob  <- ggplotGrob(plot5)
p6_grob  <- ggplotGrob(plot6)
p7_grob  <- ggplotGrob(plot7)
p8_grob  <- ggplotGrob(plot8)
p9_grob  <- ggplotGrob(plot9)
p10_grob <- ggplotGrob(plot10)
p11_grob <- ggplotGrob(plot11)
p12_grob <- ggplotGrob(plot12)

# Put all grobs in a list
all_grobs <- list(
  p1_grob, p2_grob, p3_grob, p4_grob, p5_grob, p6_grob,
  p7_grob, p8_grob, p9_grob, p10_grob, p11_grob, p12_grob
)

# 1) Unify widths
maxWidth <- do.call(unit.pmax, lapply(all_grobs, function(g) g$widths))
for (i in seq_along(all_grobs)) {
  all_grobs[[i]]$widths <- maxWidth
}

# 2) Unify heights
maxHeight <- do.call(unit.pmax, lapply(all_grobs, function(g) g$heights))
for (i in seq_along(all_grobs)) {
  all_grobs[[i]]$heights <- maxHeight
}









# Create a title for each column
col1_title <- textGrob("vGRF", gp=gpar(fontsize=14, fontface="bold"))
col2_title <- textGrob("KJM", gp=gpar(fontsize=14, fontface="bold"))

# Create a nullGrob to act as the spacer between columns
spacer <- nullGrob()

Hjust <- 0
# Create row titles
row1_title <- textGrob("TWT", rot=90, hjust = Hjust, gp=gpar(fontsize=12, fontface="bold"))
row2_title <- textGrob("IWT", rot=90, hjust = Hjust, gp=gpar(fontsize=12, fontface="bold"))
row3_title <- textGrob("SPM", rot=90, hjust = Hjust, gp=gpar(fontsize=12, fontface="bold"))
row4_title <- textGrob("F-max", rot=90, hjust = Hjust, gp=gpar(fontsize=12, fontface="bold"))
row5_title <- textGrob("ERL", rot=90, hjust = Hjust, gp=gpar(fontsize=12, fontface="bold"))
row6_title <- textGrob("IATSE", rot=90, hjust = Hjust, gp=gpar(fontsize=12, fontface="bold"))


##############################
final_plot <- grid.arrange(
  spacer, col1_title, spacer, col2_title,         # Column titles
  row1_title, all_grobs[[1]], spacer, all_grobs[[7]],
  row2_title, all_grobs[[2]], spacer, all_grobs[[8]],
  row3_title, all_grobs[[3]], spacer, all_grobs[[9]],
  row4_title, all_grobs[[4]], spacer, all_grobs[[10]],
  row5_title, all_grobs[[5]], spacer, all_grobs[[11]],
  row6_title, all_grobs[[6]], spacer, all_grobs[[12]],
  ncol = 4,
  layout_matrix = rbind(
    c(1, 2, 3, 4),             
    c(5, 6, 7, 8),             
    c(9, 10, 11, 12),          
    c(13, 14, 15, 16),         
    c(17, 18, 19, 20),         
    c(21, 22, 23, 24),         
    c(25, 26, 27, 28)          
  ),        
  heights = c(0.15, 1, 1, 1, 1, 1, 1),
  widths  = c(0.1, 1, 0.1, 1)
)

grid.newpage()
grid.draw(final_plot)

##############################

# Combine the main plot with the shared legend as the last row
final_plot_with_legend <- grid.arrange(
  final_plot,      # The main plot grid
  shared_legend,   # The shared legend
  ncol = 1,        # Arrange in a single column
  heights = c(10, 1)  # Adjust relative heights as needed (e.g., main plot gets 10x the space)
)

# Display the final plot with shared legend
grid.newpage()
grid.draw(final_plot_with_legend)
#save plot
#ggsave("Outputs/plot_final.jpeg", final_plot_with_legend, width = 190, height = 196, units = "mm", dpi = 1200)


#####################################
source("R/plot_functions.R")
source("R/utilities.R")
source("R/Data_functions.R")

plot1 <- Data_plot(vGRF_data_Phan("both"), TITLE = "vGRF") +
  ylab("Force (Body Weight)") +
  xlab("% Stance Phase")
plot2 <- Data_plot(Moment_data("both")[, c(2, 1)],TITLE = "KJM") +
  ylab(expression(N ~ . ~ m ~ . ~ kg^{-1})) +
  xlab("% Stance Phase")


# # Extract a common legend from one of the plots (they share the same aesthetic)
# grob_legend <- ggplotGrob(parametric_plot)
# 
# # Extract legend component from the gtable
# legend_index <- which(sapply(grob_legend$grobs, function(x) x$name) == "guide-box")
# shared_legend <- grob_legend$grobs[[legend_index]]


#plot plot1 and 2 in a same row
data_plot <- grid.arrange(plot1, plot2, ncol=2)

#ggsave("Outputs/plot_data.jpeg", data_plot, width = 190, height = 120, units = "mm", dpi = 1200)




#####################FWER########################



FWER_data <- read_excel("/Users/more0056/Downloads/FWER_vGRF_KMJ.xlsx")

results_vGRF <- FWER_data[FWER_data$noise_sd %in% c(0.4,0.5,0.6),]

results_KJM <- FWER_data[FWER_data$noise_sd %in% c(0.8,1,1.2),]

# Create heatmaps for each column


heatmap_TWT_vGRF <- FWER_heatmap(results_vGRF, "TWT", "TWT Minimum Sample Size")
heatmap_IWT_vGRF <- FWER_heatmap(results_vGRF, "IWT", "IWT Minimum Sample Size")
heatmap_SPM_vGRF <- FWER_heatmap(results_vGRF, "SPM", "SPM Minimum Sample Size")
heatmap_Fmax_vGRF <- FWER_heatmap(results_vGRF, "Fmax", "Fmax Minimum Sample Size")
heatmap_ERL_vGRF <- FWER_heatmap(results_vGRF, "ERL", "ERL Minimum Sample Size")
heatmap_IATSE_vGRF <- FWER_heatmap(results_vGRF, "IATSE", "IATSE Minimum Sample Size")

# Print heatmaps
print(heatmap_TWT_vGRF)
print(heatmap_IWT_vGRF)
print(heatmap_SPM_vGRF)
print(heatmap_Fmax_vGRF)
print(heatmap_ERL_vGRF)
print(heatmap_IATSE_vGRF)
##################################
##############MF data#############

# Create heatmaps for each column
heatmap_TWT_KJM <- FWER_heatmap(results_KJM, "TWT", "TWT Minimum Sample Size")
heatmap_IWT_KJM <- FWER_heatmap(results_KJM, "IWT", "IWT Minimum Sample Size")
heatmap_SPM_KJM <- FWER_heatmap(results_KJM, "SPM", "Nonparametric SPM Minimum Sample Size")
heatmap_Fmax_KJM <- FWER_heatmap(results_KJM, "Fmax", "Fmax Minimum Sample Size")
heatmap_ERL_KJM <- FWER_heatmap(results_KJM, "ERL", "ERL Minimum Sample Size")
heatmap_IATSE_KJM <- FWER_heatmap(results_KJM, "IATSE", "IATSE Minimum Sample Size")

# Print heatmaps
print(heatmap_TWT_KJM)
print(heatmap_IWT_KJM)
print(heatmap_SPM_KJM)
print(heatmap_Fmax_KJM)
print(heatmap_ERL_KJM)
print(heatmap_IATSE_KJM)
##########################################
legend_plot <- heatmap_IATSE_KJM + theme(legend.position = "bottom") +
  # increase foint size of the legend title
  theme(legend.title = element_text(size = 14)) 

# Extract a common legend from one of the plots (they share the same aesthetic)
grob_legend <- ggplotGrob(legend_plot)

# Extract legend component from the gtable
legend_index <- which(sapply(grob_legend$grobs, function(x) x$name) == "guide-box")
shared_legend <- grob_legend$grobs[[legend_index]]

###########################################


# Plot all the datasets using the function
# Assume plot_heatmap is a custom function for generating heatmaps.
# Replace the function calls with your actual plot_heatmap code.
plot1 <- FWER_heatmap(results_vGRF, "TWT", "") + theme(legend.position = "none", axis.title.x=element_blank(),
                                                                    axis.text.x=element_blank(),
                                                                    axis.ticks.x=element_blank()) + labs(title = NULL) 
plot2 <- FWER_heatmap(results_vGRF, "IWT", "") + theme(legend.position = "none", axis.title.x=element_blank(),
                                                                    axis.text.x=element_blank(),
                                                                    axis.ticks.x=element_blank()) + labs(title = NULL) 
plot3 <- FWER_heatmap(results_vGRF, "SPM", "") + theme(legend.position = "none", axis.title.x=element_blank(),
                                                                    axis.text.x=element_blank(),
                                                                    axis.ticks.x=element_blank()) + labs(title = NULL) 
plot4 <- FWER_heatmap(results_vGRF, "Fmax", "") + theme(legend.position = "none", axis.title.x=element_blank(),
                                                                     axis.text.x=element_blank(),
                                                                     axis.ticks.x=element_blank()) + labs(title = NULL) 
plot5 <- FWER_heatmap(results_vGRF, "ERL", "") + theme(legend.position = "none", axis.title.x=element_blank(),
                                                                    axis.text.x=element_blank(),
                                                                    axis.ticks.x=element_blank()) + labs(title = NULL) 
plot6 <- FWER_heatmap(results_vGRF, "IATSE", "") + theme(legend.position = "none") + labs(title = NULL) 



plot7 <- FWER_heatmap(results_KJM, "TWT", "") + theme(legend.position = "none", axis.title.x=element_blank(),
                                                                   axis.text.x=element_blank(),
                                                                   axis.ticks.x=element_blank()) + labs(title = NULL) 
plot8 <- FWER_heatmap(results_KJM, "IWT", "") + theme(legend.position = "none", axis.title.x=element_blank(),
                                                                   axis.text.x=element_blank(),
                                                                   axis.ticks.x=element_blank()) + labs(title = NULL) 
plot9 <- FWER_heatmap(results_KJM, "SPM", "") + theme(legend.position = "none", axis.title.x=element_blank(),
                                                                   axis.text.x=element_blank(),
                                                                   axis.ticks.x=element_blank()) + labs(title = NULL) 
plot10 <- FWER_heatmap(results_KJM, "Fmax", "") + theme(legend.position = "none", axis.title.x=element_blank(),
                                                                     axis.text.x=element_blank(),
                                                                     axis.ticks.x=element_blank()) + labs(title = NULL) 
plot11 <- FWER_heatmap(results_KJM, "ERL", "") + theme(legend.position = "none", axis.title.x=element_blank(),
                                                                    axis.text.x=element_blank(),
                                                                    axis.ticks.x=element_blank()) + labs(title = NULL) 
plot12 <- FWER_heatmap(results_KJM, "IATSE", "") + theme(legend.position = "none") + labs(title = NULL) 

##############################
# library(grid)
# library(gridExtra)

# Convert each ggplot to a grob
p1_grob  <- ggplotGrob(plot1)
p2_grob  <- ggplotGrob(plot2)
p3_grob  <- ggplotGrob(plot3)
p4_grob  <- ggplotGrob(plot4)
p5_grob  <- ggplotGrob(plot5)
p6_grob  <- ggplotGrob(plot6)
p7_grob  <- ggplotGrob(plot7)
p8_grob  <- ggplotGrob(plot8)
p9_grob  <- ggplotGrob(plot9)
p10_grob <- ggplotGrob(plot10)
p11_grob <- ggplotGrob(plot11)
p12_grob <- ggplotGrob(plot12)

# Put all grobs in a list
all_grobs <- list(
  p1_grob, p2_grob, p3_grob, p4_grob, p5_grob, p6_grob,
  p7_grob, p8_grob, p9_grob, p10_grob, p11_grob, p12_grob
)

# 1) Unify widths
maxWidth <- do.call(unit.pmax, lapply(all_grobs, function(g) g$widths))
for (i in seq_along(all_grobs)) {
  all_grobs[[i]]$widths <- maxWidth
}

# 2) Unify heights
maxHeight <- do.call(unit.pmax, lapply(all_grobs, function(g) g$heights))
for (i in seq_along(all_grobs)) {
  all_grobs[[i]]$heights <- maxHeight
}









# Create a title for each column
col1_title <- textGrob("vGRF", gp=gpar(fontsize=14, fontface="bold"))
col2_title <- textGrob("KJM", gp=gpar(fontsize=14, fontface="bold"))

# Create a nullGrob to act as the spacer between columns
spacer <- nullGrob()

Hjust <- 0
# Create row titles
row1_title <- textGrob("TWT", rot=90, hjust = Hjust, gp=gpar(fontsize=12, fontface="bold"))
row2_title <- textGrob("IWT", rot=90, hjust = Hjust, gp=gpar(fontsize=12, fontface="bold"))
row3_title <- textGrob("SPM", rot=90, hjust = Hjust, gp=gpar(fontsize=12, fontface="bold"))
row4_title <- textGrob("F-max", rot=90, hjust = Hjust, gp=gpar(fontsize=12, fontface="bold"))
row5_title <- textGrob("ERL", rot=90, hjust = Hjust, gp=gpar(fontsize=12, fontface="bold"))
row6_title <- textGrob("IATSE", rot=90, hjust = Hjust, gp=gpar(fontsize=12, fontface="bold"))


##############################
final_plot <- grid.arrange(
  spacer, col1_title, spacer, col2_title,         # Column titles
  row1_title, all_grobs[[1]], spacer, all_grobs[[7]],
  row2_title, all_grobs[[2]], spacer, all_grobs[[8]],
  row3_title, all_grobs[[3]], spacer, all_grobs[[9]],
  row4_title, all_grobs[[4]], spacer, all_grobs[[10]],
  row5_title, all_grobs[[5]], spacer, all_grobs[[11]],
  row6_title, all_grobs[[6]], spacer, all_grobs[[12]],
  ncol = 4,
  layout_matrix = rbind(
    c(1, 2, 3, 4),             
    c(5, 6, 7, 8),             
    c(9, 10, 11, 12),          
    c(13, 14, 15, 16),         
    c(17, 18, 19, 20),         
    c(21, 22, 23, 24),         
    c(25, 26, 27, 28)          
  ),        
  heights = c(0.15, 1, 1, 1, 1, 1, 1),
  widths  = c(0.1, 1, 0.1, 1)
)

grid.newpage()
grid.draw(final_plot)

##############################

# Combine the main plot with the shared legend as the last row
final_plot_with_legend <- grid.arrange(
  final_plot,      # The main plot grid
  shared_legend,   # The shared legend
  ncol = 1,        # Arrange in a single column
  heights = c(10, 1)  # Adjust relative heights as needed (e.g., main plot gets 10x the space)
)

# Display the final plot with shared legend
grid.newpage()
grid.draw(final_plot_with_legend)
#save plot
#ggsave("Outputs/plot_final_FWER.jpeg", final_plot_with_legend, width = 190, height = 196, units = "mm", dpi = 1200)








########Plot sample data##########
library(readxl)
library(ggplot2)
library(gridExtra)
library(grid)
library(dplyr)
source("R/plot_functions.R")
source("R/utilities.R")
source("R/Data_functions.R")
set.seed(123)
Noise_data_1_vGRF <- noise_guassian_curve(number_of_curves=10, continuum_size=101)
NFWHM_10_1_vGRF <- smoothed_gussian_curves(data=Noise_data_1_vGRF, mu=0, sig=0.4, fwhm=10)
NFWHM_30_1_vGRF <- smoothed_gussian_curves(data=Noise_data_1_vGRF, mu=0, sig=0.5, fwhm=30)
NFWHM_50_1_vGRF <- smoothed_gussian_curves(data=Noise_data_1_vGRF, mu=0, sig=0.6, fwhm=50)

Noise_data_2_vGRF <- noise_guassian_curve(number_of_curves=10, continuum_size=101)
NFWHM_10_2_vGRF <- smoothed_gussian_curves(data=Noise_data_2_vGRF, mu=0, sig=0.4, fwhm=10)
NFWHM_30_2_vGRF <- smoothed_gussian_curves(data=Noise_data_2_vGRF, mu=0, sig=0.5, fwhm=30)
NFWHM_50_2_vGRF <- smoothed_gussian_curves(data=Noise_data_2_vGRF, mu=0, sig=0.6, fwhm=50)

NFWHM_10_curve_vGRF <- Noise_plot(NFWHM_10_1_vGRF) + ylim(-1.5, 1.5) +
  theme(legend.position = "none", axis.title.x=element_blank(),
        axis.text.x=element_blank(), axis.title.y=element_blank(),
        axis.ticks.x=element_blank(), axis.ticks.y = element_blank()) + labs(title = NULL)
NFWHM_30_curve_vGRF <- Noise_plot(NFWHM_30_1_vGRF) + ylim(-1.5, 1.5) +
  theme(legend.position = "none", axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), axis.ticks.y = element_blank()) + labs(title = NULL)
NFWHM_50_curve_vGRF <- Noise_plot(NFWHM_50_1_vGRF) + ylim(-1.5, 1.5) +
  theme(legend.position = "none",
        axis.title.y=element_blank(),
        axis.ticks.y = element_blank()) + labs(title = NULL)



plot1_vGRF <- sample_plot(data_type = "two-sample", Org_data = vGRF_data_Phan("both"),
                       Signal_curve = NULL, noise1_data = NFWHM_10_1_vGRF,
                       noise2_data = NFWHM_10_2_vGRF,Title = "", legend = "none") + ylim(-1, 4) +
  theme(legend.position = "none", axis.title.x=element_blank(),
  axis.text.x=element_blank(), axis.title.y=element_blank(),
  axis.ticks.x=element_blank(), axis.ticks.y = element_blank()) + labs(title = NULL)
plot2_vGRF <- sample_plot(data_type = "two-sample", Org_data = vGRF_data_Phan("both"),
                        Signal_curve = NULL, noise1_data = NFWHM_30_1_vGRF,
                        noise2_data = NFWHM_30_2_vGRF,Title = "", legend = "none") + ylim(-1, 4)+
  theme(legend.position = "none", axis.title.x=element_blank(),
        axis.text.x=element_blank(), axis.title.y=element_blank(),
        axis.ticks.x=element_blank(), axis.ticks.y = element_blank()) + labs(title = NULL)
plot3_vGRF <- sample_plot(data_type = "two-sample", Org_data = vGRF_data_Phan("both"),
                       Signal_curve = NULL, noise1_data = NFWHM_50_1_vGRF,
                       noise2_data = NFWHM_50_2_vGRF,Title = "", legend = "none") + ylim(-1, 4) +
  theme(legend.position = "none",
       axis.title.y=element_blank(),
       axis.ticks.y = element_blank()) + labs(title = NULL)


Noise_data_1_KJM <- noise_guassian_curve(number_of_curves=10, continuum_size=101)
NFWHM_10_1_KJM <- smoothed_gussian_curves(data=Noise_data_1_KJM, mu=0, sig=0.8, fwhm=10)
NFWHM_30_1_KJM <- smoothed_gussian_curves(data=Noise_data_1_KJM, mu=0, sig=1, fwhm=30)
NFWHM_50_1_KJM <- smoothed_gussian_curves(data=Noise_data_1_KJM, mu=0, sig=1.2, fwhm=50)

Noise_data_2_KJM <- noise_guassian_curve(number_of_curves=10, continuum_size=101)
NFWHM_10_2_KJM <- smoothed_gussian_curves(data=Noise_data_2_KJM, mu=0, sig=0.8, fwhm=10)
NFWHM_30_2_KJM <- smoothed_gussian_curves(data=Noise_data_2_KJM, mu=0, sig=1, fwhm=30)
NFWHM_50_2_KJM <- smoothed_gussian_curves(data=Noise_data_2_KJM, mu=0, sig=1.2, fwhm=50)

NFWHM_10_curve_KJM <- Noise_plot(NFWHM_10_1_KJM) + ylim(-2.5, 3) + 
  theme(legend.position = "none", axis.title.x=element_blank(),
        axis.text.x=element_blank(), axis.title.y=element_blank(),
        axis.ticks.x=element_blank(), axis.ticks.y = element_blank()) + labs(title = NULL)
NFWHM_30_curve_KJM <- Noise_plot(NFWHM_30_1_KJM) + ylim(-2.5, 3) +
  theme(legend.position = "none", axis.title.x=element_blank(),
        axis.text.x=element_blank(), axis.title.y=element_blank(),
        axis.ticks.x=element_blank(), axis.ticks.y = element_blank()) + labs(title = NULL)
NFWHM_50_curve_KJM <- Noise_plot(NFWHM_50_1_KJM) + ylim(-2.5, 3) +
  theme(legend.position = "none",
        axis.title.y=element_blank(),
        axis.ticks.y = element_blank()) + labs(title = NULL)

plot1_KJM <- sample_plot(data_type = "two-sample", Org_data = Moment_data("both")[, c(2, 1)],
                Signal_curve = NULL, noise1_data = NFWHM_10_1_KJM,
                noise2_data = NFWHM_10_2_KJM,Title = "", legend = "none") + ylim(-3, 3.5) +
  theme(legend.position = "none", axis.title.x=element_blank(),
        axis.text.x=element_blank(), axis.title.y=element_blank(),
        axis.ticks.x=element_blank(), axis.ticks.y = element_blank()) + labs(title = NULL)
plot2_KJM <- sample_plot(data_type = "two-sample", Org_data = Moment_data("both")[, c(2, 1)],
                Signal_curve = NULL, noise1_data = NFWHM_30_1_KJM,
                noise2_data = NFWHM_30_2_KJM,Title = "", legend = "none") + ylim(-3, 3.5) +
  theme(legend.position = "none", axis.title.x=element_blank(),
        axis.text.x=element_blank(), axis.title.y=element_blank(),
        axis.ticks.x=element_blank(), axis.ticks.y = element_blank()) + labs(title = NULL)
plot3_KJM <- sample_plot(data_type = "two-sample", Org_data = Moment_data("both")[, c(2, 1)],
                Signal_curve = NULL, noise1_data = NFWHM_50_1_KJM,
                noise2_data = NFWHM_50_2_KJM,Title = "", legend = "none") + ylim(-3, 3.5) +
  theme(legend.position = "none",
        axis.title.y=element_blank(),
        axis.ticks.y = element_blank()) + labs(title = NULL)



####################
library(gridExtra)
# #install.packages("cowplot")
# library(cowplot)
library(grid)



# Create a legend using one of the plots
legend_plot <- sample_plot(data_type = "two-sample", Org_data = vGRF_data_Phan("both"),
                           Signal_curve = NULL, noise1_data = NFWHM_10_1_vGRF,
                           noise2_data = NFWHM_10_2_vGRF,Title = "", legend = "bottom") + ylim(-1, 4)
# Convert the plot to a grob object
grob_legend <- ggplotGrob(legend_plot)

# Extract legend component from the gtable
legend_index <- which(sapply(grob_legend$grobs, function(x) x$name) == "guide-box")
shared_legend <- grob_legend$grobs[[legend_index]]

#######new#####
# -------------------------------------------------------
# CONVERT YOUR PLOTS TO GROBS
# -------------------------------------------------------
# Your list of plots (make sure these objects are created from your earlier code)
plots <- list(
  NFWHM_10_curve_vGRF, plot1_vGRF, NFWHM_10_curve_KJM, plot1_KJM,
  NFWHM_30_curve_vGRF, plot2_vGRF, NFWHM_30_curve_KJM, plot2_KJM,
  NFWHM_50_curve_vGRF, plot3_vGRF, NFWHM_50_curve_KJM, plot3_KJM
)

# Convert each ggplot object into a grob
grob_list <- lapply(plots, ggplotGrob)

# -------------------------------------------------------
# UNIFY WIDTHS & HEIGHTS ACROSS ALL PLOTS
# -------------------------------------------------------
# Unify the widths: find the maximum widths across all grobs and set each grob's widths to that value.
maxWidth <- do.call(unit.pmax, lapply(grob_list, function(g) g$widths))
for (i in seq_along(grob_list)) {
  grob_list[[i]]$widths <- maxWidth
}

# Unify the heights: find the maximum heights across all grobs and set each grob's heights to that value.
maxHeight <- do.call(unit.pmax, lapply(grob_list, function(g) g$heights))
for (i in seq_along(grob_list)) {
  grob_list[[i]]$heights <- maxHeight
}

# -------------------------------------------------------
# ARRANGE THE PLOTS IN A GRID
# -------------------------------------------------------
# Here we arrange the grob_list into a grid.
# You can change ncol to arrange them into the layout you desire.
grid_plots <- arrangeGrob(grobs = grob_list, ncol = 4)

# (Optional) If you want to add row and column labels, you can follow an approach similar to your first block.
# For example, create the labels:
column_labels <- arrangeGrob(
  textGrob("Noise", gp = gpar(fontsize = 10, fontface = "bold"), hjust = -1.1),
  textGrob("vGRF Sample Curves", gp = gpar(fontsize = 10, fontface = "bold"), hjust = 0.2),
  textGrob("Noise", gp = gpar(fontsize = 10, fontface = "bold"), hjust = -0.1),
  textGrob("KJM Sample Curves", gp = gpar(fontsize = 10, fontface = "bold"), hjust = 0.5),
  ncol = 4
)


row_labels <- arrangeGrob(
  grobs = lapply(c("Noise FWHM = 10", "Noise FWHM = 30", "Noise FWHM = 50"), function(label) {
    textGrob(label, rot = 90, gp = gpar(fontsize = 10, fontface = "bold"))
  }),
  nrow = 3
)

# Combine the row labels with the grid of plots.
grid_with_labels <- arrangeGrob(
  row_labels, grid_plots,
  ncol = 2,
  widths = unit.c(unit(1, "cm"), unit(0.9, "npc"))
)

# Combine everything using cowplot's plot_grid or grid.arrange.
# Here we use cowplot for an easy layout of the grid and the legend:
library(cowplot)
final_plot <- plot_grid(
  arrangeGrob(column_labels, grid_with_labels, ncol = 1, 
              heights = unit.c(unit(1, "cm"), unit(0.9, "npc"))),
  plot_grid(shared_legend),   # Legend at the bottom
  ncol = 1, rel_heights = c(0.9, 0.1)
)

# Display the final plot
grid.newpage()
grid.draw(final_plot)

#ggsave("Outputs/plot_samples.jpeg", final_plot, width = 190, height = 196, units = "mm", dpi = 1200)

