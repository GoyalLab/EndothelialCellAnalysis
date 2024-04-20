########################################################################################
#Description: To take in data from 3 csv files for 3 replicates and plot radial data after subsampling
#Author: Keerthana
#Version: 1
#Created on: 18th April 2024
########################################################################################

library(ggplot2)
library(dplyr)
library(plotrix)
library(rstatix)
library(ggpubr)
library(svglite)
library(glue)

subsampleData <- function(df, subsampledNumber){
  subsampled_data <- df %>%
  group_by(Treatment, Condition) %>%  # Group data by Treatment and Condition
  slice_sample(n = subsampledNumber) %>%  # Subsample up to 200 observations per group
  ungroup() 
  subsampled_data <- subset(subsampled_data, select = -Image)
  return(subsampled_data)
}

shuffleData <- function(df, subsampledNumber){
  subsampled_data <- df %>%
    group_by(Treatment, Condition) %>%  # Group data by Treatment and Condition
    slice_sample(n = subsampledNumber) %>%  # Subsample up to 200 observations per group
    ungroup() 
  return(subsampled_data)
}

#1 is 22TL, 2 is 20TL and 3 is WMJ
rep3_1 <- read.csv("/projects/b1042/GoyalLab/Keerthana/MoiseEtAl/plotData/Fig5_data_20TL_5z7.csv")
rep3_2 <- read.csv("/projects/b1042/GoyalLab/Keerthana/MoiseEtAl/plotData/Fig5_data_20TL_Tub.csv")
replicate3 <- rbind(rep3_1, rep3_2)

rep2_1 <- read.csv("/projects/b1042/GoyalLab/Keerthana/MoiseEtAl/plotData/Fig5_data_20TL_5z7.csv")
rep2_2 <- read.csv("/projects/b1042/GoyalLab/Keerthana/MoiseEtAl/plotData/Fig5_data_20TL_Tub.csv")
replicate2 <- rbind(rep2_1, rep2_2)

replicate1 <- read.csv("/projects/b1042/GoyalLab/Keerthana/MoiseEtAl/plotData/Fig5_data_22TL_all.csv")
Caption = ""
#Subsample each of them and save the file

replicate1 <- mutate(replicate1, replicate = "22TL")
replicate2 <- mutate(replicate2, replicate = "20TL")
replicate3 <- mutate(replicate3, replicate = "WMJ")

subRep1 <- subsampleData(replicate1, 33)
subRep2 <- subsampleData(replicate2, 33)
subRep3 <- subsampleData(replicate3,34)

df_comb <- bind_rows(subRep1, subRep2, subRep3)

cv_df <- df_comb %>%
  group_by(Treatment, Condition, replicate) %>%
  summarise(
    mean = mean(Orientation, na.rm = TRUE),
    sd = sd(Orientation, na.rm = TRUE),
    cv = (sd / mean) * 100  # CV as a percentage
  )
save_path <- "/projects/b1042/GoyalLab/Keerthana/MoiseEtAl/plotData/allReplicatesCombinedRadialPlots/"
write_csv(df_comb, paste0(save_path, "allReplicatesRadialPlot_Fig5.csv"))
write_csv(cv_df, paste0(save_path, "allReplicatesRadialPlot_Fig5_CV.csv"))

svglite(paste0("/projects/b1042/GoyalLab/Keerthana/MoiseEtAl/plot/RadialOrientation_Fig5_5z7.svg"), height = 20, width = 10)

testlen <- c(0,1)
testpos<- c(0,360)
#Make scale 0 to 180
df_comb$Orientation <- 90+df_comb$Orientation
df_comb <- shuffleData(df_comb, 100)
par(mfrow=c(4,2))
index = 0
# Assuming df_comb is pre-loaded and available
for (i in unique(df_comb$Treatment)) {
  for (j in unique(df_comb$Condition)) {
    if(index ==8){
      dev.off()
      svglite(paste0("/projects/b1042/GoyalLab/Keerthana/MoiseEtAl/plot/RadialOrientation_Fig5_Tubacin.svg"), height = 20, width = 10)
      par(mfrow=c(4,2))
    }
    index = index + 1
    polar.plot(testlen, testpos, main=paste0(i, ": ", j),
               line.col=1, rp.type="p", start=90,
               grid.col="black", show.radial.grid=F, point.symbols=NA,
               show.grid.labels=0, show.centroid=F, radial.lim=1.0,
               cex.main=3.5, labels = (""))
    
    label_radius = 1.15  # Adjust this to place labels further from center
    angles_deg <- c(0, 90, 180, 270)
    angles_rad <- angles_deg * pi / 180
    labels <- c("0", "90", "180", "270")
    text(x=label_radius * cos(angles_rad), y=label_radius * sin(angles_rad), labels=labels, cex=1.4)
    
    subsetted_df <- df_comb[df_comb$Treatment == i & df_comb$Condition == j, ]
    
    # Draw diameters for each entry in the subset
    for (k in seq_len(nrow(subsetted_df))) {
      orientation <- subsetted_df$Orientation[k] * pi / 180  # Convert to radians
      x_start <- cos(orientation + pi)
      y_start <- sin(orientation + pi)
      x_end <- cos(orientation)
      y_end <- sin(orientation)
      color <- get_color(subsetted_df$replicate[k])
      
      # Draw the diameter segment
      segments(x_start, y_start, x_end, y_end, col = color, lwd = 1.5) # Set line width to 2 for visibility
    }
    
      }
   
    
    # ... existing code to finalize each plot ...
  mtext("Figure 5", side = 3, outer = TRUE, line = -3, cex = 1.5)
}
dev.off()

# Define a named vector of colors for each replicate value
replicate_colors <- c("20TL" = "#1b9e66", "22TL" = "#d95f02", "WMJ" = "#7570b3")

# Function to get color or return a message if unexpected value is found
get_color <- function(rep_value) {
  if (!rep_value %in% names(replicate_colors)) {
    warning("Unexpected replicate value found: ", rep_value)
    return("black") # Default color for unexpected values
  }
  replicate_colors[rep_value]
}

library(ggplot2)

# Assuming 'cv_df' is the dataframe with the calculated CV and 'replicate' is the grouping variable.
cv_df <- cv_df %>%
  mutate(combined = factor(paste(Treatment, Condition, sep = "-")))

# Plotting
ggplot(cv_df, aes(x = combined, y = cv, color = replicate)) +
  geom_point(size = 4) +
  labs(y = "Coefficient of Variation (%)", x = "Replicate - Condition", title = "Coefficient of Variation by Replicate and Condition") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
ggplot(cv_df, aes(x = Treatment, y = cv, color = replicate)) +
  geom_point(size = 4) +  # Adjust the size as needed
  scale_color_brewer(palette = "Set1") + # Optional: to use a color palette for the points
  labs(y = "Coefficient of Variation (%)", x = "Replicate", title = "Coefficient of Variation by Replicate") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) # To prevent overlap in replicate labels

