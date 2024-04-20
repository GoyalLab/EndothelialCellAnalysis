########################################################################################
#Description: Histograms and plotting eccentricity during MT inhibition
#Author: Maalavika Pillai
#Version: 1
#Includes MT inhibitors under static and Flow conditions
#Quantification for Fig. 1 of Moise et al., 2022
#Created on: 17th November 2022
#Added number of cells (n) on 18th January,2022
#Modified by Keerthana Arun on April 11, 2024
########################################################################################

library(ggplot2)
library(dplyr)
library(plotrix)
library(rstatix)
library(ggpubr)
library(svglite)

#Set working directory and list folders
setwd("~/Keerthana/MoiseEtAl/extractedData/selectedImages") 

convert_to_csv_path <- function(image_path) {
  csv_path <- gsub("/Images/", "/Parameters/", image_path)  # Change directory from Images to Parameters
  csv_path <- sub("\\.png$", ".csv", csv_path)              # Change extension from .png to .csv
  return(csv_path)
}
image_paths <- list.files(path = "~/Keerthana/MoiseEtAl/extractedData/selectedImages/figure1/2024_03_31_20Tl_mtinhib_20x", pattern = "\\.png$", recursive = TRUE, full.names = TRUE)
csv_paths <- sapply(image_paths, convert_to_csv_path)

df_comb <- data.frame(
  Image = integer(),
  Eccentricity = numeric(),
  Orientation = numeric(),
  Treatment = character(),
  Condition = character(),
  stringsAsFactors = FALSE  # Avoid factors if not necessary
)
CoefVar <- function(x){sd(x)/mean(x)}

for (j in csv_paths) {
  if (file.exists(j)) {
    df <- read.csv(j, row.names = 1)
    labels <- regmatches(j, regexec("/20TL (\\w+) (\\w+)/", j))
    
    # Apply transformations and cleanups
    df <- df %>%
      filter(major_axis_length > 0.2 * max(major_axis_length),
             major_axis_length/minor_axis_length < 10) %>%
      mutate(
        orientation = 180 * orientation / pi,
        orientation = if_else(orientation < -45, 180 + orientation, orientation),
        orientation = orientation - median(orientation, na.rm = TRUE)
      )
    
    # Determine condition based on filename
    condition <- ifelse(grepl("static", j, ignore.case = TRUE), "Static", "Flow")
    
    # Bind to combined dataframe
    df_comb <- rbind(df_comb, data.frame(
      Image = j,
      Eccentricity = df$major_axis_length / df$minor_axis_length,
      Orientation = df$orientation,
      Treatment = labels[[1]][2],
      Condition = condition
    ))
  } else {
    warning(paste("File does not exist:", j))
  }
}

df_comb <- df_comb[df_comb$Eccentricity <10,] #Remove samples with eccentricity >10
df_comb$Condition <- factor(df_comb$Condition, levels = c("Static", "Flow"))
df_comb$Treatment <- factor(df_comb$Treatment, levels = unique(df_comb$Treatment))
df_main <- df_comb
write.csv(df_comb, "~/Keerthana/MoiseEtAl/plotData/MTinhibitors_data_20TL.csv")


#Function for summarizing number of samples in each column
n_fun <- function(x){
  return(data.frame(y = lab_height, label = paste0("n = ",length(x))))
}

lab_height = 7
set.seed(100)
set.seed(150)

subsampled_data <- df_comb %>%
  group_by(Treatment, Condition) %>%  # Group data by Treatment and Condition
  slice_sample(n = 200) %>%  # Subsample up to 200 observations per group
  ungroup() 
write.csv(subsampled_data, "~/Keerthana/MoiseEtAl/plotData/MTinhibitors_subsampled_data_20TL.csv")
# stat.test <- subsampled_data %>% 
#   ungroup() %>%
#   group_by(Treatment) %>%
#   wilcox_test(Eccentricity~Condition)%>%
#   add_significance()
# stat.test <- stat.test %>% add_xy_position(x = "Treatment")
# stat.test$y.position[stat.test$y.position>10] <- 10

plot <- ggplot(subsampled_data, aes(x=Treatment, y=Eccentricity, color=Condition)) +
  geom_point(position=position_jitterdodge(dodge.width=1), alpha=0.5) +
  geom_boxplot(alpha=0, outlier.shape=NA, position=position_dodge(1)) +
  # stat_summary(fun.data=n_fun, geom="text", position=position_dodge(1), size=3) +  # Adjusted text size here
  # stat_pvalue_manual(stat.test, tip.length=0.02, bracket.nudge.y=0, linewidth=1.5) +
  scale_color_manual(values=c("grey", "red")) +
  # Smaller base size for all text elements
  theme(
    axis.line=element_line(colour='black', size=1.5),
    axis.ticks=element_line(colour="black", size=1.5),
    text=element_text(size=18, family="Helvetica"),  # Consistent text size for all textual elements
    legend.title=element_blank(),
    strip.background=element_blank(),
    strip.placement="outside"
  ) +
  theme_classic(base_size=18) + 
  ylab("Elongation Factor")  # Adjust axis label text size if needed

# Print the plot
print(plot)
ggsave(plot, file = paste0("/projects/b1042/GoyalLab/Keerthana/MoiseEtAl/plot/Eccentricity_MTinhibitors_20TL.svg"), width = 13.7, height =13.7)

#Radial plot
testlen <- c(0,4)
testpos<- c(0,360)
#Make scale 0 to 180
df_comb$Orientation <- 90+df_comb$Orientation

svglite(paste0("/projects/b1042/GoyalLab/Keerthana/MoiseEtAl/plot/RadialOrientation_MTinhibitors_20TL.svg"), height = 20, width = 10)

par(mfrow=c(4,2))
for (i in unique(df_comb$Treatment)) {
  for (j in unique(df_comb$Condition)) {
    polar.plot(testlen, testpos, main=paste0(i, ": ", j),
               line.col=1, rp.type="p", start=90,
               grid.col="black", show.radial.grid=F, point.symbols=NA,
               show.grid.labels=0, show.centroid=F, radial.lim=1.0, # Slightly extended limit
               cex.main=3.5, labels = (""))
    
    # Manually add labels for degrees
    label_radius = 1.15  # Adjust this to place labels further from center
    angles_deg <- c(0, 90, 180, 270)
    angles_rad <- angles_deg * pi / 180
    labels <- c("0", "90", "180", "270")
    text(x=label_radius * cos(angles_rad), y=label_radius * sin(angles_rad), labels=labels, cex=1.4)
    
    # Process subsets and add lines
    n <- min(c(100, sum(df_comb$Treatment == i & df_comb$Condition == j)))
    subsetted_df <- df_comb[df_comb$Treatment == i & df_comb$Condition == j,]
    if(n > 0) {
      subsetted_df <- subsetted_df[sample(nrow(subsetted_df), n),]
    }
    add.line <- subsetted_df$Orientation
    add.line <- c(add.line, add.line + 180)
    pos <- add.line * pi / 180
    if (j == "Static") {
      segments(0, 0, cos(pos), sin(pos), col = "grey")
    }
    if (j == "Flow") {
      segments(0, 0, cos(pos), sin(pos), col = "red")
    }
  }
  mtext("20TL", side = 3, outer = TRUE, line = -3, cex = 1.5)
}

dev.off()


