########################################################################################
#Description: Histograms and plotting eccentricity for confluent cells
#Author: Keerthana Arun
#Version: 1
#Modified by Keerthana Arun on 30 July 2024
########################################################################################
library(ggplot2)
library(dplyr)
library(plotrix)
library(rstatix)
library(ggpubr)
library(svglite)


#Set working directory and list folders
setwd("~/Keerthana/MoiseEtAl/extractedData/selectedImages/resubmission") 

convert_to_csv_path <- function(image_path) {
  csv_path <- gsub("/Images/", "/Parameters/", image_path)  # Change directory from Images to Parameters
  csv_path <- sub("\\.png$", ".csv", csv_path)              # Change extension from .png to .csv
  return(csv_path)
}

image_paths <- list.files(path = image_path, pattern = "\\.png$", recursive = TRUE, full.names = TRUE)
# image_paths <- list.files(path = "/home/mzo5929/Keerthana/MoiseEtAl/extractedData/selectedImages/figure5/2024_03_29_wmj_doubletub_20x/", pattern = "\\.png$", recursive = TRUE, full.names = TRUE)
# csv_paths <- sapply(image_paths, convert_to_csv_path)
csv_paths <- list.files(path = "/Volumes/fsmresfiles/Basic_Sciences/CDB/GoyalLab/People/KeerthanaArun/ArispeLab/MoisesEtAl/extractedData/selectedImages/resubmission/", pattern = "\\.csv$", recursive = TRUE, full.names = TRUE)
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
    # matches <- regexec("/WMJ ([^/]+) ([^/]+)/", j)
    # labels <- regmatches(j, matches)
    
    # Apply transformations and cleanups
    df <- df %>%
    filter(major_axis_length > 0.2 * max(major_axis_length),
            major_axis_length/minor_axis_length < 10) %>%
    mutate(
        orientation = 180 * orientation / pi,
        orientation = if_else(orientation < -45, 180 + orientation, orientation),
        orientation = orientation - median(orientation, na.rm = TRUE)
    )
    condition <- ifelse(grepl("subconfluent", j, ignore.case = TRUE), "subconfluent", "confluent")
    time <- ifelse(grepl("static", j, ignore.case = TRUE), "static", ifelse(grepl("48h", j, ignore.case = TRUE), "48h", ifelse(grepl("24h", j, ignore.case = TRUE), "24h", "8h")))
    # Determine condition based on filename
    # condition <- ifelse(grepl("static", j, ignore.case = TRUE), "Static", "Flow")
    
    # Bind to combined dataframe
    df_comb <- rbind(df_comb, data.frame(
    Image = j,
    Eccentricity = df$major_axis_length / df$minor_axis_length,
    Orientation = df$orientation,
    Condition = condition,
    TimePoint = time
    ))
} else {
    warning(paste("File does not exist:", j))
}
}

df_comb <- df_comb[df_comb$Eccentricity <10,] #Remove samples with eccentricity >10
df_main <- df_comb
write.csv(df_comb, "/Volumes/fsmresfiles/Basic_Sciences/CDB/GoyalLab/People/KeerthanaArun/ArispeLab/MoisesEtAl/plotData/Resubmission.csv")
#Function for summarizing number of samples in each column
n_fun <- function(x){
return(data.frame(y = lab_height, label = paste0("n = ",length(x))))
}
lab_height = 7
set.seed(100)
set.seed(150)
subsampled_data <- df_comb %>%
group_by(TimePoint, Condition) %>%  # Group data by Treatment and Condition
slice_sample(n = 100) %>%  # Subsample up to 200 observations per group
ungroup() 
write.csv(subsampled_data, "/Volumes/fsmresfiles/Basic_Sciences/CDB/GoyalLab/People/KeerthanaArun/ArispeLab/MoisesEtAl/plotData/ResubmissionSubsampled.csv")
subsampled_data <- read.csv("/Volumes/fsmresfiles/Basic_Sciences/CDB/GoyalLab/People/KeerthanaArun/ArispeLab/MoisesEtAl/plotData/ResubmissionSubsampled.csv")
# First, define the order you want for TimePoint
timepoint_order <- c("static", "8h", "24h", "48h")  # Adjust this to match your actual time points

# Convert TimePoint to a factor with the specified order
subsampled_data$TimePoint <- factor(subsampled_data$TimePoint, levels = timepoint_order)


stat.test <- subsampled_data %>% ungroup() %>%
              group_by(TimePoint) %>%
              wilcox_test(Eccentricity~Condition)%>%
            add_significance()

stat.test <- stat.test %>% add_xy_position(x = "TimePoint")

stat.test$y.position[stat.test$y.position>10] <- 10

plot <- ggplot(subsampled_data, aes(x=TimePoint, y=Eccentricity, color=Condition)) +
  geom_point(position=position_jitterdodge(jitter.width=0.2, dodge.width=0.8), alpha=0.5) +
  geom_boxplot(alpha=0, outlier.shape=NA, 
               position=position_dodge2(width=0.8, preserve="single"), 
               width=0.7) +  # Uniform width set
  scale_color_manual(values=c("grey", "red")) +
  theme_classic(base_size=10) +
  theme(
    axis.line=element_line(colour='black', size=1.5),
    axis.ticks=element_line(colour="black", size=1.5),
    text=element_text(size=10, family="Helvetica"),
    legend.title=element_blank(),
    strip.background=element_blank(),
    strip.placement="outside"
  ) +
  ylab("Elongation Factor")

print(plot)
ggsave(plot, file = paste0("/Volumes/fsmresfiles/Basic_Sciences/CDB/GoyalLab/People/KeerthanaArun/ArispeLab/MoisesEtAl/Plots/Eccentricity_Confluency.svg"), width = 13.7, height =13.7)

#Make scale 0 to 180
subsampled_data$Orientation <- 90+subsampled_data$Orientation

#Radial plot

svglite(paste0("/Volumes/fsmresfiles/Basic_Sciences/CDB/GoyalLab/People/KeerthanaArun/ArispeLab/MoisesEtAl/Plots/ResubmissionRadialPlots.svg"), height = 20, width = 10)
par(mfrow=c(4,2))

testlen <- c(0,4.6)
testpos<- c(0,360)

timepoint_48h <- c("timepoint_48h")
for (i in timepoint_order) {
for (j in unique(subsampled_data$Condition)) {
    polar.plot(testlen, testpos,main=paste(i, j),
            line.col=1,rp.type = "p", start = 90, labels = c(0,90,180, 270),
            label.pos = c(0,pi/2, pi, 3*pi/2), grid.col ="black", show.radial.grid = F,point.symbols=NA,
            show.grid.labels=0, show.centroid = F, radial.lim =4) 
    n <- min(c(100, sum(subsampled_data$TimePoint==i&subsampled_data$Condition==j)))
    add.line <- subsampled_data[subsampled_data$TimePoint==i&subsampled_data$Condition==j,]
    add.line <- add.line$Orientation
    add.line <- c(add.line, add.line + 180)
    pos <- add.line*pi/180
    if(j == "confluent"){
    segments(0, 0, cos(pos), sin(pos),
            col = "grey")
    }
    if(j == "subconfluent"){
    segments(0, 0, cos(pos), sin(pos),
            col = "red")
    }
    
}
mtext(text = paste("Confluency" ), side = 3, outer = TRUE, line = -3, cex = 1.5)

}

dev.off()

cv_df <- subsampled_data %>%
  group_by(TimePoint, Condition) %>%
  summarise(
    mean = mean(Orientation, na.rm = TRUE),
    sd = sd(Orientation, na.rm = TRUE),
    cv = (sd / mean)   # CV as a percentage
  )
write.csv(cv_df, "/Volumes/fsmresfiles/Basic_Sciences/CDB/GoyalLab/People/KeerthanaArun/ArispeLab/MoisesEtAl/plotData/ResubmissionSubsampled_coefficientOfVariation.csv")
