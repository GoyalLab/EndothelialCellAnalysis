########################################################################################
#Description: To subsample data from csv files from previously segmened and analysed data
#Author: Keerthana
#Version: 1
#Quantification for Fig. 1 of Moise et al., 2022
#Created on: 18th April 2024
########################################################################################

library(ggplot2)
library(dplyr)
library(plotrix)
library(rstatix)
library(ggpubr)
library(svglite)
library(glue)

df_comb <- data.frame(
  Image = integer(),
  Eccentricity = numeric(),
  Orientation = numeric(),
  Treatment = character(),
  Condition = character(),
  stringsAsFactors = FALSE  # Avoid factors if not necessary
)

csv_path <- list.files(path = "~/Keerthana/MoiseEtAl/extractedData/selectedImages/figure1/2024_03_31_20Tl_mtinhib_20x", pattern = "\\.csv$", recursive = TRUE, full.names = TRUE)
#This is 200 for figure 1 and 140 for figure 5
subsampledNumber = 140
df_comb = read_csv("/projects/b1042/GoyalLab/Keerthana/MoiseEtAl/plotData/Fig5_data_22TL_all.csv")
subsampled_data <- df_comb %>%
  group_by(Experiment, Treatment, Condition) %>%  # Group data by Treatment and Condition
  slice_sample(n = subsampledNumber) %>%  # Subsample up to 200 observations per group
  ungroup() 
write.csv(subsampled_data, "~/Keerthana/MoiseEtAl/plotData/Fig5_subsampled_data_22T_allL.csv")


#Calculate coefficient of Variation
s1 <- read.csv("/projects/b1042/GoyalLab/Keerthana/MoiseEtAl/plotData/Fig6_SM9_RadialPlot_aTAT1KO_Flow.csv")
s2 <- read.csv("/projects/b1042/GoyalLab/Keerthana/MoiseEtAl/plotData/Fig6_SM9_RadialPlot_aTAT1KO_Static.csv")
s3 <- read.csv("/projects/b1042/GoyalLab/Keerthana/MoiseEtAl/plotData/Fig6_SM9_RadialPlot_Control_Flow.csv")
s4 <- read.csv("/projects/b1042/GoyalLab/Keerthana/MoiseEtAl/plotData/Fig6_SM9_RadialPlot_Control_Static.csv")
s5 <- read.csv("/projects/b1042/GoyalLab/Keerthana/MoiseEtAl/plotData/Fig6_SM9_RadialPlot_HDAC6KO_Flow.csv")
s6 <- read.csv("/projects/b1042/GoyalLab/Keerthana/MoiseEtAl/plotData/Fig6_SM9_RadialPlot_HDAC6KO_Static.csv")
s <- rbind(s1,s2,s3,s4,s5,s6)
cv_df <- s %>%
  group_by(Treatment, Condition) %>%
  summarise(
    mean = mean(Orientation, na.rm = TRUE),
    sd = sd(Orientation, na.rm = TRUE),
    cv = (sd / mean) * 100  # CV as a percentage
  )
write_csv(cv_df, "/projects/b1042/GoyalLab/Keerthana/MoiseEtAl/plotData/Fig6_RadialPlot_CV.csv")
