library(ggplot2)
library(dplyr)
library(plotrix)
library(rstatix)
library(ggpubr)
library(svglite)

extract_components <- function(path) {
  # Define the options for each category
  category1 <- c("Control", "Tubacin", "5z-7-oxo")
  category2 <- c("DMSO-control", "Taxol", "Colchicine", "Nocodazole")
  
  # Initialize variables to store results
  found_category1 <- NULL
  found_category2 <- NULL
  
  # Check if any of the category1 options are in the path
  for (item in category1) {
    if (grepl(item, path)) {
      found_category1 <- item
      break
    }
  }
  
  # Check if any of the category2 options are in the path
  for (item in category2) {
    if (grepl(item, path)) {
      found_category2 <- item
      break
    }
  }
  
  # Return the results as a list
  return(list(Experiment = found_category1, Treatment = found_category2))
}


#Set working directory and list folders
# setwd("/projects/b1042/GoyalLab/Keerthana/MoiseEtAl/MaalvikaReorganisedData/extractedData/figure5/parameterResults/MTinhibitors")
folders <- list.files("/projects/b1042/GoyalLab/Keerthana/MoiseEtAl/MaalvikaReorganisedData/extractedData/figure5/parameterResults/MTinhibitors/")
df_comb <- data.frame()
root <- "/projects/b1042/GoyalLab/Keerthana/MoiseEtAl/MaalvikaReorganisedData/extractedData/figure5/parameterResults/MTinhibitors/"

# Read all top-level folders in the root directory
folders <- list.dirs(root, full.names = TRUE, recursive = FALSE)

# Iterate over each folder
for (folder in folders) {
  # Get sub-folders within each top-level folder
  sub_folders <- list.dirs(folder, full.names = TRUE, recursive = FALSE)
  
  # Iterate over each sub-folder
  for (sub_folder in sub_folders) {
    # Check if you need to do any specific filtering or additional steps
    # Listing all files with full paths within sub_folder, recursively
    files <- list.files(sub_folder, full.names = TRUE, recursive = TRUE)
    files <- files[grepl(".csv", files)]
    
    #Read files in each folder
    for (j in files) {
      
      df <- read.csv(paste0(j), row.names = 1)
      
      #Remove cells which are <20% of largest cell
      df <- df[which(df$major_axis_length>0.2*max(df$major_axis_length)),]
      
      #Remove cells with eccentricity>10
      df <- df[which(df$major_axis_length/df$minor_axis_length < 10),]
      
      #Convert radians to degrees
      df$orientation <- 180*df$orientation/pi
      
      #Shift range from -90/+90 to -45/+135
      df$orientation[df$orientation < (-45)] = 180+ df$orientation[ df$orientation< (-45)]
      
      #Normalize orientation against median
      df$orientation <- df$orientation - median(df$orientation)
      
      #Assign labels for condition
      if(grepl("static",j)){
        condition <- "Static"
      }else{ condition <- "Flow"}
      labels <- extract_components(j)
      if(nrow(df)>0){
        #Combine samples
        df_comb <- rbind(data.frame("Image"= j, "Eccentricity" = df$major_axis_length/df$minor_axis_length,
                                    "Orientation" = df$orientation,
                                    "Treatment" =labels$Treatment, "Condition"=  condition,
                                    "Experiment" =labels$Experiment), df_comb)
      }
    }
    # setwd("../../")
  }
}


df_comb <- df_comb[df_comb$Eccentricity <10,]
df_comb$Treatment <- factor(df_comb$Treatment, levels = c("DMSO-control","Taxol","Colchicine", "Nocodazole"), 
                            labels = c("CON", "TAX", "COL", "NOC"))
df_comb$Experiment <- factor(df_comb$Experiment, levels = c("Control", "Tubacin","5z-7-oxo"))
df_comb$Condition <- factor(df_comb$Condition, levels = c("Static", "Flow"))
df_main <- df_comb
write_csv(df_main, "/projects/b1042/GoyalLab/Keerthana/MoiseEtAl/plotData/Fig5_data_22TL_all.csv")
