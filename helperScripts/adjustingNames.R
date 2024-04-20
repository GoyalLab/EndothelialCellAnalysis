# Set the working directory to the folder containing the CSV files
setwd("/projects/b1042/GoyalLab/Keerthana/MoiseEtAl/plotData/")

# Define the replacements as a named vector
# Ensure more specific terms are ordered before more general terms if there's overlap
replacements <- c("10nm Taxol" = "TAX",
                  "100nm Nocodazole" = "NOC",
                  "10nm Colchicine" = "COL",
                  "DMSO-control" = "CON",
                  "Colchicine" = "COL",   # More specific should come first
                  "Colchi" = "COL",       # General or partial matches later
                  "Taxol" = "TAX",
                  "Nocodazole" = "NOC",
                  "Noco" = "NOC",
                  "Tub" = "Tubacin",
                  "5z-7-oxo" = "5z7",
                  "Control" = "CON")

columns_to_modify <- c("Treatment", "Experiment")  # Update these to your actual column names

# List all CSV files in the directory
file_list <- list.files(pattern = "\\.csv$", recursive = TRUE, full.names = TRUE)

for (file_name in file_list) {
  # Read the CSV file
  full_path <- file.path(getwd(), file_name) # Get the full path of the file
  data <- read.csv(full_path, stringsAsFactors = FALSE) # Ensure that strings are read in as characters
  
  # Apply replacements only to specified columns
  for (col in columns_to_modify) {
    if (col %in% names(data) && is.character(data[[col]])) {
      data[[col]] <- sapply(data[[col]], function(cell) {
        modified_cell <- cell
        for (key in names(replacements)) {
          # Use regex with word boundaries to ensure only whole words are replaced
          pattern <- paste0("\\b", key, "\\b")
          modified_cell <- gsub(pattern, replacements[key], modified_cell)
        }
        return(modified_cell)
      })
    }
  }
  
  # Write the modified data back to CSV
  write.csv(data, full_path, row.names = FALSE)
}

# Optional: Print completion message
print("All files have been processed.")
