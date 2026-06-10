# Define the directory path
directory_path <- "C:/Users/rbmahbub/Documents/RProjects/AmerifluxDataSubmission_LandscapeFlux/Data/LAI/Way3"

# List all CSV files in the directory
csv_files <- list.files(path = directory_path, pattern = "*.csv", full.names = TRUE)

# Read all CSV files into a list of data frames
way3lai <- lapply(csv_files, read.csv)

# Loop through each data frame in the list and standardize the date format
for (i in seq_along(way3lai)) {
  # Convert the date column to a consistent format (YYYY-MM-DD)
  way3lai[[i]]$date <- as.Date(parse_date_time(way3lai[[i]]$date, orders = c("ymd", "mdy")))
}
# Loop through each data frame in the list and print the first row
for (i in seq_along(way3lai)) {
  cat("First row of", names(way3lai)[i], ":\n")
  print(way3lai[[i]][1, ])
  cat("\n")  # Add a newline for better readability
}

# Define the directory path for Way4
directory_path_way4 <- "C:/Users/rbmahbub/Documents/RProjects/AmerifluxDataSubmission_LandscapeFlux/Data/LAI/Way4"

# List all CSV files in the Way4 directory
csv_files_way4 <- list.files(path = directory_path_way4, pattern = "*.csv", full.names = TRUE)

# Read all CSV files into a list of data frames
way4lai <- lapply(csv_files_way4, read.csv)

# Load the lubridate package for date parsing
library(lubridate)

# Loop through each data frame in the list and standardize the date format
for (i in seq_along(way4lai)) {
  # Convert the date column to a consistent format (YYYY-MM-DD)
  way4lai[[i]]$date <- as.Date(parse_date_time(way4lai[[i]]$date, orders = c("ymd", "mdy")))
}

# Loop through each data frame in the list and print the first row
for (i in seq_along(way4lai)) {
  cat("First row of", basename(csv_files_way4[i]), ":\n")  # Use basename to show only the file name
  print(way4lai[[i]][1, ])
  cat("\n")  # Add a newline for better readability
}

