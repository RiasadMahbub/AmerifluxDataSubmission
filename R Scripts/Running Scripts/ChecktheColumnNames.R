library(tidyverse)
library(dplyr)
library(lubridate)

# Function to process each dataset (for TIMESTAMP and derived columns)
process_data <- function(data) {
  # Create TIMESTAMP_START and TIMESTAMP_END columns
  data <- cbind(TIMESTAMP_START = NA, TIMESTAMP_END = NA, data)
  
  # Convert TIMESTAMP column to POSIXct format
  data$TIMESTAMP <- ymd_hms(data$TIMESTAMP)
  
  # Create TIMESTAMP_START in the desired format
  data$TIMESTAMP_START <- format(data$TIMESTAMP, "%Y%m%d%H%M")
  
  # Create TIMESTAMP_END by adding 30 minutes to TIMESTAMP and formatting it
  data$TIMESTAMP_END <- format(data$TIMESTAMP + minutes(30), "%Y%m%d%H%M")
  
  # Create additional columns: HOUR, MONTH, DAY_OF_YEAR
  data$HOUR <- hour(data$TIMESTAMP)
  data$MONTH <- month(data$TIMESTAMP)
  data$DOY <- yday(data$TIMESTAMP)
  
  return(data)
}


####################################
####Check the required columns
######################################
# Define the directories

# Define the directories
way3_directory <- "C:/Users/rbmahbub/Documents/RProjects/AmerifluxDataSubmission_LandscapeFlux/Data/Way3"
way4_directory <- "C:/Users/rbmahbub/Documents/RProjects/AmerifluxDataSubmission_LandscapeFlux/Data/Way4"

# Define the files to read
way3_files_to_read <- c("Way3 2018.csv", "Way3 2019.csv", "Way3 2020.csv", "Way3 2021.csv", "Way3 2022.csv", "Way3 2023.csv", "Way3 2024.csv")
way4_files_to_read <- c("Way4 2018.csv", "Way4 2019.csv", "Way4 2020.csv", "Way4 2021.csv", "Way4 2022.csv", "Way4 2023.csv", "Way4 2024.csv")

# Read and process Way3 and Way4 data
way3_data <- lapply(way3_files_to_read, function(filename) {
  file_path <- file.path(way3_directory, filename)
  data <- read.csv(file_path)
  #apply_column_mapping(data) # Apply column name mapping
})

way4_data <- lapply(way4_files_to_read, function(filename) {
  file_path <- file.path(way4_directory, filename)
  data <- read.csv(file_path)
  #apply_column_mapping(data) # Apply column name mapping
})

# Mapping of equivalent column names
column_mapping <- list(
  "x_70_" = c("x_70."),
  "x_90_" = c("x_90."),
  "co2_flux" = c("NEE"),
  "RH" = c("rH"),
  "wind_dir" = c("WD_EC"),
  "X_z_d__L" = c("X.z.d..L"),
  "air_temperature" = c("Tair"),
  "SW_IN_Avg" = c( "Rg")  # Multiple possible names
)
# Function to standardize column names
standardize_column_names <- function(data, column_mapping) {
  # Rename columns based on the mapping
  for (standard_name in names(column_mapping)) {
    equivalent_name <- column_mapping[[standard_name]]
    if (equivalent_name %in% names(data)) {
      names(data)[names(data) == equivalent_name] <- standard_name
    }
  }
  return(data)
}

# Standardize and process the data for Way3 and Way4
way3_data <- lapply(way3_data, standardize_column_names, column_mapping)
way4_data <- lapply(way4_data, standardize_column_names, column_mapping)

# Apply the processing function to all datasets in Way3 and Way4
way3_processed_data <- lapply(way3_data, process_data)
way4_processed_data <- lapply(way4_data, process_data)

# List of required columns
required_columns <- c(
  "TIMESTAMP_START", "TIMESTAMP_END", "x_70_", "x_90_", "x_peak", "ch4_mole_fraction", 
  "ch4_mixing_ratio", "co2_mole_fraction", "co2_mixing_ratio", "co2_flux", "ch4_flux", 
  "h2o_mole_fraction", "h2o_mixing_ratio", "h2o_flux", "H", "LE", "H_strg", "LE_strg", 
  "air_pressure", "RH", "sonic_temperature", "qc_co2_flux", "qc_ch4_flux", "qc_H", 
  "qc_LE", "qc_Tau", "co2_var", "co2_strg", "ch4_strg", "u_var", "v_var", "w_var", 
  "wind_dir", "wind_speed", "max_wind_speed", "X_z_d__L", "air_temperature", "VPD", 
  "LW_IN_Avg", "LW_OUT_Avg", "PAR_IN_Avg", "PAR_OUT_Avg", "SW_IN_Avg", "SW_OUT_Avg", 
  "SWC_1_1_1", "L", "Tau"
)

# Function to check for missing columns and print file names
check_missing_columns <- function(data, required_columns, filename) {
  missing_columns <- setdiff(required_columns, names(data))
  if (length(missing_columns) == 0) {
    cat("The file", filename, "has all the required columns.\n")
  } else {
    cat("The file", filename, "is missing the following columns:\n")
    print(missing_columns)
  }
}

# Check Way3 files for missing columns
for (i in seq_along(way3_processed_data)) {
  check_missing_columns(way3_processed_data[[i]], required_columns, way3_files[i])
}

# Check Way4 files for missing columns
for (i in seq_along(way4_processed_data)) {
  check_missing_columns(way4_processed_data[[i]], required_columns, way4_files[i])
}


####Check NULL and NOn NULL columns 

# Define the directories
way3_directory <- "C:/Users/rbmahbub/Documents/RProjects/AmerifluxDataSubmission_LandscapeFlux/Data/Way3"
way4_directory <- "C:/Users/rbmahbub/Documents/RProjects/AmerifluxDataSubmission_LandscapeFlux/Data/Way4"

# Define the files to read
way3_files_to_read <- c("Way3 2018.csv", "Way3 2019.csv", "Way3 2020.csv", "Way3 2021.csv", "Way3 2023.csv", "Way3 2024.csv")
way4_files_to_read <- c("Way4 2018.csv", "Way4 2019.csv", "Way4 2020.csv", "Way4 2021.csv", "Way4 2023.csv", "Way4 2024.csv")

# Read and process Way3 and Way4 data
way3_data <- lapply(way3_files_to_read, function(filename) {
  file_path <- file.path(way3_directory, filename)
  data <- read.csv(file_path)
  #apply_column_mapping(data) # Apply column name mapping
})

way4_data <- lapply(way4_files_to_read, function(filename) {
  file_path <- file.path(way4_directory, filename)
  data <- read.csv(file_path)
  #apply_column_mapping(data) # Apply column name mapping
})


# Print all columns starting with "SWC" from the first six datasets
# Print column names that start with "SWC" from the first six datasets
lapply(way3_data[1:6], function(df) {
  swc_columns <- names(df)[grep("^WTD", names(df))]
  print(swc_columns)
})
# Identify columns that are completely NA and those that have some data, including dataframe index
lapply(seq_along(way3_data[1:7]), function(i) {
  df <- way3_data[[i]]
  
  # Select columns starting with "lvl"
  t_columns <- names(df)[grep("^Lvl", names(df))]
  
  if (length(t_columns) > 0) {  # Check if any columns start with "lvl"
    # Subset the data frame to just those columns
    df_subset <- df[, t_columns, drop = FALSE]  # Ensure we retain the dataframe structure
    
    # Fully NA columns
    na_columns <- t_columns[sapply(df_subset, function(col) all(is.na(col)))] 
    
    # Columns with some data
    data_columns <- t_columns[sapply(df_subset, function(col) any(!is.na(col)))]  
    
    cat("Dataset index:", i, "\n")  # Print dataset index
    cat("Fully NA columns:\n")
    print(na_columns)
    
    cat("Columns with some data:\n")
    print(data_columns)
    cat("\n----------------------\n")  # Separator for readability
  } else {
    cat("Dataset index:", i, "\nNo 'lvl' columns found.\n")
  }
})



# Identify columns that are completely NA and those that have some data, including dataframe index
lapply(seq_along(way4_data[1:7]), function(i) {
  df <- way4_data[[i]]
  
  # Select columns starting with "lvl"
  t_columns <- names(df)[grep("^WTD", names(df))]
  
  if (length(t_columns) > 0) {  # Check if any columns start with "lvl"
    # Subset the data frame to just those columns
    df_subset <- df[, t_columns, drop = FALSE]  # Ensure we retain the dataframe structure
    
    # Fully NA columns
    na_columns <- t_columns[sapply(df_subset, function(col) all(is.na(col)))] 
    
    # Columns with some data
    data_columns <- t_columns[sapply(df_subset, function(col) any(!is.na(col)))]  
    
    cat("Dataset index:", i, "\n")  # Print dataset index
    cat("Fully NA columns:\n")
    print(na_columns)
    
    cat("Columns with some data:\n")
    print(data_columns)
    cat("\n----------------------\n")  # Separator for readability
  } else {
    cat("Dataset index:", i, "\nNo 'lvl' columns found.\n")
  }
})



# Identify columns that are completely NA and those that have some data, including dataframe index
lapply(seq_along(way4_data[1:7]), function(i) {
  df <- way4_data[[i]]
  
  # Select columns starting with "WTD"
  t_columns <- names(df)[grep("^WTD", names(df))]
  
  if (length(t_columns) > 0) {  # Check if any columns start with "WTD"
    # Subset the data frame to just those columns
    df_subset <- df[, t_columns, drop = FALSE]  # Ensure we retain the dataframe structure
    
    # Fully NA columns
    na_columns <- t_columns[sapply(df_subset, function(col) all(is.na(col)))] 
    
    # Columns with some data
    data_columns <- t_columns[sapply(df_subset, function(col) any(!is.na(col)))]  
    
    # Print dataset index and columns information
    cat("Dataset index:", i, "\n")  # Print dataset index
    cat("Fully NA columns:\n")
    print(na_columns)
    
    cat("Columns with some data:\n")
    print(data_columns)
    
    # Calculate and print the percentage of data present for each WTD column
    cat("Percentage of data present in each WTD column:\n")
    for (col in t_columns) {
      total_values <- length(df[[col]])  # Total number of rows in the column
      non_na_values <- sum(!is.na(df[[col]]))  # Count of non-NA values
      percentage <- (non_na_values / total_values) * 100  # Calculate percentage
      cat(col, ": ", round(percentage, 2), "% data present\n")
    }
    
    cat("\n----------------------\n")  # Separator for readability
  } else {
    cat("Dataset index:", i, "\nNo 'WTD' columns found.\n")
  }
})


summary(as.numeric(way4_data[[7]]$WTD_Avg))

