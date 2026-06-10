# Install and load necessary libraries
library(readr)
library(readxl)
library(dplyr)
library(lubridate)
library(base)
library(ggplot2)
library(ggpmisc)

# names(way3_wtd_uni_t)
# way3_data[[1-7]] 2018, 2019, 2020, 2021, 2022, 2023, 2024 
# way4_data[[1-7]] 2018, 2019, 2020, 2021, 2022, 2023, 2024
# way3_data[[4]]$TIMESTAMP
# way3_wtd_uni_t$Way3_UnileverTowerStation_2021$TIMESTAMP
## 2020,2022, 2023 are in cm and 2021(way3) are in meters
## Convert all of them to meters
### Add Precipitation data from Way 4 to Way 3

# List of Excel files
excel_files <- c(
  "C:/Users/rbmahbub/Documents/RProjects/AmerifluxDataSubmission_LandscapeFlux/Data/InputLocalRawData/WTD/MasterFile_Way3_SoilProf_VWC.xlsx",
  "C:/Users/rbmahbub/Documents/RProjects/AmerifluxDataSubmission_LandscapeFlux/Data/InputLocalRawData/WTD/MasterFile_Way4SoilProfile.xlsx",
  "C:/Users/rbmahbub/Documents/RProjects/AmerifluxDataSubmission_LandscapeFlux/Data/InputLocalRawData/WTD/2021_MasterFile_Way3_SoilProf_All.xlsx",
  "C:/Users/rbmahbub/Documents/RProjects/AmerifluxDataSubmission_LandscapeFlux/Data/InputLocalRawData/WTD/2021_MasterFile_Way4SoilProfile.xlsx",
  "C:/Users/rbmahbub/Documents/RProjects/AmerifluxDataSubmission_LandscapeFlux/Data/InputLocalRawData/WTD/2022_Way4_SoilProfile_MasterFile.xlsx",
  "C:/Users/rbmahbub/Documents/RProjects/AmerifluxDataSubmission_LandscapeFlux/Data/InputLocalRawData/WTD/2022Way3SoilProfileMasterFile.xlsx",
  "C:/Users/rbmahbub/Documents/RProjects/AmerifluxDataSubmission_LandscapeFlux/Data/InputLocalRawData/WTD/2023Way3SoilProfileMasterFile copy.xlsx",
  "C:/Users/rbmahbub/Documents/RProjects/AmerifluxDataSubmission_LandscapeFlux/Data/InputLocalRawData/WTD/2023Way4_SoilProfile_MasterFile copy.xlsx"
)
# Create an empty list to store the data frames
data_list <- list()
# Loop through each file in the list
for (file in excel_files) {
  # Extract the base name of the file (without path and extension)
  file_name <- tools::file_path_sans_ext(basename(file))
  # Extract Way3/Way4 and Year from the file name
  if (grepl("Way3", file_name, ignore.case = TRUE)) {
    way <- "Way3"
  } else if (grepl("Way4", file_name, ignore.case = TRUE)) {
    way <- "Way4"
  } else {
    way <- "Unknown"
  }
  # Extract the year (assuming the year is always the first 4 digits)
  year <- substr(file_name, 1, 4)
  # Create the new name in the desired format
  df_name <- paste0(way, "_UnileverTowerStation_", year)
  # Read the Excel file
  data <- read_excel(file, skip =1)
  # Assign the data to a variable with the new name
  assign(df_name, data)
  # Store the data frame in the list
  data_list[[df_name]] <- data
  # Print the first few rows and structure of the data frame
  cat("File:", file_name, "\n")
  cat("Data Frame Name:", df_name, "\n")
  print(head(data))
  print(str(data))
  cat("\n")  # Add a newline for readability
}

# Update the names in data_list to reflect the changes
names(data_list) <- gsub("Mast", "2020", names(data_list))

# Loop through each data frame in data_list
for (df_name in names(data_list)) {
  data_list[[df_name]] <- data_list[[df_name]] %>%
    mutate(
      TIMESTAMP = as.POSIXct(
        as.numeric(TIMESTAMP) * 86400,  # Convert Excel serial date to seconds
        origin = "1899-12-30",          # Excel's origin date
        tz = "UTC"                       # Set time zone (adjust as needed)
      )
    )
  
  # Print confirmation message
  cat("Converted TIMESTAMP for:", df_name, "\n")
}

### 2021 wt_corr to wt_corr_AVG_cm_fixedBias and change Fixed to fixed
data_list$Way3_UnileverTowerStation_2021$wt_corr_AVG_cm_fixedBias <- data_list$Way3_UnileverTowerStation_2021$wt_corr
data_list$Way3_UnileverTowerStation_2022$wt_corr_AVG_cm_fixedBias <- data_list$Way3_UnileverTowerStation_2022$wt_corr_AVG_cm_FixedBias
data_list$Way4_UnileverTowerStation_2020$wt_corr_AVG_cm_fixedBias <- data_list$Way4_UnileverTowerStation_2020$wt_corr_AVG_cm_FixedBias

missing_columns <- sapply(data_list, function(df) {
  !"wt_corr_AVG_cm_fixedBias" %in% colnames(df)
})
column_variants <- sapply(data_list, function(df) {
  list(
    has_fixedBias = "wt_corr_AVG_cm_fixedBias" %in% colnames(df),
    has_FixedBias = "wt_corr_AVG_cm_FixedBias" %in% colnames(df)
  )
})

# Convert list to a dataframe for better readability
column_variants_df <- as.data.frame(do.call(rbind, column_variants))

# Print which datasets have each column va
filtered_data_list <- lapply(data_list, function(df) {
  df %>% select(TIMESTAMP, wt_corr_AVG_cm_fixedBias)
})

# Separate into Way3 and Way4
way3_wtd_uni_t <- filtered_data_list[grep("Way3", names(filtered_data_list))]
way4_wtd_uni_t <- filtered_data_list[grep("Way4", names(filtered_data_list))]


# Replace non-numeric values with NA (keeping the structure intact)
way3_wtd_uni_t$Way3_UnileverTowerStation_2021$wt_corr_AVG_cm_fixedBias[way3_wtd_uni_t$Way3_UnileverTowerStation_2021$wt_corr_AVG_cm_fixedBias %in% c("m", "Avg")] <- NA
# Convert to numeric
way3_wtd_uni_t$Way3_UnileverTowerStation_2021$wt_corr_AVG_cm_fixedBias <- as.numeric(
  way3_wtd_uni_t$Way3_UnileverTowerStation_2021$wt_corr_AVG_cm_fixedBias
)
# Round to 2 significant digits (NA values remain unchanged)
way3_wtd_uni_t$Way3_UnileverTowerStation_2021$wt_corr_AVG_cm_fixedBias <- signif(
  way3_wtd_uni_t$Way3_UnileverTowerStation_2021$wt_corr_AVG_cm_fixedBias, 2
)
way3_wtd_uni_t$Way3_UnileverTowerStation_2021$wt_corr_AVG_cm_fixedBias<-
  way3_wtd_uni_t$Way3_UnileverTowerStation_2021$wt_corr_AVG_cm_fixedBias*100

# Function to convert wt_corr_AVG_cm_fixedBias from cm to meters
convert_cm_to_m <- function(df) {
  if ("wt_corr_AVG_cm_fixedBias" %in% names(df)) {
    df$wt_corr_AVG_cm_fixedBias <- df$wt_corr_AVG_cm_fixedBias / 100
  }
  return(df)
}

# Apply to all data frames in way3_wtd_uni_t
way3_wtd_uni_t <- lapply(way3_wtd_uni_t, convert_cm_to_m)
# Apply to all data frames in way4_wtd_uni_t
way4_wtd_uni_t <- lapply(way4_wtd_uni_t, convert_cm_to_m)

########################################################################################################
##########################2020 and 2021 are in 5/15 minutes resolution 
# Function to aggregate data into 30-minute intervals
########################################################################################################s
aggregate_to_30min <- function(df, timestamp_col, value_cols) {
  df <- df %>%
    mutate(
      !!sym(timestamp_col) := as.POSIXct(!!sym(timestamp_col), format="%Y-%m-%d %H:%M:%S", tz="UTC"),
      Rounded_Time = as.POSIXct(format(!!sym(timestamp_col), "%Y-%m-%d %H:%M:00"), tz="UTC")
    ) %>%
    mutate(
      Rounded_Time = case_when(
        as.numeric(format(Rounded_Time, "%M")) < 30 ~ as.POSIXct(format(Rounded_Time, "%Y-%m-%d %H:00:00"), tz="UTC"),
        TRUE ~ as.POSIXct(format(Rounded_Time, "%Y-%m-%d %H:30:00"), tz="UTC")
      )
    ) %>%
    group_by(Rounded_Time) %>%
    summarise(across(all_of(value_cols), mean, na.rm = TRUE), .groups = "drop") %>%
    rename(TIMESTAMP = Rounded_Time)  # Rename column back to TIMESTAMP
  
  return(df)
}

way3_wtd_uni_t$Way3_UnileverTowerStation_2020 <- aggregate_to_30min(
  way3_wtd_uni_t$Way3_UnileverTowerStation_2020,
  timestamp_col = "TIMESTAMP",
  value_cols = c("wt_corr_AVG_cm_fixedBias")  # Replace with actual column names
)

way3_wtd_uni_t$Way3_UnileverTowerStation_2021 <- aggregate_to_30min(
  way3_wtd_uni_t$Way3_UnileverTowerStation_2021,
  timestamp_col = "TIMESTAMP",
  value_cols = c("wt_corr_AVG_cm_fixedBias")  # Replace with actual column names
)

plot(way3_wtd_uni_t$Way3_UnileverTowerStation_2020$TIMESTAMP, way3_wtd_uni_t$Way3_UnileverTowerStation_2020$wt_corr_AVG_cm_fixedBias)
plot(way3_wtd_uni_t$Way3_UnileverTowerStation_2021$TIMESTAMP, way3_wtd_uni_t$Way3_UnileverTowerStation_2021$wt_corr_AVG_cm_fixedBias)

# Function to check if timestamps are 30-minute intervals
check_time_interval <- function(df, timestamp_col) {
  df[[timestamp_col]] <- as.POSIXct(df[[timestamp_col]], format="%Y-%m-%d %H:%M:%S")  # Adjust format if necessary
  time_diff <- diff(df[[timestamp_col]])  # Compute time differences
  all(time_diff == 1800)  # 1800 seconds = 30 minutes
}

# Function to find the minimum time interval in a dataframe
find_min_interval <- function(df, timestamp_col) {
  df[[timestamp_col]] <- as.POSIXct(df[[timestamp_col]], format="%Y-%m-%d %H:%M:%S")  # Convert to POSIXct
  time_diff <- diff(df[[timestamp_col]])  # Compute time differences
  return(min(time_diff, na.rm = TRUE))  # Return the minimum interval
}

# Check each dataset in way3_wtd_uni_t
for (i in seq_along(way3_wtd_uni_t)) {
  cat("Checking Way3 WTD Data", i, ":", check_time_interval(way3_wtd_uni_t[[i]], "TIMESTAMP"), "\n")
  cat("Minimum interval in Way3 WTD Data", i, ":", find_min_interval(way3_wtd_uni_t[[i]], "TIMESTAMP"), "seconds\n")
}


# Define the output directory
output_dir <- "C:/Users/rbmahbub/Documents/RProjects/AmerifluxDataSubmission_LandscapeFlux/Data/InputLocalProcessedData/WTD"
# Function to save data frames as CSV
save_data_list <- function(data_list, output_dir) {
  for (df_name in names(data_list)) {
    # Extract the data frame
    data <- data_list[[df_name]]
    # Create the new file name
    if (grepl("Mast", df_name)) {
      new_file_name <- gsub("Mast", "2020", df_name)  # Replace "Mast" with "2020"
    } else {
      new_file_name <- df_name
    }
    # Add the .csv extension
    new_file_name <- paste0(new_file_name, ".csv")
    # Create the full file path
    file_path <- file.path(output_dir, new_file_name)
    # Save the data frame as a CSV file
    write.csv(data, file_path, row.names = FALSE)
    # Print a confirmation message
    cat("Saved:", file_path, "\n")
  }
}

# Save data frames from both lists
save_data_list(way3_wtd_uni_t, output_dir)
save_data_list(way4_wtd_uni_t, output_dir)



#----------------------------------------
# Define the directories
#----------------------------------------

way3_directory <- "C:/Users/rbmahbub/Documents/RProjects/AmerifluxDataSubmission_LandscapeFlux/Data/InputLocalRawData/Masterfiles/Way3"
way4_directory <- "C:/Users/rbmahbub/Documents/RProjects/AmerifluxDataSubmission_LandscapeFlux/Data/InputLocalRawData/Masterfiles/Way4"

# Define the files to read
way3_files_to_read <- c("Way3 2018.csv", "Way3 2019.csv", "Way3 2020.csv", "Way3 2021.csv", "Way3 2022.csv", "Way3 2023.csv", "Way3 2024.csv", "Way3 2025.csv")
way4_files_to_read <- c("Way4 2018.csv", "Way4 2019.csv", "Way4 2020.csv", "Way4 2021.csv", "Way4 2022.csv", "Way4 2023.csv", "Way4 2024.csv", "Way4 2025.csv")



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
way3_data[[8]] <- way3_data[[8]] %>%
  rename(TIMESTAMP = X)
way4_data[[8]] <- way4_data[[8]] %>%
  rename(TIMESTAMP = X)
###UNITS ROWS######
# Step 1: Extract unit row and store separately
unit_row <- way3_data[[6]][1, ] # Assuming the unit row is in the 6th list element
# Convert from µmol to nmol
unit_row$ch4_mixing_ratio <- "nmol mol-1"
column_mapping <- list(
  "x_70_" = c("x_70."),
  "x_90_" = c("x_90."),
  "co2_flux" = c("NEE"),
  "RH" = c("rH"),
  "wind_dir" = c("WD_EC"),
  "X_z_d__L" = c("X.z.d..L"),
  "air_temperature" = c("Tair"),
  "u_" = c("ustar"),
  "SW_IN_Avg" = c( "Rg")  
)
# Function to apply column name mapping
apply_column_mapping <- function(data) {
  for (old_name in names(column_mapping)) {
    new_name <- column_mapping[[old_name]]
    if (new_name %in% names(data) && !old_name %in% names(data)) {
      names(data)[names(data) == new_name] <- old_name
    }
  }
  return(data)
}
# Apply column mapping Way3 and Way4 data
way3_data <- lapply(way3_data, apply_column_mapping)
way4_data <- lapply(way4_data, apply_column_mapping)
unit_row<-apply_column_mapping(unit_row)
#############
# Windspeed of way 4 in the year 2021 will be replaced by biomet data###
#####This analysis was found in the graphs plotting biomet and aenonometer wind speed
way4_data[[4]]$wind_speed <- way4_data[[4]]$WS_Avg

# Add an NA column for P_RAIN_Tot in way3_data
for (i in seq_along(way3_data)) {
  way3_data[[i]]$P_RAIN_Tot <- NA
}

# Filter and rename
filtered_columns <- c("TIMESTAMP", "TIMESTAMP_START", "TIMESTAMP_END", "x_70_", "x_90_", "x_peak", 
                      "ch4_mole_fraction", "ch4_mixing_ratio", "co2_mole_fraction", "co2_mixing_ratio", 
                      "co2_flux", "ch4_flux", "h2o_mole_fraction", "h2o_mixing_ratio", "h2o_flux", 
                      "H", "LE", "H_strg", "LE_strg", "air_pressure", "RH", "sonic_temperature", 
                      "qc_co2_flux", "qc_ch4_flux", "qc_H", "qc_LE", "qc_Tau", "co2_var", "co2_strg", 
                      "ch4_strg", "u_var", "v_var", "w_var", "wind_dir", "wind_speed", "max_wind_speed", 
                      "X_z_d__L", "air_temperature", "VPD", "LW_IN_T_Corr_Avg", "LW_OUT_T_Corr_Avg", "PAR_IN_Avg", 
                      "PAR_OUT_Avg", "SW_IN_Avg", "SW_OUT_Avg", "SWC_2_1_1_Avg", "L", "Tau", "TS_mean.2.", "u_", 
                      "Lvl_m_Avg", "WS_Avg", "P_RAIN_Tot", "rssi_77_mean", "rand_err_ch4_flux", 
                      "co2_signal_strength_7500_mean", "rand_err_Tau", "rand_err_H", "rand_err_LE",
                      "rand_err_co2_flux", "rand_err_h2o_flux", "rand_err_ch4_flux")

# Filter `unit_row` to include only the columns in `filtered_columns`
filtered_unit_row <- unit_row[names(unit_row) %in% filtered_columns]

# View the filtered unit row
print(filtered_unit_row)
#### Get rid of the unit rows for 2022 to 2024####
way3_data[[5]] <- way3_data[[5]][-1, ]
way3_data[[6]] <- way3_data[[6]][-1, ]
way3_data[[7]] <- way3_data[[7]][-1, ]
way3_data[[8]] <- way3_data[[8]][-1, ]

way4_data[[5]] <- way4_data[[5]][-1, ]
way4_data[[6]] <- way4_data[[6]][-1, ]
way4_data[[7]] <- way4_data[[7]][-1, ]
way4_data[[8]] <- way4_data[[8]][-1, ]

##### 2022, 2023, and 2024 has -9999 instead of NaN
# Function to replace -9999 with NaN in specific columns
replace_9999_with_NaN <- function(df) {
  # Identify columns that are not "TIMESTAMP"
  cols_to_modify <- colnames(df)[colnames(df) != "TIMESTAMP"]
  # Apply ifelse only to the relevant columns
  df[cols_to_modify] <- lapply(df[cols_to_modify], function(x) {
    ifelse(x %in% c(-9999, "-9999", "-9999.0"), NaN, x)
  })
  return(df)
}

# Apply the function to the 5th, 6th, and 7th elements of way3_data and way4_data
way3_data[[5]] <- replace_9999_with_NaN(way3_data[[5]])
way3_data[[6]] <- replace_9999_with_NaN(way3_data[[6]])
way3_data[[7]] <- replace_9999_with_NaN(way3_data[[7]])
way3_data[[8]] <- replace_9999_with_NaN(way3_data[[8]])
way4_data[[5]] <- replace_9999_with_NaN(way4_data[[5]])
way4_data[[6]] <- replace_9999_with_NaN(way4_data[[6]])
way4_data[[7]] <- replace_9999_with_NaN(way4_data[[7]])
way4_data[[8]] <- replace_9999_with_NaN(way4_data[[8]])

# Print the first row of the TIMESTAMP column
cat("First row of TIMESTAMP column (way3_data[[5]]):", way3_data[[5]]$TIMESTAMP[1], "\n")
cat("First row of TIMESTAMP column (way3_data[[6]]):", way3_data[[6]]$TIMESTAMP[1], "\n")
cat("First row of TIMESTAMP column (way3_data[[7]]):", way3_data[[7]]$TIMESTAMP[1], "\n")
cat("First row of TIMESTAMP column (way3_data[[8]]):", way3_data[[8]]$TIMESTAMP[1], "\n")

cat("First row of TIMESTAMP column (way4_data[[5]]):", way4_data[[5]]$TIMESTAMP[1], "\n")
cat("First row of TIMESTAMP column (way4_data[[6]]):", way4_data[[6]]$TIMESTAMP[1], "\n")
cat("First row of TIMESTAMP column (way4_data[[7]]):", way4_data[[7]]$TIMESTAMP[1], "\n")
cat("First row of TIMESTAMP column (way4_data[[8]]):", way4_data[[8]]$TIMESTAMP[1], "\n")

# # Convert TIMESTAMP to a consistent format
# way4_data[[5]]$TIMESTAMP <- as.character(mdy_hm(way4_data[[5]]$TIMESTAMP))

# Print the first row to verify the change
cat("First row of TIMESTAMP column (way4_data[[5]]):", way4_data[[5]]$TIMESTAMP[1], "\n")
# Function to check and convert TIMESTAMP
convert_timestamp <- function(timestamp) {
  # If the TIMESTAMP is already in the correct format, return it as is
  if (grepl("^\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}$", timestamp)) {
    return(timestamp)
  } else {
    # Otherwise, attempt to convert using mdy_hm
    return(as.character(mdy_hm(timestamp)))
  }
}

# Apply the conversion to each TIMESTAMP column
way3_data[[5]]$TIMESTAMP <- sapply(way3_data[[5]]$TIMESTAMP, convert_timestamp)
way3_data[[6]]$TIMESTAMP <- sapply(way3_data[[6]]$TIMESTAMP, convert_timestamp)
way4_data[[5]]$TIMESTAMP <- sapply(way4_data[[5]]$TIMESTAMP, convert_timestamp)
way4_data[[6]]$TIMESTAMP <- sapply(way4_data[[6]]$TIMESTAMP, convert_timestamp)

# Print the first rows to verify the changes
cat("First row of TIMESTAMP column (way3_data[[5]]):", way3_data[[5]]$TIMESTAMP[1], "\n")
cat("First row of TIMESTAMP column (way3_data[[6]]):", way3_data[[6]]$TIMESTAMP[1], "\n")
cat("First row of TIMESTAMP column (way4_data[[5]]):", way4_data[[5]]$TIMESTAMP[1], "\n")
cat("First row of TIMESTAMP column (way4_data[[6]]):", way4_data[[6]]$TIMESTAMP[1], "\n")

##############################################################################
# Function to check if timestamps are 30-minute intervals
##############################################################################
check_time_interval <- function(df, timestamp_col) {
  df[[timestamp_col]] <- as.POSIXct(df[[timestamp_col]], format="%Y-%m-%d %H:%M:%S")  # Adjust format if necessary
  time_diff <- diff(df[[timestamp_col]])  # Compute time differences
  all(time_diff == 1800)  # 1800 seconds = 30 minutes
}

# Function to find the minimum time interval in a dataframe
find_min_interval <- function(df, timestamp_col) {
  df[[timestamp_col]] <- as.POSIXct(df[[timestamp_col]], format="%Y-%m-%d %H:%M:%S")  # Convert to POSIXct
  time_diff <- diff(df[[timestamp_col]])  # Compute time differences
  return(min(time_diff, na.rm = TRUE))  # Return the minimum interval
}

# Check each dataset in way3_wtd_uni_t
for (i in seq_along(way3_wtd_uni_t)) {
  cat("Checking Way3 WTD Data", i, ":", check_time_interval(way3_wtd_uni_t[[i]], "TIMESTAMP"), "\n")
  cat("Minimum interval in Way3 WTD Data", i, ":", find_min_interval(way3_wtd_uni_t[[i]], "TIMESTAMP"), "minutes\n")
}

# Function to find rows where a specific time difference occurs
find_problematic_rows <- function(df, timestamp_col, target_diff) {
  df[[timestamp_col]] <- as.POSIXct(df[[timestamp_col]], format="%Y-%m-%d %H:%M:%S")  # Convert to POSIXct
  time_diff <- diff(df[[timestamp_col]])  # Compute time differences
  problem_indices <- which(time_diff == target_diff)  # Find indices where time difference matches target
  
  if (length(problem_indices) > 0) {
    return(df[problem_indices:(problem_indices + 1), ])  # Return the problematic rows
  } else {
    return(NULL)  # Return NULL if no issues found
  }
}

# Find and print problematic rows in Way3 WTD Data 1 (5 seconds)
cat("\nRows with 5-second interval in Way3 WTD Data 1:\n")
print(find_problematic_rows(way3_wtd_uni_t[[1]], "TIMESTAMP", 5))

# Find and print problematic rows in Way3 WTD Data 2 (-55 seconds)
cat("\nRows with -55-second interval in Way3 WTD Data 2:\n")
print(find_problematic_rows(way3_wtd_uni_t[[2]], "TIMESTAMP", -55))

merge_p_rain_tot <- function(way3_data, way4_data) {
  for (i in seq_along(way3_data)) {
    if ("P_RAIN_Tot" %in% colnames(way3_data[[i]])) {
      way3_data[[i]]$P_RAIN_Tot <- NULL
    }
    way3_data[[i]] <- merge(way3_data[[i]], way4_data[[i]][, c("TIMESTAMP", "P_RAIN_Tot")], 
                            by = "TIMESTAMP", all.x = TRUE)
  }
  return(way3_data)
}
way3_data <- merge_p_rain_tot(way3_data, way4_data)

#############################################################################
##########################MERGING WtD With WAY 3 and 4 #####################
############################################################################
# Function to merge corresponding way3_wtd_uni_t and way3_data dataframes
merge_dataframes <- function(wtd_df, data_df) {
  # Convert TIMESTAMP to POSIXct (handling time zone differences)
  wtd_df <- wtd_df %>% 
    mutate(TIMESTAMP = ymd_hms(TIMESTAMP, tz = "UTC"))
  data_df <- data_df %>% 
    mutate(TIMESTAMP = ymd_hms(TIMESTAMP))
  # Perform full join to keep all records
  merged_df <- full_join(data_df, wtd_df, by = "TIMESTAMP")
  return(merged_df)
}

# Loop through the matching years
merged_list <- list()
years <- c(2020, 2021, 2022, 2023)

for (i in seq_along(years)) {
  year <- years[i]
  wtd_name <- paste0("Way3_UnileverTowerStation_", year)
  if (wtd_name %in% names(way3_wtd_uni_t)) {
    way3_data[[i+2]] <- merge_dataframes(way3_wtd_uni_t[[wtd_name]], way3_data[[i + 2]])  # Adjust index
  }
}


for (i in seq_along(years)) {
  year <- years[i]
  wtd_name <- paste0("Way3_UnileverTowerStation_", year)
  if (wtd_name %in% names(way4_wtd_uni_t)) {
    way4_data[[i+2]] <- merge_dataframes(way4_wtd_uni_t[[wtd_name]], way4_data[[i + 2]])  # Adjust index
  }
}


###Add wt_corr_AVG_cm_fixedBias to the dataframes of way 3
# Loop through each dataframe in the list
for (i in seq_along(way3_data)) {
  # Check if the column exists in the current dataframe
  if (!"wt_corr_AVG_cm_fixedBias" %in% colnames(way3_data[[i]])) {
    # Add the column with NA values
    way3_data[[i]]$wt_corr_AVG_cm_fixedBias <- NA
    print(paste("Added 'wt_corr_AVG_cm_fixedBias' column with NA values to dataframe", i))
  } else {
    print(paste("Column 'wt_corr_AVG_cm_fixedBias' is already present in dataframe", i))
  }
}

###Add wt_corr_AVG_cm_fixedBias to the dataframes of way 4
# Loop through each dataframe in the list
for (i in seq_along(way4_data)) {
  # Check if the column exists in the current dataframe
  if (!"wt_corr_AVG_cm_fixedBias" %in% colnames(way4_data[[i]])) {
    # Add the column with NA values
    way4_data[[i]]$wt_corr_AVG_cm_fixedBias <- NA
    print(paste("Added 'wt_corr_AVG_cm_fixedBias' column with NA values to dataframe", i))
  } else {
    print(paste("Column 'wt_corr_AVG_cm_fixedBias' is already present in dataframe", i))
  }
}

#################################################################
###############Adding WTD, Canopy Height, and LAI units #########
###################################################################
# Add the new column "wt_corr_AVG_cm_fixedBias" with unit "[m]" to filtered_unit_row
filtered_unit_row$wt_corr_AVG_cm_fixedBias <- "[m]"
filtered_unit_row$canopy_height <- "[m]"
filtered_unit_row$LAI_corrected<- "[m^2/m^2]"
filtered_unit_row$LAI_corrected_gapfilled<- "[m^2/m^2]"
filtered_unit_row$canopy_height_gapfilled <- "[m]"
filtered_unit_row$P_RAIN_Tot <- "[mm]"

# Verify the updated filtered_unit_row
print(filtered_unit_row)


#############################################################################
##########################MERGING CanopyHeight With WAY 3 and 4 #####################
############################################################################
###Data comes from the script LAICanopyHeight.R script
# Function to merge two data frames by TIMESTAMP
merge_pairwise_safe <- function(df1, df2 = NULL) {
  df1 <- df1 %>% mutate(TIMESTAMP = ymd_hms(TIMESTAMP, tz = "UTC"))
  if (!is.null(df2)) {
    df2 <- df2 %>% mutate(TIMESTAMP = ymd_hms(TIMESTAMP, tz = "UTC"))
    merged_df <- full_join(df1, df2, by = "TIMESTAMP")
  } else {
    # add missing columns as NA
    df1 <- df1 %>%
      mutate(
        LAI_corrected = NA_real_,
        canopy_height = NA_real_,
        LAI_corrected_gapfilled = NA_real_,
        canopy_height_gapfilled = NA_real_
      )
    merged_df <- df1
  }
  return(merged_df)
}


# Pairing them manually (adjust the year accordingly)
way3_data[[1]] <- merge_pairwise_safe(way3_data[[1]], Way3CHLAIlist$Way32018CHLAI_processed)
way3_data[[2]] <- merge_pairwise_safe(way3_data[[2]], Way3CHLAIlist$Way32019CHLAI_processed)
way3_data[[3]] <- merge_pairwise_safe(way3_data[[3]], Way3CHLAIlist$Way32020CHLAI_processed)
way3_data[[4]] <- merge_pairwise_safe(way3_data[[4]], Way3CHLAIlist$Way32021CHLAI_processed)
way3_data[[5]] <- merge_pairwise_safe(way3_data[[5]], Way3CHLAIlist$Way32022CHLAI_processed)
way3_data[[6]] <- merge_pairwise_safe(way3_data[[6]], Way3CHLAIlist$Way32023CHLAI_processed)
way3_data[[7]] <- merge_pairwise_safe(way3_data[[7]], Way3CHLAIlist$Way32024CHLAI_processed)
# 2025 missing
way3_data[[8]] <- merge_pairwise_safe(way3_data[[8]])

way4_data[[1]] <- merge_pairwise_safe(way4_data[[1]], Way4CHLAIlist$Way42018CHLAI_processed)
way4_data[[2]] <- merge_pairwise_safe(way4_data[[2]], Way4CHLAIlist$Way42019CHLAI_processed)
way4_data[[3]] <- merge_pairwise_safe(way4_data[[3]], Way4CHLAIlist$Way42020CHLAI_processed)
way4_data[[4]] <- merge_pairwise_safe(way4_data[[4]], Way4CHLAIlist$Way42021CHLAI_processed)
way4_data[[5]] <- merge_pairwise_safe(way4_data[[5]], Way4CHLAIlist$Way42022CHLAI_processed)
way4_data[[6]] <- merge_pairwise_safe(way4_data[[6]], Way4CHLAIlist$Way42023CHLAI_processed)
way4_data[[7]] <- merge_pairwise_safe(way4_data[[7]], Way4CHLAIlist$Way42024CHLAI_processed)
# 2025 missing
way4_data[[8]] <- merge_pairwise_safe(way4_data[[8]])


# Function to convert air pressure from Pa to kPa
convert_air_pressure_to_kpa <- function(df) {
  # Check if "air_pressure" column exists and convert it to kPa
  if ("air_pressure" %in% colnames(df)) {
    df <- df %>% mutate(air_pressure = air_pressure / 1000)
  }
  return(df)
}

# Apply the conversion to way3-data [[1]] to [[4]] (2018 to 2021 data)
way3_data[[1]] <- convert_air_pressure_to_kpa(way3_data[[1]])
way3_data[[2]] <- convert_air_pressure_to_kpa(way3_data[[2]])
way3_data[[3]] <- convert_air_pressure_to_kpa(way3_data[[3]])
way3_data[[4]] <- convert_air_pressure_to_kpa(way3_data[[4]])

way4_data[[1]] <- convert_air_pressure_to_kpa(way4_data[[1]])
way4_data[[2]] <- convert_air_pressure_to_kpa(way4_data[[2]])
way4_data[[3]] <- convert_air_pressure_to_kpa(way4_data[[3]])
way4_data[[4]] <- convert_air_pressure_to_kpa(way4_data[[4]])

#######LE DATA###############
#######2021################

plot(way3_data[[4]]$TIMESTAMP, way3_data[[4]]$LE)
# Specify the file path
file_path <- "C:/Users/rbmahbub/Documents/RProjects/AmerifluxDataSubmission_LandscapeFlux/Data/InputLocalRawData/LE2021/eddypro_LE_Fix_2023_12_06_full_output_2023-12-06T144759_adv.csv"

# Read the CSV file
# Skip the first row (header) and use the second row as the new header
LEdata2021 <- read.csv(file_path, skip = 1, header = TRUE)
# Combine the 'date' and 'time' columns into a single 'TIMESTAMP' column
LEdata2021$TIMESTAMP <- as.POSIXct(paste(LEdata2021$date, LEdata2021$time), format = "%Y-%m-%d %H:%M")
# Remove the first row from LEdata2021_filtered
LEdata2021 <- LEdata2021 %>%
  slice(-1)  # Removes the first row
# Select only the required columns from LEdata2021
LEdata2021 <- LEdata2021 %>%
  select(TIMESTAMP, LE_scf, LE_strg, LE, qc_LE, un_LE)
#### Does LE of way3_data[[1]] have the non-growing season data, we need that
way3_data[[4]]$TIMESTAMP
# # Drop the specific columns from way3_data[[4]]
# way3_data[[4]] <- way3_data[[4]] %>%
#   select(-LE_scf, -LE_strg, -LE, -qc_LE, -un_LE)
# Perform left join to bring in the summer data (from LEdata2021) to way3_data[[4]]
way3_data[[4]] <- way3_data[[4]] %>%
  left_join(LEdata2021, by = "TIMESTAMP")
# Replace summer values with those from LEdata2021 (if they exist)
way3_data[[4]] <- way3_data[[4]] %>%
  mutate(
    LE_scf = ifelse(!is.na(LE_scf.y), LE_scf.y, LE_scf.x), # Replace summer LE_scf
    LE_strg = ifelse(!is.na(LE_strg.y), LE_strg.y, LE_strg.x), # Replace summer LE_strg
    LE = ifelse(!is.na(LE.y), LE.y, LE.x), # Replace summer LE
    qc_LE = ifelse(!is.na(qc_LE.y), qc_LE.y, qc_LE.x), # Replace summer qc_LE
    un_LE = ifelse(!is.na(un_LE.y), un_LE.y, un_LE.x) # Replace summer un_LE
  ) %>%
  select(-ends_with(".x"), -ends_with(".y")) # Remove redundant columns

plot(way3_data[[4]]$TIMESTAMP, way3_data[[4]]$LE)
plot(LEdata2021$TIMESTAMP, LEdata2021$LE)



####################################
############LE2020##################
####################################


#####Rename Way 4 water table depth column WTD_Avg as Lvl_m_Avg
# Rename WTD_raw_Avg to Lvl_m_Avg in Way4 data
# Loop through each dataframe in way4_data and rename WTD_Avg to Lvl_m_Avg
way4_data <- lapply(way4_data, function(df) {
  # Drop any columns named "Lvl_m_Avg"
  df <- df[, !colnames(df) %in% "Lvl_m_Avg", drop = FALSE]
  # Rename "WTD_Avg" to "Lvl_m_Avg"
  colnames(df) <- gsub("WTD_Avg", "Lvl_m_Avg", colnames(df))
  return(df)
})

# Copy of original data before filtering by wind speed and ustar
way3_data_nfws <- way3_data
way4_data_nfws <- way4_data 
####################################
############for 2021 way 4 we will replace biomet ##################
####################################

##########################################
#######FIX USTAR WINDSPEED################
##########################################
# Function to get the correct column name dynamically
get_column_name <- function(df, possible_names) {
  return(possible_names[possible_names %in% colnames(df)][1])
}

# Initialize lists
way3_dfswindustar <- vector("list", length = 7)
way4_dfswindustar <- vector("list", length = 7)
way4_outliers <- vector("list", length = 7)  # Store outliers for each dataset

# Loop through both way3 and way4 data frames and select required columns with renaming
for (i in 1:7) {
  # Determine the correct column name for ustar dynamically
  way3_ustar_col <- get_column_name(way3_data[[i]], c("u_", "ustar"))
  way4_ustar_col <- get_column_name(way4_data[[i]], c("u_", "ustar"))
  # Process way3 data
  way3_dfswindustar[[i]] <- way3_data[[i]] %>%
    dplyr::select(
      TIMESTAMP,
      wind_speed_w3_sonic = wind_speed, 
      way3ustar = all_of(way3_ustar_col) # Select dynamically
    ) %>%
    mutate(across(c(wind_speed_w3_sonic, way3ustar), as.numeric))  # Convert to numeric
  way4_dfswindustar[[i]] <- way4_data[[i]] %>%  # Process way4 data
    dplyr::select(
      TIMESTAMP,
      wind_speed_w4_sonic = wind_speed,
      way4ustar = all_of(way4_ustar_col) # Select dynamically
    ) %>%
    mutate(across(c(wind_speed_w4_sonic, way4ustar), as.numeric))  # Convert to numeric
  # Remove rows with NA values before fitting the model
  clean_data <- way4_dfswindustar[[i]] %>%
    filter(!is.na(way4ustar) & !is.na(wind_speed_w4_sonic))
  # Fit a linear regression model for USTAR ~ WS in way4 (if data is sufficient)
  if (nrow(clean_data) > 1) {  # Ensure enough data points
    lm_model <- lm(way4ustar ~ wind_speed_w4_sonic, data = clean_data)
    # Predict values based on the regression model
    way4_dfswindustar[[i]]$predicted_ustar <- predict(lm_model, newdata = way4_dfswindustar[[i]])
    # Calculate residuals (absolute deviation from regression line)
    way4_dfswindustar[[i]]$residuals <- abs(way4_dfswindustar[[i]]$way4ustar - way4_dfswindustar[[i]]$predicted_ustar)
    threshold <- 4 * sd(way4_dfswindustar[[i]]$residuals, na.rm = TRUE)# Define a threshold for outliers (e.g., 4 standard deviations)
    way4_outliers[[i]] <- way4_dfswindustar[[i]] %>%# Identify outliers (points that deviate significantly)
      filter(residuals > threshold)# Print outliers for debugging
    print(way4_outliers[[i]])
  } else {
    message(paste("Skipping dataset", i, "due to insufficient non-NA data."))
  }
}

# Replace outlier rows with NA in the ustar column for way4_data[[1]], [[4]], [[6]], [[7]]
# NOTE: For indices 6 and 7, we increase the threshold from 4 to 4.25 SD because
# even after excluding points beyond 4 SD, the WS–u* relationship still failed  (21.8% of data flagged as outliers in second submission). This adjustment aims to reduce 
# the residual impact of extreme values that distort the regression.Replace outlier rows with NA in the ustar column for way4_data[[1]], [[4]], [[6]], [[7]]
outlier_indices <- c(1, 4, 6, 7)

for (i in outlier_indices) {
  threshold <- if (i %in% c(6, 7)) {
    3.75 * sd(way4_dfswindustar[[i]]$residuals, na.rm = TRUE)
  } else {
    4 * sd(way4_dfswindustar[[i]]$residuals, na.rm = TRUE)
  }
  way4_outliers[[i]] <- way4_dfswindustar[[i]] %>%
    filter(residuals > threshold)
  outlier_timestamps <- way4_outliers[[i]]$TIMESTAMP
  rows_to_na <- way4_data[[i]]$TIMESTAMP %in% outlier_timestamps
  # Get correct ustar column name again
  way4_ustar_col <- get_column_name(way4_data[[i]], c("u_", "ustar"))
  # Replace with NA safely using column name as a string
  way4_data[[i]][rows_to_na, way4_ustar_col] <- NA
}

# Initialize lists
way3_dfswindustar <- vector("list", length = 7)
way3_outliers <- vector("list", length = 7)

# Loop through way3 data frames and process
for (i in 1:7) {
  # Determine the correct column name for ustar dynamically
  way3_ustar_col <- get_column_name(way3_data[[i]], c("u_", "ustar"))
  
  # Process way3 data
  way3_dfswindustar[[i]] <- way3_data[[i]] %>%
    dplyr::select(
      TIMESTAMP,
      wind_speed_w3_sonic = wind_speed, 
      way3ustar = all_of(way3_ustar_col)
    ) %>%
    mutate(across(c(wind_speed_w3_sonic, way3ustar), as.numeric))
  
  # Remove rows with NA values before fitting the model
  clean_data <- way3_dfswindustar[[i]] %>%
    filter(!is.na(way3ustar) & !is.na(wind_speed_w3_sonic))
  
  # Fit a linear regression model for USTAR ~ WS in way3 (if data is sufficient)
  if (nrow(clean_data) > 1) {
    lm_model <- lm(way3ustar ~ wind_speed_w3_sonic, data = clean_data)
    
    # Predict values based on the regression model
    way3_dfswindustar[[i]]$predicted_ustar <- predict(lm_model, newdata = way3_dfswindustar[[i]])
    
    # Calculate residuals
    way3_dfswindustar[[i]]$residuals <- abs(way3_dfswindustar[[i]]$way3ustar - way3_dfswindustar[[i]]$predicted_ustar)
    
    # Identify outliers (standard 4×SD threshold)
    threshold <- 4 * sd(way3_dfswindustar[[i]]$residuals, na.rm = TRUE)
    way3_outliers[[i]] <- way3_dfswindustar[[i]] %>%
      filter(residuals > threshold)
    
    print(way3_outliers[[i]])
  } else {
    message(paste("Skipping Way3 dataset", i, "due to insufficient non-NA data."))
  }
}

# Replace outlier rows with NA in the ustar column for specific Way3 years
outlier_indices_w3 <- c(1, 6, 7)

for (i in outlier_indices_w3) {
  threshold <- if (i %in% c(6, 7)) {
    3.75 * sd(way3_dfswindustar[[i]]$residuals, na.rm = TRUE)
  } else {
    4 * sd(way3_dfswindustar[[i]]$residuals, na.rm = TRUE)
  }
  
  way3_outliers[[i]] <- way3_dfswindustar[[i]] %>%
    filter(residuals > threshold)
  
  outlier_timestamps <- way3_outliers[[i]]$TIMESTAMP
  rows_to_na <- way3_data[[i]]$TIMESTAMP %in% outlier_timestamps
  
  # Get correct ustar column name again
  way3_ustar_col <- get_column_name(way3_data[[i]], c("u_", "ustar"))
  
  # Replace with NA safely
  way3_data[[i]][rows_to_na, way3_ustar_col] <- NA
}



######################################
####Fix the Way3 2021 temperature#####
######################################
remove_air_temp_outliers <- function(df, threshold_factor = 1.7) {
  df$air_temperature <- as.numeric(df$air_temperature)  #Ensure numeric
  df$sonic_temperature <- as.numeric(df$sonic_temperature)  #Ensure numeric
  df$air_temperature_C <- df$air_temperature - 273.15  #Kelvin to Celsius
  df$sonic_temperature_C <- df$sonic_temperature - 273.15  #Kelvin to Celsius
  
  df$orig_index <- seq_len(nrow(df))  #Track original index
  filtered <- df %>% filter(!is.na(air_temperature_C), !is.na(sonic_temperature_C))  #Filter valid rows
  
  lm_model <- lm(sonic_temperature_C ~ air_temperature_C, data = filtered)
  filtered$predicted_sonic <- predict(lm_model, newdata = filtered)
  filtered$residuals <- filtered$sonic_temperature_C - filtered$predicted_sonic
  threshold <- threshold_factor * sd(filtered$residuals, na.rm = TRUE)
  outlier_indices <- filtered$orig_index[abs(filtered$residuals) > threshold]
  
  df$air_temperature[outlier_indices] <- NA  #Replace with NA in original dataframe
  return(df)
}

# Apply to your data
way3_data[[6]] <- remove_air_temp_outliers(way3_data[[6]])
# Function to check presence of column
check_ch4_column <- function(datalist) {
  sapply(seq_along(datalist), function(i) {
    "ch4_mole_fraction" %in% names(datalist[[i]])
  })
}

# Check both datasets
way3_has_ch4 <- check_ch4_column(way3_data)
way4_has_ch4 <- check_ch4_column(way4_data)

# Show indices where missing
which(!way3_has_ch4)
which(!way4_has_ch4)

##Change the unit mathematical of CH4
##The unit file is edited later
# Safe conversion function
convert_ch4 <- function(df) {
  if ("ch4_mole_fraction" %in% names(df)) {
    df$ch4_mole_fraction <- as.numeric(df$ch4_mole_fraction) * 1000
    df$ch4_flux <- as.numeric(df$ch4_flux) * 1000
    df$ch4_mixing_ratio <- as.numeric(df$ch4_mixing_ratio) * 1000
  }
  return(df)
}


# Apply to both datasets
way3_data <- lapply(way3_data, convert_ch4)
way4_data <- lapply(way4_data, convert_ch4)
max(way3_data[[1]]$ch4_flux, na.rm = TRUE)
max(way3_data_nfws[[1]]$ch4_flux, na.rm = TRUE)

hist(way4_data[[1]]$ch4_flux)
hist(way4_data_nfws[[1]]$ch4_flux)
#---------------------------------------------------
#WAY3[[7]] AIR temperature fix
#---------------------------------------------------
# Remove NA rows
way3_data[[7]]$air_temperature<- as.numeric(way3_data[[7]]$air_temperature)
way3_data[[7]]$sonic_temperature<- as.numeric(way3_data[[7]]$sonic_temperature)
df <- way3_data[[7]] %>%
  filter(!is.na(air_temperature), !is.na(sonic_temperature))
# Fit linear model
lm_model <- lm(sonic_temperature ~ air_temperature, data=df)
# Predicted values and residuals
df$predicted <- predict(lm_model, newdata=df)
df$residuals <- df$sonic_temperature - df$predicted
# Identify outliers
threshold <- 3 * sd(df$residuals, na.rm=TRUE)
df$outlier <- abs(df$residuals) > threshold
# Replace outliers with NA in original dataframe
outlier_index <- which(!is.na(way3_data[[7]]$air_temperature) & !is.na(way3_data[[7]]$sonic_temperature))
way3_data[[7]]$air_temperature[outlier_index[df$outlier]] <- NA
# Plot
ggplot(df, aes(x=air_temperature, y=sonic_temperature, color=outlier)) +
  geom_point(size=2, alpha=0.7) +
  geom_smooth(method="lm", se=FALSE, color="red", linetype="dashed") +
  labs(title="Way3 [[7]]: Air Temp vs Sonic Temp", x="Air Temperature", y="Sonic Temperature") +
  theme_minimal(base_size=10)


#---------------------------------------------------
#WAY3[[6]] AIR temperature fix
#---------------------------------------------------
# Remove NA rows
df <- way3_data[[6]] %>%
  filter(!is.na(air_temperature), !is.na(sonic_temperature))
# Fit linear model
lm_model <- lm(sonic_temperature ~ air_temperature, data=df)
# Predicted values and residuals
df$predicted <- predict(lm_model, newdata=df)
df$residuals <- df$sonic_temperature - df$predicted
# Identify outliers
threshold <- 1.8 * sd(df$residuals, na.rm=TRUE)
df$outlier <- abs(df$residuals) > threshold
# Replace outliers with NA in original dataframe
outlier_index <- which(!is.na(way3_data[[6]]$air_temperature) & !is.na(way3_data[[6]]$sonic_temperature))
way3_data[[6]]$air_temperature[outlier_index[df$outlier]] <- NA
# Plot
ggplot(df, aes(x=air_temperature, y=sonic_temperature, color=outlier)) +
  geom_point(size=2, alpha=0.7) +
  geom_smooth(method="lm", se=FALSE, color="red", linetype="dashed") +
  labs(title="Way3 [[6]]: Air Temp vs Sonic Temp", x="Air Temperature", y="Sonic Temperature") +
  theme_minimal(base_size=10)

#---------------------------------------------------
#WAY3[[5]] AIR temperature fix
#---------------------------------------------------
way3_data[[5]]$air_temperature <- as.numeric(way3_data[[5]]$air_temperature)
way3_data[[5]]$sonic_temperature <- as.numeric(way3_data[[5]]$sonic_temperature)
df <- way3_data[[5]] %>%
  filter(!is.na(air_temperature), !is.na(sonic_temperature))
# Convert columns to numeric (NA where conversion fails)

lm_model <- lm(sonic_temperature ~ air_temperature, data=df)
df$predicted <- predict(lm_model, newdata=df)
df$residuals <- df$sonic_temperature - df$predicted
threshold <- 1.7 * sd(df$residuals, na.rm=TRUE)
df$outlier <- abs(df$residuals) > threshold
outlier_index <- which(!is.na(way3_data[[5]]$air_temperature) & !is.na(way3_data[[5]]$sonic_temperature))
way3_data[[5]]$air_temperature[outlier_index[df$outlier]] <- NA
ggplot(df, aes(x=air_temperature, y=sonic_temperature, color=outlier)) +
  geom_point(size=2, alpha=0.7) +
  geom_smooth(method="lm", se=FALSE, color="red", linetype="dashed") +
  labs(title="Way3 [[5]]: Air Temp vs Sonic Temp", x="Air Temperature", y="Sonic Temperature") +
  theme_minimal(base_size=10)



#---------------------------------------------------
#WAY4[[7]] AIR temperature fix
#---------------------------------------------------
# Convert columns to numeric
way4_data[[7]]$air_temperature <- as.numeric(way4_data[[7]]$air_temperature)
way4_data[[7]]$sonic_temperature <- as.numeric(way4_data[[7]]$sonic_temperature)

# Remove NA rows
df <- way4_data[[7]] %>%
  filter(!is.na(air_temperature), !is.na(sonic_temperature))

# Fit linear model
lm_model <- lm(sonic_temperature ~ air_temperature, data=df)

# Predicted values and residuals
df$predicted <- predict(lm_model, newdata=df)
df$residuals <- df$sonic_temperature - df$predicted

# Identify outliers
threshold <- 2.5 * sd(df$residuals, na.rm=TRUE)
df$outlier <- abs(df$residuals) > threshold

# Replace outliers with NA in original dataframe
outlier_index <- which(!is.na(way4_data[[7]]$air_temperature) & !is.na(way4_data[[7]]$sonic_temperature))
way4_data[[7]]$air_temperature[outlier_index[df$outlier]] <- NA

# Plot
ggplot(df, aes(x=air_temperature, y=sonic_temperature, color=outlier)) +
  geom_point(size=2, alpha=0.7) +
  geom_smooth(method="lm", se=FALSE, color="red", linetype="dashed") +
  labs(title="Way4 [[7]]: Air Temp vs Sonic Temp", x="Air Temperature", y="Sonic Temperature") +
  theme_minimal(base_size=10)


#---------------------------------------------------
#WAY4[[6]] AIR temperature fix
#---------------------------------------------------
# Convert columns to numeric
way4_data[[6]]$air_temperature <- as.numeric(way4_data[[6]]$air_temperature)
way4_data[[6]]$sonic_temperature <- as.numeric(way4_data[[6]]$sonic_temperature)

# Remove NA rows
df <- way4_data[[6]] %>%
  filter(!is.na(air_temperature), !is.na(sonic_temperature))

# Fit linear model
lm_model <- lm(sonic_temperature ~ air_temperature, data=df)

# Predicted values and residuals
df$predicted <- predict(lm_model, newdata=df)
df$residuals <- df$sonic_temperature - df$predicted

# Identify outliers
threshold <- 1.5 * sd(df$residuals, na.rm=TRUE)
df$outlier <- abs(df$residuals) > threshold

# Replace outliers with NA in original dataframe
outlier_index <- which(!is.na(way4_data[[6]]$air_temperature) & !is.na(way4_data[[6]]$sonic_temperature))
way4_data[[6]]$air_temperature[outlier_index[df$outlier]] <- NA

# Plot
ggplot(df, aes(x=air_temperature, y=sonic_temperature, color=outlier)) +
  geom_point(size=2, alpha=0.7) +
  geom_smooth(method="lm", se=FALSE, color="red", linetype="dashed") +
  labs(title="Way4 [[6]]: Air Temp vs Sonic Temp", x="Air Temperature", y="Sonic Temperature") +
  theme_minimal(base_size=10)


#---------------------------------------------------
#WAY4[[5]] AIR temperature fix
#---------------------------------------------------
# Convert columns to numeric
way4_data[[5]]$air_temperature <- as.numeric(way4_data[[5]]$air_temperature)
way4_data[[5]]$sonic_temperature <- as.numeric(way4_data[[5]]$sonic_temperature)

# Remove NA rows
df <- way4_data[[5]] %>%
  filter(!is.na(air_temperature), !is.na(sonic_temperature))

# Fit linear model
lm_model <- lm(sonic_temperature ~ air_temperature, data=df)

# Predicted values and residuals
df$predicted <- predict(lm_model, newdata=df)
df$residuals <- df$sonic_temperature - df$predicted

# Identify outliers
threshold <- 5 * sd(df$residuals, na.rm=TRUE)
df$outlier <- abs(df$residuals) > threshold

# Replace outliers with NA in original dataframe
outlier_index <- which(!is.na(way4_data[[5]]$air_temperature) & !is.na(way4_data[[5]]$sonic_temperature))
way4_data[[5]]$air_temperature[outlier_index[df$outlier]] <- NA

# Plot
ggplot(df, aes(x=air_temperature, y=sonic_temperature, color=outlier)) +
  geom_point(size=2, alpha=0.7) +
  geom_smooth(method="lm", se=FALSE, color="red", linetype="dashed") +
  labs(title="Way4 [[5]]: Air Temp vs Sonic Temp", x="Air Temperature", y="Sonic Temperature") +
  theme_minimal(base_size=10)



#####Save the way3 data
# Directory path
output_dir <- "C:/Users/rbmahbub/Documents/RProjects/AmerifluxDataSubmission_LandscapeFlux/Data/InputLocalProcessedData/MasterFiles/Way3"
#Years corresponding to the dataframes
years <- c(2018, 2019, 2020, 2021, 2022, 2023, 2024, 2025)
# Save each dataframe as a CSV file
for (i in seq_along(way3_data)) {
  file_name <- paste0("way3_", years[i], ".csv")
  file_path <- file.path(output_dir, file_name)
  write.csv(way3_data[[i]], file_path, row.names = FALSE)  # Save without row indices
  print(paste("Saved", file_path))
}
print("All files saved successfully.")

#### Save the way4 data
# Directory path
output_dir <- "C:/Users/rbmahbub/Documents/RProjects/AmerifluxDataSubmission_LandscapeFlux/Data/InputLocalProcessedData/MasterFiles/Way4"
#Years corresponding to the dataframes
years <- c(2018, 2019, 2020, 2021, 2022, 2023, 2024, 2025)
# Save each dataframe as a CSV file
for (i in seq_along(way4_data)) {
  file_name <- paste0("way4_", years[i], ".csv")
  file_path <- file.path(output_dir, file_name)
  write.csv(way4_data[[i]], file_path, row.names = FALSE)  # Save without row indices
  print(paste("Saved", file_path))
}
print("All files saved successfully.")

#####UnitFiles####
# Define the directory path
output_dir <- "C:/Users/rbmahbub/Documents/RProjects/AmerifluxDataSubmission_LandscapeFlux/Data/InputLocalProcessedData/MasterFiles/unitdf"
# Replace unit for ch4_mixing_ratio
filtered_unit_row$ch4_mixing_ratio <- gsub("µmol", "nmol", filtered_unit_row$ch4_mixing_ratio)
filtered_unit_row$ch4_flux <- gsub("µmol", "nmol", filtered_unit_row$ch4_flux)
filtered_unit_row$ch4_mole_fraction <- gsub("µmol", "nmol", filtered_unit_row$ch4_mole_fraction)

# Save the filtered_unit_row dataframe as Unit_df.csv
file_path <- file.path(output_dir, "Unit_df.csv")
write.csv(filtered_unit_row, file_path, row.names = FALSE)

ncol(filtered_unit_row)
plot(way3_data[[7]]$TIMESTAMP, way3_data[[7]]$canopy_height_gapfilled)

plot(way3_data[[1]]$ch4_flux)
#+-------------------------------------------------------------------
#+-------------------------------------------------------------------
#+#+-------------------------------------------------------------------
#+END OF CODE THE ANALYSIS PART
#+
#+#+-------------------------------------------------------------------
#+-------------------------------------------------------------------
#+#+-------------------------------------------------------------------
#+
#+
#+
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Function to check if timestamps are 30-minute intervals
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
check_time_interval <- function(df, timestamp_col) {
  df[[timestamp_col]] <- as.POSIXct(df[[timestamp_col]], format="%Y-%m-%d %H:%M:%S")  # Adjust format if necessary
  time_diff <- diff(df[[timestamp_col]])  # Compute time differences
  all(time_diff == 1800)  # 1800 seconds = 30 minutes
}
# List of way3_data and corresponding CHLAI data frames
way3_dfs <- list(
  way3_data[[1]], way3_data[[2]], way3_data[[3]], 
  way3_data[[4]], way3_data[[5]], way3_data[[6]], way3_data[[7]]
)

# List of way4_data and corresponding CHLAI data frames
way4_dfs <- list(
  way4_data[[1]], way4_data[[2]], way4_data[[3]], 
  way4_data[[4]], way4_data[[5]], way4_data[[6]], way4_data[[7]]
)
way3_chlai_dfs <- list(
  Way3CHLAIlist$Way32018CHLAI_processed, 
  Way3CHLAIlist$Way32019CHLAI_processed,
  Way3CHLAIlist$Way32020CHLAI_processed, 
  Way3CHLAIlist$Way32021CHLAI_processed, 
  Way3CHLAIlist$Way32022CHLAI_processed, 
  Way3CHLAIlist$Way32023CHLAI_processed, 
  Way3CHLAIlist$Way32024CHLAI_processed
)
# Check each dataset
for (i in seq_along(way3_dfs)) {
  cat("Checking Way3 Data", i, ":", check_time_interval(way3_dfs[[i]], "TIMESTAMP"), "\n")
  cat("Checking Way3 CHLAI Data", i, ":", check_time_interval(way3_chlai_dfs[[i]], "TIMESTAMP"), "\n")
}

# Function to find the minimum time interval in a dataframe
find_min_interval <- function(df, timestamp_col) {
  df[[timestamp_col]] <- as.POSIXct(df[[timestamp_col]], format="%Y-%m-%d %H:%M:%S")  # Convert to POSIXct
  time_diff <- diff(df[[timestamp_col]])  # Compute time differences
  return(min(time_diff, na.rm = TRUE))  # Return the minimum interval
}

# Iterate over each dataset and find the minimum interval
for (i in seq_along(way3_dfs)) {
  cat("Minimum interval in Way3 Data", i, ":", find_min_interval(way3_dfs[[i]], "TIMESTAMP"), "minutes\n")
  cat("Minimum interval in Way3 CHLAI Data", i, ":", find_min_interval(way3_chlai_dfs[[i]], "TIMESTAMP"), "minutes\n")
}


# Function to check for duplicates in TIMESTAMP_START column
check_duplicates <- function(data, name) {
  # Ensure TIMESTAMP_START is numeric
  data$TIMESTAMP <- as.numeric(data$TIMESTAMP)
  # Check for duplicates in TIMESTAMP_START
  duplicate_timestamps <- which(duplicated(data$TIMESTAMP))

  # Print the results
  if (length(duplicate_timestamps) > 0) {
    cat("Duplicates found in", name, "at rows:", duplicate_timestamps, "\n")
    cat("Duplicate rows and their TIMESTAMP_START values:\n")
    
    # Print both the row and the duplicated value
    for (index in duplicate_timestamps) {
      cat("Row:", index, "- TIMESTAMP_START:", data$TIMESTAMP_START[index], "\n")
      print(data[index, ])  # Display the duplicate row
    }
  } else {
    cat("No duplicates found in", name, "\n")
  }
}

# Check for duplicates in Way3 data
for (i in seq_along(way3_dfs)) {
  check_duplicates(way3_dfs[[i]], names(way3_dfs)[i])
}

# Check for duplicates in Way4 data
for (i in seq_along(way4_dfs)) {
  check_duplicates(way4_dfs[[i]], names(way4_dfs)[i])
}

##############plot way 3 wind speed from Eddypro vs way 3 wind speed for ###########
#Wind speed column of eddypro: wind_speed; 
#Wind speed column of biomet: WS_Avg
#### Wind speed of anemometer of way 4 2021 has been changed by biomet wind speed
# Function to create and save scatter plot
plot_and_save <- function(df, year, site, save_dir) {
  # Convert columns to numeric
  df$wind_speed <- as.numeric(df$wind_speed)
  df$WS_Avg <- as.numeric(df$WS_Avg)
  
  # Ensure no NA values
  df <- df[!is.na(df$wind_speed) & !is.na(df$WS_Avg), ]
  
  # Create the plot
  p <- ggplot(df, aes(x = wind_speed, y = WS_Avg)) +
    geom_point(color = "blue", alpha = 0.5) +  # Scatter points
    geom_smooth(method = "lm", color = "red", formula = y ~ x) +  # Regression line
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +  # 1:1 line
    stat_poly_eq(aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
                 formula = y ~ x, parse = TRUE) +  # Regression equation and R2
    labs(
      x = expression("Sonic Anemometer Wind Speed "~ms^{-1}), 
      y = expression("Biomet Wind Speed "~ms^{-1}),
      title = paste("Wind Speed Comparison -", site, year)
    ) +
    theme_minimal()
  
  # Define file name
  file_name <- paste0(save_dir, "/", site, "_WindSpeed_", year, ".png")
  
  # Save plot
  ggsave(file_name, plot = p, width = 8, height = 6, dpi = 300)
}

# Define save directory
save_dir <- "C:/Users/rbmahbub/Documents/RProjects/AmerifluxDataSubmission_LandscapeFlux/Figure/WindSpeed"

# Apply function to all datasets
years <- 2018:2024  # Corresponding years
for (i in 1:7) {
  plot_and_save(way3_dfs[[i]], years[i], "Way3", save_dir)
  plot_and_save(way4_dfs[[i]], years[i], "Way4", save_dir)
}

# Initialize empty lists to store filtered data frames
way3_dfswind <- vector("list", length = 7)
way4_dfswind <- vector("list", length = 7)

# Loop through both way3 and way4 data frames and select required columns with renaming
for (i in 1:7) {
  # Process way3 data
  way3_dfswind[[i]] <- way3_dfs[[i]] %>%
    dplyr::select(
      TIMESTAMP,
      wind_speed_w3_sonic = wind_speed, # Wind speed column of eddypro: wind_speed
      WS_Avg_w3_biomet = WS_Avg       # Wind speed column of biomet: WS_Avg
    ) %>%
    # Filter values greater than 11.38 for WS_Avg_w3_biomet
    dplyr::filter(WS_Avg_w3_biomet < 11.38)
  
  # Process way4 data
  way4_dfswind[[i]] <- way4_dfs[[i]] %>%
    dplyr::select(
      TIMESTAMP,
      wind_speed_w4_sonic = wind_speed,
      WS_Avg_w4_biomet = WS_Avg
    )
}

# Initialize an empty list to store the merged data frames
w3w4_dfswind <- vector("list", length = 7)

# Loop through and merge each corresponding pair of data frames
for (i in 1:7) {
  w3w4_dfswind[[i]] <- dplyr::inner_join(
    way3_dfswind[[i]], 
    way4_dfswind[[i]], 
    by = "TIMESTAMP"
  )
}
# Remove NA values and then find the top 5 maximum values
top_5_max_values <- sort(na.omit(way3_dfswind[[1]]$WS_Avg_w3_biomet), decreasing = TRUE)[1:5]

# Display the top 5 maximum values
top_5_max_values

# Define the save directory for way3biomet vs way4biomet plots
save_dir1 <- "C:/Users/rbmahbub/Documents/RProjects/AmerifluxDataSubmission_LandscapeFlux/Figure/WindSpeed/way3biomet_way4biomet"

# Ensure the directory exists
if (!dir.exists(save_dir1)) dir.create(save_dir1, recursive = TRUE)

# Function to create and save scatter plot
plot_and_save <- function(df, year, site, save_dir, x_var, y_var, x_label, y_label) {
  # Convert columns to numeric
  df[[x_var]] <- as.numeric(df[[x_var]])
  df[[y_var]] <- as.numeric(df[[y_var]])
  
  # Remove NA values
  df <- df[!is.na(df[[x_var]]) & !is.na(df[[y_var]]), ]
  
  # Create the plot
  p <- ggplot(df, aes_string(x = x_var, y = y_var)) +
    geom_point(color = "blue", alpha = 0.5) +  # Scatter points
    geom_smooth(method = "lm", color = "red", formula = y ~ x) +  # Regression line
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +  # 1:1 line
    stat_poly_eq(aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
                 formula = y ~ x, parse = TRUE) +  # Regression equation and R2
    labs(
      x = x_label, 
      y = y_label,
      title = paste("Wind Speed Comparison -", site, year)
    ) +
    theme_minimal()
  # Define file name
  file_name <- paste0(save_dir, "/", site, "_WindSpeed_", year, ".png")
  
  # Save plot
  ggsave(file_name, plot = p, width = 8, height = 6, dpi = 300)
}

# Updated years range
years <- 2018:2024  

# Loop through the years and create the first set of plots
for (i in 1:7) {
  plot_and_save(
    df = w3w4_dfswind[[i]],
    year = years[i],
    site = "way3way4biomet",
    save_dir = save_dir1,
    x_var = "WS_Avg_w3_biomet",
    y_var = "WS_Avg_w4_biomet",
    x_label = expression("Way3 Biomet Wind Speed "~ms^{-1}),
    y_label = expression("Way4 Biomet Wind Speed "~ms^{-1})
  )
}



# Define save directory for way3biomet vs way4sonic
save_dir_sonic <- "C:/Users/rbmahbub/Documents/RProjects/AmerifluxDataSubmission_LandscapeFlux/Figure/WindSpeed/way3biomet_way4sonic"

# Ensure the directory exists
if (!dir.exists(save_dir_sonic)) dir.create(save_dir_sonic, recursive = TRUE)

# Function to create and save scatter plots
plot_and_save <- function(df, year, save_dir, x_var, y_var, x_label, y_label, site) {
  # Convert columns to numeric
  df[[x_var]] <- as.numeric(df[[x_var]])
  df[[y_var]] <- as.numeric(df[[y_var]])
  # Remove NA values
  df <- df[!is.na(df[[x_var]]) & !is.na(df[[y_var]]), ]
  # Create the plot
  p <- ggplot(df, aes_string(x = x_var, y = y_var)) +
    geom_point(color = "blue", alpha = 0.5) +  # Scatter points
    geom_smooth(method = "lm", color = "red", formula = y ~ x) +  # Regression line
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +  # 1:1 line
    stat_poly_eq(aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
                 formula = y ~ x, parse = TRUE) +  # Regression equation and R2
    labs(
      x = x_label, 
      y = y_label,
      title = paste("Wind Speed Comparison -", site, year)
    ) +
    theme_minimal()
  
  # Define file name
  file_name <- paste0(save_dir, "/", site, "_WindSpeed_", year, ".png")
  
  # Save plot
  ggsave(file_name, plot = p, width = 8, height = 6, dpi = 300)
}

# Updated years range
years <- 2018:2024  

# Loop through the years and create the scatter plots
for (i in 1:7) {
  plot_and_save(
    df = w3w4_dfswind[[i]],
    year = years[i],
    save_dir = save_dir_sonic,
    x_var = "WS_Avg_w3_biomet",
    y_var = "wind_speed_w4_sonic",
    x_label = expression("Way3 Biomet Wind Speed "~ms^{-1}),
    y_label = expression("Way4 Sonic Anemometer Wind Speed "~ms^{-1}),
    site = "way3biomet_way4sonic"
  )
}


# Define save directory for way3sonic vs way4biomet
save_dir_biomet <- "C:/Users/rbmahbub/Documents/RProjects/AmerifluxDataSubmission_LandscapeFlux/Figure/WindSpeed/way3sonic_way4biomet"
# Ensure the directory exists
if (!dir.exists(save_dir_biomet)) dir.create(save_dir_biomet, recursive = TRUE)

# Function to create and save scatter plots
plot_and_save <- function(df, year, save_dir, x_var, y_var, x_label, y_label, site) {
  # Convert columns to numeric
  df[[x_var]] <- as.numeric(df[[x_var]])
  df[[y_var]] <- as.numeric(df[[y_var]])
  
  # Remove NA values
  df <- df[!is.na(df[[x_var]]) & !is.na(df[[y_var]]), ]
  
  # Create the plot
  p <- ggplot(df, aes_string(x = x_var, y = y_var)) +
    geom_point(color = "blue", alpha = 0.5) +  # Scatter points
    geom_smooth(method = "lm", color = "red", formula = y ~ x) +  # Regression line
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +  # 1:1 line
    stat_poly_eq(aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
                 formula = y ~ x, parse = TRUE) +  # Regression equation and R2
    labs(
      x = x_label, 
      y = y_label,
      title = paste("Wind Speed Comparison -", site, year)
    ) +
    theme_minimal()
  # Define file name
  file_name <- paste0(save_dir, "/", site, "_WindSpeed_", year, ".png")
  # Save plot
  ggsave(file_name, plot = p, width = 8, height = 6, dpi = 300)
}
# Updated years range
years <- 2018:2024  
# Loop through the years and create the scatter plots
for (i in 1:7) {
  plot_and_save(
    df = w3w4_dfswind[[i]],
    year = years[i],
    save_dir = save_dir_biomet,
    x_var = "wind_speed_w3_sonic",
    y_var = "WS_Avg_w4_biomet",
    x_label = expression("Way3 Sonic Anemometer Wind Speed "~ms^{-1}),
    y_label = expression("Way4 Biomet Wind Speed "~ms^{-1}),
    site = "way3sonic_way4biomet"
  )
}

# Define save directory for way3sonic vs way4sonic
save_dir_sonic <- "C:/Users/rbmahbub/Documents/RProjects/AmerifluxDataSubmission_LandscapeFlux/Figure/WindSpeed/way3sonic_way4sonic"
# Ensure the directory exists
if (!dir.exists(save_dir_sonic)) dir.create(save_dir_sonic, recursive = TRUE)

# Function to create and save scatter plots
plot_and_save <- function(df, year, save_dir, x_var, y_var, x_label, y_label, site) {
  # Convert columns to numeric
  df[[x_var]] <- as.numeric(df[[x_var]])
  df[[y_var]] <- as.numeric(df[[y_var]])
  
  # Remove NA values
  df <- df[!is.na(df[[x_var]]) & !is.na(df[[y_var]]), ]
  
  # Create the plot
  p <- ggplot(df, aes_string(x = x_var, y = y_var)) +
    geom_point(color = "blue", alpha = 0.5) +  # Scatter points
    geom_smooth(method = "lm", color = "red", formula = y ~ x) +  # Regression line
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +  # 1:1 line
    stat_poly_eq(aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
                 formula = y ~ x, parse = TRUE) +  # Regression equation and R2
    labs(
      x = x_label, 
      y = y_label,
      title = paste("Wind Speed Comparison -", site, year)
    ) +
    theme_minimal()
  
  # Define file name
  file_name <- paste0(save_dir, "/", site, "_WindSpeed_", year, ".png")
  
  # Save plot
  ggsave(file_name, plot = p, width = 8, height = 6, dpi = 300)
}
# Updated years range
years <- 2018:2024  
# Loop through the years and create the scatter plots
for (i in 1:7) {
  plot_and_save(
    df = w3w4_dfswind[[i]],
    year = years[i],
    save_dir = save_dir_sonic,
    x_var = "wind_speed_w3_sonic",
    y_var = "wind_speed_w4_sonic",
    x_label = expression("Way3 Sonic Anemometer Wind Speed "~ms^{-1}),
    y_label = expression("Way4 Sonic Anemometer Wind Speed "~ms^{-1}),
    site = "way3sonic_way4sonic"
  )
}


######################################
######FIND THE OUTER VALUES OF USTAR##
######################################
######################################
### way3_data_nfws : NON CORRECTED
### way4_data_nfws : NON CORRECTED
### way3_data : CORRECTED
### way4_data : CORRECTED

# Function to get the correct column name dynamically
get_column_name <- function(df, possible_names) {
  return(possible_names[possible_names %in% colnames(df)][1])
}

# Initialize lists
way3_dfswindustar <- vector("list", length = 7)
way4_dfswindustar <- vector("list", length = 7)
way4_outliers <- vector("list", length = 7)  # Store outliers for each dataset

# Loop through both way3 and way4 data frames and select required columns with renaming
for (i in 1:7) {
  # Determine the correct column name for ustar dynamically
  way3_ustar_col <- get_column_name(way3_data_nfws[[i]], c("u_", "ustar"))
  way4_ustar_col <- get_column_name(way4_data_nfws[[i]], c("u_", "ustar"))
  
  # Process way3 data
  way3_dfswindustar[[i]] <- way3_data_nfws[[i]] %>%
    dplyr::select(
      TIMESTAMP,
      wind_speed_w3_sonic = wind_speed, 
      way3ustar = all_of(way3_ustar_col) # Select dynamically
    ) %>%
    mutate(across(c(wind_speed_w3_sonic, way3ustar), as.numeric))  # Convert to numeric
  
  # Process way4 data
  way4_dfswindustar[[i]] <- way4_data_nfws[[i]] %>%
    dplyr::select(
      TIMESTAMP,
      wind_speed_w4_sonic = wind_speed,
      way4ustar = all_of(way4_ustar_col) # Select dynamically
    ) %>%
    mutate(across(c(wind_speed_w4_sonic, way4ustar), as.numeric))  # Convert to numeric
  
  # Remove rows with NA values before fitting the model
  clean_data <- way4_dfswindustar[[i]] %>%
    filter(!is.na(way4ustar) & !is.na(wind_speed_w4_sonic))
  
  # Fit a linear regression model for USTAR ~ WS in way4 (if data is sufficient)
  if (nrow(clean_data) > 1) {
    lm_model <- lm(way4ustar ~ wind_speed_w4_sonic, data = clean_data)
    way4_dfswindustar[[i]]$predicted_ustar <- predict(lm_model, newdata = way4_dfswindustar[[i]])
    way4_dfswindustar[[i]]$residuals <- abs(way4_dfswindustar[[i]]$way4ustar - way4_dfswindustar[[i]]$predicted_ustar)
    
    # Apply different thresholds for outlier detection
    threshold <- if (i %in% c(6, 7)) {
      3.75 * sd(way4_dfswindustar[[i]]$residuals, na.rm = TRUE)
    } else {
      4 * sd(way4_dfswindustar[[i]]$residuals, na.rm = TRUE)
    }
    
    # Identify outliers
    way4_outliers[[i]] <- way4_dfswindustar[[i]] %>%
      filter(residuals > threshold)
    
    # Print for verification
    print(way4_outliers[[i]])
    
    # Get timestamps and mask values in way4_data
    outlier_timestamps <- way4_outliers[[i]]$TIMESTAMP
    rows_to_na <- way4_data[[i]]$TIMESTAMP %in% outlier_timestamps
    way4_data[[i]]$ustar[rows_to_na] <- NA
    way4_data[[i]]$u_[rows_to_na] <- NA
  } else {
    message(paste("Skipping dataset", i, "due to insufficient non-NA data."))
  }
}



plot(way4_outliers[[1]]$wind_speed_w4_sonic, way4_outliers[[1]]$way4ustar,
     main = "Outliers - 2018",
     xlab = "Wind Speed (w4_sonic)", ylab = "Ustar (way4ustar)",
     pch = 19, col = "red")

plot(way4_outliers[[2]]$wind_speed_w4_sonic, way4_outliers[[2]]$way4ustar,
     main = "Outliers - 2019",
     xlab = "Wind Speed (w4_sonic)", ylab = "Ustar (way4ustar)",
     pch = 19, col = "red")

plot(way4_outliers[[3]]$wind_speed_w4_sonic, way4_outliers[[3]]$way4ustar,
     main = "Outliers - 2020",
     xlab = "Wind Speed (w4_sonic)", ylab = "Ustar (way4ustar)",
     pch = 19, col = "red")

plot(way4_outliers[[4]]$wind_speed_w4_sonic, way4_outliers[[4]]$way4ustar,
     main = "Outliers - 2021",
     xlab = "Wind Speed (w4_sonic)", ylab = "Ustar (way4ustar)",
     pch = 19, col = "red")

plot(way4_outliers[[5]]$wind_speed_w4_sonic, way4_outliers[[5]]$way4ustar,
     main = "Outliers - 2022",
     xlab = "Wind Speed (w4_sonic)", ylab = "Ustar (way4ustar)",
     pch = 19, col = "red")

plot(way4_outliers[[6]]$wind_speed_w4_sonic, way4_outliers[[6]]$way4ustar,
     main = "Outliers - 2023",
     xlab = "Wind Speed (w4_sonic)", ylab = "Ustar (way4ustar)",
     pch = 19, col = "red")

plot(way4_outliers[[7]]$wind_speed_w4_sonic, way4_outliers[[7]]$way4ustar,
     main = "Outliers - 2024",
     xlab = "Wind Speed (w4_sonic)", ylab = "Ustar (way4ustar)",
     pch = 19, col = "red")


plot(way4_data[[1]]$wind_speed, way4_data[[1]]$u_,
     main = "2018",
     xlab = "Wind Speed (w4_sonic)", ylab = "Ustar (way4ustar)",
     pch = 19, col = "blue")

plot(way4_data[[2]]$wind_speed, way4_data[[2]]$u_,
     main = "Dataset 2",
     xlab = "Wind Speed (w4_sonic)", ylab = "Ustar (way4ustar)",
     pch = 19, col = "blue")

plot(way4_data[[3]]$wind_speed, way4_data[[3]]$u_,
     main = "Dataset 3",
     xlab = "Wind Speed (w4_sonic)", ylab = "Ustar (way4ustar)",
     pch = 19, col = "blue")

plot(way4_data[[4]]$wind_speed, way4_data[[4]]$u_,
     main = "Dataset 4",
     xlab = "Wind Speed (w4_sonic)", ylab = "Ustar (way4ustar)",
     pch = 19, col = "blue")

plot(way4_data[[5]]$wind_speed, way4_data[[5]]$u_,
     main = "Dataset 5",
     xlab = "Wind Speed (w4_sonic)", ylab = "Ustar (way4ustar)",
     pch = 19, col = "blue")

plot(way4_data[[6]]$wind_speed, way4_data[[6]]$u_,
     main = "2023",
     xlab = "Wind Speed (w4_sonic)", ylab = "Ustar (way4ustar)",
     pch = 19, col = "blue")

plot(way4_data[[7]]$wind_speed, way4_data[[7]]$u_,
     main = "2024",
     xlab = "Wind Speed (w4_sonic)", ylab = "Ustar (way4ustar)",
     pch = 19, col = "blue")



###########################################################
#############windspeed ustar###############################
###########################################################
library(ggplot2)
library(viridis)
# Function to plot each dataset using ggplot2
# Function to plot each dataset using ggplot2
plot_wind_vs_ustar <- function(data, title_text) {
  ggplot(data, aes(x = as.numeric(wind_speed), 
                   y = as.numeric(u_), 
                   color = as.numeric(DOY))) +
    geom_point(size = 2, alpha = 0.8) +
    scale_color_viridis_c(direction = -1, name = "Day of the year (day)",
                          option = "mako") +
    labs(title = title_text,
         x = "Wind Speed (w4_sonic)",
         y = "Ustar (way4ustar)") +
    theme_minimal(base_size = 14)
}



plot_wind_vs_ustar(way4_data_nfws[[1]], "2018")
plot_wind_vs_ustar(way4_data_nfws[[2]], "2019")
plot_wind_vs_ustar(way4_data_nfws[[3]], "2020")
plot_wind_vs_ustar(way4_data_nfws[[4]], "2021")
plot_wind_vs_ustar(way4_data_nfws[[5]], "2022")
plot_wind_vs_ustar(way4_data_nfws[[6]], "2023")
plot_wind_vs_ustar(way4_data_nfws[[7]], "2024")


#2023 TA_1_1_1 T-Sonic fail 0.652 -30%
# Convert air_temperature and sonic_temperature to numeric (if necessary) and then to Celsius
way3_data_nfws[[6]]$air_temperature <- as.numeric(way3_data_nfws[[6]]$air_temperature)
way3_data_nfws[[6]]$sonic_temperature <- as.numeric(way3_data_nfws[[6]]$sonic_temperature)

way3_data_nfws[[6]]$air_temperature[way3_data_nfws[[6]]$air_temperature < 250] <- NA

# Now convert from Kelvin to Celsius
way3_data_nfws[[6]]$air_temperature_C <- way3_data_nfws[[6]]$air_temperature - 273.15
way3_data_nfws[[6]]$sonic_temperature_C <- way3_data_nfws[[6]]$sonic_temperature - 273.15
way3_data_nfws[[6]]$air_temperature_C[way3_data_nfws[[6]]$air_temperature_C < -10] <- NA
# Replace air_temperature values less than -10 with NA to retain row alignment across the dataset



ggplot(way3_data_nfws[[6]], aes(x = air_temperature_C, y = sonic_temperature_C)) +
  geom_point(color = "blue", size = 2, alpha = 0.7) + # Scatter plot
  geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed") + # Add linear regression line
  labs(title = "Way 3 (2023)",
       x = "Air Temperature (°C)",
       y = "Sonic Temperature (°C)") +
  theme_minimal(base_size = 10)




way3_data_nfws[[6]]$air_temperature <- as.numeric(way3_data_nfws[[6]]$air_temperature)  # Ensure numeric
way3_data_nfws[[6]]$sonic_temperature <- as.numeric(way3_data_nfws[[6]]$sonic_temperature)  # Ensure numeric
way3_data_nfws[[6]]$air_temperature_C <- way3_data_nfws[[6]]$air_temperature - 273.15  # Kelvin to Celsius
way3_data_nfws[[6]]$sonic_temperature_C <- way3_data_nfws[[6]]$sonic_temperature - 273.15  # Kelvin to Celsius
df <- way3_data_nfws[[6]] %>% filter(!is.na(air_temperature_C), !is.na(sonic_temperature_C))  # Remove NA rows
lm_model <- lm(sonic_temperature_C ~ air_temperature_C, data = df)  # Fit linear model
df$predicted_sonic <- predict(lm_model, newdata = df)  # Predict values
df$residuals <- df$sonic_temperature_C - df$predicted_sonic  # Calculate residuals
threshold <- 1.7 * sd(df$residuals, na.rm = TRUE)  # Set 3 SD threshold
df$outlier <- abs(df$residuals) > threshold  # Identify outliers
way3_data_nfws[[6]]$air_temperature_C[df$outlier] <- NA  # Replace outliers with NA in Celsius
way3_data_nfws[[6]]$air_temperature[df$outlier] <- NA  # Replace outliers with NA in Kelvin
way3_data_nfws[[6]]$air_temperature[df$index[df$outlier]] <- NA  #Replace outlier values with NA

ggplot(df, aes(x = air_temperature_C, y = sonic_temperature_C, color = outlier)) +  # Plot
  geom_point(size = 2, alpha = 0.7) +  # Scatter
  geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed") +  # Regression line
  labs(title = "Way 3 (2023): Air Temp vs Sonic Temp", x = "Air Temperature (°C)", y = "Sonic Temperature (°C)") +
  theme_minimal(base_size = 10)  # Theme
plot(way3_data_nfws[[6]]$air_temperature_C, way3_data_nfws[[6]]$sonic_temperature)
plot(way3_data_nfws[[6]]$air_temperature, way3_data_nfws[[6]]$sonic_temperature)
plot(way3_data_nfws[[6]]$air_temperature_C, way3_data_nfws[[6]]$sonic_temperature)
plot(way3_data_nfws[[6]]$air_temperature, way3_data_nfws[[6]]$sonic_temperature)



plot(way3_data[[6]]$air_temperature, way3_data[[6]]$sonic_temperature) ##dataframe where the function was applied before 
plot(way3_data[[5]]$air_temperature, way3_data[[5]]$sonic_temperature)
plot(way4_data[[5]]$air_temperature, way4_data[[5]]$sonic_temperature)
plot(way4_data[[6]]$air_temperature, way4_data[[6]]$sonic_temperature)






##############################################
######### USTAR WINDSPEED OUTLIERS AND RETAINED DATA
###############################################
# Loop to generate plots for all 7 years
for (i in 1:7) {
  # Plot retained data in blue
  plot(way4_dfswindustar[[i]]$wind_speed_w4_sonic, way4_dfswindustar[[i]]$way4ustar,
       main = paste("Ustar vs Wind Speed -", 2017 + i),
       xlab = "Wind Speed (w4_sonic)", ylab = "Ustar (way4ustar)",
       pch = 19, col = "blue")
  
  # Add outliers in red (if available)
  if (!is.null(way4_outliers[[i]]) && nrow(way4_outliers[[i]]) > 0) {
    points(way4_outliers[[i]]$wind_speed_w4_sonic, way4_outliers[[i]]$way4ustar,
           pch = 19, col = "red")
  }
  
  # Add legend
  legend("topright", legend = c("Retained Data", "Outliers"),
         col = c("blue", "red"), pch = 19)
}


##Plot Ustar vs FC_1_1_1 for the year 2019

#########################
#########################
###Plot the data get a ustar without th filter#########


#### Merge with Caanopy Height Data
# # Merge 2019 data (CHDLWay3[[1]] into way3_data[[2]])
# way3_data[[2]] <- merge(way3_data[[2]], CHDLWay3[[1]], by = "TIMESTAMP", all = TRUE)
# # Merge 2020 data (CHDLWay3[[2]] into way3_data[[3]])
# way3_data[[3]] <- merge(way3_data[[3]], CHDLWay3[[2]], by = "TIMESTAMP", all = TRUE)
# # Merge 2021 data (CHDLWay3[[3]] into way3_data[[4]])
# way3_data[[4]] <- merge(way3_data[[4]], CHDLWay3[[3]], by = "TIMESTAMP", all = TRUE)
# # Merge 2022 data (CHDLWay3[[4]] into way3_data[[5]])
# way3_data[[5]] <- merge(way3_data[[5]], CHDLWay3[[4]], by = "TIMESTAMP", all = TRUE)
# # Merge 2023 data (CHDLWay3[[5]] into way3_data[[6]])
# way3_data[[6]] <- merge(way3_data[[6]], CHDLWay3[[5]], by = "TIMESTAMP", all = TRUE)
# # Merge 2024 data (CHDLWay3[[6]] into way3_data[[7]])
# way3_data[[7]] <- merge(way3_data[[7]], CHDLWay3[[6]], by = "TIMESTAMP", all = TRUE)


