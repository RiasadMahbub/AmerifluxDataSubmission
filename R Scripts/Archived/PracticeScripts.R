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
way3_directory <- "C:/Users/rbmahbub/Documents/RProjects/AmerifluxDataSubmission_LandscapeFlux/Data/Way3"
way4_directory <- "C:/Users/rbmahbub/Documents/RProjects/AmerifluxDataSubmission_LandscapeFlux/Data/Way4"

# List all CSV files in the directories
way3_files <- list.files(way3_directory, pattern = "\\.csv$", full.names = TRUE)
way4_files <- list.files(way4_directory, pattern = "\\.csv$", full.names = TRUE)

# Read the CSV files into lists of dataframes for both Way3 and Way4
way3_data <- lapply(way3_files, read.csv)
way4_data <- lapply(way4_files, read.csv)

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


##### ##### ##### ##### ##### ##### 
##### ##### ##### ##### ##### ##### 
##### CHANGE THE COLUMN NAMES ##### 
##### ##### ##### ##### ##### ##### 
##### ##### ##### ##### ##### ##### 
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

# Define the directories
way3_directory <- "C:/Users/rbmahbub/Documents/RProjects/AmerifluxDataSubmission_LandscapeFlux/Data/Way3"
way4_directory <- "C:/Users/rbmahbub/Documents/RProjects/AmerifluxDataSubmission_LandscapeFlux/Data/Way4"

# Define the files to read
way3_files_to_read <- c("Way3 2018.csv", "Way3 2019.csv", "Way3 2020.csv", "Way3 2021.csv", "Way3 2023.csv", "Way3 2024.csv")
way4_files_to_read <- c("Way4 2018.csv", "Way4 2019.csv", "Way4 2020.csv", "Way4 2021.csv", "Way4 2023.csv", "Way4 2024.csv")


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

# Read and process Way3 and Way4 data
way3_data <- lapply(way3_files_to_read, function(filename) {
  file_path <- file.path(way3_directory, filename)
  data <- read.csv(file_path)
  apply_column_mapping(data) # Apply column name mapping
})

way4_data <- lapply(way4_files_to_read, function(filename) {
  file_path <- file.path(way4_directory, filename)
  data <- read.csv(file_path)
  apply_column_mapping(data) # Apply column name mapping
})

# Function to filter and rename columns
filter_and_rename <- function(data) {
  data_filtered <- data %>%
    select(
      TIMESTAMP, TIMESTAMP_START, TIMESTAMP_END, x_70_, `x_90_`, x_peak, ch4_mole_fraction, 
      ch4_mixing_ratio, co2_mole_fraction, co2_mixing_ratio, co2_flux, ch4_flux, 
      h2o_mole_fraction, h2o_mixing_ratio, h2o_flux, H, LE, H_strg, LE_strg, 
      air_pressure, RH, sonic_temperature, qc_co2_flux, qc_ch4_flux, qc_H, qc_LE, 
      qc_Tau, co2_var, co2_strg, ch4_strg, u_var, v_var, w_var, wind_dir, wind_speed, 
      max_wind_speed, X_z_d__L, air_temperature, VPD, LW_IN_Avg, LW_OUT_Avg, 
      PAR_IN_Avg, PAR_OUT_Avg, SW_IN_Avg, SW_OUT_Avg, SWC_1_1_1, L, Tau
    )
  
  # Rename filtered columns
  data_renamed <- data_filtered %>%
    rename(
      TIMESTAMP = TIMESTAMP, TIMESTAMP_START = TIMESTAMP_START, TIMESTAMP_END = TIMESTAMP_END, 
      FETCH_70 = `x_70_`, FETCH_90 = `x_90_`, FETCH_MAX = x_peak, CH4 = ch4_mole_fraction, 
      CH4_MIXING_RATIO = ch4_mixing_ratio, CO2 = co2_mole_fraction, CO2_MIXING_RATIO = co2_mixing_ratio, 
      FC = co2_flux, FCH4 = ch4_flux, H2O = h2o_mole_fraction, H2O_MIXING_RATIO = h2o_mixing_ratio, 
      FH2O = h2o_flux, H = H, LE = LE, SH = H_strg, SLE = LE_strg, PA = air_pressure, RH = RH, 
      T_SONIC = sonic_temperature, FC_SSITC_TEST = qc_co2_flux, FCH4_SSITC_TEST = qc_ch4_flux, 
      H_SSITC_TEST = qc_H, LE_SSITC_TEST = qc_LE, TAU_SSITC_TEST = qc_Tau, CO2_SIGMA = co2_var, 
      SC = co2_strg, SCH4 = ch4_strg, U_SIGMA = u_var, V_SIGMA = v_var, W_SIGMA = w_var, 
      WD = wind_dir, WS = wind_speed, WS_MAX = max_wind_speed, ZL = X_z_d__L, TA = air_temperature, 
      VPD = VPD, LW_IN = LW_IN_Avg, LW_OUT = LW_OUT_Avg, PPFD_IN = PAR_IN_Avg, PPFD_OUT = PAR_OUT_Avg, 
      SW_IN = SW_IN_Avg, SW_OUT = SW_OUT_Avg, SWC = SWC_1_1_1, MO_LENGTH = L, TAU = Tau
    )
  
  return(data_renamed)
}

# Process Way3 and Way4 data
way3_processed_data <- lapply(way3_data, process_data)
way4_processed_data <- lapply(way4_data, process_data)

# Apply filtering and renaming
way3_filtered_renamed <- lapply(way3_processed_data, filter_and_rename)
way4_filtered_renamed <- lapply(way4_processed_data, filter_and_rename)


###potential LSWR
directory <- "C:/Users/rbmahbub/Documents/RProjects/AmerifluxDataSubmission_LandscapeFlux/Data/PotentialSWR"
filename <- "US-HRA_HH_2017.csv"
file_path <- file.path(directory, filename)
data <- read.csv(file_path) # Read the CSV file

# Convert TIMESTAMP_START and TIMESTAMP_END to datetime format
data$TIMESTAMP_START <- ymd_hm(data$TIMESTAMP_START, tz = "UTC")
data$TIMESTAMP_END <- ymd_hm(data$TIMESTAMP_END, tz = "UTC")
data$HOUR <- hour(data$TIMESTAMP_START)# Extract the hour from TIMESTAMP_START
data <- data %>%
  mutate(
    TIMESTAMP_START = ymd_hms(TIMESTAMP_START),
    DOY = yday(TIMESTAMP_START),
    HOUR = hour(TIMESTAMP_START) + ifelse(minute(TIMESTAMP_START) == 30, 0.5, 0),
    DOY_HOUR = paste(DOY, HOUR, sep = "_")
  )
### Drop DOY so that it does not cause DOY.x and DOY.y
data <- data %>%
  select(-any_of(c("DOY", "HOUR", "TIMESTAMP_START", "TIMESTAMP_END")))

# Define a function to process and plot data for a single file
process_and_plot <- function(file_name, way_data, other_data, output_dir) {
  # Access the data for the given file
  file_data <- way_data[[file_name]]
  
  # Process the `way` data
  file_data <- file_data %>%
    mutate(
      TIMESTAMP = ymd_hms(TIMESTAMP),
      DOY = yday(TIMESTAMP),
      HOUR = hour(TIMESTAMP) + ifelse(minute(TIMESTAMP) == 30, 0.5, 0),
      DOY_HOUR = paste(DOY, HOUR, sep = "_")
    ) %>%
    filter(DOY >= 1 & DOY <= 365)
  
  # Adjust the timestamp for daylight saving time
  file_data <- file_data %>%
    mutate(
      TIMESTAMP = case_when(
        DOY >= 60 & DOY <= 315 ~ TIMESTAMP - hours(1),
        TRUE ~ TIMESTAMP
      ),
      DOY = yday(TIMESTAMP),
      HOUR = hour(TIMESTAMP) + ifelse(minute(TIMESTAMP) == 30, 0.5, 0),
      DOY_HOUR = paste(DOY, HOUR, sep = "_")
    )
  
  # Merge with the other dataset using DOY_HOUR
  merged_data <- file_data %>%
    inner_join(other_data, by = "DOY_HOUR") %>%
    mutate(
      PERIOD = ceiling(DOY / 15)  # Create non-overlapping 15-day periods
    )
  
  # Plot SW_IN_Avg, SW_IN_POT, and PAR_IN_Avg for each 15-day period
  plot <- ggplot(merged_data, aes(x = HOUR)) +
    geom_line(aes(y = SW_IN, color = "SW_IN"), size = 1) +
    geom_line(aes(y = SW_IN_POT, color = "SW_IN_POT"), size = 1, linetype = "dashed") +
    geom_line(aes(y = PPFD_IN, color = "PPFD_IN"), size = 1, linetype = "dotdash") +
    facet_wrap(~ PERIOD, scales = "free_y") +
    labs(
      title = paste("Radiation by Hour and Period for", file_name),
      x = "Hour of the Day",
      y = "Radiation",
      color = "Legend"
    ) +
    theme_minimal()
  
  # Save the plot
  ggsave(
    filename = file.path(output_dir, paste0(gsub(".csv", "", file_name), "_RadiationPlot.png")),
    plot = plot,
    width = 10, height = 6
  )
  
  # Return the processed data
  return(merged_data)
}

# Directories to save plots
way3_output_dir <- "C:/Users/rbmahbub/Documents/RProjects/AmerifluxDataSubmission_LandscapeFlux/Figure/ShortwaveRadiation/AllPlot/Way3"
way4_output_dir <- "C:/Users/rbmahbub/Documents/RProjects/AmerifluxDataSubmission_LandscapeFlux/Figure/ShortwaveRadiation/AllPlot/Way4"

# Create directories if they don't exist
dir.create(way3_output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(way4_output_dir, recursive = TRUE, showWarnings = FALSE)

names(way3_filtered_renamed) <- way3_files_to_read# Assign names to the processed Way3 data
names(way4_filtered_renamed) <- way4_files_to_read# Assign names to the processed Way4 data

# Process all files in Way3
all_processed_data_way3 <- list()
for (way_name in names(way3_filtered_renamed)) {
  all_processed_data_way3[[way_name]] <- process_and_plot(
    file_name = way_name, 
    way_data = way3_filtered_renamed, 
    other_data = data, 
    output_dir = way3_output_dir
  )
}

# Process all files in Way4
all_processed_data_way4 <- list()
for (way_name in names(way4_filtered_renamed)) {
  all_processed_data_way4[[way_name]] <- process_and_plot(
    file_name = way_name, 
    way_data = way4_filtered_renamed, 
    other_data = data, 
    output_dir = way4_output_dir
  )
}

# Columns to drop
columns_to_drop <- c("DOY", "HOUR", "DOY_HOUR", "SW_IN_POT", "PERIOD", "TIMESTAMP")

# Drop the columns from each data frame in the list
all_processed_data_way3 <- lapply(all_processed_data_way3, function(df) {
  df <- df[, !(colnames(df) %in% columns_to_drop)] # Keep only columns not in the drop list
  return(df)
})


# Assuming `all_processed_data_way3` is a named list containing the data frames
all_processed_data_way4 <- lapply(all_processed_data_way4, function(df) {
  df <- df[, !(colnames(df) %in% columns_to_drop)] # Keep only columns not in the drop list
  return(df)
})



# Define save directories
way3_save_directory <- "C:/Users/rbmahbub/Documents/RProjects/AmerifluxDataSubmission_LandscapeFlux/Data/AFguidedSubmitted/Way3"
way4_save_directory <- "C:/Users/rbmahbub/Documents/RProjects/AmerifluxDataSubmission_LandscapeFlux/Data/AFguidedSubmitted/Way4"

# Function to save filtered data
save_filtered_data <- function(data, filename, directory) {
  data$TIMESTAMP_START <- as.character(data$TIMESTAMP_START)# Timestamp Start
  data$TIMESTAMP_END <- as.character(data$TIMESTAMP_END) ## Timestamp END
  data[is.na(data)] <- -9999 ## null values as -999
  data[is.nan.data.frame(data)] <- -9999
  
  file_path <- file.path(directory, filename)
  write.csv(data, file = file_path, row.names = FALSE)
  
  cat(filename, "saved successfully to", directory, "\n")
}

# Assign names to the processed Way3 data
names(all_processed_data_way3) <- way3_files_to_read

# Assign names to the processed Way4 data
names(all_processed_data_way4) <- way4_files_to_read


# Save Way3 data
for (i in seq_along(all_processed_data_way3)) {
  filename <- names(all_processed_data_way3)[i]
  save_filtered_data(all_processed_data_way3[[i]], filename, way3_save_directory)
}

# Save Way4 data
for (i in seq_along(all_processed_data_way4)) {
  filename <- names(all_processed_data_way4)[i]
  save_filtered_data(all_processed_data_way4[[i]], filename, way4_save_directory)
}

