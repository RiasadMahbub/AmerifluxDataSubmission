####################################
####Check the required columns
######################################

way3_directory <- "C:/Users/rbmahbub/Documents/RProjects/AmerifluxDataSubmission_LandscapeFlux/Data/Way3"
way4_directory <- "C:/Users/rbmahbub/Documents/RProjects/AmerifluxDataSubmission_LandscapeFlux/Data/Way4"

# List all CSV files in the directories
way3_files <- list.files(way3_directory, pattern = "\\.csv$", full.names = TRUE)
way4_files <- list.files(way4_directory, pattern = "\\.csv$", full.names = TRUE)

# Read the CSV files into lists of dataframes for both Way3 and Way4
way3_data <- lapply(way3_files, read.csv)
way4_data <- lapply(way4_files, read.csv)

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



