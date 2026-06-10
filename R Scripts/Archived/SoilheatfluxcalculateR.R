##### SOIL HEAT FLUX CALCULATION #####
##### ##### ##### ##### ##### ##### 
##### ##### ##### ##### ##### ##### 
##### CHANGE THE COLUMN NAMES ##### 
##### ##### ##### ##### ##### ##### 
##### ##### ##### ##### ##### ##### 
# Way 3 and Way 4 are calculated in different ways 
#Lvl_m_Avg column for way 3 and WTD_avg column for way 4

TS_mean.1.  = Tsoil_wat_mean(1)
TS_mean.2.  = Tsoil_wat_mean(2)
TS_mean.3.  = Tsoil_wat_mean(3)
TS_mean.4. = Tw

del_TS.1. = del_Tsoil_wat(1)
del_TS.2.= del_Tsoil_wat(2)
del_TS.3.= del_Tsoil_wat(3)
del_TS.4.= deltw

shf_Avg.1. == shf_Avg(1)
shf_Avg.2. == shf_Avg(2)
shf_Avg.3. == shf_Avg(3)

panel_tmpr_Avg
BattV_Avg

SWC_1_1_1_Avg == vwc
Watertable == Lvl_m_Avg


library(tidyverse)
library(dplyr)
library(lubridate)
library(data.table)
library(ggplot2)

# Define the directories
way3_directory <- "C:/Users/rbmahbub/Documents/RProjects/AmerifluxDataSubmission_LandscapeFlux/Data/Way3"
way4_directory <- "C:/Users/rbmahbub/Documents/RProjects/AmerifluxDataSubmission_LandscapeFlux/Data/Way4"

# Define the files to read
way3_files_to_read <- c("Way3 2018.csv", "Way3 2019.csv", "Way3 2020.csv", "Way3 2021.csv", "Way3 2023.csv", "Way3 2024.csv")
way4_files_to_read <- c("Way4 2018.csv", "Way4 2019.csv", "Way4 2020.csv", "Way4 2021.csv", "Way4 2023.csv", "Way4 2024.csv")

# # Read and process Way3 and Way4 data
# way3_data <- lapply(way3_files_to_read, function(filename) {
#   file_path <- file.path(way3_directory, filename)
#   data <- read.csv(file_path)
#   #apply_column_mapping(data) # Apply column name mapping
# })
# 
# way4_data <- lapply(way4_files_to_read, function(filename) {
#   file_path <- file.path(way4_directory, filename)
#   data <- read.csv(file_path)
#   #apply_column_mapping(data) # Apply column name mapping
# })

way3_data <- lapply(way3_files_to_read, function(filename) {
  file_path <- file.path(way3_directory, filename)
  data <- fread(file_path)
  as.data.frame(data)  # Convert data.table to data.frame
})

way4_data <- lapply(way4_files_to_read, function(filename) {
  file_path <- file.path(way4_directory, filename)
  data <- fread(file_path)
  as.data.frame(data)  # Convert data.table to data.frame
})

#### #### #### #### #### #### #### 
#### Get the non_NA dataframes for soil #### 
#### #### #### #### #### #### #### 
# List of columns to check for each data frame
vwc_columns <- c('Redox_1_Avg', 'Redox_2_Avg', 'Redox_3_Avg', 'WSDiag_Tot', 'TA_1_2_1_Avg', 'RH_1_2_1_Avg', 
                 'WLM_Us_1_Avg', 'TA_2_1_2_Avg', 'TA_Avg', 'RH_Avg', 'HF1_Avg', 'HF2_Avg', 'HF3_Avg', 
                 'SHF_Cal.1.', 'SHF_Cal.2.', 'SHF_Cal.3.', 'TS1_Avg', 'TS2_Avg', 'TS3_Avg', 'SWUp_Avg', 
                 'SWDn_Avg', 'LWUpTempCorr_Avg', 'LWDnTempCorr_Avg', 'Hyp1_1_Avg', 'Hyp1_2_Avg', 
                 'WaterTemp_Avg', 'WaterDepth_V_Avg', 'WaterDepth_Avg', 'LWUp_RAW_Avg', 'LWDn_RAW_Avg', 
                 'TA_Lower_Avg', 'RH_Lower_Avg', 'DOmV', 'DOppm', 'PAR_Den_up_Avg', 'PAR_Den_up', 
                 'PAR_Tot_up_Tot', 'PAR_Den_dn_Avg', 'PAR_Den_dn', 'PAR_Tot_dn_Tot', 'ORP_Avg', 'Pt1_Avg', 
                 'Pt2_Avg', 'Pt3_Avg', 'VWC_3.x', 'Ts_3.x', 'BRP_3.x', 'EC_3.x', 'VWC_6.x', 'Ts_6.x', 
                 'BRP_6.x', 'EC_6.x', 'VWC_3_Avg.x', 'Ts_3_Avg.x', 'BRP_3_Avg.x', 'EC_3_Avg.x', 
                 'VWC_6_Avg.x', 'Ts_6_Avg.x', 'BRP_6_Avg.x', 'EC_6_Avg.x', 'response..1..x', 
                 'response..2..x', 'response..3..x', 'response..4..x', 'response..5..x', 'response..6..x', 
                 'response..7..x', 'response..8..x', 'response..9..x', 'WLM_Us_1', 'WLM_Us_1_Min', 
                 'WLM_Us_1_TMn', 'WLM_Us_1_Max', 'WLM_Us_1_TMx', 'TA_2_1_1_Avg', 'RH_2_1_1_Avg', 'TS', 
                 'RN', 'Volts', 'Volts.1', 'Deg.C', 'X.', 'W.m.2', 'W.m.2.1', 'W.m.2.2', 'W..m.2mV.', 
                 'W..m.2mV..1', 'W..m.2mV..2', 'Deg.C.1', 'Deg.C.2', 'Deg.C.3', 'W.m.2.3', 'W.m.2.4', 
                 'W.m.2.5', 'W.m.2.6', 'Deg.C.4', 'Deg.C.5', 'meters.second', 'degrees', 'meters.second.1', 
                 'meters.second.2', 'Deg', 'Deg.1', 'Deg.C.6', 'mV', 'cm', 'W.m.2.7', 'W.m.2.8', 'K', 
                 'Deg.C.7', 'X..1', 'mV.1', 'ppm', 'umol.s.m.2', 'umol.s.m.2.1', 'mmol.m.2', 
                 'umol.s.m.2.2', 'umol.s.m.2.3', 'mmol.m.2.1', 'X..2', 'C', 'X..3', 'Ds.m', 'X..4', 'C.1', 
                 'X..5', 'dS.m', 'X..6', 'C.2', 'X..7', 'Ds.m.1', 'X..8', 'C.3', 'X..9', 'dS.m.1', 'X', 
                 'X.1', 'X.2', 'X.3', 'X.4', 'X.5', 'X.6', 'X.7', 'X.8', 'mV.2', 'mV.3', 'mV.4', 'mV.5', 
                 'mV.6', 'Deg.C.8', 'X..10', 'Deg.C.9', 'X..11', 'W.m.2.9', 'W.m.2.10', 'W.m.2.11', 
                 'W.m.2.12', 'W.m.2.13', 'W.m.2.14', 'W.m.2.15', 'X..12', 'umol.s.m.2.4', 'umol.s.m.2.5', 
                 'X.9', 'umol.s.m.2.6', 'X.10', 'umol.s.m.2.7', 'umol.s.m.2.8', 'X.11', 'umol.s.m.2.9', 
                 'X.12', 'mmol.m.2.2', 'mmol.m.2.3', 'umol.s.m.2.10', 'mV.7', 'mV.8', 'mV.9', 'unitless', 
                 'cm.1', 'Deg.C.10', 'X..13', 'Deg.C.11', 'cm.2', 'cm.3', 'X.13', 'cm.4', 'X.14', 
                 'RECORD.y', 'panel_tmpr_Avg', 'batt_volt_Avg', 'SOILFILEID', 'shf_Avg.1.', 'shf_Avg.2.', 
                 'shf_Avg.3.', 'shf_cal.1.', 'shf_cal.2.', 'shf_cal.3.', 'n_Tot', 'response..1..y', 
                 'response..2..y', 'response..3..y', 'response..4..y', 'response..5..y', 'response..6..y', 
                 'response..7..y', 'response..8..y', 'response..9..y', 'TS_mean.1.', 'TS_mean.2.', 
                 'TS_mean.3.', 'TS_mean.4.', 'del_TS.1.', 'del_TS.2.', 'del_TS.3.', 'del_TS.4.', 
                 'SWC_1_1_1', 'TS_2_1_2', 'BRP_1_1_1', 'EC_1_1_1', 'SWC_2_1_1', 'TS_2_2_2', 'BRP_2_1_1', 
                 'EC_2_1_1', 'SWC_1_1_1_Avg', 'TS_2_1_2_Avg', 'BRP_1_1_1_Avg', 'EC_1_1_1_Avg', 
                 'SWC_2_1_1_Avg', 'TS_2_2_2_Avg', 'BRP_2_1_1_Avg', 'EC_2_1_1_Avg', 'TS_mean.5.', 
                 'TS_mean.6.', 'del_TS.5.', 'del_TS.6.', 'IRR_Body_Avg', 'IRR_Corr_Avg', 'Temp_C_Avg', 
                 'Lvl_m_Avg', 'Tsoil_wat_mean.1.', 'Tsoil_wat_mean.2.', 'Tsoil_wat_mean.3.', 
                 'del_Tsoil_wat.1.', 'del_Tsoil_wat.2.', 'del_Tsoil_wat.3.', 'WTD_raw_Avg', 'WTD_Avg', 
                 'fw_1_Avg', 'Tsoil_wat_mean', 'wnd_spd_Avg', 'wnd_dir_Unit_Vec', 'wnd_dir_Std', 
                 'process_time_Avg', 'process_time_Max', 'buff_depth_Max', 'pH_1_Avg', 'pH_2_Avg', 
                 'ORP_1_Avg', 'ORP_2_Avg', 'slowsequence_Tot', 'VWC_3.y', 'Ts_3.y', 'BRP_3.y', 'EC_3.y', 
                 'VWC_6.y', 'Ts_6.y', 'BRP_6.y', 'EC_6.y', 'VWC_3_Avg.y', 'Ts_3_Avg.y', 'BRP_3_Avg.y', 
                 'EC_3_Avg.y', 'VWC_6_Avg.y', 'Ts_6_Avg.y', 'BRP_6_Avg.y', 'EC_6_Avg.y', 
                 'Lvl_m_corr_Avg', 'begin', 'end', 'YEAR')

# Initialize empty lists for non-NA and NA columns, including the data frame index
non_na_columns <- list()
na_columns_list <- list()  # List to store NA columns for each data frame

# Loop through each data frame in way3_data (from 1 to 6)
for (i in 1:6) {
  # Initialize a list for storing NA columns specific to this data frame
  na_columns <- c()
  
  # Check each specified column in vwc_columns
  for (col in vwc_columns) {
    if (col %in% names(way3_data[[i]])) {
      tryCatch({
        if (all(is.na(way3_data[[i]][[col]]))) {
          # Add to the NA columns list specific to this data frame
          na_columns <- append(na_columns, col)
        } else {
          # Add to the non-NA columns list
          non_na_columns <- append(non_na_columns, col)
        }
      }, error = function(e) {
        # If an error occurs (e.g., column doesn't exist), skip and continue
        cat(sprintf("Warning: Column '%s' not found in data frame %d. Skipping...\n", col, i))
      })
    } else {
      # If column doesn't exist in this data frame, skip it
      cat(sprintf("Warning: Column '%s' not found in data frame %d. Skipping...\n", col, i))
    }
  }
  
  # Ensure the na_columns_list has an entry for this data frame
  na_columns_list[[i]] <- na_columns
}

# Print results for verification
cat("Non-NA Columns:\n")
print(non_na_columns)

cat("\nNA Columns:\n")
for (i in 1:6) {
  cat(sprintf("DataFrame %d NA columns: %s\n", i, paste(na_columns_list[[i]], collapse = ", ")))
}

# Create a new list 'way3_data_non_na' by keeping only non-NA columns
way3_data_non_na <- list()

for (i in 1:6) {
  # Subset the data frame by keeping only non-NA columns (specific to each data frame)
  way3_data_non_na[[i]] <- way3_data[[i]][, setdiff(names(way3_data[[i]]), na_columns_list[[i]])]
}

# Verify the result
cat("\nUpdated data frames (columns remaining):\n")
for (i in 1:6) {
  cat(sprintf("DataFrame %d columns:\n", i))
  print(names(way3_data_non_na[[i]]))
}


# Create a new list 'way4_data_non_na' by keeping only non-NA columns
way4_data_non_na <- list()

for (i in 1:6) {
  # Subset the data frame by keeping only non-NA columns (specific to each data frame)
  way4_data_non_na[[i]] <- way4_data[[i]][, setdiff(names(way4_data[[i]]), na_columns_list[[i]])]
}

# Verify the result
cat("\nUpdated data frames (columns remaining) for way4_data:\n")
for (i in 1:6) {
  cat(sprintf("DataFrame %d columns:\n", i))
  print(names(way4_data_non_na[[i]]))
}



#### #### #### #### #### #### #### 
#### Check the presence of columns #### 
#### #### #### #### #### #### #### 
# List of columns to check for each d
# Define the list of column names to check
columns_to_check <- c(
  "TIMESTAMP", "Tsoil_wat_mean.1.", "Tsoil_wat_mean.2.", "Tsoil_wat_mean.3.",
  "del_Tsoil_wat.1.", "del_Tsoil_wat.2.", "del_Tsoil_wat.3.", "shf_Avg.1.", "shf_Avg.2.",
  "shf_Avg.3.", "shf_cal.1.", "shf_cal.2.", "shf_cal.3.", "panel_tmpr_Avg", "batt_volt_Avg",
  "n_Tot", "SWC_1_1_1_Avg", "VWC3corr", "Tw", "del_Tw", "Lvl_m_Avg"
)

# Loop through each dataframe in way4_data[[1]] to way4_data[[6]]
for (i in 1:4) {
  # Access the dataframe from way4_data
  df <- way3_data_non_na[[i]]
  
  # Check for missing columns
  missing_columns <- setdiff(columns_to_check, colnames(df))
  
  # Print the results
  if (length(missing_columns) > 0) {
    cat(sprintf("Missing columns in way3_data_non_na[[%d]]: %s\n", i, paste(missing_columns, collapse = ", ")))
  } else {
    cat(sprintf("All columns are present in way4_data[[%d]].\n", i))
  }
}


# Loop through each dataframe in way4_data[[1]] to way4_data[[6]]
for (i in 1:4) {
  # Access the dataframe from way4_data
  df <- way3_data_non_na[[i]]
  
  # Check for present columns
  present_columns <- intersect(columns_to_check, colnames(df))
  
  # Print the results
  if (length(present_columns) > 0) {
    cat(sprintf("Present columns in way3_data_non_na[[%d]]: %s\n", i, paste(present_columns, collapse = ", ")))
  } else {
    cat(sprintf("No specified columns are present in way4_data[[%d]].\n", i))
  }
}

#### #### #### #### #### #### #### 
####SWC #### 
#### #### #### #### #### #### #### 
# List of columns to check for each d
####Check SWC####
# Loop through each data frame in way3_data
for (i in 1:length(way3_data)) {
  # Find column names containing 'SWC'
  swc_columns <- grep("SWC", names(way3_data[[i]]), value = TRUE)
  
  # Count the number of such columns in the current data frame
  count <- length(swc_columns)
  total_swc_columns <- total_swc_columns + count
  
  # Print details for each data frame
  cat(sprintf("Data frame %d has %d columns with 'SWC' in the name: %s\n", 
              i, count, paste(swc_columns, collapse = ", ")))
}

# Print the total count across all data frames
cat(sprintf("Total number of columns with 'SWC' in the name: %d\n", total_swc_columns))


###########################################
#### Check columns starting with "T" ####
# Initialize the total counter for columns starting with "T"
total_t_columns <- 0

# Loop through each data frame in way3_data
for (i in 1:length(way3_data_non_na)) {
  # Find column names starting with 'T'
  t_columns <- grep("^T", names(way3_data_non_na[[i]]), value = TRUE)
  
  # Count the number of such columns in the current data frame
  count <- length(t_columns)
  total_t_columns <- total_t_columns + count
  
  # Print details for each data frame
  cat(sprintf("Data frame %d has %d columns starting with 'T': %s\n", 
              i, count, paste(t_columns, collapse = ", ")))
}

# Print the total count across all data frames
cat(sprintf("Total number of columns starting with 'T': %d\n", total_t_columns))


# Initialize the total counter for columns starting with "T"
total_del_columns <- 0

# Loop through each data frame in way3_data
for (i in 1:length(way3_data_non_na)) {
  # Find column names starting with 'T'
  del_columns <- grep("^del", names(way3_data_non_na[[i]]), value = TRUE)
  
  # Count the number of such columns in the current data frame
  count <- length(del_columns)
  total_del_columns <- total_del_columns + count
  
  # Print details for each data frame
  cat(sprintf("Data frame %d has %d columns starting with 'del': %s\n", 
              i, count, paste(del_columns, collapse = ", ")))
}

# Print the total count across all data frames
cat(sprintf("Total number of columns starting with 'del': %d\n", total_del_columns))


########################################
############SOIL HEAT FLUX CAL ##########
########################################
way3_data_non_na[[1]]$shf_Avg.1.[way3_data_non_na[[1]]$shf_Avg.1. > 80 | way3_data_non_na[[1]]$shf_Avg.1. < -60] <- NaN
way3_data_non_na[[1]]$shf_Avg.2.[way3_data_non_na[[1]]$shf_Avg.2. > 80 | way3_data_non_na[[1]]$shf_Avg.2. < -60] <- NaN
way3_data_non_na[[1]]$shf_Avg.3.[way3_data_non_na[[1]]$shf_Avg.3. > 80 | way3_data_non_na[[1]]$shf_Avg.3. < -60] <- NaN
way3_data_non_na[[1]]$Lvl_m_Avg[way3_data_non_na[[1]]$Lvl_m_Avg < -0.5] <- NaN

# Create a function to process each column
process_column <- function(data, column) {
  # Replace values in specific columns that are >80 or < -60
  if (column %in% c("shf_Avg.1.", "shf_Avg.2.", "shf_Avg.3.")) {
    data[[column]][data[[column]] > 80 | data[[column]] < -60] <- NaN
  }
  # Ensure Lvl_m_Avg values are greater than -0.5
  if (column == "Lvl_m_Avg") {
    data[[column]][data[[column]] < -0.5] <- NaN
  }
  
  # Calculate standard deviation and mean
  sd_value <- sd(data[[column]], na.rm = TRUE)
  avg_value <- mean(data[[column]], na.rm = TRUE)
  
  # Calculate avg + 3*sd and avg - 3*sd
  avg_plus_3sd <- avg_value + 3 * sd_value
  avg_minus_3sd <- avg_value - 3 * sd_value
  
  # Filter values based on the threshold and set NaN for those beyond the range
  data[[column]] <- ifelse(data[[column]] < avg_minus_3sd | 
                             data[[column]] > avg_plus_3sd, NaN, 
                           data[[column]])
  
  # Return the updated data and the summary values
  return(list(data = data, summary = c(avg_value, avg_plus_3sd, avg_minus_3sd)))
}

# Apply the process_column function to each of the specified columns and update the dataset
columns <- c("del_TS.1.", "del_TS.2.", "del_TS.3.", "del_TS.4.", "shf_Avg.1.", "shf_Avg.2.", "shf_Avg.3.")
for (col in columns) {
  result <- process_column(way3_data_non_na[[1]], col)
  way3_data_non_na[[1]] <- result$data
  cat(sprintf("For %s: \n  Average: %.2f\n  +3*SD: %.2f\n  -3*SD: %.2f\n", col, result$summary[1], result$summary[2], result$summary[3]))
}
way3_data_non_na[[1]]$swcorr <- (way3_data_non_na[[1]]$SWC_1_1_1_Avg*0.0951)+49.107
way3_data_non_na[[1]]$G1<-(
  way3_data_non_na[[1]]$shf_Avg.1. + 
    ((way3_data_non_na[[1]]$del_TS.1. * 0.08 * 1300 * 
        (900 + 4190 * (way3_data_non_na[[1]]$swcorr) / 100)
      + way3_data_non_na[[1]]$del_TS.4. * 
        way3_data_non_na[[1]]$Lvl_m_Avg * 4190 * 1000) / 1800)
)
way3_data_non_na[[1]]$G2<-(
  way3_data_non_na[[1]]$shf_Avg.2. + 
    ((way3_data_non_na[[1]]$del_TS.2. * 0.08 * 1300 * 
       (900 + 4190 * (way3_data_non_na[[1]]$swcorr) / 100)
     + way3_data_non_na[[1]]$del_TS.4. * 
       way3_data_non_na[[1]]$Lvl_m_Avg * 4190 * 1000) / 1800)
)
way3_data_non_na[[1]]$G3<-(
  way3_data_non_na[[1]]$shf_Avg.3. + 
    ((way3_data_non_na[[1]]$del_TS.3. * 0.08 * 1300 * 
        (900 + 4190 * (way3_data_non_na[[1]]$swcorr) / 100)
      + way3_data_non_na[[1]]$del_TS.4. * 
        way3_data_non_na[[1]]$Lvl_m_Avg * 4190 * 1000) / 1800)
)


way3_data_non_na[[1]]$Gavg <- rowMeans(cbind(way3_data_non_na[[1]]$G1, 
                                             way3_data_non_na[[1]]$G2, 
                                             way3_data_non_na[[1]]$G3), 
                                       na.rm = TRUE)

plot(way3_data_non_na[[1]]$Gavg)


#########
###Forloop###
##########
# Define a function to process each data frame in the list
process_data <- function(data) {
  # Replace out-of-bound values in soil heat flux columns
  data$shf_Avg.1.[data$shf_Avg.1. > 80 | data$shf_Avg.1. < -60] <- NaN
  data$shf_Avg.2.[data$shf_Avg.2. > 80 | data$shf_Avg.2. < -60] <- NaN
  data$shf_Avg.3.[data$shf_Avg.3. > 80 | data$shf_Avg.3. < -60] <- NaN
  data$Lvl_m_Avg[data$Lvl_m_Avg < -0.5] <- NaN
  data$del_TS.4.[data$del_TS.4. > 1 | data$del_TS.4. < -1] <- NaN
  data$del_TS.2.[data$del_TS.2. > 5 | data$del_TS.2. < -5] <- NaN
  
  # Process specific columns
  columns <- c("del_TS.1.", "del_TS.2.", "del_TS.3.", "del_TS.4.", "shf_Avg.1.", "shf_Avg.2.", "shf_Avg.3.")
  for (col in columns) {
    # Calculate thresholds and replace out-of-bound values
    sd_value <- sd(data[[col]], na.rm = TRUE)
    avg_value <- mean(data[[col]], na.rm = TRUE)
    avg_plus_3sd <- avg_value + 3 * sd_value
    avg_minus_3sd <- avg_value - 3 * sd_value
    data[[col]] <- ifelse(data[[col]] < avg_minus_3sd | data[[col]] > avg_plus_3sd, NaN, data[[col]])
  }
  
  # Calculate SWC correction with fallback column
  if ("SWC_1_1_1_Avg" %in% colnames(data)) {
    data$swcorr <- (data$SWC_1_1_1_Avg * 0.0951) + 49.107
  } else if ("SWC_2_1_1_Avg" %in% colnames(data)) {
    data$swcorr <- (data$SWC_2_1_1_Avg * 0.0951) + 49.107
  } else {
    # Handle the case where neither column exists
    data$swcorr <- NaN
    warning("Neither SWC_1_1_1_Avg nor SWC_2_1_1_Avg column found.")
  }
  
  # Calculate G1, G2, G3
  data$G1 <- data$shf_Avg.1. + 
    ((data$del_TS.1. * 0.08 * 1300 * 
        (900 + 4190 * (data$swcorr) / 100) +
        data$del_TS.4. * data$Lvl_m_Avg * 4190 * 1000) / 1800)
  
  data$G2 <- data$shf_Avg.2. + 
    ((data$del_TS.2. * 0.08 * 1300 * 
        (900 + 4190 * (data$swcorr) / 100) +
        data$del_TS.4. * data$Lvl_m_Avg * 4190 * 1000) / 1800)
  
  data$G3 <- data$shf_Avg.3. + 
    ((data$del_TS.3. * 0.08 * 1300 * 
        (900 + 4190 * (data$swcorr) / 100) +
        data$del_TS.4. * data$Lvl_m_Avg * 4190 * 1000) / 1800)
  
  # Calculate average G
  data$Gavg <- rowMeans(cbind(data$G1, data$G2, data$G3), na.rm = TRUE)
  
  return(data)
}

# Apply the process_data function to each element of the list
way3_data_non_na <- lapply(way3_data_non_na, process_data)


# Function to filter only the required columns
filter_columns <- function(data) {
  # Define the columns to keep
  required_columns <- c("shf_Avg.1.", "shf_Avg.2.", "shf_Avg.3.", 
                        "del_TS.1.", "del_TS.2.", "del_TS.3.", "del_TS.4.",
                        "swcorr", "Lvl_m_Avg", "Gavg", "G1", "G2", "G3")
  
  # Filter the data to keep only the required columns
  filtered_data <- data[, required_columns, drop = FALSE]
  
  return(filtered_data)
}

# Apply the filter_columns function to each element of the list
way3_data_filtered <- lapply(way3_data_non_na, filter_columns)


# Function to print rows with minimum 10 values of Gavg
print_min_10_Gavg <- function(data) {
  # Check if Gavg column exists
  if ("Gavg" %in% colnames(data)) {
    # Sort the data by Gavg in ascending order and select the first 10 rows
    min_10_rows <- data[order(data$Gavg, na.last = TRUE), ][1:10, ]
    print(min_10_rows)
  } else {
    cat("No Gavg column found in the dataset.\n")
  }
}

# Apply the function to each filtered data frame in the list
lapply(way3_data_filtered, print_min_10_Gavg)
tail(sort(way3_data_non_na[[3]]$G3, decreasing = TRUE), 40000)



plot(way3_data_non_na[[1]]$del_TS.4.)
plot(way3_data_non_na[[2]]$del_TS.4.)
plot(way3_data_non_na[[3]]$del_TS.4.)
plot(way3_data_non_na[[4]]$del_TS.4.)
summary(way3_data_non_na[[1]]$Lvl_m_Avg)
summary(way3_data_non_na[[2]]$Lvl_m_Avg)
summary(way3_data_non_na[[3]]$Lvl_m_Avg)
summary(way3_data_non_na[[4]]$Lvl_m_Avg)

summary(way3_data[[1]]$del_TS.4.)
summary(way3_data[[2]]$del_TS.4.)
summary(way3_data[[3]]$del_TS.4.)
summary(way3_data[[4]]$del_TS.4.)

# Summary for each dataset's G1 column, excluding NA and NaN values
summary(na.omit(way3_data_non_na[[1]]$G2))
summary(na.omit(way3_data_non_na[[2]]$del_TS.3.))
summary(na.omit(way3_data_non_na[[3]]$del_TS.3.))
summary(na.omit(way3_data_non_na[[4]]$del_TS.3.))


plot(way3_data_non_na[[1]]$Gavg)
plot(way3_data_non_na[[2]]$Gavg)
plot(way3_data_non_na[[3]]$Gavg)
plot(way3_data_non_na[[4]]$Gavg)

plot(way3_data_non_na[[2]]$shf_Avg.1.)
plot(way3_data_non_na[[3]]$del_TS.4.)
plot(way3_data_non_na[[4]]$del_TS.4.)

tail(sort(way3_data_non_na[[3]]$del_TS.4., decreasing = TRUE), 100)


####plotting###
# Load the ggplot2 package
library(ggplot2)

# Ensure TIMESTAMP is in Date-Time format
way3_data_non_na[[1]]$TIMESTAMP <- as.POSIXct(way3_data_non_na[[1]]$TIMESTAMP, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
way3_data_non_na[[2]]$TIMESTAMP <- as.POSIXct(way3_data_non_na[[2]]$TIMESTAMP, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
way3_data_non_na[[3]]$TIMESTAMP <- as.POSIXct(way3_data_non_na[[3]]$TIMESTAMP, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
way3_data_non_na[[4]]$TIMESTAMP <- as.POSIXct(way3_data_non_na[[4]]$TIMESTAMP, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
# Plotting using ggplot2 (Scatter Plot)
# Plotting using ggplot2 (Scatter Plot) with superscript unit on y-axis and year in the title
ggplot(way3_data_non_na[[1]], aes(x = TIMESTAMP, y = Gavg)) +
  geom_point(color = "blue") +
  labs(x = "Timestamp", y = expression(Gavg ~ (W ~ m^{-2})), title = "Gavg vs TIMESTAMP for Way3 Data 2018") +
  theme_minimal()

ggplot(way3_data_non_na[[2]], aes(x = TIMESTAMP, y = Gavg)) +
  geom_point(color = "green") +
  labs(x = "Timestamp", y = expression(Gavg ~ (W ~ m^{-2})), title = "Gavg vs TIMESTAMP for Way3 Data 2019") +
  theme_minimal()

ggplot(way3_data_non_na[[3]], aes(x = TIMESTAMP, y = Gavg)) +
  geom_point(color = "red") +
  labs(x = "Timestamp", y = expression(Gavg ~ (W ~ m^{-2})), title = "Gavg vs TIMESTAMP for Way3 Data 2020") +
  theme_minimal()

ggplot(way3_data_non_na[[4]], aes(x = TIMESTAMP, y = Gavg)) +
  geom_point(color = "purple") +
  labs(x = "Timestamp", y = expression(Gavg ~ (W ~ m^{-2})), title = "Gavg vs TIMESTAMP for Way3 Data 2021") +
  theme_minimal()





###########################################
####################WAY4###################
###########################################
#### #### #### #### #### #### #### 
####SWC #### 
#### #### #### #### #### #### #### 
# List of columns to check for each d
####Check SWC####
# Loop through each data frame in way3_data
for (i in 1:length(way4_data)) {
  # Find column names containing 'SWC'
  swc_columns <- grep("SWC", names(way4_data[[i]]), value = TRUE)
  
  # Count the number of such columns in the current data frame
  count <- length(swc_columns)
  total_swc_columns <- total_swc_columns + count
  
  # Print details for each data frame
  cat(sprintf("Data frame %d has %d columns with 'SWC' in the name: %s\n", 
              i, count, paste(swc_columns, collapse = ", ")))
}

# Print the total count across all data frames
cat(sprintf("Total number of columns with 'SWC' in the name: %d\n", total_swc_columns))


###########################################
#### Check columns starting with "T" ####
# Initialize the total counter for columns starting with "T"
total_t_columns <- 0

# Loop through each data frame in way3_data
for (i in 1:length(way4_data_non_na)) {
  # Find column names starting with 'T'
  t_columns <- grep("^T", names(way4_data_non_na[[i]]), value = TRUE)
  
  # Count the number of such columns in the current data frame
  count <- length(t_columns)
  total_t_columns <- total_t_columns + count
  
  # Print details for each data frame
  cat(sprintf("Data frame %d has %d columns starting with 'T': %s\n", 
              i, count, paste(t_columns, collapse = ", ")))
}

# Print the total count across all data frames
cat(sprintf("Total number of columns starting with 'T': %d\n", total_t_columns))


# Initialize the total counter for columns starting with "T"
total_del_columns <- 0

# Loop through each data frame in way3_data
for (i in 1:length(way4_data_non_na)) {
  # Find column names starting with 'T'
  del_columns <- grep("^del", names(way4_data_non_na[[i]]), value = TRUE)
  
  # Count the number of such columns in the current data frame
  count <- length(del_columns)
  total_del_columns <- total_del_columns + count
  
  # Print details for each data frame
  cat(sprintf("Data frame %d has %d columns starting with 'del': %s\n", 
              i, count, paste(del_columns, collapse = ", ")))
}

# Print the total count across all data frames
cat(sprintf("Total number of columns starting with 'del': %d\n", total_del_columns))


###G1 flux### = AC, Y, U, AA, BA, AP
## Water table depth  it subtracts 5.5 and adds 1.5 to WaterDepth_Avg
# shf_Fac = AC5
# shf_Avg(1)= AB
# shf_cal(1) = AF

# del_T4_filt = AA5
# del_Tsoil_wat(4) = Z5

# detT3_filt = Y5
# del_Tsoil_wat(3) = X5

# del_T1_filt = U5
# del_Tsoil_wat(1) = T5


# WVC =  AZ  
# WVcorr = BA5

# WaterDepthgreater0 = AP5 = WTD_Avg
###G2 flux##
# del_T4_filt = AA = 
# del_Tsoil_wat(4) = Z = del_TS.4.

# shf_Fac(2) = AE
# shf_Avg(2) = AD = shf_Avg.2.
# shf_cal(2) = AG = shf_cal.2.

# detT3_filt = Y 
# del_Tsoil_wat(3) = X = del_TS.3.

# del_T2_filt = W
# del_Tsoil_wat(2) = V5 = del_TS.2.

# WVCcorr = BA = SWC_1_1_1_Avg
# AP8 = WaterDepthgreater0

# Define columns to check for outliers
# Apply calculation to each row of way4_data_non_na[[1]]
# Apply the calculation to the dataframe
# Apply the calculation to the dataframe

way4_data_non_na[[1]]$shf_Fac1 <- ifelse(
  !is.na(way4_data_non_na[[1]]$shf_Avg.1.) & !is.na(way4_data_non_na[[1]]$shf_cal.1.), 
  way4_data_non_na[[1]]$shf_Avg.1. / way4_data_non_na[[1]]$shf_cal.1. * 15.5424, 
  NA
)

way4_data_non_na[[1]]$shf_Fac2 <- with(way4_data_non_na[[1]], 
                                       ifelse(!is.na(shf_Avg.2.) & !is.na(shf_cal.2.),  # Check for missing values
                                              shf_Avg.2. / shf_cal.2. * 15.8983, 
                                              NA)  # Assign NA if either value is missing
)
# Define columns to check for outliers
columns_to_check <- c("del_TS.4.", "del_TS.3.", "del_TS.1.", "del_TS.2.")
# Apply filtering for each specified column
way4_data_non_na[[1]] <- way4_data_non_na[[1]] %>%
  filter(
    abs(del_TS.4. - mean(del_TS.4., na.rm = TRUE)) <= 3 * sd(del_TS.4., na.rm = TRUE),
    abs(del_TS.3. - mean(del_TS.3., na.rm = TRUE)) <= 3 * sd(del_TS.3., na.rm = TRUE),
    abs(del_TS.2. - mean(del_TS.2., na.rm = TRUE)) <= 3 * sd(del_TS.2., na.rm = TRUE),
    abs(del_TS.1. - mean(del_TS.1., na.rm = TRUE)) <= 3 * sd(del_TS.2., na.rm = TRUE)
  )

way4_data_non_na[[1]]$SWC_1_1_1_Calculated <- ifelse(
  !is.na(way4_data_non_na[[1]]$SWC_1_1_1_Avg), 
  way4_data_non_na[[1]]$SWC_1_1_1_Avg * 0.131 + 41.586, 
  NA
)
way4_data_non_na[[1]]$WTD_Avgcorr <- ifelse(
  !is.na(way4_data_non_na[[1]]$WTD_Avg), 
  way4_data_non_na[[1]]$WTD_Avg - 5.5 + 1.5, 
  NA
)
# Assuming 'way4_data_non_na[[1]]' contains the required columns:
way4_data_non_na[[1]]$G1 <- ifelse(
  !is.na(way4_data_non_na[[1]]$shf_Fac1) & 
    !is.na(way4_data_non_na[[1]]$del_TS.3.) & 
    !is.na(way4_data_non_na[[1]]$del_TS.1.) & 
    !is.na(way4_data_non_na[[1]]$SWC_1_1_1_Calculated) & 
    !is.na(way4_data_non_na[[1]]$del_TS.4.) & 
    !is.na(way4_data_non_na[[1]]$WTD_Avgcorr),
  
  way4_data_non_na[[1]]$shf_Fac1 + 
    (way4_data_non_na[[1]]$del_TS.1. * 0.08 * 1390 * 
       (900 + 4190 * way4_data_non_na[[1]]$SWC_1_1_1_Calculated / 100 * 1000 / 1390) + 
       (way4_data_non_na[[1]]$del_TS.3. + way4_data_non_na[[1]]$del_TS.4.) * 
       way4_data_non_na[[1]]$WTD_Avgcorr / 100 * 4190 * 1000 / 2) / 1800, 
  
  NA
)

way4_data_non_na[[1]]$G2 <- ifelse(
  !is.na(way4_data_non_na[[1]]$WTD_Avgcorr) & 
    !is.na(way4_data_non_na[[1]]$SWC_1_1_1_Avg) & 
    !is.na(way4_data_non_na[[1]]$del_TS.2.) & 
    !is.na(way4_data_non_na[[1]]$del_TS.3.) & 
    !is.na(way4_data_non_na[[1]]$shf_Fac2) & 
    !is.na(way4_data_non_na[[1]]$del_TS.4.), 
  
  way4_data_non_na[[1]]$shf_Fac2 + 
    (way4_data_non_na[[1]]$del_TS.2. * 0.08 * 1390 * 
       (900 + 4190 * way4_data_non_na[[1]]$SWC_1_1_1_Avg / 100 * 1000 / 1390) + 
       (way4_data_non_na[[1]]$del_TS.3. + way4_data_non_na[[1]]$del_TS.4.) * 
       way4_data_non_na[[1]]$WTD_Avgcorr / 100 * 4190 * 1000 / 2) / 1800, 
  
  NA
)

way4_data_non_na[[1]]$Gavg <- rowMeans(
  way4_data_non_na[[1]][, c("G1", "G2")], 
  na.rm = TRUE
)
way4_data_non_na[[1]] <- way4_data_non_na[[1]] %>%
  mutate(Gavg = ifelse(Gavg > 1000 | Gavg < -1000, NA, Gavg))


plot(way4_data_non_na[[1]]$Gavg)

# Ensure TIMESTAMP is in Date-Time format
way4_data_non_na[[1]]$TIMESTAMP <- as.POSIXct(way4_data_non_na[[1]]$TIMESTAMP, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

# Ensure TIMESTAMP is in Date-Time format
way4_data_non_na[[1]]$TIMESTAMP <- as.POSIXct(way4_data_non_na[[1]]$TIMESTAMP, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

# Plot using ggplot2
ggplot(way4_data_non_na[[1]], aes(x = TIMESTAMP, y = Gavg)) +
  geom_point(color = "blue") +
  labs(x = "Timestamp", y = expression(Gavg ~ (W ~ m^{-2})), title = "Gavg vs TIMESTAMP for Way4 Data 2018") +
  theme_minimal()


####################
#### For loop ####
####################

# Apply the same logic to all data from way4_data_non_na[[1]] to way4_data_non_na[[4]]
for (i in 1:4) {
  way4_data_non_na[[i]]$shf_Fac1 <- ifelse(
    !is.na(way4_data_non_na[[i]]$shf_Avg.1.) & !is.na(way4_data_non_na[[i]]$shf_cal.1.), 
    way4_data_non_na[[i]]$shf_Avg.1. / way4_data_non_na[[i]]$shf_cal.1. * 15.5424, 
    NA
  )
  
  way4_data_non_na[[i]]$shf_Fac2 <- with(way4_data_non_na[[i]], 
                                         ifelse(!is.na(shf_Avg.2.) & !is.na(shf_cal.2.),  # Check for missing values
                                                shf_Avg.2. / shf_cal.2. * 15.8983, 
                                                NA)  # Assign NA if either value is missing
  )
  
  # Define columns to check for outliers
  columns_to_check <- c("del_TS.4.", "del_TS.3.", "del_TS.1.", "del_TS.2.")
  
  # Apply filtering for each specified column
  way4_data_non_na[[i]] <- way4_data_non_na[[i]] %>%
    filter(
      abs(del_TS.4. - mean(del_TS.4., na.rm = TRUE)) <= 3 * sd(del_TS.4., na.rm = TRUE),
      abs(del_TS.3. - mean(del_TS.3., na.rm = TRUE)) <= 3 * sd(del_TS.3., na.rm = TRUE),
      abs(del_TS.2. - mean(del_TS.2., na.rm = TRUE)) <= 3 * sd(del_TS.2., na.rm = TRUE),
      abs(del_TS.1. - mean(del_TS.1., na.rm = TRUE)) <= 3 * sd(del_TS.2., na.rm = TRUE)
    )
  
  way4_data_non_na[[i]]$SWC_1_1_1_Calculated <- ifelse(
    !is.na(way4_data_non_na[[i]]$SWC_1_1_1_Avg), 
    way4_data_non_na[[i]]$SWC_1_1_1_Avg * 0.131 + 41.586, 
    NA
  )
  
  way4_data_non_na[[i]]$WTD_Avgcorr <- ifelse(
    !is.na(way4_data_non_na[[i]]$WTD_Avg), 
    way4_data_non_na[[i]]$WTD_Avg - 5.5 + 1.5, 
    NA
  )
  
  # Assuming 'way4_data_non_na[[i]]' contains the required columns:
  way4_data_non_na[[i]]$G1 <- ifelse(
    !is.na(way4_data_non_na[[i]]$shf_Fac1) & 
      !is.na(way4_data_non_na[[i]]$del_TS.3.) & 
      !is.na(way4_data_non_na[[i]]$del_TS.1.) & 
      !is.na(way4_data_non_na[[i]]$SWC_1_1_1_Calculated) & 
      !is.na(way4_data_non_na[[i]]$del_TS.4.) & 
      !is.na(way4_data_non_na[[i]]$WTD_Avgcorr),
    
    way4_data_non_na[[i]]$shf_Fac1 + 
      (way4_data_non_na[[i]]$del_TS.1. * 0.08 * 1390 * 
         (900 + 4190 * way4_data_non_na[[i]]$SWC_1_1_1_Calculated / 100 * 1000 / 1390) + 
         (way4_data_non_na[[i]]$del_TS.3. + way4_data_non_na[[i]]$del_TS.4.) * 
         way4_data_non_na[[i]]$WTD_Avgcorr / 100 * 4190 * 1000 / 2) / 1800, 
    
    NA
  )
  
  way4_data_non_na[[i]]$G2 <- ifelse(
    !is.na(way4_data_non_na[[i]]$WTD_Avgcorr) & 
      !is.na(way4_data_non_na[[i]]$SWC_1_1_1_Avg) & 
      !is.na(way4_data_non_na[[i]]$del_TS.2.) & 
      !is.na(way4_data_non_na[[i]]$del_TS.3.) & 
      !is.na(way4_data_non_na[[i]]$shf_Fac2) & 
      !is.na(way4_data_non_na[[i]]$del_TS.4.), 
    
    way4_data_non_na[[i]]$shf_Fac2 + 
      (way4_data_non_na[[i]]$del_TS.2. * 0.08 * 1390 * 
         (900 + 4190 * way4_data_non_na[[i]]$SWC_1_1_1_Avg / 100 * 1000 / 1390) + 
         (way4_data_non_na[[i]]$del_TS.3. + way4_data_non_na[[i]]$del_TS.4.) * 
         way4_data_non_na[[i]]$WTD_Avgcorr / 100 * 4190 * 1000 / 2) / 1800, 
    
    NA
  )
  
  way4_data_non_na[[i]]$Gavg <- rowMeans(
    way4_data_non_na[[i]][, c("G1", "G2")], 
    na.rm = TRUE
  )
  
  plot(way4_data_non_na[[i]]$Gavg)
}




way4_data_non_na[[2]]$WaterDepth_corr.1
way4_data_non_na[[4]]$WTD_Avg
way4_data_non_na[[2]]$water

# Iterate over each dataset in way4_data_non_na[[1]] to way4_data_non_na[[4]]
for (i in 1:4) {
  # Get the current dataset
  current_data <- way4_data_non_na[[i]]
  
  # Find all column names starting with 'water'
  water_columns <- grep("^Water", names(current_data), value = TRUE)
  
  # Check if all rows in the 'water' columns are NA
  if (length(water_columns) > 0) {
    for (col in water_columns) {
      # Check if all values in the column are NA
      all_na <- all(is.na(current_data[[col]]))
      message(paste("Dataset", i, "Column", col, "has all NAs:", all_na))
    }
  } else {
    message(paste("Dataset", i, "has no columns starting with 'water'"))
  }
}




##### PLOT WTD
plot(way3_data[[1]]$WTD_Avg, way3_data[[1]]$WTD_raw_Avg)

plot(way3_data[[1]]$VWC_3.x, way3_data[[1]]$VWC_3_Avg.x)

summary(way3_data[[2]]$SWC_1_1_1)
summary(way3_data[[2]]$VWC_6.x)
summary(way3_data[[3]]$VWC_3.y)
summary(way3_data[[3]]$VWC_6.y)
summary(way3_data[[3]]$VWC_3_Avg.x)
summary(way3_data[[3]]$VWC_6_Avg.x)
summary(way3_data[[3]]$VWC_3_Avg.y)
summary(way3_data[[3]]$VWC_6_Avg.y)

if (all(is.na(way3_data[[1]]$SWC_1_1_1))) {
  cat("Column contains only NA values.\n")
} else {
  min_value <- min(way3_data[[1]]$SWC_1_1_1, na.rm = TRUE)
  print(min_value)
}


# Initialize a counter
total_swc_columns <- 0

plot(way3_data[[1]]$SWC_1_1_1, way3_data[[1]]$SWC_2_1_1_Avg)






