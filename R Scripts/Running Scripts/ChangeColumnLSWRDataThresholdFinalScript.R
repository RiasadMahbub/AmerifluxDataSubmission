library(tidyverse)
library(dplyr)
library(lubridate)

# Define the directories
way3_directory <- "C:/Users/rbmahbub/Documents/RProjects/AmerifluxDataSubmission_LandscapeFlux/Data/InputLocalProcessedData/MasterFiles/Way3"
way4_directory <- "C:/Users/rbmahbub/Documents/RProjects/AmerifluxDataSubmission_LandscapeFlux/Data/InputLocalProcessedData/MasterFiles/Way4"

# Define the files to read
way3_files_to_read <- c("Way3_2018.csv", "Way3_2019.csv", "Way3_2020.csv", "Way3_2021.csv", "Way3_2022.csv", "Way3_2023.csv", "Way3_2024.csv", "Way3_2025.csv")
way4_files_to_read <- c("Way4_2018.csv", "Way4_2019.csv", "Way4_2020.csv", "Way4_2021.csv", "Way4_2022.csv", "Way4_2023.csv", "Way4_2024.csv", "Way4_2025.csv")

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

###UNITS ROWS######
####Since we made change in the data now we do not have the units of the filtered rows####
# Step 1: Extract unit row and store separately
#unit_row <- way3_data[[6]][1, ] # Assuming the unit row is in the 6th list element
# Remove the first row from the 5th and 6th data frames
# Step 2: Filter and rename columns
filtered_columns <- c("TIMESTAMP", "TIMESTAMP_START", "TIMESTAMP_END", "x_70_", "x_90_", "x_peak", 
                      "ch4_mole_fraction", "ch4_mixing_ratio", "co2_mole_fraction", "co2_mixing_ratio", 
                      "co2_flux", "ch4_flux", "h2o_mole_fraction", "h2o_mixing_ratio", "h2o_flux", 
                      "H", "LE", "H_strg", "LE_strg", "air_pressure", "RH", "sonic_temperature", 
                      "qc_co2_flux", "qc_ch4_flux", "qc_H", "qc_LE", "qc_Tau", "co2_var", "co2_strg", 
                      "ch4_strg", "u_var", "v_var", "w_var", "wind_dir", "wind_speed", "max_wind_speed", 
                      "X_z_d__L", "air_temperature", "VPD", "LW_IN_T_Corr_Avg", "LW_OUT_T_Corr_Avg", "PAR_IN_Avg", 
                      "PAR_OUT_Avg", "SW_IN_Avg", "SW_OUT_Avg", "SWC_2_1_1_Avg", "L", "Tau", "TS_mean.2.", "u_", 
                      "canopy_height",	"LAI_corrected",	"LAI_corrected_gapfilled", "canopy_height_gapfilled", "Lvl_m_Avg",
                      "wt_corr_AVG_cm_fixedBias", "P_RAIN_Tot"
                      , "co2_signal_strength_7500_mean"
                      ,"rssi_77_mean",
                      "rand_err_ch4_flux","rand_err_Tau",
                      "rand_err_H",
                      "rand_err_LE",
                      "rand_err_co2_flux",
                      "rand_err_h2o_flux",
                      "rand_err_ch4_flux",
                      "TS_mean.1.", "TS_mean.2.", "TS_mean.3.", "TS_mean.4.", 
                      "TS_mean.5.", "TS_mean.6.", "TS_mean.7.", "TS_mean.8.", 
                      "TS_mean.9.", 
                      "del_TS.1.", "del_TS.2.", "del_TS.3.", "del_TS.4.", 
                      "shf_Avg.1.", "shf_Avg.2.","shf_Avg.3." , 
                      "panel_tmpr_Avg","BattV_Avg"
                      )

# Filter `unit_row` to include only the columns in `filtered_columns`
unit_df <- read.csv("C:/Users/rbmahbub/Documents/RProjects/AmerifluxDataSubmission_LandscapeFlux/Data/InputLocalProcessedData/MasterFiles/unitdf/Unit_df.csv", stringsAsFactors = FALSE)
filtered_unit_row <- unit_df[names(unit_df) %in% filtered_columns]
# View the filtered unit row
print(filtered_unit_row)

######################################
##########AIRPRESSURE################
###I think both of them have air pressure##
######################################
# Function to process each dataset (for TIMESTAMP and derived columns)
### Creates Hour/Month/DOY, TIMESTAMP, TIMESTAMP_START, TIMESTAMP_END
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

# Process Way3 and Way4 data
way3_processed_data <- lapply(way3_data, process_data)
way4_processed_data <- lapply(way4_data, process_data)
unit_row_processed<-process_data(unit_df)
unit_row_processed

#####################################
########### Multiple possible names
####################################
column_mapping <- list( # Mapping of equivalent column names
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
way3_CM_data <- lapply(way3_processed_data, apply_column_mapping)
way4_CM_data <- lapply(way4_processed_data, apply_column_mapping)
unit_row_CM<-apply_column_mapping(unit_row_processed)

# Count the number of columns with the name "Lvl_m_Avg"
num_duplicates <- sum(colnames(way4_CM_data[[7]]) == "Lvl_m_Avg")

# Verify the change for the first few dataframes
head(way4_data[[1]])
head(way4_data[[7]])

# Function to filter and rename columns
filter_and_rename <- function(data) {
  # Add missing columns with NA
  missing_cols <- setdiff(required_columns, names(data))
  if(length(missing_cols) > 0){
    data[missing_cols] <- NA
  }
  data_filtered <- data %>%
    select(
      TIMESTAMP, TIMESTAMP_START, TIMESTAMP_END, x_70_, `x_90_`, x_peak, ch4_mole_fraction, 
      ch4_mixing_ratio, co2_mole_fraction, co2_mixing_ratio, co2_flux, ch4_flux, 
      h2o_mole_fraction, h2o_mixing_ratio, h2o_flux, H, LE, H_strg, LE_strg, 
      air_pressure, RH, sonic_temperature, qc_co2_flux, qc_ch4_flux, qc_H, qc_LE, 
      qc_Tau, co2_var, co2_strg, ch4_strg, u_var, v_var, w_var, wind_dir, wind_speed, 
      max_wind_speed, X_z_d__L, air_temperature, VPD, LW_IN_T_Corr_Avg, LW_OUT_T_Corr_Avg, 
      PAR_IN_Avg, PAR_OUT_Avg, SW_IN_Avg, SW_OUT_Avg, SWC_2_1_1_Avg, L, Tau, TS_mean.2., Lvl_m_Avg, u_,
      canopy_height,	LAI_corrected,	LAI_corrected_gapfilled, canopy_height_gapfilled,
      wt_corr_AVG_cm_fixedBias, P_RAIN_Tot,co2_signal_strength_7500_mean
       ,rssi_77_mean, rand_err_ch4_flux, rand_err_Tau, rand_err_H, rand_err_LE,
       rand_err_co2_flux, rand_err_h2o_flux, rand_err_ch4_flux,
      TS_mean.1., TS_mean.2., TS_mean.3., TS_mean.4., 
      del_TS.1., del_TS.2., del_TS.3., del_TS.4., 
      shf_Avg.1., shf_Avg.2.,shf_Avg.3., 
      panel_tmpr_Avg,BattV_Avg)
  
  # Rename filtered columns # left of = ameriflux, right of = our names
  data_renamed <- data_filtered %>%
    rename(
      TIMESTAMP = TIMESTAMP, TIMESTAMP_START = TIMESTAMP_START, TIMESTAMP_END = TIMESTAMP_END, 
      FETCH_70 = `x_70_`, FETCH_90 = `x_90_`, FETCH_MAX = x_peak, CH4_1_1_1 = ch4_mole_fraction, 
      CH4_MIXING_RATIO = ch4_mixing_ratio, CO2_1_1_1 = co2_mole_fraction, CO2_MIXING_RATIO = co2_mixing_ratio, 
      FC_1_1_1 = co2_flux, FCH4 = ch4_flux, H2O_1_1_1 = h2o_mole_fraction, H2O_MIXING_RATIO = h2o_mixing_ratio, 
      FH2O = h2o_flux, H = H, LE = LE, SH = H_strg, SLE = LE_strg, PA = air_pressure, RH_1_1_1 = RH, 
      T_SONIC = sonic_temperature, FC_SSITC_TEST = qc_co2_flux, FCH4_SSITC_TEST = qc_ch4_flux, 
      H_SSITC_TEST = qc_H, LE_SSITC_TEST = qc_LE, TAU_SSITC_TEST = qc_Tau, CO2_SIGMA = co2_var, 
      SC = co2_strg, SCH4 = ch4_strg, U_SIGMA = u_var, V_SIGMA = v_var, W_SIGMA = w_var, 
      WD = wind_dir, WS = wind_speed, WS_MAX = max_wind_speed, ZL = X_z_d__L, TA_1_1_1 = air_temperature, 
      VPD = VPD, LW_IN = LW_IN_T_Corr_Avg, LW_OUT = LW_OUT_T_Corr_Avg, PPFD_IN = PAR_IN_Avg, PPFD_OUT = PAR_OUT_Avg, 
      SW_IN = SW_IN_Avg, SW_OUT = SW_OUT_Avg, MO_LENGTH = L, TAU = Tau,  SWC_1_1_1=SWC_2_1_1_Avg, WTD_1_1_1=Lvl_m_Avg,USTAR = u_,
      TS_1_1_1=TS_mean.2., canopy_height = canopy_height,	LAI_corrected= LAI_corrected,	
      LAI_corrected_gapfilled= LAI_corrected_gapfilled, canopy_height_gapfilled=canopy_height_gapfilled,
      WTD_1_2_1= wt_corr_AVG_cm_fixedBias, P_RAIN = P_RAIN_Tot
      , mean_value_RSSI_LI_7500_ = co2_signal_strength_7500_mean
       ,
       rssi_77_mean_ =rssi_77_mean, rand_err_ch4_flux =rand_err_ch4_flux,
       rand_err_Tau= rand_err_Tau, rand_err_H= rand_err_H, rand_err_LE= rand_err_LE,
       rand_err_co2_flux = rand_err_co2_flux, rand_err_h2o_flux = rand_err_h2o_flux, 
       rand_err_ch4_flux = rand_err_ch4_flux,
      
      TS_mean_1 = TS_mean.1., TS_mean_2=TS_mean.2., TS_mean_3=TS_mean.3., TS_mean_4= TS_mean.4., 
      del_TS_1=del_TS.1., del_TS_2=del_TS.2.,del_TS_3= del_TS.3.,del_TS_4= del_TS.4., 
      shf_Avg_1= shf_Avg.1., shf_Avg_2=shf_Avg.2.,shf_Avg_3=shf_Avg.3., 
      panel_tmpr_Avg=panel_tmpr_Avg,BattV_Avg=BattV_Avg
    )
  
  return(data_renamed)
}


# Define the required column names
required_columns <- c(
  "TIMESTAMP", "TIMESTAMP_START", "TIMESTAMP_END", "x_70_", "x_90_", "x_peak", 
  "ch4_mole_fraction", "ch4_mixing_ratio", "co2_mole_fraction", "co2_mixing_ratio", 
  "co2_flux", "ch4_flux", "h2o_mole_fraction", "h2o_mixing_ratio", "h2o_flux", 
  "H", "LE", "H_strg", "LE_strg", "air_pressure", "RH", "sonic_temperature", 
  "qc_co2_flux", "qc_ch4_flux", "qc_H", "qc_LE", "qc_Tau", "co2_var", "co2_strg", 
  "ch4_strg", "u_var", "v_var", "w_var", "wind_dir", "wind_speed", "max_wind_speed", 
  "X_z_d__L", "air_temperature", "VPD", "LW_IN_T_Corr_Avg", "LW_OUT_T_Corr_Avg", "PAR_IN_Avg", 
  "PAR_OUT_Avg", "SW_IN_Avg", "SW_OUT_Avg", "SWC_2_1_1_Avg", "L", "Tau", "TS_mean.1.", "Lvl_m_Avg", "u_",
  "canopy_height",	"LAI_corrected",	"LAI_corrected_gapfilled", "canopy_height_gapfilled",
  "wt_corr_AVG_cm_fixedBias", "P_RAIN_Tot", "rand_err_Tau", "rand_err_H", "rand_err_LE",
  "rand_err_co2_flux", "rand_err_h2o_flux", "rand_err_ch4_flux",
  "TS_mean.1.", "TS_mean.2.", "TS_mean.3.", "TS_mean.4.", "del_TS.1.", "del_TS.2.", 
  "del_TS.3.", "del_TS.4.", "shf_Avg.1.", "shf_Avg.2.","shf_Avg.3." , "panel_tmpr_Avg","BattV_Avg"
)

# Get the column names of way4_CM_data[[5]]
existing_columns <- names(way4_CM_data[[5]])
# Identify missing columns
missing_columns <- setdiff(required_columns, existing_columns)
# Print missing columns
cat("Missing columns in way4_CM_data[[5]]:\n")
print(missing_columns)


# Apply filtering and renaming
way3_filtered_renamed <- lapply(way3_CM_data, filter_and_rename)
way4_filtered_renamed <- lapply(way4_CM_data, filter_and_rename)
filtered_unit_row<-filter_and_rename(unit_row_CM)


# plot(way4_CM_data[[2]]$TIMESTAMP, way4_CM_data[[2]]$LAI_corrected_gapfilled)
# plot(all_processed_data_way4[[2]]$TIMESTAMP, all_processed_data_way4[[2]]$LAI_corrected_gapfilled)
###potential LSWR
directory <- "C:/Users/rbmahbub/Documents/RProjects/AmerifluxDataSubmission_LandscapeFlux/Data/InputLocalRawData/PotentialSWR"
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

#### process and plot all the data
process_and_plot <- function(file_name, way_data, other_data, output_dir) {
  # Access the data for the given file
  file_data <- way_data[[file_name]]
  # Process the way data
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
      DOY_HOUR = paste(DOY, HOUR, sep = "_"),
      PPFD_IN_Adjusted = PPFD_IN / 2.02  # 1010 umol m-1 s-1 = 500 Wm-2, 2.02 conversion factor
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
    geom_line(aes(y = PPFD_IN_Adjusted, color = "PPFD_IN"), size = 1, linetype = "dotdash") +
    facet_wrap(~ PERIOD, scales = "free_y") +
    labs(
      title = paste("Radiation by Hour and Period for", file_name),
      x = "Hour of the Day",
      y = expression("Radiation (Wm"^-2*")"),
      color = "Legend"
    ) +
    theme_minimal()
  
  # Save the plot
  ggsave(
    filename = file.path(output_dir, paste0(gsub(".csv", "", file_name), "_RadiationPlot.jpeg")),
    plot = plot,
    width = 10, height = 6
  )
  
  # Return the processed data
  return(merged_data)
}
#### process and plot for the max values
process_and_plot <- function(file_name, way_data, other_data, output_dir) {
  # Access the data for the given file
  file_data <- way_data[[file_name]]
  
  # Process the `way` data
  file_data <- file_data %>%
    mutate(
      TIMESTAMP = ymd_hms(TIMESTAMP),
      DOY = yday(TIMESTAMP),
      HOUR = hour(TIMESTAMP) + ifelse(minute(TIMESTAMP) == 30, 0.5, 0),
      DOY_HOUR = paste(DOY, HOUR, sep = "_"),
      PPFD_IN_Adjusted = PPFD_IN / 2.02  # 1010 umol m-1 s-1 = 500 Wm-2, 2.02 conversion factor
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
      PERIOD = ceiling(DOY / 15),  # Create non-overlapping 15-day periods
      PPFD_IN = ifelse(PPFD_IN_Adjusted > SW_IN_POT, NA, PPFD_IN),  # Set PPFD_IN to NA if PPFD_IN_Adjusted > SW_IN_POT
      PPFD_IN_Adjusted = ifelse(PPFD_IN_Adjusted > SW_IN_POT, NA, PPFD_IN_Adjusted)  # Set PPFD_IN_Adjusted to NA if it's greater than SW_IN_POT
    )
  
  # Define a custom function to handle max with NaN values
  max_with_nan <- function(x) {
    if (all(is.na(x))) {
      return(NA_real_)
    } else {
      return(max(x, na.rm = TRUE))
    }
  }
  
  # Calculate the maximum diurnal composite for each hour within each 15-day period
  max_diurnal_composite <- merged_data %>%
    group_by(PERIOD, HOUR) %>%
    summarize(
      SW_IN_Avg_max = max_with_nan(SW_IN),
      SW_IN_POT_max = max_with_nan(SW_IN_POT),
      PPFD_IN_Adjusted_max = max_with_nan(PPFD_IN_Adjusted)
    ) %>%
    ungroup()
  
  # Plot SW_IN_Avg, SW_IN_POT, and PAR_IN_Avg for each 15-day period
  plot <- ggplot(max_diurnal_composite, aes(x = HOUR)) +
    geom_line(aes(y = SW_IN_Avg_max, color = "SW_IN"), size = 1) +
    geom_line(aes(y = SW_IN_POT_max, color = "SW_IN_POT"), size = 1, linetype = "dashed") +
    geom_line(aes(y = PPFD_IN_Adjusted_max, color = "PPFD_IN"), size = 1, linetype = "dotdash") +
    facet_wrap(~ PERIOD, scales = "free_y") +
    labs(
      title = paste("Radiation by Hour and Period for", file_name),
      x = "Hour of the Day",
      y = expression("Radiation (Wm"^-2*")"),
      color = "Legend"
    ) +
    theme_minimal()
  
  # Save the plot
  ggsave(
    filename = file.path(output_dir, paste0(gsub(".csv", "", file_name), "_RadiationPlot.jpeg")),
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

way3_filtered_renamed[[5]]$PPFD_IN<-as.numeric(way3_filtered_renamed[[5]]$PPFD_IN)
way3_filtered_renamed[[5]]$SW_IN<-as.numeric(way3_filtered_renamed[[5]]$SW_IN)

way3_filtered_renamed[[6]]$PPFD_IN<-as.numeric(way3_filtered_renamed[[6]]$PPFD_IN)
way3_filtered_renamed[[6]]$SW_IN<-as.numeric(way3_filtered_renamed[[6]]$SW_IN)

way4_filtered_renamed[[5]]$PPFD_IN<-as.numeric(way4_filtered_renamed[[5]]$PPFD_IN)
way4_filtered_renamed[[5]]$SW_IN<-as.numeric(way4_filtered_renamed[[5]]$SW_IN)
way4_filtered_renamed[[5]]$PPFD_IN[way4_filtered_renamed[[5]]$PPFD_IN > 2000] <- NaN

way4_filtered_renamed[[6]]$PPFD_IN<-as.numeric(way4_filtered_renamed[[6]]$PPFD_IN)
way4_filtered_renamed[[6]]$SW_IN<-as.numeric(way4_filtered_renamed[[6]]$SW_IN)

way3_filtered_renamed[[7]]$PPFD_IN<-as.numeric(way3_filtered_renamed[[7]]$PPFD_IN)
way3_filtered_renamed[[7]]$SW_IN<-as.numeric(way3_filtered_renamed[[7]]$SW_IN)

way4_filtered_renamed[[7]]$PPFD_IN<-as.numeric(way4_filtered_renamed[[7]]$PPFD_IN)
way4_filtered_renamed[[7]]$SW_IN<-as.numeric(way4_filtered_renamed[[7]]$SW_IN)

########################################
#### Readjustment of the alignment####
########################################
## Way 3 misalignment Fix
#1-4 = 1 hour less; 5-7 = 30 minutes less
# Fix the TIMESTAMP in all_processed_data_way3[[5]]
#Way 3 PPFD_IN is misaligned after the alignment in 2023 (1,2,3,4,5,6,7)
way3_filtered_renamed[[5]] <- way3_filtered_renamed[[5]] %>%
  mutate(
    # Recalculate DOY and HOUR based on the updated TIMESTAMP
    DOY = yday(TIMESTAMP),
    HOUR = hour(TIMESTAMP) + ifelse(minute(TIMESTAMP) == 30, 0.5, 0),
    # Adjust TIMESTAMP based on the PERIOD
    TIMESTAMP = case_when(
      DOY >= 1 & DOY <= 60 ~ TIMESTAMP - minutes(30),  # Subtract 1 hour for periods 1-4
      DOY >= 61 & DOY <= 105 ~ TIMESTAMP - minutes(30),  # Subtract 30 minutes for periods 5-7
      TRUE ~ TIMESTAMP  # Keep unchanged for other periods
    ),
    # Recalculate DOY and HOUR based on the updated TIMESTAMP
    DOY = yday(TIMESTAMP),
    HOUR = hour(TIMESTAMP) + ifelse(minute(TIMESTAMP) == 30, 0.5, 0),
    DOY_HOUR = paste(DOY, HOUR, sep = "_")
  )

way3_filtered_renamed[[6]] <- way3_filtered_renamed[[6]] %>%
  mutate(
    # Recalculate DOY and HOUR based on the updated TIMESTAMP
    DOY = yday(TIMESTAMP),
    HOUR = hour(TIMESTAMP) + ifelse(minute(TIMESTAMP) == 30, 0.5, 0),
    # Adjust TIMESTAMP based on the PERIOD
    TIMESTAMP = case_when(
      DOY >= 1 & DOY <= 60 ~ TIMESTAMP - minutes(120),  # Subtract 1 hour for periods 1-4
      DOY >= 61 & DOY <= 105 ~ TIMESTAMP - hours(1),  # Subtract 30 minutes for periods 5-7
      TRUE ~ TIMESTAMP  # Keep unchanged for other periods
    ),
    # Recalculate DOY and HOUR based on the updated TIMESTAMP
    DOY = yday(TIMESTAMP),
    HOUR = hour(TIMESTAMP) + ifelse(minute(TIMESTAMP) == 30, 0.5, 0),
    DOY_HOUR = paste(DOY, HOUR, sep = "_")
  )



#Way 4 PPFD_IN is misaligned after the alignment in 2018 (11), 2019 (22, 23, 24, 25), 2020 (1), 2024 (1,2)#
## way4 PPFD_IN is misaligned after the alignment in 2018 (11), 2019 (22, 23, 24, 25), 2020 (1), 2024 (1,2)
#2018
way4_filtered_renamed[[1]] <- way4_filtered_renamed[[1]] %>%
  mutate(
    # Recalculate DOY and HOUR based on the updated TIMESTAMP
    DOY = yday(TIMESTAMP),
    HOUR = hour(TIMESTAMP) + ifelse(minute(TIMESTAMP) == 30, 0.5, 0),
    # Adjust TIMESTAMP based on the PERIOD
    TIMESTAMP = case_when(
      DOY >= 165 & DOY <= 195 ~ TIMESTAMP - minutes(30),  # Subtract 1 hour for periods 1-4
      TRUE ~ TIMESTAMP  # Keep unchanged for other periods
    ),
    # Recalculate DOY and HOUR based on the updated TIMESTAMP
    DOY = yday(TIMESTAMP),
    HOUR = hour(TIMESTAMP) + ifelse(minute(TIMESTAMP) == 30, 0.5, 0),
    DOY_HOUR = paste(DOY, HOUR, sep = "_")
  )
#2019
way4_filtered_renamed[[2]] <- way4_filtered_renamed[[2]] %>%
  mutate(
    # Recalculate DOY and HOUR based on the updated TIMESTAMP
    DOY = yday(TIMESTAMP),
    HOUR = hour(TIMESTAMP) + ifelse(minute(TIMESTAMP) == 30, 0.5, 0),
    # Adjust TIMESTAMP based on the PERIOD
    TIMESTAMP = case_when(
      DOY >= 1 & DOY <= 15 ~ TIMESTAMP + minutes(210),
      DOY >= 15 & DOY <= 30 ~ TIMESTAMP + minutes(300), 
      DOY >= 315 & DOY <= 365 ~ TIMESTAMP - hours(2),  # Subtract 1 hour for periods 1-4
      TRUE ~ TIMESTAMP  # Keep unchanged for other periods
    ),
    # Recalculate DOY and HOUR based on the updated TIMESTAMP
    DOY = yday(TIMESTAMP),
    HOUR = hour(TIMESTAMP) + ifelse(minute(TIMESTAMP) == 30, 0.5, 0),
    DOY_HOUR = paste(DOY, HOUR, sep = "_")
  )

#2020
way4_filtered_renamed[[3]] <- way4_filtered_renamed[[3]] %>%
  mutate(
    # Recalculate DOY and HOUR based on the updated TIMESTAMP
    DOY = yday(TIMESTAMP),
    HOUR = hour(TIMESTAMP) + ifelse(minute(TIMESTAMP) == 30, 0.5, 0),
    # Adjust TIMESTAMP based on the PERIOD
    TIMESTAMP = case_when(
      DOY >= 1 & DOY <= 15 ~ TIMESTAMP - minutes(30),  # Subtract 1 hour for periods 1-4
      TRUE ~ TIMESTAMP  # Keep unchanged for other periods
    ),
    # Recalculate DOY and HOUR based on the updated TIMESTAMP
    DOY = yday(TIMESTAMP),
    HOUR = hour(TIMESTAMP) + ifelse(minute(TIMESTAMP) == 30, 0.5, 0),
    DOY_HOUR = paste(DOY, HOUR, sep = "_")
  )

#2023
way4_filtered_renamed[[6]] <- way4_filtered_renamed[[6]] %>%
  mutate(
    # Recalculate DOY and HOUR based on the updated TIMESTAMP
    DOY = yday(TIMESTAMP),
    HOUR = hour(TIMESTAMP) + ifelse(minute(TIMESTAMP) == 30, 0.5, 0),
    # Adjust TIMESTAMP based on the PERIOD
    TIMESTAMP = case_when(
      DOY >= 1 & DOY <= 30 ~ TIMESTAMP - minutes(30),  # Subtract 30 mins for periods 1-4
      DOY >= 30 & DOY <= 60 ~ TIMESTAMP - minutes(30),  # Subtract 30 mins for periods 1-4
      #DOY >= 60 & DOY <= 75 ~ TIMESTAMP+minutes(30),
      DOY >= 275 & DOY <= 290 ~ TIMESTAMP + minutes(30),
      DOY >= 320 & DOY <= 365 ~ TIMESTAMP - minutes(30),  # Subtract 1 hour for periods 1-4
      #DOY >= 60 & DOY <= 75 ~ TIMESTAMP+minutes(30),
      TRUE ~ TIMESTAMP  # Keep unchanged for other periods
    ),
    # Recalculate DOY and HOUR based on the updated TIMESTAMP
    DOY = yday(TIMESTAMP),
    HOUR = hour(TIMESTAMP) + ifelse(minute(TIMESTAMP) == 30, 0.5, 0),
    DOY_HOUR = paste(DOY, HOUR, sep = "_")
  )

#2024
way4_filtered_renamed[[7]] <- way4_filtered_renamed[[7]] %>%
  mutate(
    # Recalculate DOY and HOUR based on the updated TIMESTAMP
    DOY = yday(TIMESTAMP),
    HOUR = hour(TIMESTAMP) + ifelse(minute(TIMESTAMP) == 30, 0.5, 0),
    # Adjust TIMESTAMP based on the PERIOD
    TIMESTAMP = case_when(
      DOY >= 1 & DOY <= 15 ~ TIMESTAMP - minutes(30),  # Subtract 1 hour for periods 1-4
      DOY >= 15 & DOY <= 30 ~ TIMESTAMP- minutes(30),
      DOY >= 30 & DOY <= 60 ~ TIMESTAMP - minutes(30),
      DOY >= 60 & DOY <= 75 ~ TIMESTAMP + minutes(30),
      DOY >= 105 & DOY <= 195 ~ TIMESTAMP - minutes(30),
      DOY >= 195 & DOY <= 230 ~ TIMESTAMP - minutes(30),
      TRUE ~ TIMESTAMP  # Keep unchanged for other periods
    ),
    # Recalculate DOY and HOUR based on the updated TIMESTAMP
    DOY = yday(TIMESTAMP),
    HOUR = hour(TIMESTAMP) + ifelse(minute(TIMESTAMP) == 30, 0.5, 0),
    DOY_HOUR = paste(DOY, HOUR, sep = "_")
  )

# Process all files in Way3
all_processed_data_way3 <- list()
for (way_name in names(way3_filtered_renamed)) {
  all_processed_data_way3[[way_name]] <- tryCatch(
    {
      process_and_plot(
        file_name = way_name, 
        way_data = way3_filtered_renamed, 
        other_data = data, 
        output_dir = way3_output_dir
      )
    },
    error = function(e) {
      message(sprintf("Error processing file: %s", way_name))
      message(e)
      NULL  # Return NULL for failed processing
    }
  )
}

# Process all files in Way4
all_processed_data_way4 <- list()
for (way_name in names(way4_filtered_renamed)) {
  all_processed_data_way4[[way_name]] <- tryCatch(
    {
      process_and_plot(
        file_name = way_name, 
        way_data = way4_filtered_renamed, 
        other_data = data, 
        output_dir = way4_output_dir
      )
    },
    error = function(e) {
      message(sprintf("Error processing file: %s", way_name))
      message(e)
      NULL  # Return NULL for failed processing
    }
  )
}
# plot(way4_CM_data[[2]]$TIMESTAMP, way4_CM_data[[2]]$LAI_corrected_gapfilled)
# plot(all_processed_data_way4[[2]]$TIMESTAMP, all_processed_data_way4[[2]]$LAI_corrected_gapfilled)


# Function to convert all columns (except specified ones) to numeric
convert_columns_to_numeric <- function(data) {
  # Specify columns to exclude
  exclude_columns <- c("TIMESTAMP_START", "TIMESTAMP_END", "TIMESTAMP")
  
  # Convert columns to numeric if they are not in exclude_columns
  data <- data %>%
    mutate(across(.cols = -all_of(exclude_columns), .fns = ~ as.numeric(.)))
  
  return(data)
}

# Apply the function to the specified dataframes
all_processed_data_way3[[5]] <- convert_columns_to_numeric(all_processed_data_way3[[5]])
all_processed_data_way3[[6]] <- convert_columns_to_numeric(all_processed_data_way3[[6]])
all_processed_data_way3[[7]] <- convert_columns_to_numeric(all_processed_data_way3[[7]])

all_processed_data_way4[[5]] <- convert_columns_to_numeric(all_processed_data_way4[[5]])
all_processed_data_way4[[6]] <- convert_columns_to_numeric(all_processed_data_way4[[6]])
all_processed_data_way4[[7]] <- convert_columns_to_numeric(all_processed_data_way4[[7]])


#### Remove the PPFD values greater than SW_IN_POT
## Way 4 The PPFD_IN of way 4 is having anomalous values in 2018 (12), 2019 (3,7,8,9,10, 14, 15, 16, 17, 18, 2020 (2, 3, 4,5, 6,7,8, 16,17, 18, 19, 20, 21), 2021 (2, 8, 9), 2023 (1, 11, 12, 13, 14, 15, 16, 17, 18, 22, 23, 24), 2024(1,2,3,4)

############################################################
#############################################################
# This function replaces WTD_1_1_1 < -0.5 with NA, removes rows where TA_1_1_1 < 200, and converts TA_1_1_1 from Kelvin to Celsius.
  
  # Check if "TA_1_1_1" column exists
  if ("TA_1_1_1" %in% colnames(df)) {
    # Replace only the "TA_1_1_1" column with NA where TA_1_1_1 < 200
    df <- df %>% mutate(TA_1_1_1 = ifelse(TA_1_1_1 < 200, NA, TA_1_1_1))
    
    # Convert TA_1_1_1 from Kelvin to Celsius
    df <- df %>% mutate(TA_1_1_1 = TA_1_1_1 - 273.15)
    df <- df %>% mutate(T_SONIC = T_SONIC - 273.15)
  }
  # Check if "VPD" column exists and convert units by dividing by 100
  if ("VPD" %in% colnames(df)) {
    df <- df %>% mutate(VPD = VPD / 100)
  }
  
  return(df)
}

### Convert the units of VPD



filtered_unit_row$TA_1_1_1 <- "degC"
filtered_unit_row$TS_1_1_1 <- "degC"
filtered_unit_row$T_SONIC <- "degC"
# plot(way4_CM_data[[2]]$TIMESTAMP, way4_CM_data[[2]]$LAI_corrected_gapfilled)
# plot(all_processed_data_way4[[2]]$TIMESTAMP, all_processed_data_way4[[2]]$LAI_corrected_gapfilled)


# Apply the function to all dataframes in all_processed_data_way4
all_processed_data_way3 <- lapply(all_processed_data_way3, replace_wtd_and_convert_ta)
all_processed_data_way4 <- lapply(all_processed_data_way4, replace_wtd_and_convert_ta)

plot(way4_CM_data[[1]]$Lvl_m_Avg)
plot(way4_CM_data[[2]]$Lvl_m_Avg)
plot(way4_CM_data[[3]]$Lvl_m_Avg)
plot(way4_CM_data[[4]]$Lvl_m_Avg)
plot(way4_CM_data[[5]]$Lvl_m_Avg)
plot(way4_CM_data[[6]]$Lvl_m_Avg)
plot(way4_CM_data[[7]]$Lvl_m_Avg)


# Convert WTD_1_1_1 and WTD_1_2_1 from cm to meters if values are greater than 20.
convert_wtd_to_meters <- function(df) {
  # Check if "WTD_1_1_1" column exists and convert if needed
  if ("WTD_1_1_1" %in% colnames(df)) {
    df <- df %>% mutate(WTD_1_1_1 = ifelse(WTD_1_1_1 > 20, WTD_1_1_1 / 100, WTD_1_1_1))
  }
  
  # Check if "WTD_1_2_1" column exists and convert if needed
  if ("WTD_1_2_1" %in% colnames(df)) {
    df <- df %>% mutate(WTD_1_2_1 = ifelse(WTD_1_2_1 > 20, WTD_1_2_1 / 100, WTD_1_2_1))
  }
  
  return(df)
}

all_processed_data_way3 <- lapply(all_processed_data_way3, convert_wtd_to_meters)
all_processed_data_way4 <- lapply(all_processed_data_way4, convert_wtd_to_meters)



#-------------------------------------------------------------------
#FIX PA
#-------------------------------------------------------------------
plot(all_processed_data_way3[[3]]$TIMESTAMP_START, all_processed_data_way3[[3]]$PA)
# Extract the column
# Extract the PA column
pa_values<-all_processed_data_way3[[3]]$PA  
# Create frequency table sorted in descending order
freq_table<-sort(table(pa_values),decreasing=TRUE)  
# Most common value and its frequency
most_common_value<-names(freq_table)[1]     # 1st most common value
most_common_freq<-freq_table[1]             # Frequency of 1st  
# Second most common value and its frequency
second_common_value<-names(freq_table)[2]   # 2nd most common value
second_common_freq<-freq_table[2]           # Frequency of 2nd  

# Return results as a list
list(
  most_common_value=most_common_value,      # Mode of PA
  most_common_freq=most_common_freq,        # Count of mode
  second_common_value=second_common_value,  # 2nd most frequent value
  second_common_freq=second_common_freq     # Count of 2nd
)


# Replace 100.446 with NA
all_processed_data_way3[[1]]$PA[all_processed_data_way3[[1]]$PA == 100.446] <- NA
all_processed_data_way3[[1]]$PA[all_processed_data_way3[[1]]$PA == 100.4401493] <- NA
all_processed_data_way3[[2]]$PA[all_processed_data_way3[[2]]$PA == 100.4401493] <- NA
all_processed_data_way3[[2]]$PA[all_processed_data_way3[[2]]$PA == 101.3] <- NA
all_processed_data_way3[[3]]$PA[all_processed_data_way3[[3]]$PA == 101.3] <- NA
all_processed_data_way3[[3]]$PA[all_processed_data_way3[[3]]$PA == 100.824] <- NA
all_processed_data_way3[[3]]$PA[all_processed_data_way3[[3]]$PA == 100.525] <- NA
###########################################
########CHECK FOR DUPLICATES#############
############################################

###########################################
########CHECK FOR DUPLICATES#############
############################################
# Function to check for duplicates in TIMESTAMP_START column
check_duplicates <- function(data, name) {
  # Ensure TIMESTAMP_START is numeric
  data$TIMESTAMP_START <- as.numeric(data$TIMESTAMP_START)
  # Check for duplicates in TIMESTAMP_START
  duplicate_timestamps <- which(duplicated(data$TIMESTAMP_START))
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
for (i in seq_along(all_processed_data_way3)) {
  check_duplicates(all_processed_data_way3[[i]], names(all_processed_data_way3)[i])
}

# Check for duplicates in Way4 data
for (i in seq_along(all_processed_data_way4)) {
  check_duplicates(all_processed_data_way4[[i]], names(all_processed_data_way4)[i])
}

# Check for duplicates in Way3 data
for (i in seq_along(way3_filtered_renamed)) {
  check_duplicates(way3_filtered_renamed[[i]], names(way3_filtered_renamed)[i])
}

# Check for duplicates in Way4 data
for (i in seq_along(way4_filtered_renamed)) {
  check_duplicates(way4_filtered_renamed[[i]], names(way4_filtered_renamed)[i])
}
######################################
### Wind direction filter ###########
#####################################
# Define the target columns to be replaced (excluding WD)
columns_to_replace <- c(
  #"CH4_1_1_1", "CH4_MIXING_RATIO", "CO2_MIXING_RATIO", "CO2_1_1_1","H2O_1_1_1", "H2O_MIXING_RATIO", 
  "FCH4",  "FC_1_1_1",
  "FH2O", "H", "LE", "SH", "SLE",
  "FC_SSITC_TEST", "FCH4_SSITC_TEST", "H_SSITC_TEST", "LE_SSITC_TEST",
  "TAU_SSITC_TEST", "CO2_SIGMA", "SC", "SCH4", "ZL", "USTAR", "FETCH_70", "FETCH_90", "FETCH_MAX"
)
# Updated function: only apply NA to rows where WD is outside range and not NA
replace_outside_wind_range <- function(df) {
  if (!"WD" %in% names(df)) return(df)  # skip if WD column is missing
  outside_range <- !is.na(df$WD) & (df$WD < 95 | df$WD > 265)
  existing_columns <- intersect(columns_to_replace, names(df))
  df[outside_range, existing_columns] <- NA
  return(df)
}

# ########################################
# ########Percentile based method########
# ########################################
# ###WAY 3 2018######
# # Define the columns for which percentiles need to be calculated
# columns_to_process <- c("CO2_1_1_1", "CO2_MIXING_RATIO", "FC_1_1_1", "FETCH_MAX", "SC",
#                         "SLE", "TAU", "ZL", "CO2_SIGMA", "MO_LENGTH")
# # Replace outliers with NaN in the same columns
# for (col in columns_to_process) {
#   if (col %in% names(all_processed_data_way3[[1]])) {
#     lower_bound <- quantile(all_processed_data_way3[[1]][[col]], 0.015, na.rm = TRUE)
#     upper_bound <- quantile(all_processed_data_way3[[1]][[col]], 0.985, na.rm = TRUE)
#     all_processed_data_way3[[1]][[col]][all_processed_data_way3[[1]][[col]] < lower_bound |
#                                           all_processed_data_way3[[1]][[col]] > upper_bound] <- NaN
#   } else {
#     warning(paste("Column", col, "not found in the dataframe."))
#   }
# }
# 
# ###WAY 3 2019######
# # Define the columns for which percentiles need to be calculated
# columns_to_process <- c("CO2_SIGMA", "TAU", "U_SIGMA","CO2_MIXING_RATIO","FC_1_1_1","FCH4","H",
#                         "H2O_1_1_1","H2O_MIXING_RATIO","LE","MO_LENGTH", "PA", "FETCH_90", "FETCH_70",
#                         "CH4_1_1_1", "CH4_MIXING_RATIO", "FETCH_MAX", "PA", "SCH4", "ZL") #"CH4", "CH4_MIXING_RATIO",
# for (col in columns_to_process) {
#   if (col %in% names(all_processed_data_way3[[2]])) {
#     lower_bound <- quantile(all_processed_data_way3[[2]][[col]], 0.015, na.rm = TRUE)
#     upper_bound <- quantile(all_processed_data_way3[[2]][[col]], 0.985, na.rm = TRUE)
#     all_processed_data_way3[[2]][[col]][all_processed_data_way3[[2]][[col]] < lower_bound |
#                                           all_processed_data_way3[[2]][[col]] > upper_bound] <- NaN
#   } else {
#     warning(paste("Column", col, "not found in the dataframe."))
#   }
# }
# 
# ###WAY 3 2020######
# # Define the columns for which percentiles need to be calculated
# columns_to_process <- c("FC_1_1_1", "FCH4", "LE", "SWC_1_1_1", "V_SIGMA", "U_SIGMA", "ZL", "MO_LENGTH", "TS_1_1_1")
# for (col in columns_to_process) {
#   if (col %in% names(all_processed_data_way3[[3]])) {
#     lower_bound <- quantile(all_processed_data_way3[[3]][[col]], 0.015, na.rm = TRUE)
#     upper_bound <- quantile(all_processed_data_way3[[3]][[col]], 0.985, na.rm = TRUE)
#     all_processed_data_way3[[3]][[col]][all_processed_data_way3[[3]][[col]] < lower_bound |
#                                           all_processed_data_way3[[3]][[col]] > upper_bound] <- NaN
#   } else {
#     warning(paste("Column", col, "not found in the dataframe."))
#   }
# }
# 
# ###WAY 3 2021######
# # Define the columns for which percentiles need to be calculated
# columns_to_process <- c("CO2_SIGMA", "FC_1_1_1", "FCH4", "FH2O", "H", "LE", "MO_LENGTH", "SLE", "TAU",
#                         "U_SIGMA", "V_SIGMA", "ZL", "TS_1_1_1", "SLE")
# for (col in columns_to_process) {
#   if (col %in% names(all_processed_data_way3[[4]])) {
#     lower_bound <- quantile(all_processed_data_way3[[4]][[col]], 0.015, na.rm = TRUE)
#     upper_bound <- quantile(all_processed_data_way3[[4]][[col]], 0.985, na.rm = TRUE)
#     all_processed_data_way3[[4]][[col]][all_processed_data_way3[[4]][[col]] < lower_bound |
#                                           all_processed_data_way3[[4]][[col]] > upper_bound] <- NaN
#   } else {
#     warning(paste("Column", col, "not found in the dataframe."))
#   }
# }
# 
# ###WAY 3 2022######
# # Define the columns for which percentiles need to be calculated
# columns_to_process <- c("ZL", "TS_1_1_1", "CH4_1_1_1","CH4_MIXING_RATIO", "FCH4", "LE",  "MO_LENGTH","SCH4", "SLE")
# for (col in columns_to_process) {
#   if (col %in% names(all_processed_data_way3[[5]])) {
#     lower_bound <- quantile(all_processed_data_way3[[5]][[col]], 0.015, na.rm = TRUE)
#     upper_bound <- quantile(all_processed_data_way3[[5]][[col]], 0.985, na.rm = TRUE)
#     all_processed_data_way3[[5]][[col]][all_processed_data_way3[[5]][[col]] < lower_bound |
#                                           all_processed_data_way3[[5]][[col]] > upper_bound] <- NaN
#   } else {
#     warning(paste("Column", col, "not found in the dataframe."))
#   }
# }
# 
# ###WAY 3 2023######
# # Define the columns for which percentiles need to be calculated
# columns_to_process <- c("CH4_1_1_1", "CH4_MIXING_RATIO", "FC_1_1_1", "FCH4", "FH2O", "H", "LE", "LW_OUT",
#                         "MO_LENGTH", "SCH4", "SLE", "TAU", "U_SIGMA", "V_SIGMA", "ZL", "TS_1_1_1")
# for (col in columns_to_process) {
#   if (col %in% names(all_processed_data_way3[[6]])) {
#     lower_bound <- quantile(all_processed_data_way3[[6]][[col]], 0.015, na.rm = TRUE)
#     upper_bound <- quantile(all_processed_data_way3[[6]][[col]], 0.985, na.rm = TRUE)
#     all_processed_data_way3[[6]][[col]][all_processed_data_way3[[6]][[col]] < lower_bound |
#                                           all_processed_data_way3[[6]][[col]] > upper_bound] <- NaN
#   } else {
#     warning(paste("Column", col, "not found in the dataframe."))
#   }
# }
# 
# ###WAY 3 2024######
# # Define the columns for which percentiles need to be calculated
# columns_to_process <- c("CH4_1_1_1", "FCH4", "MO_LENGTH", "SCH4", "SLE", "ZL", "SH", "TS_1_1_1",
#                         "CH4_MIXING_RATIO", "H", "TA_1_1_1")
# for (col in columns_to_process) {
#   if (col %in% names(all_processed_data_way3[[7]])) {
#     lower_bound <- quantile(all_processed_data_way3[[7]][[col]], 0.015, na.rm = TRUE)
#     upper_bound <- quantile(all_processed_data_way3[[7]][[col]], 0.985, na.rm = TRUE)
#     all_processed_data_way3[[7]][[col]][all_processed_data_way3[[7]][[col]] < lower_bound |
#                                           all_processed_data_way3[[7]][[col]] > upper_bound] <- NaN
#   } else {
#     warning(paste("Column", col, "not found in the dataframe."))
#   }
# }

# 
# ###WAY 4 2018######
# # Define the columns for which percentiles need to be calculated
# columns_to_process <- c("CO2_SIGMA", "FC_1_1_1", "FCH4", "FH2O", "H", "LE", "MO_LENGTH","SC",
#                         "SLE", "TAU", "V_SIGMA", "U_SIGMA", "W_SIGMA", "ZL")
# 
# # Replace outliers with NaN in the same columns
# for (col in columns_to_process) {
#   if (col %in% names(all_processed_data_way4[[1]])) {
#     lower_bound <- quantile(all_processed_data_way4[[1]][[col]], 0.015, na.rm = TRUE)
#     upper_bound <- quantile(all_processed_data_way4[[1]][[col]], 0.985, na.rm = TRUE)
#     all_processed_data_way4[[1]][[col]][all_processed_data_way4[[1]][[col]] < lower_bound |
#                                           all_processed_data_way4[[1]][[col]] > upper_bound] <- NaN
#   } else {
#     warning(paste("Column", col, "not found in the dataframe."))
#   }
# }
# 
# ###WAY 4 2019######
# # Define the columns for which percentiles need to be calculated
# columns_to_process <- c("FC_1_1_1", "FCH4","FETCH_MAX", "FH2O", "MO_LENGTH", "SLE", "ZL")
# for (col in columns_to_process) {
#   if (col %in% names(all_processed_data_way4[[2]])) {
#     lower_bound <- quantile(all_processed_data_way4[[2]][[col]], 0.015, na.rm = TRUE)
#     upper_bound <- quantile(all_processed_data_way4[[2]][[col]], 0.985, na.rm = TRUE)
#     all_processed_data_way4[[2]][[col]][all_processed_data_way4[[2]][[col]] < lower_bound |
#                                           all_processed_data_way4[[2]][[col]] > upper_bound] <- NaN
#   } else {
#     warning(paste("Column", col, "not found in the dataframe."))
#   }
# }
# 
# ###WAY 4 2020######
# # Define the columns for which percentiles need to be calculated
# columns_to_process <- c("FC_1_1_1", "MO_LENGTH", "ZL")
# for (col in columns_to_process) {
#   if (col %in% names(all_processed_data_way4[[3]])) {
#     lower_bound <- quantile(all_processed_data_way4[[3]][[col]], 0.015, na.rm = TRUE)
#     upper_bound <- quantile(all_processed_data_way4[[3]][[col]], 0.985, na.rm = TRUE)
#     all_processed_data_way4[[3]][[col]][all_processed_data_way4[[3]][[col]] < lower_bound |
#                                           all_processed_data_way4[[3]][[col]] > upper_bound] <- NaN
#   } else {
#     warning(paste("Column", col, "not found in the dataframe."))
#   }
# }
# 
# ###WAY 4 2021######
# # Define the columns for which percentiles need to be calculated
# columns_to_process <- c("CO2_MIXING_RATIO", "CO2_1_1_1", "CO2_SIGMA", "FC_1_1_1", "FH2O", "H","LE", "MO_LENGTH", "SC","SH", "ZL",
#                         "FCH4")
# for (col in columns_to_process) {
#   if (col %in% names(all_processed_data_way4[[4]])) {
#     lower_bound <- quantile(all_processed_data_way4[[4]][[col]], 0.015, na.rm = TRUE)
#     upper_bound <- quantile(all_processed_data_way4[[4]][[col]], 0.985, na.rm = TRUE)
#     all_processed_data_way4[[4]][[col]][all_processed_data_way4[[4]][[col]] < lower_bound |
#                                           all_processed_data_way4[[4]][[col]] > upper_bound] <- NaN
#   } else {
#     warning(paste("Column", col, "not found in the dataframe."))
#   }
# }
# 
# ###WAY 4 2022######
# # Define the columns for which percentiles need to be calculated
# columns_to_process <- c("CO2_MIXING_RATIO", "CO2_1_1_1", "CO2_SIGMA", "FC_1_1_1", "FH2O", "H","LE", "MO_LENGTH", "SC","SH", "ZL",
#                         "FCH4", "CH4_1_1_1", "CH4_MIXING_RATIO", "SCH4", "SLE")
# for (col in columns_to_process) {
#   if (col %in% names(all_processed_data_way4[[5]])) {
#     lower_bound <- quantile(all_processed_data_way4[[5]][[col]], 0.015, na.rm = TRUE)
#     upper_bound <- quantile(all_processed_data_way4[[5]][[col]], 0.985, na.rm = TRUE)
#     all_processed_data_way4[[5]][[col]][all_processed_data_way4[[5]][[col]] < lower_bound |
#                                           all_processed_data_way4[[5]][[col]] > upper_bound] <- NaN
#   } else {
#     warning(paste("Column", col, "not found in the dataframe."))
#   }
# }
# 
# ###WAY 4 2023######
# # Define the columns for which percentiles need to be calculated
# columns_to_process <- c("CH4_MIXING_RATIO", "CH4_1_1_1", "FCH4", "TA_1_1_1", "W_SIGMA", "ZL", "H", "MO_LENGTH", "SCH4", "SLE")
# for (col in columns_to_process) {
#   if (col %in% names(all_processed_data_way4[[6]])) {
#     lower_bound <- quantile(all_processed_data_way4[[6]][[col]], 0.015, na.rm = TRUE)
#     upper_bound <- quantile(all_processed_data_way4[[6]][[col]], 0.985, na.rm = TRUE)
#     all_processed_data_way4[[6]][[col]][all_processed_data_way4[[6]][[col]] < lower_bound |
#                                           all_processed_data_way4[[6]][[col]] > upper_bound] <- NaN
#   } else {
#     warning(paste("Column", col, "not found in the dataframe."))
#   }
# }
# 
# ###WAY 4 2024######
# # Define the columns for which percentiles need to be calculated
# columns_to_process <- c("H", "LE", "LW_IN", "MO_LENGTH",  "PPFD_IN", "SLE",  "SH", "SLE", "TAU", "ZL", "W_SIGMA","U_SIGMA")
# for (col in columns_to_process) {
#   if (col %in% names(all_processed_data_way4[[7]])) {
#     lower_bound <- quantile(all_processed_data_way4[[7]][[col]], 0.015, na.rm = TRUE)
#     upper_bound <- quantile(all_processed_data_way4[[7]][[col]], 0.985, na.rm = TRUE)
#     all_processed_data_way4[[7]][[col]][all_processed_data_way4[[7]][[col]] < lower_bound |
#                                           all_processed_data_way4[[7]][[col]] > upper_bound] <- NaN
#   } else {
#     warning(paste("Column", col, "not found in the dataframe."))
#   }
# }

###############################################################
##############PHYSICAL RANGE FROM SUBMISSION###################
###############################################################
replace_out_of_range_values <- function(df) {
  # Define the updated physical ranges for the relevant columns
  ranges <- list(
    CO2_1_1_1 = c(150, 1200),
    CO2_MIXING_RATIO = c(150, 1200),
    FC_1_1_1 = c(-100, 100),
    CO2_SIGMA = c(0, 150),
    SC = c(-110, 110),
    FCH4 = c(-500, 4000),
    CH4_1_1_1 = c(0, 15000),
    CH4_MIXING_RATIO = c(0, 15000),
    H2O_1_1_1 = c(0, 100),
    H2O_MIXING_RATIO = c(0, 100),
    FH2O = c(-10, 20),
    FETCH_MAX = c(0, 5000),
    FETCH_70 = c(0, 10000),
    FETCH_90 = c(0, 15000),
    H = c(-450, 900),
    SH = c(-165, 165),
    LE = c(-450, 900),
    SLE = c(-150, 150),
    PA = c(60, 105),
    VPD = c(0, 80),
    V_SIGMA = c(0, 10),
    U_SIGMA = c(0, 8),
    W_SIGMA = c(0, 5),
    TAU = c(-10, 2),
    LW_IN = c(50, 600),
    LW_OUT = c(100, 750),
    SWC_1_1_1 = c(0, 100),
    TS_1_1_1 = c(-40, 65),
    TA_1_1_1 = c(-50, 50),
    PPFD_IN = c(0, 2400)
  )
  
  # Replace values outside of range with NA
  for (col in names(ranges)) {
    if (col %in% colnames(df)) {
      min_val <- ranges[[col]][1]
      max_val <- ranges[[col]][2]
      df <- df %>% mutate(
        !!col := ifelse(get(col) < min_val | get(col) > max_val, NA, get(col))
      )
    }
  }
  return(df)
}

###TIMESTAMP START AND END AFTER ADJUSTING TIMESTAMP###
all_processed_data_way3 <- lapply(all_processed_data_way3, replace_out_of_range_values)
all_processed_data_way4 <- lapply(all_processed_data_way4, replace_out_of_range_values)
max(all_processed_data_way4[[1]]$FETCH_MAX, na.rm = TRUE)

# Apply to both lists of data frames
all_processed_data_way3 <- lapply(all_processed_data_way3, replace_outside_wind_range)
all_processed_data_way4 <- lapply(all_processed_data_way4, replace_outside_wind_range)

#=========================================
#Temperature outliers
#=========================================
find_outliers_in_8pt_window <- function(x, threshold = 5, temp_threshold = -15) {
  x <- as.numeric(x)
  n <- length(x)
  outlier_flags <- rep(FALSE, n)
  
  # First pass: flag extreme cold temperatures as outliers
  outlier_flags[!is.na(x) & x < temp_threshold] <- TRUE
  
  # Second pass: 8-point window method (only check non-NA and non-cold-outlier points)
  for (i in 4:(n - 3)) {
    if (!outlier_flags[i] && !is.na(x[i])) {  # Only check if not already flagged
      window <- x[(i - 3):(i + 4)]
      prev <- x[i - 1]
      next_val <- x[i + 1]
      
      if (!any(is.na(c(prev, next_val)))) {
        # Check if current point is much higher/lower than both neighbors
        if (abs(x[i] - prev) > threshold && abs(x[i] - next_val) > threshold) {
          outlier_flags[i] <- TRUE
        }
      }
    }
  }
  
  # Third pass: if a point is < 0 AND surrounded by outliers, mark it too
  for (i in 2:(n - 1)) {
    if (!outlier_flags[i] && !is.na(x[i]) && x[i] < 0 &&
        outlier_flags[i - 1] && outlier_flags[i + 1]) {
      outlier_flags[i] <- TRUE
    }
  }
  
  return(which(outlier_flags))
}
# Run on the specified vectors
out_w3_2023 <- find_outliers_in_8pt_window(all_processed_data_way3[[5]]$TA_1_1_1)
out_w3_2024 <- find_outliers_in_8pt_window(all_processed_data_way3[[6]]$TA_1_1_1)
out_w4_2023 <- find_outliers_in_8pt_window(all_processed_data_way4[[5]]$TA_1_1_1)
out_w4_2024 <- find_outliers_in_8pt_window(all_processed_data_way4[[6]]$TA_1_1_1)

# Print number of outliers
cat("Way3 2023 outliers:", length(out_w3_2023), "\n")
cat("Way3 2024 outliers:", length(out_w3_2024), "\n")
cat("Way4 2023 outliers:", length(out_w4_2023), "\n")
cat("Way4 2024 outliers:", length(out_w4_2024), "\n")

library(ggplot2)

plot_temperature_with_outliers <- function(df, varname, outlier_indices, title_text) {
  # Create data frame for plotting
  df$OutlierFlag <- FALSE
  df$OutlierFlag[outlier_indices] <- TRUE
  
  ggplot(df, aes(x = TIMESTAMP, y = .data[[varname]])) +
    geom_point(aes(color = OutlierFlag), size = 1.3, alpha = 0.7) +
    scale_color_manual(values = c("FALSE" = "skyblue", "TRUE" = "red")) +
    labs(title = title_text, x = "Date", y = varname, color = "Outlier") +
    theme_minimal()
}
# Plot each one
plot_temperature_with_outliers(all_processed_data_way3[[5]], "TA_1_1_1", out_w3_2023, "Way3 2023 Temperature with Outliers")
plot_temperature_with_outliers(all_processed_data_way3[[6]], "TA_1_1_1", out_w3_2024, "Way3 2024 Temperature with Outliers")
plot_temperature_with_outliers(all_processed_data_way4[[5]], "TA_1_1_1", out_w4_2023, "Way4 2023 Temperature with Outliers")
plot_temperature_with_outliers(all_processed_data_way4[[6]], "TA_1_1_1", out_w4_2024, "Way4 2024 Temperature with Outliers")

# Function to replace outliers by NaN in a data frame column
replace_outliers_with_NaN <- function(df, varname, outlier_indices) {
  df[[varname]][outlier_indices] <- NaN
  return(df)
}

# Replace outliers with NaN
all_processed_data_way3[[5]] <- replace_outliers_with_NaN(all_processed_data_way3[[5]], "TA_1_1_1", out_w3_2023)
all_processed_data_way3[[6]] <- replace_outliers_with_NaN(all_processed_data_way3[[6]], "TA_1_1_1", out_w3_2024)
all_processed_data_way4[[5]] <- replace_outliers_with_NaN(all_processed_data_way4[[5]], "TA_1_1_1", out_w4_2023)
all_processed_data_way4[[6]] <- replace_outliers_with_NaN(all_processed_data_way4[[6]], "TA_1_1_1", out_w4_2024)

# Now plot with outliers replaced by NaN
plot_temperature_with_outliers(all_processed_data_way3[[5]], "TA_1_1_1", integer(0), "Way3 2023 Temperature with Outliers Replaced")
plot_temperature_with_outliers(all_processed_data_way3[[6]], "TA_1_1_1", integer(0), "Way3 2024 Temperature with Outliers Replaced")
plot_temperature_with_outliers(all_processed_data_way4[[5]], "TA_1_1_1", integer(0), "Way4 2023 Temperature with Outliers Replaced")
plot_temperature_with_outliers(all_processed_data_way4[[6]], "TA_1_1_1", integer(0), "Way4 2024 Temperature with Outliers Replaced")

########################################
########PLOTTING ALL THE COLUMNS########
########################################
plot_way_data_with_units <- function(way_data, base_dir, way_name, unit_row) {
  # Convert -9999 to NA
  way_data <- lapply(way_data, function(df) {
    df[df == -9999] <- NA
    return(df)
  })
  # List of years for processing
  years <- c("2018", "2019", "2020", "2021", "2022", "2023", "2024")
  # Loop through each year
  for (year in years) {
    # Construct the correct key format
    data_key <- paste0(way_name, "_", year, ".csv")
    # Check if the key exists in the list
    if (!data_key %in% names(way_data)) {
      warning(paste("Data for", year, "not found. Skipping..."))
      next
    }
    # Access the dataframe for the corresponding year
    df <- way_data[[data_key]]
    # Create the output directory for the year if it doesn't exist
    output_dir <- file.path(base_dir, year)
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE)
    }
    # Get the column names except TIMESTAMP
    column_names <- setdiff(names(df), "TIMESTAMP")
    # Loop through each column and plot against TIMESTAMP
    for (col in column_names) {
      # Find the unit for the current column
      unit <- ifelse(col %in% names(unit_row), unit_row[[col]], "")
      unit_label <- ifelse(unit == "", "", paste0(" (", unit, ")"))
      # Create the plot with points (instead of lines)
      p <- ggplot(df, aes(x = TIMESTAMP, y = .data[[col]])) +
        geom_point(na.rm = TRUE) +
        labs(
          title = paste("Plot of", col, "in", year, "from", way_name), 
          x = "Timestamp",
          y = paste0(col, unit_label)
        ) +
        theme_minimal()
      # Save the plot to the corresponding directory
      ggsave(filename = file.path(output_dir, paste0(col, "_", year, ".jpeg")), 
             plot = p, width = 8, height = 4)
    }
  }
}
# Paths where you want to save the plots for Way3 and Way4
base_dir_way3 <- "C:/Users/rbmahbub/Documents/RProjects/AmerifluxDataSubmission_LandscapeFlux/Figure/Way3Columns/"
base_dir_way4 <- "C:/Users/rbmahbub/Documents/RProjects/AmerifluxDataSubmission_LandscapeFlux/Figure/Way4Columns/"

# Apply the function for Way3 and Way4
plot_way_data_with_units(all_processed_data_way3, base_dir_way3, "Way3", filtered_unit_row)
plot_way_data_with_units(all_processed_data_way4, base_dir_way4, "Way4", filtered_unit_row)


##################################################
####Few last decembers in the new year 2022 in 2023######
##################################################
all_processed_data_way3[[7]]$TIMESTAMP

# Define target years for each list element
target_years <- 2018:2024

# Loop through the list and filter TIMESTAMP year for each corresponding element
for (i in seq_along(target_years)) {
  year_i <- target_years[i]
  all_processed_data_way3[[i]] <- all_processed_data_way3[[i]][
    format(as.Date(all_processed_data_way3[[i]]$TIMESTAMP), "%Y") == year_i, 
  ]
}
# Loop through the list and filter TIMESTAMP year for each corresponding element
for (i in seq_along(target_years)) {
  year_i <- target_years[i]
  all_processed_data_way4[[i]] <- all_processed_data_way4[[i]][
    format(as.Date(all_processed_data_way4[[i]]$TIMESTAMP), "%Y") == year_i, 
  ]
}

# plot(way4_CM_data[[2]]$TIMESTAMP, way4_CM_data[[2]]$LAI_corrected_gapfilled)
# plot(all_processed_data_way4[[2]]$TIMESTAMP, all_processed_data_way4[[2]]$LAI_corrected_gapfilled)
# View(all_processed_data_way4[[2]])
####TIMESTAMP START AND END FUNCTION

# Check for duplicates in Way3 data
for (i in seq_along(all_processed_data_way3)) {
  check_duplicates(all_processed_data_way3[[i]], names(all_processed_data_way3)[i])
}

# Check for duplicates in Way4 data
for (i in seq_along(all_processed_data_way4)) {
  check_duplicates(all_processed_data_way4[[i]], names(all_processed_data_way4)[i])
}

process_data <- function(data) {
  # Ensure TIMESTAMP is in POSIXct format
  data$TIMESTAMP <- ymd_hms(data$TIMESTAMP)
  # Create or replace TIMESTAMP_START and TIMESTAMP_END
  data$TIMESTAMP_START <- format(data$TIMESTAMP, "%Y%m%d%H%M")
  data$TIMESTAMP_END <- format(data$TIMESTAMP + minutes(30), "%Y%m%d%H%M")
  # Create or replace additional columns: HOUR, MONTH, DAY_OF_YEAR
  data$HOUR <- hour(data$TIMESTAMP)
  data$MONTH <- month(data$TIMESTAMP)
  data$DOY <- yday(data$TIMESTAMP)
  data <- data[!duplicated(data$TIMESTAMP_START), ]# Remove duplicates after processing based on TIMESTAMP_START
  return(data)
}

###TIMESTAMP START AND END AFTER ADJUSTING TIMESTAMP###
all_processed_data_way3 <- lapply(all_processed_data_way3, process_data)
all_processed_data_way4 <- lapply(all_processed_data_way4, process_data)

# Check for duplicates in Way3 data
for (i in seq_along(all_processed_data_way3)) {
  check_duplicates(all_processed_data_way3[[i]], names(all_processed_data_way3)[i])
}

# Check for duplicates in Way4 data
for (i in seq_along(all_processed_data_way4)) {
  check_duplicates(all_processed_data_way4[[i]], names(all_processed_data_way4)[i])
}
#save data for ruben
all_processed_data_way3[[6]]
# Define output path
output_path <- "C:/Users/rbmahbub/Documents/RProjects/AmerifluxDataSubmission_LandscapeFlux/Data/OutputLocalProcessedData_AFguidedSubmitted/RubenPPFD_IN/Way3_2023.csv"
# Save the 6th dataframe as CSV
write.csv(all_processed_data_way3[[6]], file = output_path, row.names = FALSE)


# Columns to drop
columns_to_drop <- c("DOY", "HOUR", "DOY_HOUR", "SW_IN_POT", "PERIOD", "TIMESTAMP", 
                     "PPFD_IN_Adjusted", "MONTH", "PPFD_IN", "PPFD_OUT")

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

####################################
#####Dropping columns#####################
###CH42019###LE2020 Way 3
#############################################
# Remove columns from all_processed_data_way3[[2]]
all_processed_data_way3[[2]] <- all_processed_data_way3[[2]][, !(names(all_processed_data_way3[[2]]) %in% c("CH4_1_1_1", "CH4_MIXING_RATIO"))]
# Note about removed columns
removed_columns_notes_2019 <- "Way3_2019.csv: Dropped columns - CH4 (Methane flux), CH4_MIXING_RATIO (Methane mixing ratio)"
# Remove columns from all_processed_data_way3[[3]]
all_processed_data_way3[[3]] <- all_processed_data_way3[[3]][, !(names(all_processed_data_way3[[3]]) %in% c("LE", "SLE", "LE_SSITC_TEST"))]
# Note about removed columns
removed_columns_notes_2020 <- "Way3_2020.csv: Dropped columns - LE (Latent heat flux), SLE (Stored latent energy), LE_SSITC_TEST (Latent heat flux quality control test)"
# Print notes
print(removed_columns_notes_2019)
print(removed_columns_notes_2020)

##################################################
##############SAVE for lab#######################
##################################################
# Define save directories
way3_save_directory1 <- "C:/Users/rbmahbub/Documents/RProjects/AmerifluxDataSubmission_LandscapeFlux/Data/OutputLocalProcessedData_AFguidedSubmitted/ForLab/Way3"
way3_save_directory2 <- "C:/Users/rbmahbub/Box/Field_Data/AmeriFlux_Submission_Figures/OutputLocalProcessedData_AFguidedSubmitted/ForLabResearachPurposeMoreColumns/Way3"

way4_save_directory1 <- "C:/Users/rbmahbub/Documents/RProjects/AmerifluxDataSubmission_LandscapeFlux/Data/OutputLocalProcessedData_AFguidedSubmitted/ForLab/Way4"
way4_save_directory2 <- "C:/Users/rbmahbub/Box/Field_Data/AmeriFlux_Submission_Figures/OutputLocalProcessedData_AFguidedSubmitted/ForLabResearachPurposeMoreColumns/Way4"

save_filtered_data <- function(data, filename, directories) {
  # Force character conversion and suppress scientific notation
  options(scipen = 999)  # Avoid scientific notation globally
  # Ensure timestamps are treated as strings
  data$TIMESTAMP_START <- format(data$TIMESTAMP_START, scientific = FALSE)
  data$TIMESTAMP_END   <- format(data$TIMESTAMP_END, scientific = FALSE)
  # Replace NA and NaN with -9999
  data[is.na(data)] <- -9999
  data[is.nan.data.frame(data)] <- -9999
  for (dir in directories) {
    file_path <- file.path(dir, filename)
    write.csv(data, file = file_path, row.names = FALSE, quote = TRUE)
    cat(filename, "saved successfully to", dir, "\n")
  }
}

# Assign names to the processed data
names(all_processed_data_way3) <- way3_files_to_read
names(all_processed_data_way4) <- way4_files_to_read

# Save Way3 data to both directories
for (i in seq_along(all_processed_data_way3)) {
  filename <- names(all_processed_data_way3)[i]
  save_filtered_data(all_processed_data_way3[[i]], filename, c(way3_save_directory1, way3_save_directory2))
}

# Save Way4 data to both directories
for (i in seq_along(all_processed_data_way4)) {
  filename <- names(all_processed_data_way4)[i]
  save_filtered_data(all_processed_data_way4[[i]], filename, c(way4_save_directory1, way4_save_directory2))
}

#######################################
####Drop the columns LAI, CanopyHeight#######
##########################################
# Columns to drop
columns_to_drop <- c(
  "canopy_height_gapfilled", 
  "LAI_corrected_gapfilled", 
  "LAI_corrected", 
  "canopy_height", 
  "mean_value_RSSI_LI_7500_",
  "rssi_77_mean_",
  "rand_err_ch4_flux",
  "rand_err_Tau",
  "rand_err_H",
  "rand_err_LE",
  "rand_err_co2_flux",
  "rand_err_h2o_flux",
  "TS_mean_3",
  "TS_mean_4",
  "del_TS_2",
  "del_TS_3",
  "del_TS_4",
  "shf_Avg_1",
  "shf_Avg_2",
  "shf_Avg_3",
  "panel_tmpr_Avg",
  "BattV_Avg"
)

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


###########################################
########CHECK FOR DUPLICATES#############
############################################
# Function to check for duplicates in TIMESTAMP_START column
check_duplicates <- function(data, name) {
  # Ensure TIMESTAMP_START is numeric
  data$TIMESTAMP_START <- as.numeric(data$TIMESTAMP_START)
  # Check for duplicates in TIMESTAMP_START
  duplicate_timestamps <- which(duplicated(data$TIMESTAMP_START))
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
for (i in seq_along(all_processed_data_way3)) {
  check_duplicates(all_processed_data_way3[[i]], names(all_processed_data_way3)[i])
}

# Check for duplicates in Way4 data
for (i in seq_along(all_processed_data_way4)) {
  check_duplicates(all_processed_data_way4[[i]], names(all_processed_data_way4)[i])
}



###############################################
####These variables have all data missing: WTD_1_2_1. Previously uploaded data with the same time period will be overwritten.
###############################################
# Function to check columns for -9999, NA, or specific missing data string and store them
find_and_remove_missing_columns <- function(data, name, missing_string) {
  missing_cols <- colnames(data)[colSums(is.na(data)) == nrow(data)]
  data <- data[, !(colnames(data) %in% missing_cols)]
  attr(data, "removed_columns") <- missing_cols
  return(data)
}

missing_string <- "Any Variables with ALL Data Missing?"
all_processed_data_way3 <- lapply(all_processed_data_way3, function(data, name) find_and_remove_missing_columns(data, name, missing_string), names(all_processed_data_way3))
all_processed_data_way4 <- lapply(all_processed_data_way4, function(data, name) find_and_remove_missing_columns(data, name, missing_string), names(all_processed_data_way4))

for (i in seq_along(all_processed_data_way3)) {
  removed_columns <- attr(all_processed_data_way3[[i]], "removed_columns")
  cat(if(length(removed_columns) > 0) paste("Removed columns from", names(all_processed_data_way3)[i], ":", removed_columns) else paste("No columns removed from", names(all_processed_data_way3)[i]), "\n")
}
for (i in seq_along(all_processed_data_way4)) {
  removed_columns <- attr(all_processed_data_way4[[i]], "removed_columns")
  cat(if(length(removed_columns) > 0) paste("Removed columns from", names(all_processed_data_way4)[i], ":", removed_columns) else paste("No columns removed from", names(all_processed_data_way4)[i]), "\n")
}
 


df<-all_processed_data_way3[[4]]
# Count rows with 12-character TIMESTAMP_START
rows_with_12_start <- df[nchar(df$TIMESTAMP_START) == 12, ]
n_12_start <- nrow(rows_with_12_start)
# Count rows with 12-character TIMESTAMP_END
rows_with_12_end <- df[nchar(df$TIMESTAMP_END) == 12, ]
n_12_end <- nrow(rows_with_12_end)
# Total rows
total_rows <- nrow(df)
# Output results
cat("Rows with 12-char TIMESTAMP_START:", n_12_start, "\n")
cat("Rows with 12-char TIMESTAMP_END:  ", n_12_end, "\n")
cat("Total rows in data:               ", total_rows, "\n")
# Find rows where TIMESTAMP_START is not 12 characters
bad_start_rows <- df[nchar(df$TIMESTAMP_START) != 12, ]
# Find rows where TIMESTAMP_END is not 12 characters
bad_end_rows <- df[nchar(df$TIMESTAMP_END) != 12, ]
# Combine unique rows from both
bad_rows <- unique(rbind(bad_start_rows, bad_end_rows))
# View how many and which ones
nrow(bad_rows)
##################################################
##############SAVE for AMERIFLUX#######################
##################################################

# Define save directories for Way3
way3_save_dirs <- c(
  "C:/Users/rbmahbub/Documents/RProjects/AmerifluxDataSubmission_LandscapeFlux/Data/OutputLocalProcessedData_AFguidedSubmitted/Way3",
  "C:/Users/rbmahbub/Box/Field_Data/AmeriFlux_Submission_Figures/OutputLocalProcessedData_AFguidedSubmitted/AmerifluxSubmission/Way3"
)

# Define save directories for Way4
way4_save_dirs <- c(
  "C:/Users/rbmahbub/Documents/RProjects/AmerifluxDataSubmission_LandscapeFlux/Data/OutputLocalProcessedData_AFguidedSubmitted/Way4",
  "C:/Users/rbmahbub/Box/Field_Data/AmeriFlux_Submission_Figures/OutputLocalProcessedData_AFguidedSubmitted/AmerifluxSubmission/Way4"
)

save_filtered_data <- function(data, filename, directories, site_id, resolution) {
  
  # Clean column names
  colnames(data) <- gsub(" ", "_", colnames(data))
  colnames(data) <- gsub('"', '', colnames(data))
  
  # Ensure TIMESTAMP columns stay as character
  data$TIMESTAMP_START <- as.character(data$TIMESTAMP_START)
  data$TIMESTAMP_END   <- as.character(data$TIMESTAMP_END)
  
  # Remove malformed timestamps
  data <- data[nchar(data$TIMESTAMP_START) == 12 &
                 nchar(data$TIMESTAMP_END)   == 12, ]
  
  # Replace missing values
  data[is.na(data)] <- -9999
  data[is.nan.data.frame(data)] <- -9999
  
  # Get full 12-digit timestamp range
  ts_start <- min(data$TIMESTAMP_START)
  ts_end   <- max(data$TIMESTAMP_END)
  
  # Construct filename
  new_filename <- paste0(site_id, "_", resolution, "_", ts_start, "_", ts_end, ".csv")
  
  # Save
  for (dir in directories) {
    file_path <- file.path(dir, new_filename)
    write.csv(data, file = file_path, row.names = FALSE, quote = FALSE)
    cat(new_filename, "saved successfully to", dir, "\n")
  }
}


# Assign names to processed data
names(all_processed_data_way3) <- way3_files_to_read
names(all_processed_data_way4) <- way4_files_to_read

# Define resolution
resolution <- "HH"

# Save Way3 data to both locations
for (i in seq_along(all_processed_data_way3)) {
  filename <- names(all_processed_data_way3)[i]
  site_id <- "US-HRC"
  save_filtered_data(all_processed_data_way3[[i]], filename, way3_save_dirs, site_id, resolution)
}

# Save Way4 data to both locations
for (i in seq_along(all_processed_data_way4)) {
  filename <- names(all_processed_data_way4)[i]
  site_id <- "US-HRA"
  save_filtered_data(all_processed_data_way4[[i]], filename, way4_save_dirs, site_id, resolution)
}

#------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------
# END OF THE CODE
#------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------


# Now you can use cleaned_way3_data and cleaned_way4_data in the following code

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
### organised code for percentile method
# Initialize a list to store outlier counts for each year
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
outlier_count_way3 <- vector("list", length(all_processed_data_way3))
# Loop through each year (dataframe)
for (i in seq_along(all_processed_data_way3)) {
  df <- all_processed_data_way3[[i]]
  # Define year-specific columns
  columns_to_process <- switch(i,
                               # Year 1: 2018
                               c("CO2_1_1_1", "CO2_MIXING_RATIO", "FC_1_1_1", "FETCH_MAX", "SC", "SLE", "TAU", "ZL", "CO2_SIGMA", "MO_LENGTH"),
                               # Year 2: 2019
                               c("CO2_SIGMA", "TAU", "U_SIGMA","CO2_MIXING_RATIO","FC_1_1_1","FCH4","H",
                                 "H2O_1_1_1","H2O_MIXING_RATIO","LE","MO_LENGTH", "PA", "FETCH_90", "FETCH_70",
                                 "CH4_1_1_1", "CH4_MIXING_RATIO", "FETCH_MAX", "PA", "SCH4", "ZL"),
                               # Year 3: 2020
                               c("FC_1_1_1", "FCH4", "LE", "SWC_1_1_1", "V_SIGMA", "U_SIGMA", "ZL", "MO_LENGTH", "TS_1_1_1"),
                               # Year 4: 2021
                               c("CO2_SIGMA", "FC_1_1_1", "FCH4", "FH2O", "H", "LE", "MO_LENGTH", "SLE", "TAU",
                                 "U_SIGMA", "V_SIGMA", "ZL", "TS_1_1_1", "SLE"),
                               # Year 5: 2022
                               c("ZL", "TS_1_1_1", "CH4_1_1_1","CH4_MIXING_RATIO", "FCH4", "LE", "MO_LENGTH","SCH4", "SLE"),
                               # Year 6: 2023
                               c("CH4_1_1_1", "CH4_MIXING_RATIO", "FC_1_1_1", "FCH4", "FH2O", "H", "LE", "LW_OUT",
                                 "MO_LENGTH", "SCH4", "SLE", "TAU", "U_SIGMA", "V_SIGMA", "ZL", "TS_1_1_1"),
                               # Year 7: 2024
                               c("CH4_1_1_1", "FCH4", "MO_LENGTH", "SCH4", "SLE", "ZL", "SH", "TS_1_1_1",
                                 "CH4_MIXING_RATIO", "H", "TA_1_1_1")
  )
  
  # Initialize a named vector for this year's outlier counts
  outlier_counts <- setNames(numeric(length(columns_to_process)), columns_to_process)
  
  # Replace outliers with NaN and count
  for (col in columns_to_process) {
    if (col %in% names(df)) {
      lower_bound <- quantile(df[[col]], 0.015, na.rm = TRUE)
      upper_bound <- quantile(df[[col]], 0.985, na.rm = TRUE)
      is_outlier <- df[[col]] < lower_bound | df[[col]] > upper_bound
      outlier_counts[col] <- sum(is_outlier, na.rm = TRUE)
      df[[col]][is_outlier] <- NaN
    } else {
      warning(paste("Column", col, "not found in the dataframe."))
      outlier_counts[col] <- NA  # Mark as NA for clarity
    }
  }
  
  # Save cleaned data and outlier count
  all_processed_data_way3[[i]] <- df
  outlier_count_way3[[i]] <- outlier_counts
}
outlier_count_way3

outlier_count_way3
all_cols <- unique(unlist(lapply(outlier_count_way3, function(x) {# Step 1: Get all unique column names after cleaning
  trimws(names(x))# Trim whitespace and standardize names
})))

# Step 2: Convert each vector to a data frame with consistent columns
list_of_dfs <- lapply(outlier_count_way3, function(x) {
  current_names <- trimws(names(x))# Clean the names of the current vector
  names(x) <- current_names
  df <- as.data.frame(t(unlist(x)))# Create 1-row data frame
  missing_cols <- setdiff(all_cols, current_names)# Add missing columns as NA
  df[missing_cols] <- NA
  df <- df[, all_cols, drop = FALSE]# Reorder columns to match all_cols (important for rbind)
  df
})
tableoutlier_count_way3 <- do.call(rbind, list_of_dfs)# Step 3: Combine all data frames
rownames(tableoutlier_count_way3) <- NULL# Reset row names
years <- 2018:(2018 + nrow(tableoutlier_count_way3) - 1)# Calculate years (assuming 7 rows total, from 2018 to 2024)
tableoutlier_count_way3 <- cbind(year = years, tableoutlier_count_way3)# Add the year column as the first column
write.csv(tableoutlier_count_way3, "C:/Users/rbmahbub/Documents/RProjects/AmerifluxDataSubmission_LandscapeFlux/Data/PercentileMethodFilteringlist/tableoutlier_count_way3.csv", row.names = TRUE)


### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
# Initialize a list to store outlier counts for each year
outlier_count_way4 <- vector("list", length(all_processed_data_way4))

# Loop through each year (dataframe)
for (i in seq_along(all_processed_data_way4)) {
  df <- all_processed_data_way4[[i]]
  
  # Define year-specific columns
  columns_to_process <- switch(i,
                               # Year 1: 2018
                               c("CO2_SIGMA", "FC_1_1_1", "FCH4", "FH2O", "H", "LE", "MO_LENGTH", "SC", "SLE", "TAU", "V_SIGMA", "U_SIGMA", "W_SIGMA", "ZL"),
                               # Year 2: 2019
                               c("FC_1_1_1", "FCH4", "FETCH_MAX", "FH2O", "MO_LENGTH", "SLE", "ZL"),
                               # Year 3: 2020
                               c("FC_1_1_1", "MO_LENGTH", "ZL"),
                               # Year 4: 2021
                               c("CO2_MIXING_RATIO", "CO2_1_1_1", "CO2_SIGMA", "FC_1_1_1", "FH2O", "H", "LE", "MO_LENGTH", "SC", "SH", "ZL", "FCH4"),
                               # Year 5: 2022
                               c("CO2_MIXING_RATIO", "CO2_1_1_1", "CO2_SIGMA", "FC_1_1_1", "FH2O", "H", "LE", "MO_LENGTH", "SC", "SH", "ZL", "FCH4", "CH4_1_1_1", "CH4_MIXING_RATIO", "SCH4", "SLE"),
                               # Year 6: 2023
                               c("CH4_MIXING_RATIO", "CH4_1_1_1", "FCH4", "TA_1_1_1", "W_SIGMA", "ZL", "H", "MO_LENGTH", "SCH4", "SLE"),
                               # Year 7: 2024
                               c("H", "LE", "LW_IN", "MO_LENGTH", "PPFD_IN", "SLE", "SH", "SLE", "TAU", "ZL", "W_SIGMA", "U_SIGMA")
  )
  
  # Initialize a named vector for this year's outlier counts
  outlier_counts <- setNames(numeric(length(columns_to_process)), columns_to_process)
  
  # Replace outliers with NaN and count
  for (col in columns_to_process) {
    if (col %in% names(df)) {
      lower_bound <- quantile(df[[col]], 0.015, na.rm = TRUE)
      upper_bound <- quantile(df[[col]], 0.985, na.rm = TRUE)
      is_outlier <- df[[col]] < lower_bound | df[[col]] > upper_bound
      outlier_counts[col] <- sum(is_outlier, na.rm = TRUE)
      df[[col]][is_outlier] <- NaN
    } else {
      warning(paste("Column", col, "not found in the dataframe."))
      outlier_counts[col] <- NA  # Mark as NA for clarity
    }
  }
  
  # Save cleaned data and outlier count
  all_processed_data_way4[[i]] <- df
  outlier_count_way4[[i]] <- outlier_counts
}

outlier_count_way4
all_cols <- unique(unlist(lapply(outlier_count_way4, function(x) {# Step 1: Get all unique column names after cleaning
  trimws(names(x))# Trim whitespace and standardize names
})))

# Step 2: Convert each vector to a data frame with consistent columns
list_of_dfs <- lapply(outlier_count_way4, function(x) {
  current_names <- trimws(names(x))# Clean the names of the current vector
  names(x) <- current_names
  df <- as.data.frame(t(unlist(x)))# Create 1-row data frame
  missing_cols <- setdiff(all_cols, current_names)# Add missing columns as NA
  df[missing_cols] <- NA
  df <- df[, all_cols, drop = FALSE]# Reorder columns to match all_cols (important for rbind)
  df
})
tableoutlier_count_way4 <- do.call(rbind, list_of_dfs)# Step 3: Combine all data frames
rownames(tableoutlier_count_way4) <- NULL# Reset row names
years <- 2018:(2018 + nrow(tableoutlier_count_way4) - 1)# Calculate years (assuming 7 rows total, from 2018 to 2024)
tableoutlier_count_way4 <- cbind(year = years, tableoutlier_count_way4)# Add the year column as the first column
write.csv(tableoutlier_count_way4, "C:/Users/rbmahbub/Documents/RProjects/AmerifluxDataSubmission_LandscapeFlux/Data/PercentileMethodFilteringlist/tableoutlier_count_way4.csv", row.names = TRUE)

# plot_way_data <- function(way_data, base_dir, way_name) {
#   # Convert -9999 to NA
#   way_data <- lapply(way_data, function(df) {
#     df[df == -9999] <- NA
#     return(df)
#   })
#   # List of years for processing
#   years <- c("2018", "2019", "2020","2022", "2021", "2023", "2024")
#   
#   # Loop through each year
#   for (year in years) {
#     # Access the dataframe for the corresponding year
#     df <- way_data[[paste0(way_name, " ", year, ".csv")]]
#     # Create the output directory for the year if it doesn't exist
#     output_dir <- paste0(base_dir, year)
#     if (!dir.exists(output_dir)) {
#       dir.create(output_dir, recursive = TRUE)
#     }
#     # Get the column names except TIMESTAMP
#     column_names <- names(df)[names(df) != "TIMESTAMP"]
#     # Loop through each column and plot against TIMESTAMP
#     for (col in column_names) {
#       # Create the plot with points (instead of lines), ensuring TIMESTAMP is properly referenced
#       p <- ggplot(df, aes_string(x = "TIMESTAMP", y = col)) +
#         geom_point(na.rm = FALSE) + # Keep NA values (they will show as gaps in the plot)
#         labs(title = paste("Plot of", col, "in", year), x = "Timestamp", y = col) +
#         theme_minimal()
#       # Save the plot to the corresponding directory
#       ggsave(filename = paste0(output_dir, "/", col, "_", year, ".jpeg"), plot = p, width = 8, height = 4)
#     }
#   }
# }
# 
# plot_way_data_with_units <- function(way_data, base_dir, way_name, unit_row) {
#   # Convert -9999 to NA
#   way_data <- lapply(way_data, function(df) {
#     df[df == -9999] <- NA
#     return(df)
#   })
#   # List of years for processing
#   years <- c("2018", "2019", "2020", "2021", "2022", "2023", "2024")=
#     # Loop through each year
#     for (year in years) {
#       # Access the dataframe for the corresponding year
#       df <- way_data[[paste0(way_name, " ", year, ".csv")]]
#       # Create the output directory for the year if it doesn't exist
#       output_dir <- paste0(base_dir, year)
#       if (!dir.exists(output_dir)) {
#         dir.create(output_dir, recursive = TRUE)
#       }
#       
#       # Get the column names except TIMESTAMP
#       column_names <- names(df)[names(df) != "TIMESTAMP"]
#       
#       # Loop through each column and plot against TIMESTAMP
#       for (col in column_names) {
#         # Find the unit for the current column
#         unit <- filtered_unit_row[[col]]
#         unit_label <- ifelse(is.na(unit), "", paste0(" (", unit, ")"))
#         
#         # Create the plot with points (instead of lines), ensuring TIMESTAMP is properly referenced
#         p <- ggplot(df, aes_string(x = "TIMESTAMP", y = col)) +
#           geom_point(na.rm = FALSE) + # Keep NA values (they will show as gaps in the plot)
#           labs(
#             title = paste("Plot of", col, "in", year, "from", way_name), 
#             x = "Timestamp",
#             y = paste0(col, unit_label)
#           ) +
#           theme_minimal()
#         
#         # Save the plot to the corresponding directory
#         ggsave(filename = paste0(output_dir, "/", col, "_", year, ".jpeg"), plot = p, width = 8, height = 4)
#       }
#     }
# }


# # Function to save filtered data without quotes around column names
# save_filtered_data <- function(data, filename, directory, prefix) {
#   data$TIMESTAMP_START <- as.numeric(data$TIMESTAMP_START) # Timestamp Start
#   data$TIMESTAMP_END <- as.numeric(data$TIMESTAMP_END) # Timestamp END
#   data[is.na(data)] <- -9999 # Replace NA values
#   data[is.nan.data.frame(data)] <- -9999 # Replace NaN values
#   new_filename <- paste0(prefix, "_", filename) # Add prefix
#   file_path <- file.path(directory, new_filename)
#   
#   # Set quote = FALSE to avoid quotes around column names
#   write.csv(data, file = file_path, row.names = FALSE, quote = FALSE)
#   
#   cat(new_filename, "saved successfully to", directory, "\n")
# }


# save_filtered_data <- function(data, filename, directory, prefix) {
#   data$TIMESTAMP_START <- as.character(data$TIMESTAMP_START) # Convert to character
#   data$TIMESTAMP_END <- as.character(data$TIMESTAMP_END) # Convert to character
#   data[is.na(data)] <- -9999 # Replace NA values
#   data[is.nan.data.frame(data)] <- -9999 # Replace NaN values
#   
#   # Extract year from filename (assuming it's the last 4 digits in "Way3_2018.csv")
#   year <- gsub(".*_(\\d{4})\\.csv", "\\1", filename)
#   
#   new_filename <- paste0(prefix, "_", year, ".csv") # Create new filename format
#   file_path <- file.path(directory, new_filename)
#   
#   write.csv(data, file = file_path, row.names = FALSE)
#   cat(new_filename, "saved successfully to", directory, "\n")
# }

# 
# # Assign names to the processed Way3 data
# names(all_processed_data_way3) <- way3_files_to_read
# # Assign names to the processed Way4 data
# names(all_processed_data_way4) <- way4_files_to_read
# 
# # Save Way3 data with "USHRC_" prefix
# for (i in seq_along(all_processed_data_way3)) {
#   filename <- names(all_processed_data_way3)[i]
#   save_filtered_data(all_processed_data_way3[[i]], filename, way3_save_directory, "USHRC")
# }
# 
# # Save Way4 data with "USHRA_" prefix
# for (i in seq_along(all_processed_data_way4)) {
#   filename <- names(all_processed_data_way4)[i]
#   save_filtered_data(all_processed_data_way4[[i]], filename, way4_save_directory, "USHRA")
# }
# 



save_filtered_data <- function(data, filename, directories, site_id, resolution) {
  # Clean column names
  colnames(data) <- gsub(" ", "_", colnames(data))
  colnames(data) <- gsub('"', '', colnames(data))
  
  # Convert TIMESTAMP columns safely
  data$TIMESTAMP_START <- suppressWarnings(as.numeric(data$TIMESTAMP_START))
  data$TIMESTAMP_END <- suppressWarnings(as.numeric(data$TIMESTAMP_END))
  
  # Remove rows with invalid TIMESTAMP lengths
  valid_rows <- nchar(data$TIMESTAMP_START) == 12 & nchar(data$TIMESTAMP_END) == 12
  data <- data[valid_rows, ]
  
  if (nrow(data) == 0) {
    cat("Skipping", filename, "- No valid rows after timestamp filtering.\n")
    return()
  }
  
  # Replace missing values
  data[is.na(data)] <- -9999
  data[is.nan.data.frame(data)] <- -9999
  
  # Format timestamps for filename
  ts_start <- as.character(min(data$TIMESTAMP_START, na.rm = TRUE))
  ts_end <- as.character(max(data$TIMESTAMP_END, na.rm = TRUE))
  
  if (nchar(ts_start) != 12 | nchar(ts_end) != 12) {
    cat("Skipping", filename, "- Invalid timestamp format.\n")
    return()
  }
  
  ts_start_fmt <- paste0(substr(ts_start, 1, 4), substr(ts_start, 5, 6), substr(ts_start, 7, 8), substr(ts_start, 9, 10))
  ts_end_fmt <- paste0(substr(ts_end, 1, 4), substr(ts_end, 5, 6), substr(ts_end, 7, 8), substr(ts_end, 9, 10))
  
  # Construct new filename
  new_filename <- paste0(site_id, "_", resolution, "_", ts_start_fmt, "_", ts_end_fmt, ".csv")
  
  # Save to all specified directories
  for (dir in directories) {
    file_path <- file.path(dir, new_filename)
    write.csv(data, file = file_path, row.names = FALSE, quote = FALSE)
    cat(new_filename, "saved successfully to", dir, "\n")
  }
}

# Assign names to processed data
names(all_processed_data_way3) <- way3_files_to_read
names(all_processed_data_way4) <- way4_files_to_read

# Define resolution
resolution <- "HH"

# Save Way3 data to both locations
for (i in seq_along(all_processed_data_way3)) {
  filename <- names(all_processed_data_way3)[i]
  site_id <- "US-HRC"
  save_filtered_data(all_processed_data_way3[[i]], filename, way3_save_dirs, site_id, resolution)
}

# Save Way4 data to both locations
for (i in seq_along(all_processed_data_way4)) {
  filename <- names(all_processed_data_way4)[i]
  site_id <- "US-HRA"
  save_filtered_data(all_processed_data_way4[[i]], filename, way4_save_dirs, site_id, resolution)
}



###Review check
# Columns to check
plain_cols <- c("H2O", "RH", "TA", "FC", "CO2", "CH4")

# Check existence across all dataframes in the list
sapply(all_processed_data_way4, function(df) {
  sapply(plain_cols, function(col) col %in% colnames(df))
})
# Check existence across all dataframes in the list
sapply(all_processed_data_way3, function(df) {
  sapply(plain_cols, function(col) col %in% colnames(df))
})

# Columns with _1_1_1 to check
detailed_cols <- c("H2O_1_1_1", "RH_1_1_1", "TA_1_1_1", "FC_1_1_1", "CO2_1_1_1", "CH4_1_1_1")

# Check existence across all dataframes
sapply(all_processed_data_way4, function(df) {
  sapply(detailed_cols, function(col) col %in% colnames(df))
})
sapply(all_processed_data_way3, function(df) {
  sapply(detailed_cols, function(col) col %in% colnames(df))
})
# Function to calculate NA percentage for a set of columns in a dataframe
na_percent <- function(df, cols) {
  sapply(cols, function(col) {
    if(col %in% colnames(df)) {
      mean(is.na(df[[col]])) * 100  # % of NAs
    } else {
      NA
    }
  })
}

# Percentage of NAs in Way4
na_percent_way4 <- sapply(all_processed_data_way4, na_percent, cols = detailed_cols)
na_percent_way4

# Percentage of NAs in Way3
na_percent_way3 <- sapply(all_processed_data_way3, na_percent, cols = detailed_cols)
na_percent_way3




