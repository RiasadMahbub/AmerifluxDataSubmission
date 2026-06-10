########################################
######PotentialSWR####################
########################################
# Set the directory and filename
library(dplyr)
library(lubridate)
directory <- "C:/Users/rbmahbub/Documents/RProjects/AmerifluxDataSubmission_LandscapeFlux/Data/PotentialSWR"
filename <- "US-HRA_HH_2017.csv"

# Construct the full file path
file_path <- file.path(directory, filename)

# Read the CSV file
data <- read.csv(file_path)

# Print the column names
print(names(data))

# Convert TIMESTAMP_START and TIMESTAMP_END to datetime format
data$TIMESTAMP_START <- ymd_hm(data$TIMESTAMP_START, tz = "UTC")
data$TIMESTAMP_END <- ymd_hm(data$TIMESTAMP_END, tz = "UTC")
# Extract the hour from TIMESTAMP_START
data$HOUR <- hour(data$TIMESTAMP_START)

# Plot HOUR on x-axis and SW_IN_POT on y-axis
plot(data$HOUR, data$SW_IN_POT, 
     xlab = "Hour", 
     ylab = "SW_IN_POT", 
     main = "SW_IN_POT vs. Hour",
     type = "o",  # Use lines and points
     col = "blue",  # Set the color of the plot
     pch = 16)  # Set the shape of the points


# Current Implemented Modules 
#https://ameriflux.lbl.gov/data/flux-data-products/data-qaqc/
#https://ameriflux.lbl.gov/data/flux-data-products/data-qaqc/timestamp-alignment-module/
#https://ameriflux.lbl.gov/data/flux-data-products/data-qaqc/physical-range-module/
#https://ameriflux.lbl.gov/data/flux-data-products/data-qaqc/multivariate-comparison-module/
#https://ameriflux.lbl.gov/data/flux-data-products/data-qaqc/ustar-filtering-module/


# The task involves reading specific CSV files from two directories: Way4 and Way3.

# Files of interest in the Way4 directory:
# - "Way4 2018.csv"
# - "Way4 2019.csv"
# - "Way4 2020.csv"
# - "Way4 2021.csv"

# Files of interest in the Way3 directory:
# - "Way3 2018.csv"
# - "Way3 2019.csv"
# - "Way3 2020.csv"
# - "Way3 2021.csv"
###Apply across all the way4 dataframes 
# Define the directories
way4_directory <- "C:/Users/rbmahbub/Documents/RProjects/AmerifluxDataSubmission_LandscapeFlux/Data/Way4"
way3_directory <- "C:/Users/rbmahbub/Documents/RProjects/AmerifluxDataSubmission_LandscapeFlux/Data/Way3"

# Define the files to read
way4_files_to_read <- c("Way4 2018.csv", "Way4 2019.csv", "Way4 2020.csv", "Way4 2021.csv", "Way4 2023.csv")
way3_files_to_read <- c("Way3 2018.csv", "Way3 2019.csv", "Way3 2020.csv", "Way3 2021.csv", "Way3 2023.csv")


# Read the CSV files into lists of dataframes
way4_data <- lapply(way4_files_to_read, function(filename) {
  file_path <- file.path(way4_directory, filename)
  read.csv(file_path)
})

way3_data <- lapply(way3_files_to_read, function(filename) {
  file_path <- file.path(way3_directory, filename)
  read.csv(file_path)
})

# Assign names to the list elements for easier reference
names(way4_data) <- way4_files_to_read
names(way3_data) <- way3_files_to_read

# Example: Access the data for Way4 2018 and Way3 2018
way4_2023_data <- way4_data[[5]]
way3_2023_data <- way3_data[[5]]

# Drop the first row from way4_2023_data[[5]]
way4_2023_data <- way4_2023_data[-1, ]
way3_2023_data <- way3_2023_data[-1, ]

# Replace -9999 and -9999.0 with NaN in way4_2023_data
way4_2023_data[] <- lapply(way4_2023_data, function(x) {
  ifelse(x %in% c(-9999, -9999.0), NaN, x)
})

# Replace -9999 and -9999.0 with NaN in way3_2023_data
way3_2023_data[] <- lapply(way3_2023_data, function(x) {
  ifelse(x %in% c(-9999, -9999.0), NaN, x)
})

View(way3_2023_data)

# Extract DOY and HOUR from the TIMESTAMP in way4_data and concatenate them
way3_2023_data <- way3_2023_data %>%
  mutate(
    TIMESTAMP = as.POSIXct(TIMESTAMP, format = "%m/%d/%Y %H:%M"),  # Convert TIMESTAMP to POSIXct
    DOY = yday(TIMESTAMP),  # Extract day of year (DOY)
    HOUR = hour(TIMESTAMP) + ifelse(minute(TIMESTAMP) == 30, 0.5, 0),  # Adjust HOUR if minute is 30
    DOY_HOUR = paste(DOY, HOUR, sep = "_")  # Create DOY_HOUR column
  )
View(way3_2023_data)
### DOY_HOUR is for join
# Extract DOY and HOUR from the TIMESTAMP_START in data and concatenate them
data <- data %>%
  mutate(
    TIMESTAMP_START = ymd_hms(TIMESTAMP_START),
    DOY = yday(TIMESTAMP_START),
    HOUR = hour(TIMESTAMP_START) + ifelse(minute(TIMESTAMP_START) == 30, 0.5, 0),
    DOY_HOUR = paste(DOY, HOUR, sep = "_")
  )

### Drop DOY so that it does not cause DOY.x and DOY.y
data <- data %>%
  select(-any_of(c("DOY", "HOUR")))
way4_data_subset <- way3_2023_data %>%
  filter(DOY >= 1 & DOY <= 365)




way4_data_subsetafterimestam <-way4_data_subset %>%
  mutate(
    TIMESTAMP = as_datetime(TIMESTAMP),  # Ensure TIMESTAMP is in datetime format
    DOY = yday(TIMESTAMP),
    HOUR = hour(TIMESTAMP) + ifelse(minute(TIMESTAMP) == 30, 0.5, 0),  # Extract hour with 0.5 for 30 minutes
    TIMESTAMP = case_when(
      DOY >= 60 & DOY <= 315 ~ TIMESTAMP - hours(1),  # Shift by 1 hour for specified DOY range
      TRUE ~ TIMESTAMP  # Keep other timestamps the same
    
    )
  )
way4_data_subsetafterimestam <- way4_data_subsetafterimestam %>%
  mutate(
    TIMESTAMP = as_datetime(TIMESTAMP),  # Convert to POSIXct datetime if not already
    DOY = yday(TIMESTAMP),               # Extract day of the year
    HOUR = hour(TIMESTAMP) + ifelse(minute(TIMESTAMP) == 30, 0.5, 0),  # Extract hour with 0.5 for 30 minutes
    DOY_HOUR = paste(DOY, HOUR, sep = "_")  # Create DOY_HOUR by combining DOY and HOUR
  )
# Merge the datasets based on the concatenated DOY_HOUR
merged_data <- way4_data_subsetafterimestam %>%
  inner_join(data, by = "DOY_HOUR")

unique(way4_data_subsetafterimestam$DOY_HOUR)
unique(data$DOY_HOUR)



merged_data <- merged_data[(
  (merged_data$Rg <= 1500 | is.nan(merged_data$Rg)) & 
    (merged_data$PAR_IN_Avg <= 1500 | is.nan(merged_data$PAR_IN_Avg))
), ]

# Create a new column for the non-overlapping 15-day periods
merged_data <- merged_data %>%
  mutate(
    PERIOD = ceiling(DOY / 15)  # Each period represents a non-overlapping 15-day window
  )

# Plot SW_IN_Avg, SW_IN_POT, and PAR_IN_Avg for each non-overlapping 15-day period
ggplot(merged_data, aes(x = HOUR)) +
  geom_line(aes(y = Rg, color = "SW_IN_Avg"), size = 1) +
  geom_line(aes(y = SW_IN_POT, color = "SW_IN_POT"), size = 1, linetype = "dashed") +
  geom_line(aes(y = PAR_IN_Avg, color = "PAR_IN_Avg"), size = 1, linetype = "dotdash") +
  facet_wrap(~ PERIOD, scales = "free_y") +
  labs(
    title = "SW_IN_Avg, SW_IN_POT, and PAR_IN_Avg by Hour for Each Non-Overlapping 15-Day Period",
    x = "Hour of the Day",
    y = "Radiation",
    color = "Legend"
  ) +
  theme_minimal()

ggplot(merged_data, aes(x = HOUR)) +
  geom_line(aes(y = Rg, color = "SW_IN_Avg"), size = 1) +
  geom_line(aes(y = SW_IN_POT, color = "SW_IN_POT"), size = 1, linetype = "dashed") +
  geom_line(aes(y = PAR_IN_Avg, color = "PAR_IN_Avg"), size = 1, linetype = "dotdash")
  
ggsave("C:\Users\rbmahbub\Documents\RProjects\AmerifluxDataSubmission_LandscapeFlux\Figure\ShortwaveRadiation")

# Convert Rg to numeric
merged_data$Rg <- as.numeric((merged_data$Rg))
way4_2023_data$Rg <- as.numeric((way4_2023_data$Rg))
# Convert PAR_IN_Avg to numeric
merged_data$PAR_IN_Avg <- as.numeric((merged_data$PAR_IN_Avg))
way4_2023_data$PAR_IN_Avg <- as.numeric((way4_2023_data$PAR_IN_Avg))

hist(merged_data$SW_IN_POT)
hist(merged_data$Rg)
hist(merged_data$PAR_IN_Avg)
mean(merged_data$SW_IN_POT)
mean(merged_data$Rg)
mean(merged_data$PAR_IN_Avg)

hist(way4_2023_data$SW_IN_POT)
hist(way4_2023_data$Rg)
hist(way4_2023_data$PAR_IN_Avg)

###### ###### ###### ###### ###### ###### 
##### shortwave radition for loop ###### 
###### ###### ###### ###### ###### ###### 
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
    geom_line(aes(y = SW_IN_Avg, color = "SW_IN_Avg"), size = 1) +
    geom_line(aes(y = SW_IN_POT, color = "SW_IN_POT"), size = 1, linetype = "dashed") +
    geom_line(aes(y = PAR_IN_Avg, color = "PAR_IN_Avg"), size = 1, linetype = "dotdash") +
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

# Process all files in Way3
all_processed_data_way3 <- list()
for (way_name in names(way3_data)) {
  all_processed_data_way3[[way_name]] <- process_and_plot(
    file_name = way_name, 
    way_data = way3_data, 
    other_data = data, 
    output_dir = way3_output_dir
  )
}

# Process all files in Way4
all_processed_data_way4 <- list()
for (way_name in names(way4_data)) {
  all_processed_data_way4[[way_name]] <- process_and_plot(
    file_name = way_name, 
    way_data = way4_data, 
    other_data = data, 
    output_dir = way4_output_dir
  )
}












plot(merged_data$HOUR, merged_data$SW_IN_POT, 
     xlab = "Hour", 
     ylab = "SW_IN_POT", 
     main = "SW_IN_POT vs. Hour",
     type = "o",  # Use lines and points
     col = "red",  # Set the color of the plot
     pch = 16)  # Set the shape of the points

####################################
####Check the requirued columns
######################################
###Apply across all the way4 dataframes 
# Define the directories
way4_directory <- "C:/Users/rbmahbub/Documents/RProjects/AmerifluxDataSubmission_LandscapeFlux/Data/Way4"
way3_directory <- "C:/Users/rbmahbub/Documents/RProjects/AmerifluxDataSubmission_LandscapeFlux/Data/Way3"

# Define the files to read
way4_files_to_read <- c("Way4 2018.csv", "Way4 2019.csv", "Way4 2020.csv", "Way4 2021.csv")
way3_files_to_read <- c("Way3 2018.csv", "Way3 2019.csv", "Way3 2020.csv", "Way3 2021.csv")

# Read the CSV files into lists of dataframes
way4_data <- lapply(way4_files_to_read, function(filename) {
  file_path <- file.path(way4_directory, filename)
  read.csv(file_path)
})

way3_data <- lapply(way3_files_to_read, function(filename) {
  file_path <- file.path(way3_directory, filename)
  read.csv(file_path)
})

# Assign names to the list elements for easier reference
names(way4_data) <- way4_files_to_read
names(way3_data) <- way3_files_to_read 

# Load necessary libraries
library(ggplot2)
library(dplyr)
library(lubridate)

# Assuming data is a dataframe loaded elsewhere
# For example:
# data <- read.csv("path/to/your/data.csv")

# Function to process each dataframe
process_data <- function(df, data, file_name) {
  df <- df %>%
    mutate(
      TIMESTAMP = ymd_hms(TIMESTAMP),
      DOY = yday(TIMESTAMP),
      HOUR = hour(TIMESTAMP) + ifelse(minute(TIMESTAMP) == 30, 0.5, 0),
      DOY_HOUR = paste(DOY, HOUR, sep = "_")
    )
  
  # Extract DOY and HOUR from the TIMESTAMP_START in data and concatenate them
  data <- data %>%
    mutate(
      TIMESTAMP_START = ymd_hms(TIMESTAMP_START),
      DOY = yday(TIMESTAMP_START),
      HOUR = hour(TIMESTAMP_START) + ifelse(minute(TIMESTAMP_START) == 30, 0.5, 0),
      DOY_HOUR = paste(DOY, HOUR, sep = "_")
    )
  
  # Merge the datasets based on the concatenated DOY_HOUR
  merged_data <- df %>%
    inner_join(data, by = "DOY_HOUR")
  
  # Create a new column for the non-overlapping 15-day periods
  merged_data <- merged_data %>%
    mutate(
      PERIOD = ceiling(DOY.x / 15),  # Each period represents a non-overlapping 15-day window
      PAR_IN_Avg_converted = PAR_IN_Avg / 2.02  # Convert PAR_IN_Avg to radiation
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
    group_by(PERIOD, HOUR.x) %>%
    summarize(
      SW_IN_Avg_max = max_with_nan(SW_IN_Avg),
      SW_IN_POT_max = max_with_nan(SW_IN_POT),
      PAR_IN_Avg_converted_max = max_with_nan(PAR_IN_Avg_converted)
    ) %>%
    ungroup()
  
  # Plot the maximum diurnal composite for each hour within each 15-day period
  p <- ggplot(max_diurnal_composite, aes(x = HOUR.x)) +
    geom_line(aes(y = SW_IN_Avg_max, color = "SW_IN_Avg_max"), size = 1) +
    geom_line(aes(y = SW_IN_POT_max, color = "SW_IN_POT_max"), size = 1, linetype = "dashed") +
    geom_line(aes(y = PAR_IN_Avg_converted_max, color = "PAR_IN_Avg (converted) max"), size = 1, linetype = "dotdash") +
    facet_wrap(~ PERIOD, scales = "free_y") +
    labs(
      title = paste("Maximum Diurnal Composite for SW_IN_Avg, SW_IN_POT, and PAR_IN_Avg (converted) -", file_name),
      x = "Hour of the Day",
      y = "Maximum Radiation",
      color = "Legend"
    ) +
    theme_minimal()
  
  # Save the plot
  ggsave(filename = file.path("C:/Users/rbmahbub/Documents/RProjects/AmerifluxDataSubmission_LandscapeFlux/Figure", paste0(sub(".csv", "", file_name), "_max_composite.png")),
         plot = p,
         width = 10, height = 6)
  
  return(p)
}

# Process and save plots for each dataframe in way4_data
for (file_name in names(way4_data)) {
  process_data(way4_data[[file_name]], data, file_name)
}


###Way3 all data maximum diurnal composite
# Function to process each dataframe for Way3
process_data_way3 <- function(df, data, file_name) {
  df <- df %>%
    mutate(
      TIMESTAMP = ymd_hms(TIMESTAMP),
      DOY = yday(TIMESTAMP),
      HOUR = hour(TIMESTAMP) + ifelse(minute(TIMESTAMP) == 30, 0.5, 0),
      DOY_HOUR = paste(DOY, HOUR, sep = "_")
    )
  
  # Extract DOY and HOUR from the TIMESTAMP_START in data and concatenate them
  data <- data %>%
    mutate(
      TIMESTAMP_START = ymd_hms(TIMESTAMP_START),
      DOY = yday(TIMESTAMP_START),
      HOUR = hour(TIMESTAMP_START) + ifelse(minute(TIMESTAMP_START) == 30, 0.5, 0),
      DOY_HOUR = paste(DOY, HOUR, sep = "_")
    )
  
  # Merge the datasets based on the concatenated DOY_HOUR
  merged_data <- df %>%
    inner_join(data, by = "DOY_HOUR")
  
  # Create a new column for the non-overlapping 15-day periods
  merged_data <- merged_data %>%
    mutate(
      PERIOD = ceiling(DOY.x / 15),  # Each period represents a non-overlapping 15-day window
      PAR_IN_Avg_converted = PAR_IN_Avg / 2.02  # Convert PAR_IN_Avg to radiation
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
    group_by(PERIOD, HOUR.x) %>%
    summarize(
      SW_IN_Avg_max = max_with_nan(SW_IN_Avg),
      SW_IN_POT_max = max_with_nan(SW_IN_POT),
      PAR_IN_Avg_converted_max = max_with_nan(PAR_IN_Avg_converted)
    ) %>%
    ungroup()
  
  # Plot the maximum diurnal composite for each hour within each 15-day period
  p <- ggplot(max_diurnal_composite, aes(x = HOUR.x)) +
    geom_line(aes(y = SW_IN_Avg_max, color = "SW_IN_Avg_max"), size = 1) +
    geom_line(aes(y = SW_IN_POT_max, color = "SW_IN_POT_max"), size = 1, linetype = "dashed") +
    geom_line(aes(y = PAR_IN_Avg_converted_max, color = "PAR_IN_Avg (converted) max"), size = 1, linetype = "dotdash") +
    facet_wrap(~ PERIOD, scales = "free_y") +
    labs(
      title = paste("Maximum Diurnal Composite for SW_IN_Avg, SW_IN_POT, and PAR_IN_Avg (converted) -", file_name),
      x = "Hour of the Day",
      y = "Maximum Radiation",
      color = "Legend"
    ) +
    theme_minimal()
  
  # Save the plot
  ggsave(filename = file.path("C:/Users/rbmahbub/Documents/RProjects/AmerifluxDataSubmission_LandscapeFlux/Figure", paste0(sub(".csv", "", file_name), "_max_composite_way3.jpeg")),
         plot = p,
         width = 10, height = 6)
  
  return(p)
}

# Process and save plots for each dataframe in way3_data
for (file_name in names(way3_data)) {
  process_data_way3(way3_data[[file_name]], data, file_name)
}

###Way3 all data
# Function to process each dataframe
# Function to process each dataframe for Way3 with timestamp alignment checks
# Function to process each dataframe for Way3 with timestamp alignment checks
# Function to process each dataframe for Way3 with timestamp alignment checks
# Function to process each dataframe for Way3 with timestamp alignment checks
process_data_way3 <- function(df, data, file_name) {
  df <- df %>%
    mutate(
      TIMESTAMP = ymd_hms(TIMESTAMP),
      DOY = yday(TIMESTAMP),
      HOUR = hour(TIMESTAMP) + ifelse(minute(TIMESTAMP) == 30, 0.5, 0),
      DOY_HOUR = paste(DOY, HOUR, sep = "_")
    )
  
  # Extract DOY and HOUR from the TIMESTAMP_START in data and concatenate them
  data <- data %>%
    mutate(
      TIMESTAMP_START = ymd_hms(TIMESTAMP_START),
      DOY = yday(TIMESTAMP_START),
      HOUR = hour(TIMESTAMP_START) + ifelse(minute(TIMESTAMP_START) == 30, 0.5, 0),
      DOY_HOUR = paste(DOY, HOUR, sep = "_")
    )
  
  # Merge the datasets based on the concatenated DOY_HOUR
  merged_data <- df %>%
    inner_join(data, by = "DOY_HOUR")
  
  # Create a new column for the non-overlapping 15-day periods
  merged_data <- merged_data %>%
    mutate(
      PERIOD = ceiling(DOY.x / 15),  # Each period represents a non-overlapping 15-day window
      PAR_IN_Avg_converted = PAR_IN_Avg / 2.02  # Convert PAR_IN_Avg to radiation
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
    group_by(PERIOD, HOUR.x) %>%
    summarize(
      SW_IN_Avg_max = max_with_nan(SW_IN_Avg),
      SW_IN_POT_max = max_with_nan(SW_IN_POT),
      PAR_IN_Avg_converted_max = max_with_nan(PAR_IN_Avg_converted)
    ) %>%
    ungroup()
  
  # --- Timestamp Alignment Checks ---
  
  # 1. Check for exceedances: SW_IN or PPFD_IN exceeding SW_IN_POT
  exceedance_check <- max_diurnal_composite %>%
    summarize(
      exceedance_sw_in = sum(SW_IN_Avg_max > SW_IN_POT_max, na.rm = TRUE) / n() * 100,
      exceedance_ppfd_in = sum(PAR_IN_Avg_converted_max > SW_IN_POT_max, na.rm = TRUE) / n() * 100
    )
  
  print(paste("Percentage exceedance of SW_IN over SW_IN_POT:", exceedance_check$exceedance_sw_in))
  print(paste("Percentage exceedance of PPFD_IN over SW_IN_POT:", exceedance_check$exceedance_ppfd_in))
  
  # 2. Cross-correlation between SW_IN and SW_IN_POT, and PPFD_IN and SW_IN_POT
  cross_correlation <- function(x, y) {
    valid_data <- complete.cases(x, y)  # Only keep rows with no missing values
    x_clean <- x[valid_data]
    y_clean <- y[valid_data]
    
    if (length(x_clean) > 1 && length(y_clean) > 1) {
      return(ccf(x_clean, y_clean, plot = FALSE)$acf)
    } else {
      return(NA_real_)  # Return NA if no valid data is available
    }
  }
  
  cross_corr_results <- max_diurnal_composite %>%
    summarize(
      cross_corr_sw_in = cross_correlation(SW_IN_Avg_max, SW_IN_POT_max),
      cross_corr_ppfd_in = cross_correlation(PAR_IN_Avg_converted_max, SW_IN_POT_max)
    )
  
  print("Cross-correlation results for SW_IN and SW_IN_POT:")
  print(cross_corr_results$cross_corr_sw_in)
  
  print("Cross-correlation results for PPFD_IN and SW_IN_POT:")
  print(cross_corr_results$cross_corr_ppfd_in)
  
  # --- Plot the maximum diurnal composite ---
  
  p <- ggplot(max_diurnal_composite, aes(x = HOUR.x)) +
    geom_line(aes(y = SW_IN_Avg_max, color = "SW_IN_Avg_max"), size = 1) +
    geom_line(aes(y = SW_IN_POT_max, color = "SW_IN_POT_max"), size = 1, linetype = "dashed") +
    geom_line(aes(y = PAR_IN_Avg_converted_max, color = "PAR_IN_Avg (converted) max"), size = 1, linetype = "dotdash") +
    facet_wrap(~ PERIOD, scales = "free_y") +
    labs(
      title = paste("Maximum Diurnal Composite for SW_IN_Avg, SW_IN_POT, and PAR_IN_Avg (converted) -", file_name),
      x = "Hour of the Day",
      y = "Maximum Radiation",
      color = "Legend"
    ) +
    theme_minimal()
  
  # Save the plot
  ggsave(filename = file.path("C:/Users/rbmahbub/Documents/RProjects/AmerifluxDataSubmission_LandscapeFlux/Figure", paste0(sub(".csv", "", file_name), "_max_composite_way3.png")),
         plot = p,
         width = 10, height = 6)
  
  return(p)
}

# Process and save plots for each dataframe in way3_data
for (file_name in names(way3_data)) {
  process_data_way3(way3_data[[file_name]], data, file_name)
}



#### Plot all the columns and check the data
#### Qualify filtering 
#### Look at the column
#### CF card - week of the data
#### look at the column and see if the data has been moved or not
#### Data changing in the check_same_columns
#### Plot all the columns
# Load necessary libraries

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
way4_files_to_read <- c("Way4 2018.csv", "Way4 2019.csv", "Way4 2020.csv", "Way4 2021.csv")
way3_files_to_read <- c("Way3 2018.csv", "Way3 2019.csv", "Way3 2020.csv", "Way3 2021.csv")

# Read the CSV files into lists of dataframes for both Way3 and Way4
way4_data <- lapply(way4_files_to_read, function(filename) {
  file_path <- file.path(way4_directory, filename)
  read.csv(file_path)
})

way3_data <- lapply(way3_files_to_read, function(filename) {
  file_path <- file.path(way3_directory, filename)
  read.csv(file_path)
})

# Apply the processing function to all datasets in Way3 and Way4
way3_processed_data <- lapply(way3_data, process_data)
way4_processed_data <- lapply(way4_data, process_data)

# Assign names to the list elements for easier reference
names(way3_processed_data) <- way3_files_to_read
names(way4_processed_data) <- way4_files_to_read

# Function to filter and rename columns (corrected version)
filter_and_rename <- function(data) {
  # Filter the columns
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
  
  # Rename the filtered columns
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
  
  # Return the renamed data
  return(data_renamed)
}

# Apply the filtering and renaming function to the processed data
way3_filtered_renamed <- lapply(way3_processed_data, filter_and_rename)
way4_filtered_renamed <- lapply(way4_processed_data, filter_and_rename)

# Load necessary libraries
library(ggplot2)

# Function to convert -9999 to NA and plot data
plot_way_data <- function(way_data, base_dir, way_name) {
  # Convert -9999 to NA
  way_data <- lapply(way_data, function(df) {
    df[df == -9999] <- NA
    return(df)
  })
  
  # List of years for processing
  years <- c("2018", "2019", "2020", "2021")
  
  # Loop through each year
  for (year in years) {
    # Access the dataframe for the corresponding year
    df <- way_data[[paste0(way_name, " ", year, ".csv")]]
    # Create the output directory for the year if it doesn't exist
    output_dir <- paste0(base_dir, year)
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE)
    }
    
    # Get the column names except TIMESTAMP
    column_names <- names(df)[names(df) != "TIMESTAMP"]
    
    # Loop through each column and plot against TIMESTAMP
    for (col in column_names) {
      # Create the plot with points (instead of lines), ensuring TIMESTAMP is properly referenced
      p <- ggplot(df, aes_string(x = "TIMESTAMP", y = col)) +
        geom_point(na.rm = FALSE) + # Keep NA values (they will show as gaps in the plot)
        labs(title = paste("Plot of", col, "in", year), x = "Timestamp", y = col) +
        theme_minimal()
      
      # Save the plot to the corresponding directory
      ggsave(filename = paste0(output_dir, "/", col, "_", year, ".jpeg"), plot = p, width = 8, height = 4)
    }
  }
}

# Paths where you want to save the plots for Way3 and Way4
base_dir_way3 <- "C:/Users/rbmahbub/Documents/RProjects/AmerifluxDataSubmission_LandscapeFlux/Figure/Way3Columns/"
base_dir_way4 <- "C:/Users/rbmahbub/Documents/RProjects/AmerifluxDataSubmission_LandscapeFlux/Figure/Way4Columns/"

# Apply the function for Way3 and Way4
plot_way_data(way3_filtered_renamed, base_dir_way3, "Way3")
plot_way_data(way4_filtered_renamed, base_dir_way4, "Way4")



###########


