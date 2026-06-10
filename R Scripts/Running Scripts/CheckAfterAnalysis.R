library(dplyr)
library(ggplot2)
library(lubridate)


#-------------------------------------------------------------------
#Check Temperature data
#-------------------------------------------------------------------
plot( all_processed_data_way3[[6]]$TA_1_1_1, all_processed_data_way3[[6]]$T_SONIC)
plot(all_processed_data_way3[[5]]$TA_1_1_1, all_processed_data_way3[[5]]$T_SONIC)
plot(all_processed_data_way3[[7]]$TA_1_1_1, all_processed_data_way3[[7]]$T_SONIC)

plot(all_processed_data_way4[[5]]$TA_1_1_1, all_processed_data_way4[[5]]$T_SONIC)
plot(all_processed_data_way4[[6]]$TA_1_1_1, all_processed_data_way4[[6]]$T_SONIC)
plot(all_processed_data_way4[[7]]$TA_1_1_1, all_processed_data_way4[[7]]$T_SONIC)



#-----------------------------------------------------------
#CHECK USTAR
#-------------------------------------------------------------
plot(all_processed_data_way4[[1]]$WS, all_processed_data_way4[[1]]$USTAR)
plot(all_processed_data_way4[[4]]$WS, all_processed_data_way4[[4]]$USTAR)
plot(all_processed_data_way4[[6]]$WS, all_processed_data_way4[[6]]$USTAR)
plot(all_processed_data_way4[[7]]$WS, all_processed_data_way4[[7]]$USTAR)


plot(all_processed_data_way4[[4]]$WS, all_processed_data_way4[[4]]$USTAR)
plot(all_processed_data_way4[[6]]$WS, all_processed_data_way4[[6]]$USTAR)
plot(all_processed_data_way4[[7]]$WS, all_processed_data_way4[[7]]$USTAR)

#----------------------------------------------------------
#READ THE FILES
#----------------------------------------------------------
directory_path <- "C:/Users/rbmahbub/Documents/RProjects/AmerifluxDataSubmission_LandscapeFlux/Data/AFguidedSubmitted/Way3"# Set the directory path
csv_files <- list.files(path = directory_path, pattern = "\\.csv$", full.names = TRUE)# List all CSV files in the directory
data_frames <- lapply(csv_files, read.csv)# Read all CSV files into a list of data frames
names(data_frames) <- basename(csv_files)# Optionally, name the list elements using the file names (without the path)
for (name in names(data_frames)) {# Print the names of the files and preview the first few rows of each data frame
  cat("Preview of", name, ":\n")
  print(head(data_frames[[name]]))
  cat("\n")
}

data_frames <- lapply(data_frames, function(df) {# Convert the TIMESTAMP_START column to POSIXct for all data frames
  if ("TIMESTAMP_START" %in% colnames(df)) { # Check if the column exists
    df$TIMESTAMP_START <- as.character(df$TIMESTAMP_START)    # Ensure TIMESTAMP_START is character
    df$TIMESTAMP_START <- as.POSIXct(df$TIMESTAMP_START, format = "%Y%m%d%H%M", tz = "UTC")    # Convert to POSIXct
  }
  return(df)
})
for (i in 1:length(data_frames)) {# Confirm the conversion for all data frames
  cat("Preview of TIMESTAMP_START in data frame", names(data_frames)[i], ":\n")
  print(head(data_frames[[i]]$TIMESTAMP_START))
  cat("\n")
}

# Function to plot and save data for Way3
plot_way3_data <- function(way3_data, base_dir) {
  way3_data <- lapply(way3_data, function(df) {  # Convert -9999 to NA
    df[df == -9999] <- NA
    return(df)
  })
  years <- unique(gsub("Way3 (\\d{4})\\.csv", "\\1", names(way3_data)))  # Extract years directly from file names
  for (year in years) {  # Loop through each year
    df <- way3_data[[paste0("Way3 ", year, ".csv")]]    # Access the dataframe for the corresponding year
    output_dir <- file.path(base_dir, year)# Create the output directory for the year if it doesn't exist
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE)
    }
    column_names <- names(df)[names(df) != "TIMESTAMP_START"]# Get the column names except TIMESTAMP_START
    for (col in column_names) {# Loop through each column and plot against TIMESTAMP_START
      p <- ggplot(df, aes_string(x = "TIMESTAMP_START", y = col)) +# Create the plot with points (instead of lines)
        geom_point(na.rm = TRUE) + # Exclude NA values in the plot
        labs(
          title = paste("Plot of", col, "in", year),
          x = "Timestamp",
          y = col
        ) +
        theme_minimal()
      ggsave(      # Save the plot to the corresponding directory
        filename = file.path(output_dir, paste0(col, "_", year, ".jpeg")),
        plot = p,
        width = 8,
        height = 4
      )
    }
  }
}





# Set the output directory for the plots
output_dir <- "C:/Users/rbmahbub/Documents/RProjects/AmerifluxDataSubmission_LandscapeFlux/Figure/Way3ProcessedPlots"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Placeholder for other data (adjust accordingly)
# Replace `other_data` with your actual dataset for merging
other_data <- data.frame(
  DOY_HOUR = paste0(rep(1:365, each = 24), "_", rep(0:23, times = 365)),
  SW_IN_POT = runif(365 * 24, 0, 1000)
)

# Apply the function to all Way3 files
processed_data <- lapply(names(data_frames), function(file) {
  process_and_plot(file, data_frames, other_data, output_dir)
})

# Combine processed data for all files into one list or data frame as needed
names(processed_data) <- names(data_frames)

table(merged_data$PERIOD, useNA = "ifany")
nrow(merged_data)
merged_data <- merged_data %>% filter(!is.na(PERIOD))
merged_data %>%
  mutate(PERIOD = ceiling(DOY / 15)) %>%
  select(DOY, PERIOD) %>%
  distinct() %>%
  head()






library(ggplot2)

# Set the directory path
directory_path <- "C:/Users/rbmahbub/Documents/RProjects/AmerifluxDataSubmission_LandscapeFlux/Data/AFguidedSubmitted/Way3"

# List all CSV files in the directory
csv_files <- list.files(path = directory_path, pattern = "\\.csv$", full.names = TRUE)

# Read all CSV files into a list of data frames
data_frames <- lapply(csv_files, read.csv)

# Optionally, name the list elements using the file names (without the path)
names(data_frames) <- basename(csv_files)

# Print the names of the files and preview the first few rows of each data frame
for (name in names(data_frames)) {
  cat("Preview of", name, ":\n")
  print(head(data_frames[[name]]))
  cat("\n")
}

# Convert the TIMESTAMP_START column to POSIXct for all data frames
data_frames <- lapply(data_frames, function(df) {
  if ("TIMESTAMP_START" %in% colnames(df)) { # Check if the column exists
    # Ensure TIMESTAMP_START is character
    df$TIMESTAMP_START <- as.character(df$TIMESTAMP_START)
    # Convert to POSIXct
    df$TIMESTAMP_START <- as.POSIXct(df$TIMESTAMP_START, format = "%Y%m%d%H%M", tz = "UTC")
  }
  return(df)
})

##################################
##################################
### single file
# Access the data for the given file
##################################
##################################
file_data <- data_frames[[1]]

# Replace missing value placeholders (-9999, "-9999", etc.) with NA
file_data <- file_data %>%
  mutate(across(
    everything(),
    ~ case_when(
      . %in% c(-9999.0, -9999, "-9999.0", "-9999") ~ NA_real_,
      TRUE ~ as.numeric(.)
    )
  ))

# Process the `way` data (step-by-step):
# Convert TIMESTAMP_START to datetime format (already in UTC)
file_data$TIMESTAMP <- ymd_hms(file_data$TIMESTAMP_START, tz = "UTC")
# Extract Day of Year (DOY) from TIMESTAMP
file_data$DOY <- yday(file_data$TIMESTAMP)
# Extract Hour and Adjust for 30-minute intervals (if minute is 30, add 0.5)
file_data$HOUR <- hour(file_data$TIMESTAMP) + ifelse(minute(file_data$TIMESTAMP) == 30, 0.5, 0)
# Combine DOY and HOUR into a single column DOY_HOUR
file_data$DOY_HOUR <- paste(file_data$DOY, file_data$HOUR, sep = "_")

file_data <- file_data %>%
  mutate(
    PPFD_IN_Adjusted = PPFD_IN / 2.02  # Use the correct column name
  )

# Adjust the timestamp for daylight saving time
file_data$TIMESTAMP <- case_when(
  file_data$DOY >= 60 & file_data$DOY <= 315 ~ file_data$TIMESTAMP - hours(1),
  TRUE ~ file_data$TIMESTAMP
)

# Recalculate DOY, HOUR, and DOY_HOUR after adjusting timestamp
file_data$DOY <- yday(file_data$TIMESTAMP)
file_data$HOUR <- hour(file_data$TIMESTAMP) + ifelse(minute(file_data$TIMESTAMP) == 30, 0.5, 0)
file_data$DOY_HOUR <- paste(file_data$DOY, file_data$HOUR, sep = "_")

# Process the potential SWR data (US-HRA_HH_2017.csv)
directory <- "C:/Users/rbmahbub/Documents/RProjects/AmerifluxDataSubmission_LandscapeFlux/Data/PotentialSWR"
filename <- "US-HRA_HH_2017.csv"
file_path <- file.path(directory, filename)
swr_data <- read.csv(file_path)  # Read the CSV file

# Convert TIMESTAMP_START and TIMESTAMP_END to datetime format
swr_data$TIMESTAMP_START <- ymd_hm(swr_data$TIMESTAMP_START, tz = "UTC")
swr_data$TIMESTAMP_END <- ymd_hm(swr_data$TIMESTAMP_END, tz = "UTC")
swr_data$HOUR <- hour(swr_data$TIMESTAMP_START)  # Extract the hour from TIMESTAMP_START

# Process SWR data step-by-step
swr_data$TIMESTAMP_START <- ymd_hms(swr_data$TIMESTAMP_START)
swr_data$DOY <- yday(swr_data$TIMESTAMP_START)
swr_data$HOUR <- hour(swr_data$TIMESTAMP_START) + ifelse(minute(swr_data$TIMESTAMP_START) == 30, 0.5, 0)
swr_data$DOY_HOUR <- paste(swr_data$DOY, swr_data$HOUR, sep = "_")


# Merge the `way` data with `swr_data` using DOY_HOUR
merged_data <- file_data %>%
  inner_join(swr_data, by = "DOY_HOUR") %>%
  mutate(
    PERIOD = ceiling(DOY/ 15)  # Create non-overlapping 15-day periods
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

# Plot SW_IN_Avg, SW_IN_POT, and PPFD_IN_Adjusted for each 15-day period
ggplot(max_diurnal_composite, aes(x = HOUR)) +
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
plot
# Save the plot
ggsave(
  filename = file.path(output_dir, paste0(gsub(".csv", "", file_name), "_RadiationPlot.jpeg")),
  plot = plot,
  width = 10, height = 6
)

# Return the processed data
return(merged_data)




# Set folder path
folder_path <- "C:/Users/rbmahbub/Documents/RProjects/AmerifluxDataSubmission_LandscapeFlux/Data/OutputLocalProcessedData_AFguidedSubmitted/Way3"
# List all CSV files in the folder
csv_files <- list.files(folder_path, pattern = "\\.csv$", full.names = TRUE)
# Loop over each file
for (file_path in csv_files) {
  # Read the CSV file
  df <- read.csv(file_path, stringsAsFactors = FALSE)
  # Check TIMESTAMP lengths
  bad_start_rows <- df[nchar(df$TIMESTAMP_START) != 12, ]
  bad_end_rows   <- df[nchar(df$TIMESTAMP_END) != 12, ]
  # Combine and remove duplicates
  bad_rows <- unique(rbind(bad_start_rows, bad_end_rows))
  # Print summary
  cat("\nFile:", basename(file_path), "\n")
  cat("  Total rows:                     ", nrow(df), "\n")
  cat("  Rows with invalid START time:   ", nrow(bad_start_rows), "\n")
  cat("  Rows with invalid END time:     ", nrow(bad_end_rows), "\n")
  cat("  Total unique bad rows reported: ", nrow(bad_rows), "\n")
  # Optional: View first few bad rows
  if (nrow(bad_rows) > 0) {
    print(head(bad_rows[, c("TIMESTAMP_START", "TIMESTAMP_END")]))
  }
}


# Set folder path to Way3
folder_path <- "C:/Users/rbmahbub/Documents/RProjects/AmerifluxDataSubmission_LandscapeFlux/Data/OutputLocalProcessedData_AFguidedSubmitted/Way4"
# List all CSV files in the folder
csv_files <- list.files(folder_path, pattern = "\\.csv$", full.names = TRUE)
# Loop over each file
for (file_path in csv_files) {
  # Read the CSV file
  df <- read.csv(file_path, stringsAsFactors = FALSE)
  # Check IMESTAMP lengths
  bad_start_rows <- df[nchar(df$TIMESTAMP_START) != 12, ]
  bad_end_rows   <- df[nchar(df$TIMESTAMP_END) != 12, ]
  # Combine and remove duplicates
  bad_rows <- unique(rbind(bad_start_rows, bad_end_rows))
  # Print summary
  cat("\nFile:", basename(file_path), "\n")
  cat("  Total rows:                     ", nrow(df), "\n")
  cat("  Rows with invalid START time:   ", nrow(bad_start_rows), "\n")
  cat("  Rows with invalid END time:     ", nrow(bad_end_rows), "\n")
  cat("  Total unique bad rows reported: ", nrow(bad_rows), "\n")
  # Optional: View first few bad rows
  if (nrow(bad_rows) > 0) {
    print(head(bad_rows[, c("TIMESTAMP_START", "TIMESTAMP_END")]))
  }
}


#-------------------------------------------------------------
#PLOT of SWIN TIMESTAMP CHECK
#-------------------------------------------------------------

# Base directory for saving Way3 plots
base_dir_way3 <- "C:/Users/rbmahbub/Documents/RProjects/AmerifluxDataSubmission_LandscapeFlux/Figure/Way3CheckafterAnalysis/"
plot_way3_data(data_frames, base_dir_way3)# Apply the function for Way
data_frames[[1]]$S
process_and_plot <- function(file_name, way_data, other_data, output_dir) {
  file_data <- way_data[[file_name]] # Access the data for the given file
  file_data <- file_data %>%# Replace missing value placeholders (-9999, "-9999", etc.) with NA
    mutate(across(
      everything(),
      ~ case_when(
        . %in% c(-9999.0, -9999, "-9999.0", "-9999") ~ NA_real_,
        TRUE ~ as.numeric(.)
      )
    ))
  file_data <- file_data %>%  # Process the `way` data
    mutate(
      TIMESTAMP = ymd_hms(TIMESTAMP_START),
      DOY = yday(TIMESTAMP),
      HOUR = hour(TIMESTAMP) + ifelse(minute(TIMESTAMP) == 30, 0.5, 0),
      DOY_HOUR = paste(DOY, HOUR, sep = "_"),
      PPFD_IN_Adjusted = PPFD_IN / 2.02  # Conversion factor
    ) %>%
    filter(DOY >= 1 & DOY <= 365)
  file_data <- file_data %>%# Adjust the timestamp for daylight saving time
    mutate(
      TIMESTAMP = case_when(
        DOY >= 60 & DOY <= 315 ~ TIMESTAMP - hours(1),
        TRUE ~ TIMESTAMP
      ),
      DOY = yday(TIMESTAMP),
      HOUR = hour(TIMESTAMP) + ifelse(minute(TIMESTAMP) == 30, 0.5, 0),
      DOY_HOUR = paste(DOY, HOUR, sep = "_")
    )
  
  # Process the potential SWR data (US-HRA_HH_2017.csv)
  directory <- "C:/Users/rbmahbub/Documents/RProjects/AmerifluxDataSubmission_LandscapeFlux/Data/PotentialSWR"
  filename <- "US-HRA_HH_2017.csv"
  file_path <- file.path(directory, filename)
  swr_data <- read.csv(file_path) # Read the CSV file
  
  # Convert TIMESTAMP_START and TIMESTAMP_END to datetime format
  swr_data$TIMESTAMP_START <- ymd_hm(swr_data$TIMESTAMP_START, tz = "UTC")
  swr_data$TIMESTAMP_END <- ymd_hm(swr_data$TIMESTAMP_END, tz = "UTC")
  swr_data$HOUR <- hour(swr_data$TIMESTAMP_START) # Extract the hour from TIMESTAMP_START
  
  # Process SWR data
  swr_data <- swr_data %>%
    mutate(
      TIMESTAMP_START = ymd_hms(TIMESTAMP_START),
      DOY = yday(TIMESTAMP_START),
      HOUR = hour(TIMESTAMP_START) + ifelse(minute(TIMESTAMP_START) == 30, 0.5, 0),
      DOY_HOUR = paste(DOY, HOUR, sep = "_")
    ) %>%
    select(-any_of(c("DOY", "HOUR", "TIMESTAMP_START", "TIMESTAMP_END")))  # Drop unnecessary columns
  
  # Merge the `way` data with `swr_data` using DOY_HOUR
  merged_data <- file_data %>%
    inner_join(other_data, by = "DOY_HOUR") %>%
    inner_join(swr_data, by = "DOY_HOUR") %>%
    mutate(
      PERIOD = ceiling(DOY / 15)  # Create non-overlapping 15-day periods
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
  
  # Plot SW_IN_Avg, SW_IN_POT, and PPFD_IN_Adjusted for each 15-day period
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


#-------------------------------------------------------
# CH4 umol and nanomol check 
#-------------------------------------------------------
# directory path
dir_path <- "C:/Users/rbmahbub/Documents/RProjects/AmerifluxDataSubmission_LandscapeFlux/Data/OutputLocalProcessedData_AFguidedSubmitted/Way3"
# list all files
files <- list.files(dir_path, full.names = TRUE)
# read all files into a list
way3_ch4_check <- lapply(files, read.csv, stringsAsFactors = FALSE)
# name list elements by file name
names(way3_ch4_check) <- basename(files)
way3_ch4_check[[1]]

# directory path
dir_path_old <- "C:/Users/rbmahbub/Documents/RProjects/AmerifluxDataSubmission_LandscapeFlux/Data/OutputLocalProcessedData_AFguidedSubmitted/Submission_10_9_2025/Way3"
# list all files
files_old <- list.files(dir_path_old, full.names = TRUE)
# read all files into a list
way3_ch4_check_old <- lapply(files_old, read.csv, stringsAsFactors = FALSE)
# name list elements by file name
names(way3_ch4_check_old) <- basename(files_old)
way3_ch4_check <- lapply(way3_ch4_check, function(df) {
  df[df == -9999] <- NA   # replace all -9999 with NA
  return(df)
})
way3_ch4_check_old <- lapply(way3_ch4_check_old, function(df) {
  df[df == -9999] <- NA
  return(df)
})



library(ggplot2)

plot_ch4_gg <- function(df_old, df_new, var_name, file_label) {
  
  # check column existence
  required_cols <- c("TIMESTAMP_START", var_name)
  if (!all(required_cols %in% names(df_old)) ||
      !all(required_cols %in% names(df_new))) {
    message("Skipping ", var_name, " for ", file_label, " (column missing)")
    return(NULL)
  }
  
  # merge old and new
  merged <- merge(
    df_old[, required_cols],
    df_new[, required_cols],
    by = "TIMESTAMP_START",
    suffixes = c("_old", "_new")
  )
  
  ggplot(
    merged,
    aes(
      x = .data[[paste0(var_name, "_old")]],
      y = .data[[paste0(var_name, "_new")]]
    )
  ) +
    geom_point(size = 0.7, alpha = 0.6) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    labs(
      title = paste(var_name, "Old vs New"),
      subtitle = file_label,
      x = paste(var_name, "(old)"),
      y = paste(var_name, "(new)")
    ) +
    theme_bw()
}



for (i in 1:7) {
  
  df_old <- way3_ch4_check_old[[i]]
  df_new <- way3_ch4_check[[i]]
  file_name <- names(way3_ch4_check)[i]
  
  p1 <- plot_ch4_gg(df_old, df_new, "CH4_MIXING_RATIO", file_name)
  p2 <- plot_ch4_gg(df_old, df_new, "CH4_1_1_1", file_name)
  p3 <- plot_ch4_gg(df_old, df_new, "FCH4", file_name)
  
  print(p1)
  print(p2)
  print(p3)
}


#--------------------------------------------------------------------------
# columns  "CH4" , "CO2" , "FC" , H2O", "P", "RH" , "TA",  "WTD_1_2_1"
#  CH4_1_1_1, CO2_1_1_1, FC_1_1_1, H2O_1_1_1, RH_1_1_1, and TA_1_1_1
#--------------------------------------------------------------------------
# Directory containing your Way3 files
directory <- "C:/Users/rbmahbub/Documents/RProjects/AmerifluxDataSubmission_LandscapeFlux/Data/OutputLocalProcessedData_AFguidedSubmitted/Way3"
# List all CSV files
files <- list.files(directory, pattern = "\\.csv$", full.names = TRUE)
# Columns to check
columns_to_check <- c(
  "CH4", "CO2", "FC", "H2O", "P", "RH", "TA", "WTD_1_2_1",
  "CH4_1_1_1", "CO2_1_1_1", "FC_1_1_1", "H2O_1_1_1", "RH_1_1_1", "TA_1_1_1"
)
# Initialize results list
summary_results <- lapply(files, function(file) {
  data <- read.csv(file, header = TRUE)
  present <- columns_to_check %in% colnames(data)
  names(present) <- columns_to_check
  # Return a data frame for this file
  df <- data.frame(
    file = basename(file),
    t(present)
  )
  return(df)
})
# Combine all results into one data frame
summary_df <- do.call(rbind, summary_results)
# Optional: count missing/present per file
summary_df$columns_present <- apply(summary_df[, -1], 1, function(x) sum(x))
summary_df$columns_missing <- length(columns_to_check) - summary_df$columns_present
# View summary
print(summary_df)

# Directory containing your Way4 files
directory <- "C:/Users/rbmahbub/Documents/RProjects/AmerifluxDataSubmission_LandscapeFlux/Data/OutputLocalProcessedData_AFguidedSubmitted/Way4"
# List all CSV files in the directory
files <- list.files(directory, pattern = "\\.csv$", full.names = TRUE)
# Columns to check
columns_to_check <- c(
  "CH4", "CO2", "FC", "H2O", "P", "RH", "TA", "WTD_1_2_1",
  "CH4_1_1_1", "CO2_1_1_1", "FC_1_1_1", "H2O_1_1_1", "RH_1_1_1", "TA_1_1_1"
)
# Initialize results list
summary_results <- lapply(files, function(file) {
  data <- read.csv(file, header = TRUE)
  present <- columns_to_check %in% colnames(data)
  names(present) <- columns_to_check
  # Return a data frame for this file
  df <- data.frame(
    file = basename(file),
    t(present)
  )
  return(df)
})
# Combine all results into one data frame
summary_df <- do.call(rbind, summary_results)
# Optional: count missing/present per file
summary_df$columns_present <- apply(summary_df[, -1], 1, function(x) sum(x))
summary_df$columns_missing <- length(columns_to_check) - summary_df$columns_present
# View summary
print(summary_df)



#-------------------------------------------------------------------------- 
# Script to check for specific Ameriflux columns across multiple directories
# Columns: "CH4", "CO2", "FC", "H2O", "P", "RH", "TA", "WTD_1_2_1", etc.
#-------------------------------------------------------------------------- 

# Define the columns we are looking for
columns_to_check <- c( 
  "CH4", "CO2", "FC", "H2O", "P", "RH", "TA", "WTD_1_2_1", 
  "CH4_1_1_1", "CO2_1_1_1", "FC_1_1_1", "H2O_1_1_1", "RH_1_1_1", "TA_1_1_1", "P_RAIN"
)

# Function to perform the check on a specific directory
check_directory_columns <- function(dir_path, label) {
  cat("\n--- Checking Directory:", label, "---\n")
  
  # List all CSV files
  files <- list.files(dir_path, pattern = "\\.csv$", full.names = TRUE) 
  
  if (length(files) == 0) {
    cat("No CSV files found in:", dir_path, "\n")
    return(NULL)
  }
  
  # Process files
  summary_results <- lapply(files, function(file) { 
    # Read only the header for speed
    data_header <- read.csv(file, header = TRUE, nrows = 1) 
    present <- columns_to_check %in% colnames(data_header) 
    names(present) <- columns_to_check 
    
    # Create data frame for this file 
    df <- data.frame( 
      file = basename(file), 
      t(present) 
    ) 
    return(df) 
  }) 
  
  # Combine results
  summary_df <- do.call(rbind, summary_results) 
  
  # Calculate summary metrics
  summary_df$columns_present <- apply(summary_df[, -1, drop = FALSE], 1, sum) 
  summary_df$columns_missing <- length(columns_to_check) - summary_df$columns_present 
  
  print(summary_df)
  return(summary_df)
}

# 1. Way3 Files
way3_path <- "C:/Users/rbmahbub/Documents/RProjects/AmerifluxDataSubmission_LandscapeFlux/Data/OutputLocalProcessedData_AFguidedSubmitted/Way3"
way3_results <- check_directory_columns(way3_path, "Way3")

# 2. Way4 Files
way4_path <- "C:/Users/rbmahbub/Documents/RProjects/AmerifluxDataSubmission_LandscapeFlux/Data/OutputLocalProcessedData_AFguidedSubmitted/Way4"
way4_results <- check_directory_columns(way4_path, "Way4")

# 3. Ben's Old Submission (US-HRA and US-HRC)
# Note: R uses forward slashes '/' or escaped backslashes '\\'
bens_path <- "C:/Users/rbmahbub/Documents/RProjects/AmerifluxDataSubmission_LandscapeFlux/Data/OutputLocalProcessedData_AFguidedSubmitted/BensOldSubmission"
bens_results <- check_directory_columns(bens_path, "BensOldSubmission")




#
# AmeriFlux Data Submission - Timestamp Repair Script
# Purpose: Fix timestamps that were corrupted into scientific notation (e.g., 2.01504E+11) 
# by Excel and restore them to the standard YYYYMMDDHHMM format.
library(tidyverse)

# 1. Define paths
input_dir <- "C:/Users/rbmahbub/Documents/RProjects/AmerifluxDataSubmission_LandscapeFlux/Data/OutputLocalProcessedData_AFguidedSubmitted/BensOldSubmission"
output_dir <- file.path(input_dir, "Fixed_Files")

if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# 2. Identify target files
file_list <- list.files(path = input_dir, pattern = "US-HRC_HH_.*\\.csv$", full.names = TRUE)

# 3. Process files
for (file_path in file_list) {
  
  # Read the CSV (using col_types = cols(.default = "c") prevents initial auto-parsing errors)
  df <- read_csv(file_path, col_types = cols(.default = "c"))
  
  # Remove the P_RAIN column if it exists
  df <- df %>% select(-any_of("P_RAIN"))
  
  # Fix Timestamps: 
  # Convert scientific notation back to YYYYMMDDHHMM
  # We use %.0f to force a float/double into a non-scientific string
  df <- df %>%
    mutate(across(starts_with("TIMESTAMP"), ~ {
      val <- as.numeric(.)
      sprintf("%.0f", val)
    }))
  
  # 4. Save the fixed file
  file_name <- basename(file_path)
  write_csv(df, file.path(output_dir, file_name), na = "-9999")
  
  message(paste("Processed and saved:", file_name))
}

#---------------------------------------------------------
#WTD
#---------------------------------------------------------
check_wtd_range <- function(data_list, wtd_col, years, way_label) {
  results <- list()
  
  for (i in seq_along(data_list)) {
    df <- data_list[[i]]
    
    total <- sum(!is.na(df[[wtd_col]]))
    between_05_5 <- sum(df[[wtd_col]] >= -0.5 & df[[wtd_col]] <= 5, na.rm = TRUE)
    
    results[[i]] <- data.frame(
      Dataset = paste0(way_label, "_", years[i]),
      Total_Valid = total,
      Between_0.5_5m = between_05_5,
      Percent = ifelse(total == 0, NA, (between_05_5 / total) * 100)
    )
  }
  
  do.call(rbind, results)
}

years <- c(2018, 2019, 2020, 2021, 2023, 2024, NA, NA)

way3_results <- check_wtd_range(
  way3_filtered_renamed, 
  "WTD_1_1_1", 
  years, 
  "Way3"
)

print(way3_results)

way4_results <- check_wtd_range(
  way4_filtered_renamed, 
  "WTD_1_1_1", 
  years, 
  "Way4"
)

print(way4_results)

