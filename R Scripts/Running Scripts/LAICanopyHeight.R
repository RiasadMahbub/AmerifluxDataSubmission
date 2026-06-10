library(readxl)
library(dplyr)
library(tidyr)
library(lubridate)
library(dplyr)
library(zoo)  # For na.approx function

##### 2018-2020 ##################
# Define file path
file_path <- "C:/Users/rbmahbub/Documents/RProjects/AmerifluxDataSubmission_LandscapeFlux/Data/InputLocalRawData/LAI/2018-2024/2018-2020/2018-2020LAI_Canopy.xlsx"
# Read the first sheet
df <- read_excel(file_path, sheet = 1, col_names = TRUE)
# Rename the first column (unnamed) to "Field_Year"
colnames(df)[1] <- "Field_Year"
# Rename other columns for consistency
df <- df %>%
  rename(
    LAI_corrected = `LAI-Corrected`,
    Avg_Canopy_Height = `Avg Canopy Height`
  )
# Fill down missing values in "Field_Year"
df <- df %>%
  fill(Field_Year, .direction = "down")
# Filter for W3 and W4 fields
filtered_data20182020 <- df %>%
  filter(grepl("^W3_|^W4_", Field_Year)) %>%  # Selects rows where Field_Year starts with W3_ or W4_
  select(Field_Year, Date, LAI_corrected, Avg_Canopy_Height)  # Selecting relevant columns
# Extract year from Field_Year and create a new column
filtered_data20182020 <- filtered_data20182020 %>%
  mutate(Year = as.numeric(substr(Field_Year, nchar(Field_Year) - 3, nchar(Field_Year))))
# Create separate data frames for W3 and W4 fields for each year# Create separate data frames for W3 and W4 fields for each year
# Create separate data frames for W3 and W4 fields for each year
Way32018CHLAI <- filtered_data20182020 %>%
  filter(grepl("^W3_", Field_Year) & Year == 2018) %>%
  select(-Year, -Field_Year)

Way32019CHLAI <- filtered_data20182020 %>%
  filter(grepl("^W3_", Field_Year) & Year == 2019) %>%
  select(-Year, -Field_Year)

Way32020CHLAI <- filtered_data20182020 %>%
  filter(grepl("^W3_", Field_Year) & Year == 2020) %>%
  select(-Year, -Field_Year)

Way42018CHLAI <- filtered_data20182020 %>%
  filter(grepl("^W4_", Field_Year) & Year == 2018) %>%
  select(-Year, -Field_Year)

Way42019CHLAI <- filtered_data20182020 %>%
  filter(grepl("^W4_", Field_Year) & Year == 2019) %>%
  select(-Year, -Field_Year)

Way42020CHLAI <- filtered_data20182020 %>%
  filter(grepl("^W4_", Field_Year) & Year == 2020) %>%
  select(-Year, -Field_Year)
Way42020CHLAI <- Way42020CHLAI[as.Date(Way42020CHLAI$Date) <= as.Date("2020-08-14"), ]

#################2021################
#####################################
############LAI 2021#############
#####################################
# Define file path
file_path <- "C:/Users/rbmahbub/Documents/RProjects/AmerifluxDataSubmission_LandscapeFlux/Data/InputLocalRawData/LAI/2018-2024/2021/LAIMasterFileScatteringCorrections2021.xlsx"
# Read the first sheet
df <- read_excel(file_path, sheet = 1, col_names = TRUE)
# Rename columns for consistency
df <- df %>%
  rename(
    LAI_corrected = `LAI corrected`
  )
# Filter for W3 and W4 data in the LAI_File column
filtered_data <- df %>%
  filter(grepl("^W3|^W4", LAI_File)) %>%  # Selects rows where LAI_File starts with W3 or W4
  select(LAI_File, Date, LAI_corrected)  # Selecting relevant columns
# Convert Date column to proper date format (removing time)
LAI2021 <- filtered_data %>%
  mutate(Date = as.Date(substr(Date, 1, 8), format = "%Y%m%d"))

# Create Way32021LAI (filter rows where LAI_File starts with "W3")
Way32021LAI <- LAI2021 %>%
  filter(grepl("^W3", LAI_File))

# Create Way42021LAI (filter rows where LAI_File starts with "W4")
Way42021LAI <- LAI2021 %>%
  filter(grepl("^W4", LAI_File))

# Function to average values for rows with the same date
average_by_date <- function(data, date_col) {
  data %>%
    group_by(!!sym(date_col)) %>%
    summarise(across(everything(), mean, na.rm = TRUE)) %>%
    ungroup()
}

# Apply the function to 
# Drop the LAI_File column before averaging
Way32021LAI <- Way32021LAI %>%
  select(-LAI_File)  # Drop the LAI_File column
Way42021LAI <- Way42021LAI %>%
  select(-LAI_File)  # Drop the LAI_File column
Way32021LAI <- average_by_date(Way32021LAI, "Date")
Way42021LAI <- average_by_date(Way42021LAI, "Date")


#####################################
############CANOPYHEIGHT2021#########
#####################################
file_path <- "C:/Users/rbmahbub/Documents/RProjects/AmerifluxDataSubmission_LandscapeFlux/Data/InputLocalRawData/LAI/2018-2024/2021/RiceFieldDataCanopyLAI2021.xlsx"
# Sheets to read
sheets_to_read <- c("W3E", "W3C", "W4E", "W4C")
# Function to read a sheet with correct headers and filter required columns
read_and_filter_sheet <- function(sheet) {
  read_excel(file_path, sheet = sheet, skip = 5, col_names = TRUE) %>%
    select(`Date`, 
           `Canopy Height Avg.`) %>%
    rename(Avg_Canopy_Height = `Canopy Height Avg.`)  # Rename the column
}
# Read all specified sheets and filter columns
filtered_data_list <- lapply(sheets_to_read, read_and_filter_sheet)
# Assign names to the list for easy reference
names(filtered_data_list) <- sheets_to_read
# Print the first few rows of each sheet
lapply(filtered_data_list, head)
# Print column names of each dataframe in the list
lapply(names(filtered_data_list), function(sheet) {
  cat("\nColumn names for sheet:", sheet, "\n")
  print(colnames(filtered_data_list[[sheet]]))
})

filtered_data_list <- lapply(filtered_data_list, function(df) {
  df <- df[, c("Date", "Avg_Canopy_Height")]
  return(df)
})
# Print column names again to confirm the new column is added
lapply(names(filtered_data_list), function(sheet) {
  cat("\nColumn names for sheet:", sheet, "\n")
  print(colnames(filtered_data_list[[sheet]]))
})
# Merge W4E and W4C
merged_W4 <- bind_rows(filtered_data_list[["W4E"]], filtered_data_list[["W4C"]])
# Merge W3E and W3C
merged_W3 <- bind_rows(filtered_data_list[["W3E"]], filtered_data_list[["W3C"]])
# Print the structure of the merged data frames
print(head(merged_W4))
print(head(merged_W3))
# Function to average values for rows with the same date
average_by_date <- function(data, date_col) {
  data %>%
    group_by(!!sym(date_col)) %>%
    summarise(across(everything(), mean, na.rm = TRUE)) %>%
    ungroup()
}
# Assuming the date column is named "date"
Way4canopyheight2021 <- average_by_date(merged_W4, "Date")
Way3canopyheight2021 <- average_by_date(merged_W3, "Date")

# Join W3 data (Way32021LAI with Way3canopyheight2021)
Way32021CHLAI <- Way32021LAI %>%
  full_join(Way3canopyheight2021, by = "Date")

# Join W4 data (Way42021LAI with Way4canopyheight2021)
Way42021CHLAI <- Way42021LAI %>%
  full_join(Way4canopyheight2021, by = "Date")


#################2022################
#####################################
############LAI 2022#############
#####################################
# Define file path
file_path <- "C:/Users/rbmahbub/Documents/RProjects/AmerifluxDataSubmission_LandscapeFlux/Data/InputLocalRawData/LAI/2018-2024/2022/LAI_MasterFileScatteringCorrections2022.xlsx"
# Read the CSV file
df <- read_excel(file_path, sheet = 1, col_names = TRUE)
# Rename columns for consistency
df <- df %>%
  rename(LAI_corrected = `LAI corrected`)  # Adjust column name if needed
# Filter for W3 and W4 data in the LAI_File column
filtered_data <- df %>%
  filter(grepl("^W3|^W4", LAI_File)) %>%  # Selects rows where LAI_File starts with W3 or W4
  select(LAI_File, Date, LAI_corrected)  # Selecting relevant columns
# Convert Date column to proper date format (removing time)
LAI2022 <- filtered_data %>%
  mutate(Date = as.Date(substr(Date, 1, 8), format = "%Y%m%d"))

# Create Way32021LAI (filter rows where LAI_File starts with "W3")
Way32022LAI <- LAI2022 %>%
  filter(grepl("^W3", LAI_File))

# Create Way42021LAI (filter rows where LAI_File starts with "W4")
Way42022LAI <- LAI2022 %>%
  filter(grepl("^W4", LAI_File))

# Function to average values for rows with the same date
average_by_date <- function(data, date_col) {
  data %>%
    group_by(!!sym(date_col)) %>%
    summarise(across(everything(), mean, na.rm = TRUE)) %>%
    ungroup()
}

# Apply the function to 
# Drop the LAI_File column before averaging
Way32022LAI <- Way32022LAI %>%
  select(-LAI_File)  # Drop the LAI_File column
Way42022LAI <- Way42022LAI %>%
  select(-LAI_File)  # Drop the LAI_File column
Way32022LAI <- average_by_date(Way32022LAI, "Date")
Way42022LAI <- average_by_date(Way42022LAI, "Date")

#####################################
############CANOPYHEIGHT 2022#############
#####################################
# Define file path
file_path <- "C:/Users/rbmahbub/Documents/RProjects/AmerifluxDataSubmission_LandscapeFlux/Data/InputLocalRawData/LAI/2018-2024/2022/RiceFieldDataCanopyLAI2022.xlsx"
# Sheets to read
sheets_to_read <- c("W3E", "W3C", "W4E", "W4C")
# Function to read a sheet with correct headers and filter required columns
read_and_filter_sheet <- function(sheet) {
  read_excel(file_path, sheet = sheet, skip = 5, col_names = TRUE) %>%
    select(`Date`, 
           `Canopy Height Avg.`) %>%
    rename(Avg_Canopy_Height = `Canopy Height Avg.`)  # Rename the column
}
# Read all specified sheets and filter columns
filtered_data_list <- lapply(sheets_to_read, read_and_filter_sheet)
# Assign names to the list for easy reference
names(filtered_data_list) <- sheets_to_read
# Print the first few rows of each sheet
lapply(filtered_data_list, head)
# Print column names of each dataframe in the list
lapply(names(filtered_data_list), function(sheet) {
  cat("\nColumn names for sheet:", sheet, "\n")
  print(colnames(filtered_data_list[[sheet]]))
})


filtered_data_list <- lapply(filtered_data_list, function(df) {
  df <- df[, c("Date", "Avg_Canopy_Height")]
  return(df)
})
# Print column names again to confirm the new column is added
lapply(names(filtered_data_list), function(sheet) {
  cat("\nColumn names for sheet:", sheet, "\n")
  print(colnames(filtered_data_list[[sheet]]))
})
# Merge W4E and W4C
merged_W4 <- bind_rows(filtered_data_list[["W4E"]], filtered_data_list[["W4C"]])
# Merge W3E and W3C
merged_W3 <- bind_rows(filtered_data_list[["W3E"]], filtered_data_list[["W3C"]])
# Print the structure of the merged data frames
print(head(merged_W4))
print(head(merged_W3))
# Function to average values for rows with the same date
average_by_date <- function(data, date_col) {
  data %>%
    group_by(!!sym(date_col)) %>%
    summarise(across(everything(), mean, na.rm = TRUE)) %>%
    ungroup()
}
# Assuming the date column is named "date"
Way4canopyheight2022 <- average_by_date(merged_W4, "Date")
Way3canopyheight2022 <- average_by_date(merged_W3, "Date")


# Join W3 data (Way32022LAI with Way3canopyheight2022)
Way32022CHLAI <- Way32022LAI %>%
  full_join(Way3canopyheight2022, by = "Date")

# Join W4 data (Way42021LAI with Way4canopyheight2021)
Way42022CHLAI <- Way42022LAI %>%
  full_join(Way4canopyheight2022, by = "Date")

################################################
#################2023################
################################################

# Define file path
file_path <- "C:/Users/rbmahbub/Documents/RProjects/AmerifluxDataSubmission_LandscapeFlux/Data/InputLocalRawData/LAI/2018-2024/2023/RiceFieldDataCanopyLAI2023.xlsx"
# Sheets to read
sheets_to_read <- c("W3E", "W3C", "W4E", "W4C")
# Function to read a sheet with correct headers and filter required columns
read_and_filter_sheet <- function(sheet) {
  df <- read_excel(file_path, sheet = sheet, skip = 5, col_names = TRUE)
  
  # Check if "LAI R 1" exists and rename it to "LAI R"
  if ("LAI R 1" %in% colnames(df)) {
    df <- df %>%
      rename(`LAI R` = `LAI R 1`)
  }
  
  # Rename "LAI R" to "LAI_avg"
  df <- df %>%
    rename(LAI_corrected = `LAI R`)
  
  # Select relevant columns
  df %>%
    select(`Date`, 
           `Canopy height 1 (cm)`, 
           `Canopy height 2`, 
           `Canopy height 3`, 
           `Canopy height 4`, 
           `Canopy height 5`, 
           LAI_corrected)
}

# Read all specified sheets and filter columns
filtered_data_list <- lapply(sheets_to_read, read_and_filter_sheet)
# Assign names to the list for easy reference
names(filtered_data_list) <- sheets_to_read
# Print the first few rows of each sheet
lapply(filtered_data_list, head)
# Print column names of each dataframe in the list
lapply(names(filtered_data_list), function(sheet) {
  cat("\nColumn names for sheet:", sheet, "\n")
  print(colnames(filtered_data_list[[sheet]]))
})
# Function to calculate average canopy height
calculate_avg_height <- function(df) {
  df$Avg_Canopy_Height <- rowMeans(df[, c("Canopy height 1 (cm)", "Canopy height 2", 
                                          "Canopy height 3", "Canopy height 4", 
                                          "Canopy height 5")], na.rm = TRUE)
  return(df)
}
# Apply the function to each dataframe in the list
filtered_data_list <- lapply(filtered_data_list, calculate_avg_height)
# Keep only Date and Avg_Canopy_Height columns
filtered_data_list <- lapply(filtered_data_list, function(df) {
  df <- df[, c("Date",  "LAI_corrected","Avg_Canopy_Height")]
  return(df)
})
# Print column names again to confirm the new column is added
lapply(names(filtered_data_list), function(sheet) {
  cat("\nColumn names for sheet:", sheet, "\n")
  print(colnames(filtered_data_list[[sheet]]))
})
# Merge W4E and W4C
merged_W4 <- bind_rows(filtered_data_list[["W4E"]], filtered_data_list[["W4C"]])
# Merge W3E and W3C
merged_W3 <- bind_rows(filtered_data_list[["W3E"]], filtered_data_list[["W3C"]])
# Print the structure of the merged data frames
print(head(merged_W4))
print(head(merged_W3))
# Function to average values for rows with the same date
average_by_date <- function(data, date_col) {
  data %>%
    group_by(!!sym(date_col)) %>%
    summarise(across(everything(), mean, na.rm = TRUE)) %>%
    ungroup()
}
# Assuming the date column is named "date"
Way42023CHLAI <- average_by_date(merged_W4, "Date")
Way32023CHLAI <- average_by_date(merged_W3, "Date")

################################################
#################2024################
################################################
# File path
file_path <- "C:/Users/rbmahbub/Documents/RProjects/AmerifluxDataSubmission_LandscapeFlux/Data/InputLocalRawData/LAI/2018-2024/2024/RiceFieldDataCanopyLAI_2024_Last.xlsx"
# Sheets to read
sheets_to_read <- c("W3EC", "W3CS", "W4EC", "W4CS")
# Function to read a sheet with correct headers and filter required columns
# Function to read a sheet with correct headers and filter required columns
read_and_filter_sheet <- function(sheet) {
  df <- read_excel(file_path, sheet = sheet, skip = 5, col_names = TRUE)
  
  # Check if "LAI R 1" exists and rename it to "LAI R"
  if ("LAI R 1" %in% colnames(df)) {
    df <- df %>%
      rename(`LAI R` = `LAI R 1`)
  }
  
  # Check if "LAI R" or "LAI r" exists and rename to "LAI_avg"
  if ("LAI R" %in% colnames(df)) {
    df <- df %>%
      rename(LAI_avg = `LAI R`)
  } else if ("LAI r" %in% colnames(df)) {
    df <- df %>%
      rename(LAI_corrected = `LAI r`)
  }
  
  # Convert canopy height columns to numeric
  df <- df %>%
    mutate(across(starts_with("Canopy height"), as.numeric))
  
  # Select relevant columns
  df %>%
    select(`Date`, 
           `Canopy height 1 (cm)`, 
           `Canopy height 2`, 
           `Canopy height 3`, 
           `Canopy height 4`, 
           `Canopy height 5`, 
           LAI_corrected)
}


# Read all specified sheets and filter columns
filtered_data_list <- lapply(sheets_to_read, read_and_filter_sheet)
# Assign names to the list for easy reference
names(filtered_data_list) <- sheets_to_read
# Print the first few rows of each sheet
lapply(filtered_data_list, head)
# Function to calculate average canopy height
calculate_avg_height <- function(df) {
  df$Avg_Canopy_Height <- rowMeans(df[, c("Canopy height 1 (cm)", "Canopy height 2", 
                                          "Canopy height 3", "Canopy height 4", 
                                          "Canopy height 5")], na.rm = TRUE)
  return(df)
}
# Apply the function to each dataframe in the list
filtered_data_list <- lapply(filtered_data_list, calculate_avg_height)
# Keep only Date and Avg_Canopy_Height columns
filtered_data_list <- lapply(filtered_data_list, function(df) {
  df <- df[, c("Date", "LAI_corrected", "Avg_Canopy_Height")]
  return(df)
})

# Merge W4E and W4C
merged_W4 <- bind_rows(filtered_data_list[["W4EC"]], filtered_data_list[["W4CS"]])
# Merge W3E and W3C
merged_W3 <- bind_rows(filtered_data_list[["W3EC"]], filtered_data_list[["W3CS"]])

# Function to average values for rows with the same date
average_by_date <- function(data, date_col) {
  data %>%
    group_by(!!sym(date_col)) %>%
    summarise(across(everything(), mean, na.rm = TRUE)) %>%
    ungroup()
}

# Average by date
Way4canopyheight2024 <- average_by_date(merged_W4, "Date")
Way3canopyheight2024 <- average_by_date(merged_W3, "Date")

# Remove rows where the date is greater than 2024-08-07 (values outside growing season)
Way42024CHLAI <- Way4canopyheight2024 %>%
  filter(Date <= as.POSIXct("2024-08-07 00:00:00"))
Way32024CHLAI <- Way3canopyheight2024 %>%
  filter(Date <= as.POSIXct("2024-08-07 00:00:00"))


Way32024CHLAI
Way32023CHLAI
Way32022CHLAI
Way32021CHLAI
Way32020CHLAI
Way32019CHLAI
Way32018CHLAI


Way42024CHLAI
Way42023CHLAI
Way42022CHLAI
Way42021CHLAI
Way42020CHLAI
Way42019CHLAI
Way42018CHLAI




gapfill_and_interpolate <- function(df) {
  # Ensure the date column is in POSIXct format
  df$date <- as.POSIXct(df$date, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  
  # Create a complete sequence of datetime values with 30-minute intervals
  full_seq <- seq.POSIXt(min(df$date), max(df$date), by = "30 min", tz = "UTC")
  
  # Create a dataframe with the complete datetime sequence
  full_df <- data.frame(date = full_seq)
  
  # Merge the complete sequence with the original dataframe
  merged_df <- merge(full_df, df, by = "date", all.x = TRUE)
  
  # Ensure the columns are numeric and replace non-finite values with NA
  merged_df$LAI_corrected <- as.numeric(merged_df$LAI_corrected)
  merged_df$canopy_height <- as.numeric(merged_df$canopy_height)
  
  merged_df$LAI_corrected[!is.finite(merged_df$LAI_corrected)] <- NA
  merged_df$canopy_height[!is.finite(merged_df$canopy_height)] <- NA
  
  # Perform linear interpolation on the LAI_corrected and canopy_height columns
  # No maxgap argument means all gaps in the middle will be interpolated
  merged_df$LAI_corrected_gapfilled <- na.approx(merged_df$LAI_corrected, na.rm = FALSE)
  merged_df$canopy_height_gapfilled <- na.approx(merged_df$canopy_height, na.rm = FALSE)
  
  # Rename the "date" column to "TIMESTAMP"
  colnames(merged_df)[colnames(merged_df) == "date"] <- "TIMESTAMP"
  
  return(merged_df)
}

# List of W3 data frames to process
w3_data_frames <- list(
  Way32018CHLAI,
  Way32019CHLAI,
  Way32020CHLAI,
  Way32021CHLAI,
  Way32022CHLAI,
  Way32023CHLAI,
  Way32024CHLAI
)

# List of W4 data frames to process
w4_data_frames <- list(
  Way42018CHLAI,
  Way42019CHLAI,
  Way42020CHLAI,
  Way42021CHLAI,
  Way42022CHLAI,
  Way42023CHLAI,
  Way42024CHLAI
)

# Apply the function to each W3 data frame
Way3CHLAIlist <- lapply(w3_data_frames, function(df) {
  # Ensure the columns are named correctly
  colnames(df) <- c("date", "LAI_corrected", "canopy_height")
  
  # Apply the gapfill_and_interpolate function
  gapfill_and_interpolate(df)
})

# Apply the function to each W4 data frame
Way4CHLAIlist <- lapply(w4_data_frames, function(df) {
  # Ensure the columns are named correctly
  colnames(df) <- c("date", "LAI_corrected", "canopy_height")
  
  # Apply the gapfill_and_interpolate function
  gapfill_and_interpolate(df)
})

# Assign names to the processed W3 data frames
names(Way3CHLAIlist) <- c(
  "Way32018CHLAI_processed",
  "Way32019CHLAI_processed",
  "Way32020CHLAI_processed",
  "Way32021CHLAI_processed",
  "Way32022CHLAI_processed",
  "Way32023CHLAI_processed",
  "Way32024CHLAI_processed"
)

# Assign names to the processed W4 data frames
names(Way4CHLAIlist) <- c(
  "Way42018CHLAI_processed",
  "Way42019CHLAI_processed",
  "Way42020CHLAI_processed",
  "Way42021CHLAI_processed",
  "Way42022CHLAI_processed",
  "Way42023CHLAI_processed",
  "Way42024CHLAI_processed"
)

# Print the processed W3 data frames
lapply(Way3CHLAIlist, head)

# Print the processed W4 data frames
lapply(Way4CHLAIlist, head)

# Plot for Way42023CHLAI
plot(Way42023CHLAI$Date, Way42023CHLAI$Avg_Canopy_Height, 
     main = "Avg Canopy Height vs. Date (Way42023CHLAI)",
     xlab = "Date", 
     ylab = "Average Canopy Height",
     type = "p")

# Plot for Way42022CHLAI
plot(Way42022CHLAI$Date, Way42022CHLAI$Avg_Canopy_Height,
     main = "Avg Canopy Height vs. Date (Way42022CHLAI)",
     xlab = "Date",
     ylab = "Average Canopy Height",
     type = "p")

# Plot for Way42021CHLAI
plot(Way42021CHLAI$Date, Way42021CHLAI$Avg_Canopy_Height,
     main = "Avg Canopy Height vs. Date (Way42021CHLAI)",
     xlab = "Date",
     ylab = "Average Canopy Height",
     type = "p")

# Plot for Way42020CHLAI
plot(Way42020CHLAI$Date, Way42020CHLAI$Avg_Canopy_Height,
     main = "Avg Canopy Height vs. Date (Way42020CHLAI)",
     xlab = "Date",
     ylab = "Average Canopy Height",
     type = "p")

# Plot for Way42019CHLAI
plot(Way42019CHLAI$Date, Way42019CHLAI$Avg_Canopy_Height,
     main = "Field data Avg Canopy Height (Way42019CHLAI)",
     xlab = "Date",
     ylab = "Average Canopy Height",
     type = "p")
# Plot for Way42018CHLAI
plot(Way32024CHLAI$Date, Way32024CHLAI$Avg_Canopy_Height,
     main = "8 day  Avg Canopy Height (Way32024)",
     xlab = "Date",
     ylab = "Average Canopy Height Gapfilled (cm)",
     type = "p")

# Plot for Way42018CHLAI
plot(Way3CHLAIlist$Way32024CHLAI_processed$TIMESTAMP, Way3CHLAIlist$Way32024CHLAI_processed$canopy_height_gapfilled,
     main = "30 min Avg Canopy Height (Way32024)",
     xlab = "Date",
     ylab = "Average Canopy Height Gapfilled(cm)",
     type = "p")


# Plot histograms for original and gapfilled canopy_height from 2018 to 2024

# 2018
hist(Way32018CHLAI$Avg_Canopy_Height, main = "Original Canopy Height - 2018", xlab = "Canopy Height")
hist(Way3CHLAIlist$Way32018CHLAI_processed$canopy_height_gapfilled, main = "Gapfilled Canopy Height - 2018", xlab = "Canopy Height")

# 2019
hist(Way32019CHLAI$Avg_Canopy_Height, main = "Original Canopy Height - 2019", xlab = "Canopy Height")
hist(Way3CHLAIlist$Way32019CHLAI_processed$canopy_height_gapfilled, main = "Gapfilled Canopy Height - 2019", xlab = "Canopy Height")

# 2020
hist(Way32020CHLAI$Avg_Canopy_Height, main = "Original Canopy Height - 2020", xlab = "Canopy Height")
hist(Way3CHLAIlist$Way32020CHLAI_processed$canopy_height_gapfilled, main = "Gapfilled Canopy Height - 2020", xlab = "Canopy Height")

# 2021
hist(Way32021CHLAI$Avg_Canopy_Height, main = "Original Canopy Height - 2021", xlab = "Canopy Height")
hist(Way3CHLAIlist$Way32021CHLAI_processed$canopy_height_gapfilled, main = "Gapfilled Canopy Height - 2021", xlab = "Canopy Height")

# 2022
hist(Way32022CHLAI$Avg_Canopy_Height, main = "Original Canopy Height - 2022", xlab = "Canopy Height")
hist(Way3CHLAIlist$Way32022CHLAI_processed$canopy_height_gapfilled, main = "Gapfilled Canopy Height - 2022", xlab = "Canopy Height")

# 2023
hist(Way32023CHLAI$Avg_Canopy_Height, main = "Original Canopy Height - 2023", xlab = "Canopy Height")
hist(Way3CHLAIlist$Way32023CHLAI_processed$canopy_height_gapfilled, main = "Gapfilled Canopy Height - 2023", xlab = "Canopy Height")

# 2024
hist(Way32024CHLAI$Avg_Canopy_Height, main = "Original Canopy Height - 2024", xlab = "Canopy Height")
hist(Way3CHLAIlist$Way32024CHLAI_processed$canopy_height_gapfilled, main = "Gapfilled Canopy Height - 2024", xlab = "Canopy Height")



######################way 4###########################
# 2018
hist(Way42018CHLAI$Avg_Canopy_Height, main = "Original Canopy Height - 2018", xlab = "Canopy Height")
hist(Way4CHLAIlist$Way42018CHLAI_processed$canopy_height_gapfilled, main = "Gapfilled Canopy Height - 2018", xlab = "Canopy Height")

# 2019
hist(Way42019CHLAI$Avg_Canopy_Height, main = "Original Canopy Height - 2019", xlab = "Canopy Height")
hist(Way4CHLAIlist$Way42019CHLAI_processed$canopy_height_gapfilled, main = "Gapfilled Canopy Height - 2019", xlab = "Canopy Height")

# 2020
hist(Way42020CHLAI$Avg_Canopy_Height, main = "Original Canopy Height - 2020", xlab = "Canopy Height")
hist(Way4CHLAIlist$Way42020CHLAI_processed$canopy_height_gapfilled, main = "Gapfilled Canopy Height - 2020", xlab = "Canopy Height")

# 2021
hist(Way42021CHLAI$Avg_Canopy_Height, main = "Original Canopy Height - 2021", xlab = "Canopy Height")
hist(Way4CHLAIlist$Way42021CHLAI_processed$canopy_height_gapfilled, main = "Gapfilled Canopy Height - 2021", xlab = "Canopy Height")

# 2022
hist(Way42022CHLAI$Avg_Canopy_Height, main = "Original Canopy Height - 2022", xlab = "Canopy Height")
hist(Way4CHLAIlist$Way42022CHLAI_processed$canopy_height_gapfilled, main = "Gapfilled Canopy Height - 2022", xlab = "Canopy Height")

# 2023
hist(Way42023CHLAI$Avg_Canopy_Height, main = "Original Canopy Height - 2023", xlab = "Canopy Height")
hist(Way4CHLAIlist$Way42023CHLAI_processed$canopy_height_gapfilled, main = "Gapfilled Canopy Height - 2023", xlab = "Canopy Height")

# 2024
hist(Way42024CHLAI$Avg_Canopy_Height, main = "Original Canopy Height - 2024", xlab = "Canopy Height")
hist(Way4CHLAIlist$Way42024CHLAI_processed$canopy_height_gapfilled, main = "Gapfilled Canopy Height - 2024", xlab = "Canopy Height")



# SAVE the files
# 
# Define output directories
way3_dir <- "C:/Users/rbmahbub/Documents/RProjects/AmerifluxDataSubmission_LandscapeFlux/Data/InputLocalProcessedData/CanopyHeight/Way3"
way4_dir <- "C:/Users/rbmahbub/Documents/RProjects/AmerifluxDataSubmission_LandscapeFlux/Data/InputLocalProcessedData/CanopyHeight/Way4"

# Ensure directories exist
if (!dir.exists(way3_dir)) dir.create(way3_dir, recursive = TRUE)
if (!dir.exists(way4_dir)) dir.create(way4_dir, recursive = TRUE)

# Save Way3 processed CHLAI data
for (name in names(Way3CHLAIlist)) {
  write.csv(Way3CHLAIlist[[name]], file = file.path(way3_dir, paste0(name, ".csv")), row.names = FALSE)
}

# Save Way4 processed CHLAI data
for (name in names(Way4CHLAIlist)) {
  write.csv(Way4CHLAIlist[[name]], file = file.path(way4_dir, paste0(name, ".csv")), row.names = FALSE)
}

print("Processed CHLAI data saved successfully!")




# #######OLD LAI CANOPY HEIGHT ###############
# ################################
# ### Canopy height dataset#######
# ################################
# 
# # Specify the directory#######
# directoryw3 <- "C:/Users/rbmahbub/Documents/RProjects/AmerifluxDataSubmission_LandscapeFlux/Data/InputLocalRawData/CanopyHeight/Way3"
# directoryw4 <- "C:/Users/rbmahbub/Documents/RProjects/AmerifluxDataSubmission_LandscapeFlux/Data/InputLocalRawData/CanopyHeight/Way4"
# # List all CSV and Excel files in the directory
# CHfilesw3 <- list.files(path = directoryw3, pattern = "\\.(csv|xlsx|xls)$", full.names = TRUE)
# # List all CSV and Excel files in the directory
# CHfilesw4 <- list.files(path = directoryw4, pattern = "\\.(csv|xlsx|xls)$", full.names = TRUE)
# # Function to read files based on their format
# read_data <- function(file) {
#   if (grepl("\\.csv$", file, ignore.case = TRUE)) {
#     return(read.csv(file))
#   } else if (grepl("\\.xlsx$", file, ignore.case = TRUE)) {
#     return(read_excel(file))
#   } else if (grepl("\\.xls$", file, ignore.case = TRUE)) {
#     return(read_excel(file))  # read_excel supports both .xls and .xlsx
#   } else {
#     warning(paste("Unsupported file format:", file))
#     return(NULL)
#   }
# }
# 
# # Read all files
# CHDLWay3 <- lapply(CHfilesw3, read_data)
# CHDLWay4 <- lapply(CHfilesw4, read_data)
# 
# 
# # Function to standardize dates
# standardize_date <- function(date_str) {
#   # Remove leading/trailing whitespace
#   date_str <- trimws(date_str)
#   
#   # Handle different formats
#   if (grepl("-", date_str) && grepl(":", date_str)) {
#     # Format: YYYY-MM-DD HH:MM:SS+00:00
#     # Handle timezone offset by stripping it and parsing as UTC
#     dt <- as.POSIXct(sub("\\+00:00$", "", date_str), format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
#     return(format(dt, "%Y-%m-%d %H:%M:%S"))
#   } else if (grepl("-", date_str)) {
#     # Format: YYYY-MM-DD
#     dt <- as.Date(date_str, format = "%Y-%m-%d")
#     return(paste(format(dt, "%Y-%m-%d"), "00:00:00"))
#   } else if (grepl("/", date_str)) {
#     # Format: MM/DD/YYYY
#     dt <- as.Date(date_str, format = "%m/%d/%Y")
#     return(paste(format(dt, "%Y-%m-%d"), "00:00:00"))
#   } else {
#     stop("Unsupported date format: ", date_str)
#   }
# }
# 
# # Function to update dates in a dataframe
# update_dates_in_df <- function(df) {
#   # Check if the date column exists (case-insensitive)
#   date_col <- grep("^date$", colnames(df), ignore.case = TRUE, value = TRUE)
#   
#   if (length(date_col) > 0) {
#     # Apply the standardize_date function to the date column
#     df[[date_col]] <- sapply(df[[date_col]], standardize_date)
#   }
#   return(df)
# }
# 
# # Apply the update_dates_in_df function to each dataframe in the list
# CHDLWay3 <- lapply(CHDLWay3, update_dates_in_df)
# CHDLWay4 <- lapply(CHDLWay4, update_dates_in_df)
# 
# # Print the first 2 rows of each dataframe to verify
# for (i in 1:length(CHDLWay4)) {
#   print(head(CHDLWay4[[i]], 2))  # Print the first 2 rows of each data frame
# }
# 
# library(zoo)
# # Function to gap-fill datetime and interpolate canopy_height
# # Function to gap-fill datetime and interpolate canopy_height
# gapfill_and_interpolate <- function(df) {
#   # Ensure the date column is in POSIXct format
#   df$date <- as.POSIXct(df$date, format = "%Y-%m-%d %H:%M:%S")
#   
#   # Create a complete sequence of datetime values with 30-minute intervals
#   full_seq <- seq.POSIXt(min(df$date), max(df$date), by = "30 min")
#   
#   # Create a dataframe with the complete datetime sequence
#   full_df <- data.frame(date = full_seq)
#   
#   # Merge the complete sequence with the original dataframe
#   merged_df <- merge(full_df, df, by = "date", all.x = TRUE)
#   
#   # Perform linear interpolation on the canopy_height column
#   merged_df$canopy_height <- na.approx(merged_df$canopy_height, na.rm = FALSE)
#   
#   # Rename the "date" column to "TIMESTAMP"
#   colnames(merged_df)[colnames(merged_df) == "date"] <- "TIMESTAMP"
#   
#   return(merged_df)
# }
# 
# # Apply the gapfill_and_interpolate function to each dataframe in the list
# CHDLWay3 <- lapply(CHDLWay3, gapfill_and_interpolate)
# CHDLWay4 <- lapply(CHDLWay4, gapfill_and_interpolate)
# # Print the first few rows of each dataframe to verify
# for (i in 1:length(CHDLWay3)) {
#   print(head(CHDLWay3[[i]], 10))  # Print the first 10 rows of each dataframe
# }
# 
# plot(CHDLWay3[[2]]$TIMESTAMP, CHDLWay3[[2]]$canopy_height)
# plot(CHDLWay3[[3]]$TIMESTAMP, CHDLWay3[[3]]$canopy_height)
# plot(CHDLWay3[[4]]$TIMESTAMP, CHDLWay3[[4]]$canopy_height)
# plot(CHDLWay3[[5]]$TIMESTAMP, CHDLWay3[[5]]$canopy_height)
# plot(CHDLWay3[[6]]$TIMESTAMP, CHDLWay3[[6]]$canopy_height)
# 
# plot(CHDLWay4[[2]]$TIMESTAMP, CHDLWay4[[2]]$canopy_height)
# plot(CHDLWay4[[3]]$TIMESTAMP, CHDLWay4[[3]]$canopy_height)
# plot(CHDLWay4[[1]]$TIMESTAMP, CHDLWay4[[1]]$canopy_height)
# 
# ######## LE fix #######
# 
# 
# 
