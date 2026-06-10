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


View(way3_data[[5]])
View(way3_data[[1]])

### The first row is the unit row remove it 
way3_data[[5]] <- way3_data[[5]][-1, ]
way3_data[[6]] <- way3_data[[6]][-1, ]

way4_data[[5]] <- way4_data[[5]][-1, ]
way4_data[[6]] <- way4_data[[6]][-1, ]



#### Change the date to the format of way 3 2018-2021


# Convert the TIMESTAMP column in way3_data[[5]]
way3_data[[5]]$TIMESTAMP <- as.character(
  as.POSIXct(way3_data[[5]]$TIMESTAMP, format = "%m/%d/%Y %H:%M", tz = "UTC")
)
way3_data[[6]]$TIMESTAMP <- as.character(
  as.POSIXct(way3_data[[6]]$TIMESTAMP, format = "%m/%d/%Y %H:%M", tz = "UTC")
)


### Shortwave Radiation Rules
