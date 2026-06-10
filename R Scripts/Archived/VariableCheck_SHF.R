library(ggplot2)
library(dplyr)
library(tidyr)
library(data.table)
library(fs) # For directory handling

# 1. Define New Directories and File Paths
way3_directory <- "C:/Users/rbmahbub/Documents/RProjects/AmerifluxDataSubmission_LandscapeFlux/Data/InputLocalProcessedData/MasterFiles/Way3"
way4_directory <- "C:/Users/rbmahbub/Documents/RProjects/AmerifluxDataSubmission_LandscapeFlux/Data/InputLocalProcessedData/MasterFiles/Way4"
unit_file_path <- "C:/Users/rbmahbub/Documents/RProjects/AmerifluxDataSubmission_LandscapeFlux/Data/InputLocalProcessedData/MasterFiles/unitdf.csv"

output_base_dir <- "C:/Users/rbmahbub/Documents/RProjects/AmerifluxDataSubmission_LandscapeFlux/SoilHeatFluxCalculation/Figures"

# Create directory if it doesn't exist
if (!dir.exists(output_base_dir)) {
  dir.create(output_base_dir, recursive = TRUE)
}

# 2. Load Unit Metadata
# Assuming unitdf has columns 'variable' and 'unit'
units_df <- tryCatch({
  read.csv(unit_file_path)
}, error = function(e) {
  message("Unit file not found, defaulting to generic labels.")
  NULL
})

# 3. Define Plotting Function with Unit Support
plot_and_save <- function(df, site_name, year_label, vars_to_plot, units = NULL) {
  
  # Ensure TIMESTAMP is valid
  if (!"TIMESTAMP" %in% names(df)) {
    warning(paste("No TIMESTAMP in", site_name, year_label))
    return(NULL)
  }
  
  # Ensure TIMESTAMP is POSIXct
  df$TIMESTAMP <- as.POSIXct(df$TIMESTAMP)
  
  # Create a sub-folder for this specific dataset
  save_path <- file.path(output_base_dir, paste0(site_name, "_", year_label))
  if (!dir.exists(save_path)) dir.create(save_path, recursive = TRUE)
  
  for (var in vars_to_plot) {
    if (var %in% names(df)) {
      # Try to find unit for the variable
      unit_label <- ""
      if (!is.null(units) && var %in% units$variable) {
        unit_label <- paste0(" (", units$unit[units$variable == var][1], ")")
      }
      
      p <- ggplot(df, aes(x = TIMESTAMP, y = .data[[var]])) +
        geom_line(color = "steelblue", alpha = 0.8) +
        geom_point(size = 0.5, alpha = 0.3, color = "darkblue") +
        theme_minimal() +
        labs(
          title = paste(site_name, "-", var, "(", year_label, ")"),
          x = "Time",
          y = paste0(var, unit_label)
        ) +
        theme(plot.title = element_text(hjust = 0.5))
      
      # Save the plot
      file_name <- paste0(site_name, "_", year_label, "_", var, ".png")
      ggsave(filename = file.path(save_path, file_name), plot = p, width = 10, height = 6, dpi = 300)
    }
  }
}

# 4. Define Files to Read (Updated Names)
years <- c("2018", "2019", "2020", "2021", "2023", "2024")
way3_files <- paste0("way3_", years, ".csv")
way4_files <- paste0("way4_", years, ".csv")

# 5. Define Variables for each Way
way3_vars <- c("shf_Avg.1.", "shf_Avg.2.", "shf_Avg.3.", 
               "del_TS.1.", "del_TS.2.", "del_TS.3.", "del_TS.4.", 
               "swcorr", "Lvl_m_Avg", "G1", "G2", "G3", "Gavg")

way4_vars <- c("shf_Avg.1.", "shf_Avg.2.", "shf_cal.1.", "shf_cal.2.",
               "del_TS.1.", "del_TS.2.", "del_TS.3.", "del_TS.4.",
               "SWC_1_1_1_Calculated", "WTD_Avgcorr", "G1", "G2", "Gavg")

# 6. Execute Processing and Plotting
cat("Starting plot generation with new file paths...\n")

# Process Way 3
for (i in seq_along(way3_files)) {
  file_path <- file.path(way3_directory, way3_files[i])
  if (file.exists(file_path)) {
    data <- as.data.frame(fread(file_path))
    cat(paste("Plotting Way3 - Year:", years[i], "\n"))
    plot_and_save(data, "Way3", years[i], way3_vars, units_df)
  } else {
    warning(paste("File missing:", file_path))
  }
}

# Process Way 4
for (i in seq_along(way4_files)) {
  file_path <- file.path(way4_directory, way4_files[i])
  if (file.exists(file_path)) {
    data <- as.data.frame(fread(file_path))
    cat(paste("Plotting Way4 - Year:", years[i], "\n"))
    plot_and_save(data, "Way4", years[i], way4_vars, units_df)
  } else {
    warning(paste("File missing:", file_path))
  }
}

cat("All plots saved successfully to:", output_base_dir, "\n")