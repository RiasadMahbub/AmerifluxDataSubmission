library(lubridate)

# Variables to plot
vars_airtemp_related <- list(
  SensibleHeat = "H",
  SonicTemp    = "T_SONIC",
  MO_Length    = "MO_LENGTH",
  Stability    = "ZL",
  CO2_MR       = "CO2_MIXING_RATIO",
  H2O_MR       = "H2O_MIXING_RATIO",
  CH4_MR       = "CH4_MIXING_RATIO",
  LE           = "LE",
  USTAR        = "USTAR"
)

vars_airtemp_related <- unlist(vars_airtemp_related)

# Output folder
out_dir <- "C:/Users/rbmahbub/Documents/RProjects/AmerifluxDataSubmission_LandscapeFlux/Figure/TemperatureIssue"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# Unit lookup table
unit_df <- data.frame(
  variable = c("H", "T_SONIC", "MO_LENGTH", "ZL", "CO2_MIXING_RATIO", "H2O_MIXING_RATIO",
               "CH4_MIXING_RATIO", "LE", "USTAR"),
  unit = c("W/m2", "K", "m", "-", "µmol/mol", "mmol/mol", "nmol/mol", "W/m2", "m/s"),
  stringsAsFactors = FALSE
)

get_unit <- function(var) {
  u <- unit_df$unit[unit_df$variable == var]
  if (length(u) == 0) u <- ""
  return(u)
}

# Plotting function
plot_two_row_scatter <- function(df, var, site, year) {
  
  # Convert TIMESTAMP_START to POSIXct
  df$TIME <- ymd_hm(df$TIMESTAMP_START)
  
  # Skip if missing TA or var
  if (!"TA_1_1_1" %in% names(df)) return(NULL)
  if (!var %in% names(df)) return(NULL)
  if (!is.numeric(df$TA_1_1_1) || !is.numeric(df[[var]])) return(NULL)
  
  # File name with year
  fname <- paste0(site, "_", year, "_TA_vs_", var, ".png")
  png(file.path(out_dir, fname), width = 1600, height = 1200, res = 200)
  
  par(mfrow = c(2,1), mar = c(4,4,3,1))
  
  # Row 1: TA scatter
  plot(df$TIME, df$TA_1_1_1,
       pch = 16, col = "red",
       xlab = "Time", ylab = "Air Temperature (°C)",
       main = paste(site, year, "- Air Temperature"))
  
  # Row 2: Variable scatter with units
  var_unit <- get_unit(var)
  ylab_text <- ifelse(var_unit != "", paste0(var, " [", var_unit, "]"), var)
  
  plot(df$TIME, df[[var]],
       pch = 16, col = "blue",
       xlab = "Time", ylab = ylab_text,
       main = paste(site, year, "-", var))
  
  dev.off()
}

# Way3: datasets 2018–2024
for (i in 1:7) {
  year <- 2017 + i
  df <- all_processed_data_way3[[i]]
  for (v in vars_airtemp_related) {
    plot_two_row_scatter(df, v, site = "Way3", year = year)
  }
}

# Way4: datasets 2018–2024
for (i in 1:7) {
  year <- 2017 + i
  df <- all_processed_data_way4[[i]]
  for (v in vars_airtemp_related) {
    plot_two_row_scatter(df, v, site = "Way4", year = year)
  }
}
