library(data.table)
library(dplyr)
library(ggplot2)

way3_dir <- "C:/Users/rbmahbub/Box/Field_Data/AmeriFlux_Submission_Figures/OutputLocalProcessedData_AFguidedSubmitted/ForLabResearachPurposeMoreColumns/Way3"
way4_dir <- "C:/Users/rbmahbub/Box/Field_Data/AmeriFlux_Submission_Figures/OutputLocalProcessedData_AFguidedSubmitted/ForLabResearachPurposeMoreColumns/Way4"


way3_files <- list.files(way3_dir, pattern="*.csv", full.names=TRUE)
way4_files <- list.files(way4_dir, pattern="*.csv", full.names=TRUE)

way3_data <- lapply(way3_files, fread)
way4_data <- lapply(way4_files, fread)

way3_data <- lapply(way3_data, as.data.frame)
way4_data <- lapply(way4_data, as.data.frame)


calc_way3_flux <- function(df){
  
  df$shf_Avg_1[df$shf_Avg_1 > 80 | df$shf_Avg_1 < -60] <- NA
  df$shf_Avg_2[df$shf_Avg_2 > 80 | df$shf_Avg_2 < -60] <- NA
  df$shf_Avg_3[df$shf_Avg_3 > 80 | df$shf_Avg_3 < -60] <- NA
  df$WTD_1_1_1[df$WTD_1_1_1 < -0.5] <- NA
  df$swcorr <- (df$SWC_1_1_1 * 0.0951) + 49.107
  df$G1 <- df$shf_Avg_1 +
    ((df$del_TS_1 * 0.08 * 1300 *
        (900 + 4190 * df$swcorr / 100) +
        df$del_TS_4 * df$WTD_1_1_1 * 4190 * 1000) / 1800)
  df$G2 <- df$shf_Avg_2 +
    ((df$del_TS_2 * 0.08 * 1300 *
        (900 + 4190 * df$swcorr / 100) +
        df$del_TS_4 * df$WTD_1_1_1 * 4190 * 1000) / 1800)
  df$G3 <- df$shf_Avg_3 +
    ((df$del_TS_3 * 0.08 * 1300 *
        (900 + 4190 * df$swcorr / 100) +
        df$del_TS_4 * df$WTD_1_1_1 * 4190 * 1000) / 1800)
  df$Gavg <- rowMeans(df[,c("G1","G2","G3")], na.rm=TRUE)
  return(df)
}


calc_way4_flux <- function(df){
  df$shf_Fac1 <- df$shf_Avg_1 / df$shf_cal.1. * 15.5424
  df$shf_Fac2 <- df$shf_Avg_2 / df$shf_cal.2. * 15.8983
  df$SWCcorr <- df$SWC_1_1_1 * 0.131 + 41.586
  df$WTD_corr <- df$WTD_1_1_1 - 5.5 + 1.5
  df$G1 <- df$shf_Fac1 +
    (df$del_TS_1 * 0.08 * 1390 *
       (900 + 4190 * df$SWCcorr / 100 * 1000 / 1390) +
       (df$del_TS_3 + df$del_TS_4) *
       df$WTD_corr / 100 * 4190 * 1000 / 2) / 1800
  df$G2 <- df$shf_Fac2 +
    (df$del_TS_2 * 0.08 * 1390 *
       (900 + 4190 * df$SWCcorr / 100 * 1000 / 1390) +
       (df$del_TS_3 + df$del_TS_4) *
       df$WTD_corr / 100 * 4190 * 1000 / 2) / 1800
  df$Gavg <- rowMeans(df[,c("G1","G2")], na.rm=TRUE)
  return(df)
}

way3_processed <- lapply(way3_data, calc_way3_flux)
way4_processed <- lapply(way4_data, calc_way4_flux)



##Way3 2025###
##Way3 2024###
## ================= LIBRARIES ================= ##
library(data.table)
library(dplyr)
library(ggplot2)
library(tidyr)

## ================= DIRECTORIES ================= ##
fig_dir <- "C:/Users/rbmahbub/Documents/RProjects/AmerifluxDataSubmission_LandscapeFlux/SoilHeatFluxCalculation/Figures/EachColumn"
dir.create(file.path(fig_dir,"Way3_2024"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(fig_dir,"Way3_2025"), recursive = TRUE, showWarnings = FALSE)

way3_dir <- "C:/Users/rbmahbub/Documents/RProjects/AmerifluxDataSubmission_LandscapeFlux/Data/ForLabResearachPurposeMoreColumns/Way3"

# Check filenames
list.files(way3_dir)

# Read CSVs
way3_2024 <- fread(file.path(way3_dir, "Way3_2024.csv"))
way3_2025 <- fread(file.path(way3_dir, "Way3_2025.csv"))

# Convert -9999 to NA for all numeric columns
way3_2024 <- as.data.frame(lapply(way3_2024, function(x) {
  if(is.numeric(x)) x[x == -9999] <- NA
  return(x)
}))

way3_2025 <- as.data.frame(lapply(way3_2025, function(x) {
  if(is.numeric(x)) x[x == -9999] <- NA
  return(x)
}))
## ================= FUNCTIONS ================= ##
convert_timestamp <- function(x){
  as.POSIXct(as.character(x), format="%Y%m%d%H%M", tz="UTC")
}

calc_way3_flux_steps <- function(df, year, out_dir){
  
  df <- as.data.frame(df)
  df$time <- convert_timestamp(df$TIMESTAMP_START)
  
  ### Step 1: SHF filter
  df$shf_Avg_1[df$shf_Avg_1 > 80 | df$shf_Avg_1 < -60] <- NA
  df$shf_Avg_2[df$shf_Avg_2 > 80 | df$shf_Avg_2 < -60] <- NA
  df$shf_Avg_3[df$shf_Avg_3 > 80 | df$shf_Avg_3 < -60] <- NA
  
  p1 <- ggplot(df, aes(time, shf_Avg_1)) +
    geom_line(alpha=0.6) +
    labs(title=paste(year,"SHF Plate 1"), x="Time",
         y=expression("SHF Plate 1 (W "*m^{-2}*")")) +
    theme_bw()
  ggsave(filename = file.path(out_dir,"01_SHF_plate1.png"), plot = p1, width = 10, height = 5)
  
  ### Step 2: WTD
  df$WTD_1_1_1[df$WTD_1_1_1 < -0.5] <- NA
  p2 <- ggplot(df, aes(time, WTD_1_1_1)) +
    geom_line(alpha=0.6) +
    labs(title=paste(year,"Water Table Depth"), x="Time", y="Water Table Depth (m)") +
    theme_bw()
  ggsave(filename = file.path(out_dir,"02_WTD.png"), plot = p2, width = 10, height = 5)
  
  ### Step 3: Soil water correction
  df$swcorr <- (df$SWC_1_1_1 * 0.0951) + 49.107
  p3 <- ggplot(df, aes(time, swcorr)) +
    geom_line(alpha=0.6) +
    labs(title=paste(year,"Soil Water Correction"), x="Time", y="Soil Water Content (%)") +
    theme_bw()
  ggsave(filename = file.path(out_dir,"03_swcorr.png"), plot = p3, width = 10, height = 5)
  
  ### Step 3.5: Temperature gradients (del_TS)
  ts_list <- c("del_TS_1","del_TS_2","del_TS_3","del_TS_4")
  ts_labels <- c("Delta TS 1","Delta TS 2","Delta TS 3","Delta TS 4")
  ts_files <- c("03a_delTS1.png","03b_delTS2.png","03c_delTS3.png","03d_delTS4.png")
  
  for(i in seq_along(ts_list)){
    p_ts <- ggplot(df, aes_string("time", ts_list[i])) +
      geom_line(alpha=0.6) +
      labs(title=paste(year, ts_labels[i]), x="Time", y=expression(Delta*"T (°C)")) +
      theme_bw()
    ggsave(filename = file.path(out_dir, ts_files[i]), plot = p_ts, width = 10, height = 5)
  }
  
  ### Combined TS plot
  df_ts_long <- df %>%
    select(time, del_TS_1, del_TS_2, del_TS_3, del_TS_4) %>%
    pivot_longer(-time, names_to="Sensor", values_to="DeltaTS")
  
  p_all_ts <- ggplot(df_ts_long, aes(time, DeltaTS, color=Sensor)) +
    geom_line(alpha=0.5) +
    labs(title=paste(year,"All Temperature Gradients"), x="Time", y=expression(Delta*"T (°C)")) +
    theme_bw()
  ggsave(filename = file.path(out_dir,"03e_all_delTS.png"), plot = p_all_ts, width = 10, height = 5)
  
  ### Step 4–6: G calculations
  df$G1 <- df$shf_Avg_1 + ((df$del_TS_1 * 0.08 * 1300 * (900 + 4190 * df$swcorr / 100) +
                              df$del_TS_4 * df$WTD_1_1_1 * 4190 * 1000) / 1800)
  df$G2 <- df$shf_Avg_2 + ((df$del_TS_2 * 0.08 * 1300 * (900 + 4190 * df$swcorr / 100) +
                              df$del_TS_4 * df$WTD_1_1_1 * 4190 * 1000) / 1800)
  df$G3 <- df$shf_Avg_3 + ((df$del_TS_3 * 0.08 * 1300 * (900 + 4190 * df$swcorr / 100) +
                              df$del_TS_4 * df$WTD_1_1_1 * 4190 * 1000) / 1800)
  
  p4 <- ggplot(df, aes(time, G1)) +
    geom_line(alpha=0.6) +
    labs(title=paste(year,"G1"), x="Time", y=expression("G1 (W "*m^{-2}*")")) +
    theme_bw()
  ggsave(filename = file.path(out_dir,"04_G1.png"), plot = p4, width = 10, height = 5)
  
  p5 <- ggplot(df, aes(time, G2)) +
    geom_line(alpha=0.6) +
    labs(title=paste(year,"G2"), x="Time", y=expression("G2 (W "*m^{-2}*")")) +
    theme_bw()
  ggsave(filename = file.path(out_dir,"05_G2.png"), plot = p5, width = 10, height = 5)
  
  p6 <- ggplot(df, aes(time, G3)) +
    geom_line(alpha=0.6) +
    labs(title=paste(year,"G3"), x="Time", y=expression("G3 (W "*m^{-2}*")")) +
    theme_bw()
  ggsave(filename = file.path(out_dir,"06_G3.png"), plot = p6, width = 10, height = 5)
  
  ### Step 7: Average G
  df$Gavg <- rowMeans(df[,c("G1","G2","G3")], na.rm=TRUE)
  
  p7 <- ggplot(df, aes(time, Gavg)) +
    geom_line(alpha=0.6) +
    labs(title=paste(year,"Gavg"), x="Time", y=expression("Gavg (W "*m^{-2}*")")) +
    theme_bw()
  ggsave(filename = file.path(out_dir,"07_Gavg.png"), plot = p7, width = 10, height = 5)
  
  return(df)
}

# Helper to convert timestamp strings to POSIXct objects
convert_timestamp <- function(x) {
  # Format %Y%m%d%H%M matches 202401011200 style strings
  as.POSIXct(as.character(x), format="%Y%m%d%H%M", tz="UTC")
}

calc_way3_flux_steps <- function(df, year_label, out_dir) {
  
  # Ensure directory exists
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  
  df <- as.data.frame(df)
  
  # Map the timestamp - Ensure TIMESTAMP_START exists in your dataframe
  df$time <- convert_timestamp(df$TIMESTAMP_START)
  
  ### Step 1: SHF filter (Based on Excel thresholds)
  shf_cols <- c("shf_Avg_2", "shf_Avg_3")
  for(col in shf_cols) {
    if(col %in% names(df)) {
      df[[col]][df[[col]] > 80 | df[[col]] < -60] <- NA
    }
  }
  
  # PLOT FILTERED SHF PLATES
  p_shf_inputs <- df %>%
    select(time, any_of(c("shf_Avg_2", "shf_Avg_3"))) %>%
    pivot_longer(-time, names_to="Sensor", values_to="Flux") %>%
    ggplot(aes(time, Flux, color=Sensor)) +
    geom_line(alpha=0.6) +
    scale_x_datetime(date_breaks = "1 month", date_labels = "%b") +
    labs(title=paste(year_label, "Filtered SHF Plate Readings (2 & 3)"), 
         x="Time", y=expression("SHF (W "*m^{-2}*")")) +
    theme_bw()
  ggsave(filename = file.path(out_dir, "01a_SHF_Plates.png"), plot = p_shf_inputs, width = 10, height = 5)
  
  ### Step 1.5: Delta TS Filter
  # Filters del_TS_2, 3, and 4 to be within [-5, 5] degrees Celsius
  ts_cols <- c("del_TS_2", "del_TS_3", "del_TS_4")
  for(col in ts_cols) {
    if(col %in% names(df)) {
      df[[col]][df[[col]] > 5 | df[[col]] < -5] <- NA
    }
  }
  
  # Plotting the raw Delta TS values for 2 & 3
  p_ts_inputs <- df %>%
    select(time, any_of(c("del_TS_2", "del_TS_3"))) %>%
    pivot_longer(-time, names_to="Sensor", values_to="DeltaT") %>%
    ggplot(aes(time, DeltaT, color=Sensor)) +
    geom_line(alpha=0.6) +
    scale_x_datetime(date_breaks = "1 month", date_labels = "%b") +
    labs(title=paste(year_label, "Soil Temperature Gradients (del_TS 2 & 3)"), 
         x="Time", y=expression("Temperature Change (°C)")) +
    theme_bw()
  ggsave(filename = file.path(out_dir, "01b_DeltaTS_Soil.png"), plot = p_ts_inputs, width = 10, height = 5)
  
  # PLOT DEL_TS_4 (The Water/Surface Gradient)
  if("del_TS_4" %in% names(df)) {
    p_ts4 <- ggplot(df, aes(time, del_TS_4)) +
      geom_line(alpha=0.6, color="darkred") +
      scale_x_datetime(date_breaks = "1 month", date_labels = "%b") +
      labs(title=paste(year_label, "Water/Surface Temperature Gradient (del_TS_4)"),
           x="Time", y=expression("Delta T4 (°C)")) +
      theme_bw()
    ggsave(filename = file.path(out_dir, "01c_DeltaTS4.png"), plot = p_ts4, width = 10, height = 5)
  }
  
  ### Step 2: Water Table Depth (WTD) Filter
  if("WTD_1_1_1" %in% names(df)) {
    df$WTD_1_1_1[df$WTD_1_1_1 < -0.5 | df$WTD_1_1_1 > 0.5] <- NA
    p2 <- ggplot(df, aes(time, WTD_1_1_1)) +
      geom_line(alpha=0.6, color="steelblue") +
      scale_x_datetime(date_breaks = "1 month", date_labels = "%b") +
      labs(title=paste(year_label, "Water Table Depth"),
           x="Time", y="Water Table Depth (m)") +
      theme_bw()
    ggsave(filename = file.path(out_dir, "02_WTD.png"), plot = p2, width = 10, height = 5)
  }
  
  ### Step 3: Soil water correction & Absurd Data Filter
  if("SWC_1_1_1" %in% names(df)) {
    df$SWC_1_1_1[df$SWC_1_1_1 <= -9000 | df$SWC_1_1_1 > 100] <- NA
    df$swcorr <- (df$SWC_1_1_1 * 0.0951) + 49.107
    
    p3 <- ggplot(df, aes(time, swcorr)) +
      geom_line(alpha=0.6, color="darkgreen") +
      scale_x_datetime(date_breaks = "1 month", date_labels = "%b") +
      labs(title=paste(year_label, "Soil Water Correction"),
           x="Time", y="Corrected VWC (%)") +
      theme_bw()
    ggsave(filename = file.path(out_dir, "03_swcorr.png"), plot = p3, width = 10, height = 5)
  }
  
  ### Step 4: G calculations (Breakdown of Components)
  # Soil Storage Component (Part A)
  df$soil_storage_2 <- (df$del_TS_2 * 0.08 * 1300 * (900 + 4190 * df$swcorr / 100)) / 1800
  df$soil_storage_3 <- (df$del_TS_3 * 0.08 * 1300 * (900 + 4190 * df$swcorr / 100)) / 1800
  
  # Water Table Storage Component (Part B)
  df$water_storage <- (df$del_TS_4 * df$WTD_1_1_1 * 4190 * 1000) / 1800
  
  # Physics Guard: Cap water storage if it remains physically absurd after filtering TS4
  df$water_storage[abs(df$water_storage) > 500] <- NA
  
  # Final Fluxes
  df$G2 <- df$shf_Avg_2 + df$soil_storage_2 + df$water_storage
  df$G3 <- df$shf_Avg_3 + df$soil_storage_3 + df$water_storage
  
  ### Step 5: Visualizing all components
  # Plot Soil Storage Part
  p_soil <- df %>%
    select(time, any_of(c("soil_storage_2", "soil_storage_3"))) %>%
    pivot_longer(-time, names_to="Sensor", values_to="Storage") %>%
    ggplot(aes(time, Storage, color=Sensor)) +
    geom_line(alpha=0.6) +
    scale_x_datetime(date_breaks = "1 month", date_labels = "%b") +
    labs(title=paste(year_label, "Soil Storage Component"), x="Time", y=expression("Storage (W "*m^{-2}*")")) +
    theme_bw()
  ggsave(filename = file.path(out_dir, "04a_Soil_Storage.png"), plot = p_soil, width = 10, height = 5)
  
  # Plot Water Storage Part
  p_water <- ggplot(df, aes(time, water_storage)) +
    geom_line(alpha=0.6, color="cyan4") +
    scale_x_datetime(date_breaks = "1 month", date_labels = "%b") +
    labs(title=paste(year_label, "Water Table Storage Component"), x="Time", y=expression("Storage (W "*m^{-2}*")")) +
    theme_bw()
  ggsave(filename = file.path(out_dir, "04b_Water_Storage.png"), plot = p_water, width = 10, height = 5)
  
  # Final Output Plot (Gavg)
  df$Gavg <- rowMeans(df[, c("G2", "G3")], na.rm = TRUE)
  
  p7 <- ggplot(df, aes(time, Gavg)) +
    geom_line(alpha=0.6, color="red") +
    scale_x_datetime(date_breaks = "1 month", date_labels = "%b") +
    labs(title=paste(year_label, "Final Average Soil Heat Flux (Gavg)"),
         x="Time", y=expression("Gavg (W "*m^{-2}*")")) +
    theme_bw()
  ggsave(filename = file.path(out_dir, "07_Gavg.png"), plot = p7, width = 10, height = 5)
  
  return(df)
}
## ================= RUN ================= ##
way3_2024_processed <- calc_way3_flux_steps(
  way3_2024, "Way3 2024", file.path(fig_dir,"Way3_2024")
)

way3_2025_processed <- calc_way3_flux_steps(
  way3_2025, "Way3 2025", file.path(fig_dir,"Way3_2025")
)


summarize_selected_vars <- function(df, label="Dataset") {
  
  vars <- c(
    "del_TS_1","del_TS_2","del_TS_3","del_TS_4",
    "shf_Avg_1","shf_Avg_2","shf_Avg_3",
    "panel_tmpr_Avg","BattV_Avg",
    "swcorr","soil_storage_2","soil_storage_3",
    "water_storage","G2","G3","Gavg"
  )
  
  vars <- vars[vars %in% colnames(df)]
  
  summary_stats <- df %>%
    dplyr::select(all_of(vars)) %>%
    summarise(across(everything(),
                     list(mean = ~mean(.x, na.rm = TRUE),
                          sd   = ~sd(.x, na.rm = TRUE)))) %>%
    pivot_longer(
      everything(),
      names_to = c("Variable", ".value"),
      names_pattern = "(.+)_(mean|sd)"   # ✅ KEY FIX
    )
  
  cat("\n============================\n")
  cat("Summary for:", label, "\n")
  cat("============================\n")
  
  print(summary_stats, n = Inf)
  
  return(summary_stats)
}

summary_2024 <- summarize_selected_vars(way3_2024_processed, "Way3 2024")
summary_2025 <- summarize_selected_vars(way3_2025_processed, "Way3 2025")
summary_2024

