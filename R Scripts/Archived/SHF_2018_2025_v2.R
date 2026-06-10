## ================= LIBRARIES ================= ##
library(data.table)
library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)
library(readxl)

## ================= DIRECTORIES & PATHS ================= ##
fig_dir  <- "C:/Users/rbmahbub/Documents/RProjects/AmerifluxDataSubmission_LandscapeFlux/SoilHeatFluxCalculation/Figures/EachColumn"
way3_dir <- "C:/Users/rbmahbub/Documents/RProjects/AmerifluxDataSubmission_LandscapeFlux/Data/ForLabResearachPurposeMoreColumns/Way3"
wtd_file_path <- "C:/Users/rbmahbub/Documents/RProjects/AmerifluxDataSubmission_LandscapeFlux/Data/InputLocalRawData/WTD/2024_way3_ECside.xlsx"

## ================= NEW DATA PROCESSING (WTD File) ================= ##

wtd_raw <- read_excel(wtd_file_path, sheet = "Way 3 EC Side", skip = 1)
wtd_raw <- wtd_raw[-(1:2), ]

parse_excel_time <- function(t_vec) {
  t_vec <- as.character(t_vec)
  num_vals <- suppressWarnings(as.numeric(t_vec))
  is_numeric_serial <- !is.na(num_vals) & num_vals > 30000 & num_vals < 60000
  out_posix <- as.POSIXct(rep(NA, length(t_vec)))
  
  if (any(is_numeric_serial)) {
    out_posix[is_numeric_serial] <- as.POSIXct(num_vals[is_numeric_serial] * 86400, 
                                               origin = "1899-12-30", tz = "UTC")
  }
  if (any(!is_numeric_serial)) {
    string_dates <- parse_date_time(t_vec[!is_numeric_serial], 
                                    orders = c("mdy HMS p", "mdy HM p", "mdy HMS", "ymd HMS"), tz = "UTC")
    out_posix[!is_numeric_serial] <- string_dates
  }
  return(with_tz(out_posix, tzone = "UTC"))
}

wtd_raw$TIMESTAMP <- parse_excel_time(wtd_raw$TIMESTAMP)
wtd_raw$T107_C_1cm_Avg <- as.numeric(as.character(wtd_raw$T107_C_1cm_Avg))

wtd_clean <- wtd_raw %>%
  arrange(TIMESTAMP) %>%
  mutate(del_TS_4_new = T107_C_1cm_Avg - lag(T107_C_1cm_Avg)) %>%
  filter(!is.na(TIMESTAMP)) %>%
  select(TIMESTAMP, del_TS_4_new) %>%
  rename(time_join = TIMESTAMP)

## ================= MAIN DATA PROCESSING ================= ##

way3_2024 <- fread(file.path(way3_dir, "Way3_2024.csv"))
way3_2025 <- fread(file.path(way3_dir, "Way3_2025.csv"))

prepare_and_merge <- function(df, wtd_ref) {
  df[df == -9999] <- NA
  df$time_join <- as.POSIXct(as.character(df$TIMESTAMP_START), format="%Y%m%d%H%M", tz="UTC")
  df <- left_join(df, wtd_ref, by = "time_join")
  if("del_TS_4_new" %in% names(df)) {
    df$del_TS_4 <- df$del_TS_4_new
    df$del_TS_4_new <- NULL
  }
  return(df)
}

way3_2024 <- prepare_and_merge(way3_2024, wtd_clean)
way3_2025 <- prepare_and_merge(way3_2025, wtd_clean)

## ================= FLUX CALCULATION & PLOTTING FUNCTION ================= ##

calc_way3_flux_steps <- function(df, year_label, out_dir) {
  
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  df <- as.data.frame(df)
  df$time <- df$time_join 
  
  # --- CONSTANTS (from Kosana/Manual) ---
  rho_soil <- 1300  # kg/m3 (Way3)
  Cp_dry   <- 900   # J/kg/K
  Cp_water <- 4190  # J/kg/K
  rho_water <- 1000 # kg/m3
  plate_depth <- 0.05 # 5cm
  t_interval <- 1800 # 30 mins
  
  # --- STEP 0: Filter Inputs ---
  required_cols <- c("del_TS_2", "del_TS_3", "del_TS_4", "WTD_1_1_1", "SWC_1_1_1", "shf_Avg_2", "shf_Avg_3")
  for(col in required_cols) {
    if(!(col %in% names(df))) df[[col]] <- NA
    df[[col]] <- as.numeric(as.character(df[[col]]))
    
    if(grepl("shf", col)) df[[col]][df[[col]] > 80 | df[[col]] < -60] <- NA
    if(grepl("del_TS", col)) {
      df[[col]][df[[col]] > 5 | df[[col]] < -5] <- NA
      mu <- mean(df[[col]], na.rm=T); sig <- sd(df[[col]], na.rm=T)
      df[[col]][df[[col]] > (mu+3*sig) | df[[col]] < (mu-3*sig)] <- NA
    }
  }
  
  # --- STEP 1: Process Intermediate Variables ---
  df$swcorr <- (df$SWC_1_1_1 * 0.0951) + 49.107
  
  # Effective Water Depth (Dw_eff) - Standing water layer
  # Rick Snyder: "water level >= 0.005 m" trigger
  df$Dw_eff <- ifelse(df$WTD_1_1_1 >= 0.005, df$WTD_1_1_1, 0)
  # Limit Dw_eff for surface sensor impact (if sensor is at surface, depth is capped)
  df$Dw_eff[df$Dw_eff > 0.5] <- 0.5 # Safety cap
  
  # --- STEP 2: Heat Storage Calculations (J/m2) ---
  # Based on Campbell Scientific manual equation (S = D * del_T * Cs / t)
  # Cs (Heat Capacity) = rho_soil * (Cp_dry + Cp_water * VWC)
  
  # STORAGE TERM 1: Soil (above plate)
  # If flooded (WTD > 0.005), Snyder uses water capacity for this layer too.
  # We apply conditional heat capacity:
  Cs_soil <- ifelse(df$WTD_1_1_1 >= 0.005, 
                    rho_water * Cp_water, 
                    rho_soil * (Cp_dry + Cp_water * (df$swcorr / 100)))
  
  df$S_soil_2_J <- (df$del_TS_2 * plate_depth * Cs_soil)
  df$S_soil_3_J <- (df$del_TS_3 * plate_depth * Cs_soil)
  
  # STORAGE TERM 2: Standing Water
  # del_TS_4 is change in surface/water temperature
  df$S_water_J  <- (df$del_TS_4 * df$Dw_eff * rho_water * Cp_water)
  
  # --- STEP 3: Final Flux Calculations (W/m2) ---
  df$Flux_Plate_avg <- rowMeans(df[, c("shf_Avg_2", "shf_Avg_3")], na.rm=TRUE)
  df$Flux_Soil_W <- ((df$S_soil_2_J + df$S_soil_3_J) / 2) / t_interval
  df$Flux_Water_W <- df$S_water_J / t_interval
  
  # Sum of Plate + Storage
  df$Gavg <- df$Flux_Plate_avg + df$Flux_Soil_W + df$Flux_Water_W
  
  # Snyder/Kosana Physics Guard: Flooded Paddy G should be +/- 50 W/m2.
  # We will flag values > 150 as likely erroneous
  df$Gavg_unfiltered <- df$Gavg
  df$Gavg[abs(df$Gavg) > 150] <- NA
  
  # --- STEP 4: Diagnosis of High Gavg (> 50) ---
  high_g_df <- df %>% filter(abs(Gavg) > 50)
  if(nrow(high_g_df) > 0) {
    cat(paste0("\n>>> PHYSICAL CHECK (", year_label, "): ", nrow(high_g_df), " intervals > 50 W/m2\n"))
    diag_summary <- high_g_df %>%
      summarise(
        avg_G = mean(Gavg, na.rm=T),
        contrib_Water = mean(Flux_Water_W, na.rm=T),
        avg_DwEff = mean(Dw_eff, na.rm=T),
        perc_Water = (contrib_Water / avg_G) * 100
      )
    print(diag_summary)
  }
  
  # --- STEP 5: Save Diagnostic Graphs ---
  save_p <- function(p, name) ggsave(file.path(out_dir, paste0(name, ".png")), p, width=10, height=5)
  
  # Comparison plot: Plate vs Storage vs Total
  p_comp <- df %>% 
    mutate(Total_Storage_W = Flux_Soil_W + Flux_Water_W) %>%
    select(time, Flux_Plate_avg, Total_Storage_W, Gavg) %>%
    pivot_longer(-time) %>%
    ggplot(aes(time, value, color=name)) + geom_line(alpha=0.6) + theme_bw() +
    labs(title=paste(year_label, "Component Contribution"), y=expression("W "*m^{-2}), x="Time")
  save_p(p_comp, "Stage5_Flux_Components")
  
  # Final Flux Plot
  p_final <- ggplot(df, aes(time, Gavg)) + geom_line(color="red") + theme_bw() +
    labs(title=paste(year_label, "Final Soil Heat Flux (Gavg)"), 
         subtitle="Storage-corrected using paddy-specific heat capacity logic",
         y=expression("G (W "*m^{-2}*")"), x="Time")
  save_p(p_final, "Stage7_Final_Gavg")
  
  return(df)
}

## ================= EXECUTION ================= ##

way3_2024_proc <- calc_way3_flux_steps(way3_2024, "Way3 2024", file.path(fig_dir,"Way3_2024"))
way3_2025_proc <- calc_way3_flux_steps(way3_2025, "Way3 2025", file.path(fig_dir,"Way3_2025"))

cat("\n--- EXECUTION COMPLETE: Logic aligned with Kosana/Campbell Manual ---")
