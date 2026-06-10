## ========================================================================== ##
## PROJECT: Soil Heat Flux (G) Calculation - Way 3 | Refined Version         ##
## AUTHOR: Gemini (Refining Riasad Bin Mahbub's Script)                      ##
## DESCRIPTION: G calculation using SHF plate 2 ONLY, excluding Sensor 1 & 3.##
##              Includes NetRad calculation, QAQC filtering, and G-Cutoff    ##
##              sensitivity analysis for Energy Balance Closure.             ##
## ========================================================================== ##

library(data.table)
library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)
library(readxl)
library(viridis)

## --- CONFIGURATION ---
site <- "Way3"
year <- "2024"

# Set output directory for figures
fig_dir <- "C:/Users/rbmahbub/Documents/RProjects/AmerifluxDataSubmission_LandscapeFlux/SoilHeatFluxCalculation/Figures/Refined/Way3_2024"
if (!dir.exists(fig_dir)) dir.create(fig_dir, recursive = TRUE)

flux_file <- "C:/Users/rbmahbub/Documents/RProjects/AmerifluxDataSubmission_LandscapeFlux/Data/ForLabResearachPurposeMoreColumns/Way3/Way3_2024.csv"
wtd_file  <- "C:/Users/rbmahbub/Documents/RProjects/AmerifluxDataSubmission_LandscapeFlux/Data/InputLocalRawData/WTD/2024_way3_ECside.xlsx"
wtd_sheet <- "Way 3 EC Side"

## --- HELPER FUNCTION ---
parse_time <- function(t) {
  t <- as.character(t)
  num <- suppressWarnings(as.numeric(t))
  out <- as.POSIXct(rep(NA_real_, length(t)), origin = "1970-01-01", tz = "UTC")
  is_num <- !is.na(num) & num > 30000 & num < 60000
  if (any(is_num)) {
    out[is_num] <- as.POSIXct(num[is_num] * 86400,
                              origin = "1899-12-30",
                              tz = "America/Chicago")
  }
  if (any(!is_num)) {
    out[!is_num] <- parse_date_time(
      t[!is_num],
      orders = c("mdy HMS p", "mdy HM p", "mdy HMS", "ymd HMS", "mdy HM"),
      tz = "America/Chicago",
      quiet = TRUE
    )
  }
  return(with_tz(out, "UTC"))
}

## --- 1. PROCESS WTD AND WATER TEMPERATURE ---
cat("Processing WTD and Water Temperature data...\n")

wtd_raw <- read_excel(wtd_file, sheet = wtd_sheet, skip = 1)
wtd_raw <- wtd_raw[-(1:2), ] 

wtd_clean <- wtd_raw %>%
  mutate(
    time_raw  = parse_time(TIMESTAMP),
    time_join = floor_date(time_raw, "30 minutes"),
    temp_val  = suppressWarnings(as.numeric(T107_C_1cm_Avg))
  ) %>%
  arrange(time_raw) %>%
  mutate(
    del_Tw = temp_val - lag(temp_val)
  ) %>%
  filter(!is.na(time_join)) %>%
  select(time_join, del_Tw) %>%
  distinct(time_join, .keep_all = TRUE)

## --- 2. LOAD FLUX DATA ---
cat("Loading Flux data...\n")

df <- as.data.frame(fread(flux_file))
df[df == -9999] <- NA

df <- df %>%
  mutate(
    time_join = floor_date(
      as.POSIXct(as.character(TIMESTAMP_START),
                 format = "%Y%m%d%H%M",
                 tz = "UTC"),
      "30 minutes"
    )
  )

## --- 3. NEAREST JOIN ---
cat("Merging datasets...\n")
setDT(df); setDT(wtd_clean)
setkey(df, time_join); setkey(wtd_clean, time_join)
df <- wtd_clean[df, roll = "nearest"]
setDF(df)

## --- 4. CALCULATIONS ---

# Constants (Based on Kosana's metadata)
BD_soil     <- 1300   # kg/m3 (Way 3)
Cp_dry      <- 900    # J/kg/K
Cp_water    <- 4190   # J/kg/K
rho_water   <- 1000   # kg/m3 (Density of water)
plate_depth <- 0.05   # m (D)
t_interval  <- 1800   # s (30 min)

# Standardizing energy variables and ensuring numeric types
req <- c("del_TS_2", "del_Tw", "WTD_1_1_1", "SWC_1_1_1", 
         "shf_Avg_2", "LE", "H", "SW_IN", "SW_OUT", "LW_IN", "LW_OUT",
         "LE_SSITC_TEST", "H_SSITC_TEST")
for (col in req) {
  if (!(col %in% names(df))) df[[col]] <- NA
  df[[col]] <- as.numeric(as.character(df[[col]]))
}

# 4a. Net Radiation (NetRad) Calculation
df$NetRad <- (df$SW_IN - df$SW_OUT) + (df$LW_IN - df$LW_OUT)

# 4b. Filter Raw SHF Plate 2 (3-sigma outlier removal)
# Doing this BEFORE storage term additions to clean sensor noise
shf2_mean <- mean(df$shf_Avg_2, na.rm = TRUE)
shf2_sd   <- sd(df$shf_Avg_2, na.rm = TRUE)
df$shf_Avg_2_clean <- ifelse(abs(df$shf_Avg_2 - shf2_mean) <= (3 * shf2_sd), df$shf_Avg_2, NA)

# 4c. Specific Heat Capacity of Soil (Cs)
df$Cs <- (BD_soil * Cp_dry) + (df$SWC_1_1_1 * Cp_water)

# 4d. Soil Storage Term (S_soil)
df$s_soil <- (df$del_TS_2 * plate_depth * df$Cs) / t_interval

# 4e. Water Storage Term (S_water)
df$is_flooded <- !is.na(df$WTD_1_1_1) & df$WTD_1_1_1 >= 0.005
df$Dw <- ifelse(df$is_flooded, df$WTD_1_1_1, 0)
df$s_water <- (df$del_Tw * df$Dw * Cp_water) / t_interval

# 4f. G Calculation (Using CLEANED plate 2 ONLY)
df$G_plate <- df$shf_Avg_2_clean

# Final Total G at Surface
df$G_total <- df$G_plate + df$s_soil + df$s_water

## --- 5. CLEANING & FILTERING (Final Check) ---

# 5a. Non-QAQC Filter (Final 3-sigma on the TOTAL calculated flux)
g_total_mean <- mean(df$G_total, na.rm = TRUE)
g_total_sd   <- sd(df$G_total, na.rm = TRUE)
df$G_filtered <- ifelse(abs(df$G_total - g_total_mean) <= (3 * g_total_sd), df$G_total, NA)

# 5b. QAQC Filtered G (High-quality H/LE flags 0 or 1)
df$G_filtered_qaqc <- ifelse(df$LE_SSITC_TEST %in% c(0, 1) & 
                               df$H_SSITC_TEST %in% c(0, 1), 
                             df$G_filtered, NA)

## --- 6. SENSITIVITY ANALYSIS: G-FLUX CUTOFFS ---
cat("Running G-Flux Cutoff Sensitivity Analysis...\n")

cutoffs <- c(80, 100, 120, 150, 200)
sensitivity_results <- data.frame()

for(cutoff in cutoffs) {
  cat(paste0("  Processing Cutoff: ", cutoff, " W/m2\n"))
  
  df_temp <- df %>%
    mutate(G_cutoff = ifelse(abs(G_filtered_qaqc) <= cutoff, G_filtered_qaqc, NA)) %>%
    mutate(Turb_Fluxes = H + LE, Avail_Energy = NetRad - G_cutoff) %>%
    filter(!is.na(Turb_Fluxes) & !is.na(Avail_Energy))
  
  if (nrow(df_temp) > 10) {
    fit <- lm(Turb_Fluxes ~ Avail_Energy, data = df_temp)
    res <- data.frame(
      Flux_Cutoff = cutoff,
      Slope = round(coef(fit)[2], 3),
      R2 = round(summary(fit)$r.squared, 3),
      Sample_Size = nrow(df_temp)
    )
    sensitivity_results <- rbind(sensitivity_results, res)
    
    p_cutoff <- ggplot(df_temp, aes(x = Avail_Energy, y = Turb_Fluxes)) +
      geom_bin2d() + scale_fill_viridis_c() +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
      geom_smooth(method = "lm", color = "red", se = FALSE) +
      annotate("text", x = -Inf, y = Inf, hjust = -0.1, vjust = 1.5,
               label = paste0("Slope: ", res$Slope, "\nR2: ", res$R2, "\nN: ", res$Sample_Size),
               color = "red", fontface = "bold") +
      theme_bw() +
      labs(title = paste("EB Closure - G Cutoff:", cutoff, "W/m2"),
           x = expression(Available~Energy~(NetRad - G)~(W~m^-2)),
           y = expression(Turbulent~Fluxes~(H + LE)~(W~m^-2)))
    
    ggsave(file.path(fig_dir, paste0("EB_Closure_Cutoff_", cutoff, ".png")), p_cutoff, width = 8, height = 6)
  }
}

# 6b. Rn-G Threshold Analysis (Nominal Amount > 50)
cat("Running Available Energy Sensitivity Analysis (Rn-G > 50)...\n")

df_threshold <- df %>%
  mutate(Turb_Fluxes = H + LE, Avail_Energy = NetRad - G_filtered_qaqc) %>%
  filter(!is.na(Turb_Fluxes) & !is.na(Avail_Energy)) %>%
  filter(Avail_Energy > 50)

if (nrow(df_threshold) > 10) {
  fit_t <- lm(Turb_Fluxes ~ Avail_Energy, data = df_threshold)
  slope_t <- round(coef(fit_t)[2], 3)
  r2_t    <- round(summary(fit_t)$r.squared, 3)
  
  p_threshold <- ggplot(df_threshold, aes(x = Avail_Energy, y = Turb_Fluxes)) +
    geom_bin2d() + scale_fill_viridis_c() +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey50") +
    geom_smooth(method = "lm", color = "darkgreen", se = FALSE) +
    annotate("text", x = -Inf, y = Inf, hjust = -0.1, vjust = 1.5,
             label = paste0("Slope (Rn-G > 50): ", slope_t, "\nR2: ", r2_t, "\nN: ", nrow(df_threshold)),
             color = "darkgreen", fontface = "bold") +
    theme_bw() +
    labs(title = "EB Closure Sensitivity: Available Energy > 50 W/m2",
         subtitle = "Reducing bias from values clustered near zero",
         x = expression(Available~Energy~(NetRad - G)~(W~m^-2)),
         y = expression(Turbulent~Fluxes~(H + LE)~(W~m^-2)))
  
  ggsave(file.path(fig_dir, "EB_Closure_Threshold_50.png"), p_threshold, width = 8, height = 6)
  
  # Append to results log
  res_t <- data.frame(Flux_Cutoff = "Rn-G > 50", Slope = slope_t, R2 = r2_t, Sample_Size = nrow(df_threshold))
  sensitivity_results <- rbind(sensitivity_results, res_t)
}

# Save results
audit_dir <- "C:/Users/rbmahbub/Documents/RProjects/AmerifluxDataSubmission_LandscapeFlux/SoilHeatFluxCalculation/DataPercentage"
if (!dir.exists(audit_dir)) dir.create(audit_dir, recursive = TRUE)
write.csv(sensitivity_results, file.path(audit_dir, "G_Cutoff_Sensitivity_Report.csv"), row.names = FALSE)
print(sensitivity_results)

## --- 7. DIAGNOSTIC PLOTS ---
cat("Generating diagnostic plots...\n")

hist_data <- data.frame(
  G_plate_2_raw = df$shf_Avg_2,
  G_plate_2_clean = df$shf_Avg_2_clean,
  S_soil = df$s_soil,
  G_filtered = df$G_filtered
)

p_hist <- hist_data %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Value") %>%
  ggplot(aes(x = Value, fill = Variable)) +
  geom_histogram(bins = 50, alpha = 0.7, color = "black") +
  facet_wrap(~Variable, scales = "free", ncol = 2) +
  theme_minimal() +
  labs(title = "Distribution of G Components (Pre-filtered Plate 2)", x = expression(W~m^-2), y = "Frequency") +
  theme(legend.position = "none")

ggsave(file.path(fig_dir, "01_G_Histograms_2x2.png"), p_hist, width = 10, height = 8)

p_ts <- ggplot(df, aes(x = time_join)) +
  geom_line(aes(y = G_filtered, color = "G_filtered (Final Filter)"), alpha = 0.4) +
  geom_line(aes(y = G_filtered_qaqc, color = "G_filtered (QAQC & Final Filter)"), size = 0.8) +
  scale_color_manual(values = c("G_filtered (Final Filter)" = "grey60", 
                                "G_filtered (QAQC & Final Filter)" = "red")) +
  labs(title = paste(site, year, "Soil Heat Flux Comparison (Pre-filtered Plate 2)"),
       y = expression(W~m^-2), x = "Time", color = "Filter Type") +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave(file.path(fig_dir, "03_G_timeseries_comparison.png"), p_ts, width = 12, height = 6)

## --- 8. DATA AUDIT & STATS REPORT ---
cat("Generating Data Availability and Stats Report...\n")

audit_vars <- c("shf_Avg_2", "shf_Avg_2_clean", "del_TS_2", "del_Tw", "WTD_1_1_1", 
                "SWC_1_1_1", "LE", "H", "NetRad",
                "G_total", "G_filtered", "G_filtered_qaqc")

availability_report <- data.frame(
  Variable      = audit_vars,
  Total_Records = nrow(df),
  Present_Count = sapply(df[audit_vars], function(x) sum(!is.na(x))),
  Mean_Val      = sapply(df[audit_vars], function(x) round(mean(x, na.rm = TRUE), 4)),
  Max_Val       = sapply(df[audit_vars], function(x) round(max(x, na.rm = TRUE), 4)),
  Min_Val       = sapply(df[audit_vars], function(x) round(min(x, na.rm = TRUE), 4))
) %>%
  mutate(
    Missing_Count    = Total_Records - Present_Count,
    Availability_Pct = round((Present_Count / Total_Records) * 100, 2)
  ) %>%
  select(Variable, Total_Records, Present_Count, Availability_Pct, Mean_Val, Max_Val, Min_Val)

audit_path <- file.path(audit_dir, "Way3_2024_Data_Stats_Report_SHF2.csv")
write.csv(availability_report, audit_path, row.names = FALSE)

cat("Execution Complete. Results for SHF2 in:", fig_dir, "and", audit_dir, "\n")