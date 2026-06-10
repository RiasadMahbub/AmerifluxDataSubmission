## ========================================================================== ##
## PROJECT: Soil Heat Flux (G) Calculation - Way 4 | Refined Version          ##
## AUTHOR: Gemini (Refining Way 4 based on Way 3 Structure)                  ##
## DESCRIPTION: G calculation using SHF plates 2 & 3.                        ##
##              Includes 3-sigma and SSITC QAQC filtering.                    ##
## ========================================================================== ##

library(data.table)
library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)
library(readxl)
library(viridis)

## --- CONFIGURATION ---
site <- "Way4"
year <- "2024"

# Set output directory for figures
fig_dir <- "C:/Users/rbmahbub/Documents/RProjects/AmerifluxDataSubmission_LandscapeFlux/SoilHeatFluxCalculation/Figures/Refined/Way4_2024"
if (!dir.exists(fig_dir)) dir.create(fig_dir, recursive = TRUE)

flux_file <- "C:/Users/rbmahbub/Documents/RProjects/AmerifluxDataSubmission_LandscapeFlux/Data/ForLabResearachPurposeMoreColumns/Way4/Way4_2024.csv"
wtd_file  <- "C:/Users/rbmahbub/Documents/RProjects/AmerifluxDataSubmission_LandscapeFlux/Data/InputLocalRawData/WTD/2024_Masterfile_Way4_ECside.xlsx"
wtd_sheet <- "Way 4 - EC Side"

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
wtd_raw <- wtd_raw[-(1:2), ] # Remove header units/type rows

# Identify temperature column (T107)
temp_col <- names(wtd_raw)[grepl("T107", names(wtd_raw))][1]

wtd_clean <- wtd_raw %>%
  mutate(
    time_raw  = parse_time(TIMESTAMP),
    time_join = round_date(time_raw, "30 minutes"),
    temp_val  = suppressWarnings(as.numeric(as.character(.[[temp_col]])))
  ) %>%
  arrange(time_raw) %>%
  mutate(
    # Delta T for water storage calculation
    del_Tw = temp_val - lag(temp_val)
  ) %>%
  filter(!is.na(time_join)) %>%
  select(time_join, del_Tw) %>%
  distinct(time_join, .keep_all = TRUE)

## --- 2. LOAD FLUX DATA ---
cat("Loading Flux data...\n")

df_raw <- as.data.frame(fread(flux_file))
df_raw[df_raw == -9999] <- NA

df_raw <- df_raw %>%
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
setDT(df_raw); setDT(wtd_clean)

# Track original WTD time to calculate join offset
wtd_clean[, wtd_time_orig := time_join]

setkey(df_raw, time_join); setkey(wtd_clean, time_join)

# Join and calculate time difference for precision
df <- wtd_clean[df_raw, roll = "nearest"]

# Calculate difference between flux timestamp and matched WTD timestamp
df[, diff_sec := abs(as.numeric(difftime(time_join, wtd_time_orig, units = "secs")))]

# Limit join to a 15-minute window
df[diff_sec > 900, del_Tw := NA]

# Remove temporary join tracking columns
df[, `:=`(wtd_time_orig = NULL, diff_sec = NULL)]

setDF(df)
plot(wtd_clean$time_join, wtd_clean$del_Tw)

## --- 4. CALCULATIONS ---

# Constants (Site Specific)
rho_soil    <- 1300   # kg/m3
Cp_dry      <- 900    # J/kg/K
Cp_water    <- 4190   # J/kg/K
rho_water   <- 1000   # kg/m3
plate_depth <- 0.05   # m
t_interval  <- 1800   # s (30 min)

# Standardize energy variables
if (!("NETRAD" %in% names(df)) & "SW_IN" %in% names(df)) df$NETRAD <- as.numeric(df$SW_IN)
if (!("LE" %in% names(df)) & "LE_CORR" %in% names(df)) df$LE <- as.numeric(df$LE_CORR)
if (!("H" %in% names(df)) & "H_CORR" %in% names(df)) df$H <- as.numeric(df$H_CORR)

req <- c("del_TS_2", "del_TS_3", "del_Tw", "WTD_1_1_1", "SWC_1_1_1", 
         "shf_Avg_2", "shf_Avg_3", "LE", "H", "NETRAD",
         "LE_SSITC_TEST", "H_SSITC_TEST")

for (col in req) {
  if (!(col %in% names(df))) df[[col]] <- NA
  df[[col]] <- as.numeric(as.character(df[[col]]))
}

# 4a. Specific Heat Capacity (Using Way 4 specific formula)
df$Cs_dry <- rho_soil * (Cp_dry + Cp_water * (((df$SWC_1_1_1 * 0.0951) + 49.107) / 100))

# Plot Cs_dry for debugging (handling NAs to avoid errors)
if(any(!is.na(df$Cs_dry))) {
  plot(df$time_join, df$Cs_dry, main="Debug: Cs_dry Time Series", type="l", xlab="Time", ylab="Cs_dry")
} else {
  cat("Warning: Cs_dry column contains only NA values!\n")
}

# 4b. Soil Storage Term (S_soil)
df$is_flooded <- !is.na(df$WTD_1_1_1) & df$WTD_1_1_1 >= 0.005
df$del_Tsoil_avg <- (df$del_TS_2 + df$del_TS_3) / 2

df$s_soil <- ifelse(
  df$is_flooded,
  (df$del_Tsoil_avg * plate_depth * rho_water * Cp_water) / t_interval,
  (df$del_Tsoil_avg * plate_depth * df$Cs_dry) / t_interval
)

# Plot s_soil for debugging
if(any(!is.na(df$s_soil))) {
  plot(df$time_join, df$s_soil, main="Debug: s_soil Time Series", type="l", xlab="Time", ylab="s_soil")
} else {
  cat("Warning: s_soil column contains only NA values!\n")
}

# 4c. Water Storage Term (S_water)
df$Dw_eff <- ifelse(df$is_flooded, pmin(df$WTD_1_1_1, 0.5), 0)
df$s_water <- (df$del_Tw * df$Dw_eff * rho_water * Cp_water) / t_interval

# Plot s_water for debugging
if(any(!is.na(df$s_water))) {
  plot(df$time_join, df$s_water, main="Debug: s_water Time Series", type="l", xlab="Time", ylab="s_water")
} else {
  cat("Warning: s_water column contains only NA values!\n")
}

# 4d. G Calculation (Average of plates 2 & 3)
df$G_plate <- rowMeans(df[, c("shf_Avg_2", "shf_Avg_3")], na.rm = TRUE)
df$G_total <- df$G_plate + df$s_soil + df$s_water

# Plot G_total before filtering
if(any(!is.na(df$G_total))) {
  plot(df$time_join, df$G_total, main="Debug: G_total (Raw) Time Series", type="l", xlab="Time", ylab="G (W/m2)")
}

## --- 5. CLEANING & FILTERING ---

# 5a. Non-QAQC Filter (3-sigma outlier removal)
g_mean <- mean(df$G_total, na.rm = TRUE)
g_sd   <- sd(df$G_total, na.rm = TRUE)
df$G_filtered <- ifelse(abs(df$G_total - g_mean) <= (3 * g_sd), df$G_total, NA)

# 5b. QAQC Filtered G (3-sigma AND high-quality H/LE flags 0 or 1)
df$G_filtered_qaqc <- ifelse(df$LE_SSITC_TEST %in% c(0, 1) & 
                               df$H_SSITC_TEST %in% c(0, 1), 
                             df$G_filtered, NA)

## --- 6. PLOTTING ---
cat("Generating diagnostic plots...\n")

# 6a. 2x2 Histogram Plot
cat("- Generating 2x2 Histograms...\n")
hist_data <- data.frame(
  G_plate_avg = df$G_plate,
  S_soil = df$s_soil,
  S_water = df$s_water,
  G_filtered = df$G_filtered
)

p_hist <- hist_data %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Value") %>%
  ggplot(aes(x = Value, fill = Variable)) +
  geom_histogram(bins = 60, alpha = 0.7, color = "black") +
  facet_wrap(~Variable, scales = "free", ncol = 2) +
  theme_minimal() +
  labs(title = "Way 4: Distribution of G Components", x = expression(W~m^-2), y = "Frequency") +
  theme(legend.position = "none")

ggsave(file.path(fig_dir, "01_G_Histograms_2x2.png"), p_hist, width = 10, height = 8)

# 6b. Energy Balance Closure Plot (Full Data)
cat("- Generating Energy Balance Closure (Full Dataset)...\n")
df_eb <- df %>%
  mutate(
    Turb_Fluxes = H + LE,
    Avail_Energy = NETRAD - G_filtered
  ) %>%
  filter(!is.na(Turb_Fluxes) & !is.na(Avail_Energy))

if (nrow(df_eb) > 10) {
  fit <- lm(Turb_Fluxes ~ Avail_Energy, data = df_eb)
  r2 <- round(summary(fit)$r.squared, 3)
  slope <- round(coef(fit)[2], 3)
  
  p_eb <- ggplot(df_eb, aes(x = Avail_Energy, y = Turb_Fluxes)) +
    geom_bin2d() +
    scale_fill_viridis_c() +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey50") +
    geom_smooth(method = "lm", color = "red", se = FALSE) +
    annotate("text", x = -Inf, y = Inf, hjust = -0.1, vjust = 1.5,
             label = paste0("Slope: ", slope, "\nR^2: ", r2),
             color = "red", fontface = "bold") +
    theme_bw() +
    labs(title = "Way 4: Energy Balance Closure (Full Dataset)",
         subtitle = expression(H + LE == R[n] - G[filtered]),
         x = expression(Available~Energy~(R[n] - G[filtered])~(W~m^-2)),
         y = expression(Turbulent~Fluxes~(H + LE)~(W~m^-2)))
  
  ggsave(file.path(fig_dir, "02_Energy_Balance_Closure_Full.png"), p_eb, width = 10, height = 7)
}

# 6c. QAQC Filtered Energy Balance Plot
cat("- Generating QAQC Energy Balance Closure...\n")
df_qaqc_eb <- df %>%
  filter(!is.na(G_filtered_qaqc)) %>%
  mutate(
    Turb_Fluxes = H + LE,
    Avail_Energy = NETRAD - G_filtered_qaqc
  ) %>%
  filter(!is.na(Turb_Fluxes) & !is.na(Avail_Energy))

if (nrow(df_qaqc_eb) > 10) {
  fit_q <- lm(Turb_Fluxes ~ Avail_Energy, data = df_qaqc_eb)
  r2_q <- round(summary(fit_q)$r.squared, 3)
  slope_q <- round(coef(fit_q)[2], 3)
  
  p_qaqc <- ggplot(df_qaqc_eb, aes(x = Avail_Energy, y = Turb_Fluxes)) +
    geom_bin2d() +
    scale_fill_viridis_c() +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey50") +
    geom_smooth(method = "lm", color = "blue", se = FALSE) +
    annotate("text", x = -Inf, y = Inf, hjust = -0.1, vjust = 1.5,
             label = paste0("QAQC Slope: ", slope_q, "\nQAQC R^2: ", r2_q),
             color = "blue", fontface = "bold") +
    theme_bw() +
    labs(title = "Way 4: Energy Balance Closure (QAQC Filtered)",
         subtitle = expression(H + LE == R[n] - G[filtered_qaqc]),
         x = expression(Available~Energy~(R[n] - G[filtered_qaqc])~(W~m^-2)),
         y = expression(Turbulent~Fluxes~(H + LE)~(W~m^-2)))
  
  ggsave(file.path(fig_dir, "04_QAQC_Energy_Balance_Closure.png"), p_qaqc, width = 10, height = 7)
}

# 6d. Time series of G (Comparison between Non-QAQC and QAQC)
cat("- Generating G Time Series Comparison...\n")
p_ts <- ggplot(df, aes(x = time_join)) +
  geom_line(aes(y = G_filtered, color = "G_filtered (3-sigma only)"), alpha = 0.4) +
  geom_line(aes(y = G_filtered_qaqc, color = "G_filtered (QAQC: Flags 0,1)"), size = 0.8) +
  scale_color_manual(values = c("G_filtered (3-sigma only)" = "grey60", 
                                "G_filtered (QAQC: Flags 0,1)" = "red")) +
  labs(title = paste(site, year, "Soil Heat Flux Comparison"),
       y = expression(W~m^-2), x = "Time", color = "Filter Type") +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave(file.path(fig_dir, "03_G_timeseries_comparison.png"), p_ts, width = 12, height = 6)

cat("Execution Complete. Files saved in:", fig_dir, "\n")


## --- 7. DATA AUDIT & AVAILABILITY REPORT (WAY 4) ---
cat("Generating Data Availability and Stats Report for Way 4...\n")

# Define and create the specific output directory
audit_dir <- "C:/Users/rbmahbub/Documents/RProjects/AmerifluxDataSubmission_LandscapeFlux/SoilHeatFluxCalculation/DataPercentage"
if (!dir.exists(audit_dir)) dir.create(audit_dir, recursive = TRUE)

# Define the variables used in Way 4 calculations
# Note: Added NETRAD as it is used in Way 4 instead of SW_IN for closure
audit_vars <- c("shf_Avg_1","shf_Avg_2", "shf_Avg_3", "del_TS_1","del_TS_2", "del_TS_3", 
                "del_Tw", "WTD_1_1_1", "SWC_1_1_1", "LE", "H", "NETRAD",
                "Cs_dry", "G_plate", "s_soil", "s_water", "G_total", "G_filtered_qaqc")

# Calculate metrics with NAs removed for stats
availability_report <- data.frame(
  Variable      = audit_vars,
  Total_Records = nrow(df),
  Present_Count = sapply(df[audit_vars], function(x) sum(!is.na(x))),
  Mean_Val      = sapply(df[audit_vars], function(x) round(mean(as.numeric(x), na.rm = TRUE), 4)),
  Max_Val       = sapply(df[audit_vars], function(x) round(max(as.numeric(x), na.rm = TRUE), 4)),
  Min_Val       = sapply(df[audit_vars], function(x) round(min(as.numeric(x), na.rm = TRUE), 4))
) %>%
  mutate(
    Missing_Count    = Total_Records - Present_Count,
    Availability_Pct = round((Present_Count / Total_Records) * 100, 2)
  ) %>%
  # Organize columns for the final Excel-ready CSV
  select(Variable, Total_Records, Present_Count, Availability_Pct, Mean_Val, Max_Val, Min_Val)

# Define the output path
audit_path <- file.path(audit_dir, "Way4_2024_Data_Stats_Report.csv")

# Save the report
write.csv(availability_report, audit_path, row.names = FALSE)

cat("Success: Way 4 Audit & Stats Report saved at:", audit_path, "\n")

# Display the report in the console for immediate review
print(availability_report)

