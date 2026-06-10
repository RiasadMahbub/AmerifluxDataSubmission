## ========================================================================== ##
## PROJECT: Soil Heat Flux (G) Calculation - Way 3 & Way 4                    ##
## AUTHOR: Riasad Bin Mahbub                                                  ##
## DESCRIPTION: Scalable Multi-Site, Multi-Year Pipeline                      ##
##              Includes Paddy Regime Logic and Data Quality Diagnostics      ##
##              v2.3: Fixed Year-Specific WTD File Mappings                   ##
## ========================================================================== ##

## ================= LIBRARIES ================= ##
library(data.table)
library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)
library(readxl)
library(viridis)

## ================= PATHS & CONFIGURATION ================= ##
fig_dir <- "C:/Users/rbmahbub/Documents/RProjects/AmerifluxDataSubmission_LandscapeFlux/SoilHeatFluxCalculation/Figures/EachColumn"

# Root directories for flux CSVs
way_dirs <- list(
  Way3 = "C:/Users/rbmahbub/Documents/RProjects/AmerifluxDataSubmission_LandscapeFlux/Data/ForLabResearachPurposeMoreColumns/Way3",
  Way4 = "C:/Users/rbmahbub/Documents/RProjects/AmerifluxDataSubmission_LandscapeFlux/Data/ForLabResearachPurposeMoreColumns/Way4"
)

# FIXED: Nested WTD file mapping by Site and Year
wtd_base_path <- "C:/Users/rbmahbub/Documents/RProjects/AmerifluxDataSubmission_LandscapeFlux/Data/InputLocalRawData/WTD"

wtd_files <- list(
  Way3 = list(
    `2024` = list(file = file.path(wtd_base_path, "2024_way3_ECside.xlsx"), sheet = "Way 3 EC Side"),
    `2025` = list(file = file.path(wtd_base_path, "2025_way3_ECside.xlsx"), sheet = "Way 3 EC Side")
  ),
  Way4 = list(
    `2024` = list(file = file.path(wtd_base_path, "2024_Masterfile_Way4_ECside.xlsx"), sheet = "Way 4 - EC Side"),
    `2025` = list(file = file.path(wtd_base_path, "MasterFile_Way4SoilProfile.xlsx"), sheet = "Way4SoilProfile")
  )
)

years_to_process <- c("2024", "2025")

## ================= HELPER FUNCTIONS ================= ##

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
                                    orders = c("mdy HMS p", "mdy HM p", "mdy HMS", "ymd HMS", "mdy HM"), 
                                    tz = "UTC", quiet = TRUE)
    out_posix[!is_numeric_serial] <- string_dates
  }
  return(with_tz(out_posix, "UTC"))
}

process_wtd <- function(file_path, sheet_name) {
  if(!file.exists(file_path)) {
    cat(paste0("    [!] WTD FILE NOT FOUND: ", basename(file_path), "\n"))
    return(NULL)
  }
  
  wtd_raw <- read_excel(file_path, sheet = sheet_name, skip = 1)
  wtd_raw <- wtd_raw[-(1:2), ] 
  
  wtd_raw$time_raw <- parse_excel_time(wtd_raw$TIMESTAMP)
  wtd_raw$time_join <- lubridate::round_date(wtd_raw$time_raw, "30 minutes")
  
  temp_col <- names(wtd_raw)[grepl("T107|Temp|T_water", names(wtd_raw), ignore.case = TRUE)][1]
  if(is.na(temp_col)) return(NULL)
  
  wtd_raw$temp_val <- as.numeric(as.character(wtd_raw[[temp_col]]))
  
  wtd_raw %>%
    arrange(time_raw) %>%
    mutate(del_TS_4_new = temp_val - lag(temp_val)) %>%
    filter(!is.na(time_join)) %>%
    select(time_join, del_TS_4_new)
}

debug_join_failure <- function(df_ts, wtd_ts, label) {
  cat(paste0("\n    --- JOIN FORENSIC ANALYSIS: ", label, " ---\n"))
  flux_start <- min(df_ts, na.rm=T); flux_end <- max(df_ts, na.rm=T)
  wtd_start <- min(wtd_ts, na.rm=T); wtd_end <- max(wtd_ts, na.rm=T)
  cat(paste0("    Flux Date Range: ", flux_start, " to ", flux_end, "\n"))
  cat(paste0("    WTD Date Range:  ", wtd_start, " to ", wtd_end, "\n"))
  overlap <- pmax(0, difftime(pmin(flux_end, wtd_end), pmax(flux_start, wtd_start), units="days"))
  if(overlap <= 0) cat("    [CAUSE] NO DATE OVERLAP.\n") else cat(paste0("    Overlap: ", round(overlap, 1), " days.\n"))
  cat("    ----------------------------------------------\n")
}

prepare_and_merge <- function(df, wtd_ref, label) {
  df[df == -9999] <- NA
  ts_raw <- as.character(df$TIMESTAMP_START)
  df$time_join <- lubridate::round_date(as.POSIXct(ts_raw, format="%Y%m%d%H%M", tz="UTC"), "30 minutes")
  
  if(!is.null(wtd_ref)) {
    matches <- sum(df$time_join %in% wtd_ref$time_join)
    success_rate <- (matches / nrow(df)) * 100
    if(success_rate < 80) debug_join_failure(df$time_join, wtd_ref$time_join, label)
    
    wtd_ref <- wtd_ref %>% distinct(time_join, .keep_all = TRUE)
    df <- left_join(df, wtd_ref, by="time_join")
    if("del_TS_4_new" %in% names(df)) {
      df$del_TS_4 <- df$del_TS_4_new
      df$del_TS_4_new <- NULL
    }
  }
  
  find_col <- function(d, patterns) {
    m <- names(d)[toupper(names(d)) %in% toupper(patterns)]
    if(length(m)>0) return(m[1])
    m <- names(d)[grepl(paste(patterns, collapse="|"), names(d), ignore.case=TRUE)]
    if(length(m)>0) return(m[1])
    return(NULL)
  }
  
  rn_col <- find_col(df, c("NETRAD","Rn","NET_RAD","Rn_Avg"))
  le_col <- find_col(df, c("LE","LE_CORR","LE_avg"))
  h_col  <- find_col(df, c("H","H_CORR","H_avg"))
  
  if(!is.null(rn_col)) df$NETRAD <- as.numeric(df[[rn_col]])
  else if("SW_IN" %in% names(df)) df$NETRAD <- as.numeric(as.character(df$SW_IN)) 
  
  if(!is.null(le_col)) df$LE <- as.numeric(df[[le_col]])
  if(!is.null(h_col))  df$H  <- as.numeric(df[[h_col]])
  
  return(df)
}

## ================= CORE CALCULATION & AUDIT ================= ##

calc_flux_and_diagnose <- function(df, label, out_dir) {
  
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  df <- as.data.frame(df)
  df$time <- df$time_join 
  
  rho_soil <- ifelse(grepl("Way4", label), 1390, 1300)
  Cp_dry <- 900; Cp_water <- 4190; rho_water <- 1000; plate_depth <- 0.05; t_interval <- 1800
  
  req <- c("del_TS_2","del_TS_3","del_TS_4","WTD_1_1_1","SWC_1_1_1","shf_Avg_2","shf_Avg_3")
  for(col in req) {
    if(!(col %in% names(df))) df[[col]] <- NA
    df[[col]] <- as.numeric(df[[col]])
    if(grepl("shf", col)) df[[col]][df[[col]]>200 | df[[col]]< -200] <- NA
    if(grepl("del_TS", col)) {
      lim <- ifelse(col=="del_TS_4", 1, 5)
      df[[col]][df[[col]]>lim | df[[col]]< -lim] <- NA
    }
  }
  
  df$swcorr <- (df$SWC_1_1_1 * 0.0951) + 49.107
  df$is_flooded <- !is.na(df$WTD_1_1_1) & df$WTD_1_1_1 >= 0.005
  
  Cs_dry <- rho_soil * (Cp_dry + Cp_water * (df$swcorr/100))
  Cs_wat <- rho_water * Cp_water
  s_soil_dry <- ((df$del_TS_2+df$del_TS_3)/2)*plate_depth*Cs_dry/t_interval
  s_soil_wet <- ((df$del_TS_2+df$del_TS_3)/2)*plate_depth*Cs_wat/t_interval
  df$Dw_eff <- ifelse(df$is_flooded, pmin(df$WTD_1_1_1, 0.5), 0)
  s_water <- df$del_TS_4 * df$Dw_eff * rho_water * Cp_water / t_interval
  
  df$Flux_Plate_avg <- rowMeans(df[,c("shf_Avg_2","shf_Avg_3")], na.rm=TRUE)
  df$Final_Soil_Storage <- ifelse(df$is_flooded, s_soil_wet, s_soil_dry)
  df$Final_Water_Storage <- ifelse(df$is_flooded, s_water, 0)
  
  df$Gavg <- df$Flux_Plate_avg + df$Final_Soil_Storage + df$Final_Water_Storage
  df$Avail_Energy <- df$NETRAD - df$Gavg
  df$Turb_Fluxes <- df$H + df$LE
  
  # --- AUDIT REPORT ---
  cat(paste0("\n    DIAGNOSTIC AUDIT: ", label, "\n"))
  pct <- function(x) round(sum(!is.na(x)) / length(x) * 100, 1)
  cat(paste0("    Completeness: Gavg=", pct(df$Gavg), "%, Rad=", pct(df$NETRAD), "%, EB=", pct(df$Turb_Fluxes), "%, WTD=", pct(df$WTD_1_1_1), "%\n"))
  
  # --- PLOTTING ---
  check_and_save <- function(p, name) ggsave(filename = file.path(out_dir, paste0(name, ".png")), plot = p, width = 10, height = 5)
  
  p_final <- ggplot(df, aes(time, Gavg)) + geom_line(color="red") + theme_bw() + labs(title=paste(label, "Final Gavg"))
  check_and_save(p_final, "01_Final_Gavg")
  
  p_hist <- df %>% select(Flux_Plate_avg, Gavg, S_soil = Final_Soil_Storage, S_water = Final_Water_Storage) %>% 
    pivot_longer(everything()) %>%
    ggplot(aes(value, fill=name)) + geom_histogram(bins=100, color="white") + facet_wrap(~name, scales="free") + theme_minimal()
  check_and_save(p_hist, "02_Histograms_Faceted")
  
  if(sum(!is.na(df$Avail_Energy)) > 10 & sum(!is.na(df$Turb_Fluxes)) > 10) {
    clean_df <- df %>% filter(!is.na(Avail_Energy), !is.na(Turb_Fluxes))
    fit <- lm(Turb_Fluxes ~ Avail_Energy, data = clean_df)
    eq_label <- paste0("Slope: ", round(coef(fit)[2], 3), "\nR2: ", round(summary(fit)$r.squared, 3))
    p_eb <- ggplot(clean_df, aes(x = Avail_Energy, y = Turb_Fluxes)) +
      geom_bin2d(bins = 60) + scale_fill_viridis_c() + geom_smooth(method = "lm", color = "red") +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
      annotate("text", x = -Inf, y = Inf, label = eq_label, hjust = -0.1, vjust = 1.1, size = 5, color = "darkred", fontface="bold") +
      theme_bw() + labs(title="Energy Balance Closure", subtitle=expression(H + LE == R[n] - G))
    check_and_save(p_eb, "04_Energy_Balance_Closure")
  }
  return(df)
}

## ================= MAIN EXECUTION LOOP ================= ##

final_results <- list()

for(site in names(way_dirs)) {
  cat(paste0("\n>>> Processing Site: ", site, " <<<\n"))
  
  for(year in years_to_process) {
    file_name <- paste0(site, "_", year, ".csv")
    full_path <- file.path(way_dirs[[site]], file_name)
    
    if(!file.exists(full_path)) {
      cat(paste0("  [SKIP] Flux file not found: ", file_name, "\n"))
      next
    }
    
    cat(paste0("  Loading: ", file_name, "\n"))
    
    # Load and process the site/year-specific WTD file
    wtd_config <- wtd_files[[site]][[year]]
    wtd_clean <- process_wtd(wtd_config$file, wtd_config$sheet)
    
    df_raw <- fread(full_path)
    df_merged <- prepare_and_merge(df_raw, wtd_clean, paste(site, year))
    
    target_out <- file.path(fig_dir, paste0(site, "_", year))
    res <- calc_flux_and_diagnose(df_merged, paste(site, year), target_out)
    
    final_results[[paste(site, year, sep="_")]] <- res
  }
}

cat("\n--- PIPELINE COMPLETE ---\n")