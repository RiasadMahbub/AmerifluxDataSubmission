## ========================================================================== ##
## PROJECT: Soil Heat Flux (G) Calculation - Way 4 | 2025                    ##
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
year <- "2025"
fig_dir <- "C:/Users/rbmahbub/Documents/RProjects/AmerifluxDataSubmission_LandscapeFlux/SoilHeatFluxCalculation/Figures/EachColumn/Way4_2025"
if (!dir.exists(fig_dir)) dir.create(fig_dir, recursive = TRUE)

flux_file <- "C:/Users/rbmahbub/Documents/RProjects/AmerifluxDataSubmission_LandscapeFlux/Data/ForLabResearachPurposeMoreColumns/Way4/Way4_2025.csv"
wtd_file  <- "C:/Users/rbmahbub/Documents/RProjects/AmerifluxDataSubmission_LandscapeFlux/Data/InputLocalRawData/WTD/MasterFile_Way4SoilProfile.xlsx"
wtd_sheet <- "Way4SoilProfile"

## --- HELPER FUNCTIONS ---
parse_time <- function(t) {
  t <- as.character(t)
  num <- suppressWarnings(as.numeric(t))
  out <- as.POSIXct(rep(NA, length(t)))
  is_num <- !is.na(num) & num > 30000 & num < 60000
  if (any(is_num)) out[is_num] <- as.POSIXct(num[is_num] * 86400, origin = "1899-12-30", tz = "UTC")
  if (any(!is_num)) out[!is_num] <- parse_date_time(t[!is_num], orders = c("mdy HMS p", "mdy HM p", "mdy HMS", "ymd HMS", "mdy HM"), tz = "UTC", quiet = TRUE)
  return(with_tz(out, "UTC"))
}

## --- DATA LOADING & ROLLING JOIN ---
if(!file.exists(wtd_file)) stop("2025 WTD File missing.")
wtd_raw <- read_excel(wtd_file, sheet = wtd_sheet, skip = 1)[-(1:2), ]
wtd_raw$time_raw <- parse_time(wtd_raw$TIMESTAMP)
wtd_raw$time_join <- round_date(wtd_raw$time_raw, "30 minutes")
temp_col <- names(wtd_raw)[grepl("T107", names(wtd_raw))][1]
wtd_raw$temp_val <- as.numeric(as.character(wtd_raw[[temp_col]]))
wtd_dt <- as.data.table(wtd_raw %>% arrange(time_raw) %>% mutate(del_TS_4_new = temp_val - lag(temp_val)) %>% filter(!is.na(time_join)) %>% select(time_join, del_TS_4_new))

df <- fread(flux_file)
df[df == -9999] <- NA
df$time_join <- as.POSIXct(as.character(df$TIMESTAMP_START), format="%Y%m%d%H%M", tz="UTC")
df_dt <- as.data.table(df)

setkey(df_dt, time_join); setkey(wtd_dt, time_join)
df_merged <- wtd_dt[df_dt, roll = "nearest"]
df_merged[, diff_sec := abs(as.numeric(difftime(time_join, i.time_join, units="secs"))), env=list(i.time_join="time_join")]
df_merged[diff_sec > 1860, del_TS_4_new := NA]
df <- as.data.frame(df_merged)

## --- CALCULATION ---
rho_soil <- 1390; Cp_dry <- 900; Cp_water <- 4190; rho_water <- 1000; plate_depth <- 0.05; t_interval <- 1800
req <- c("del_TS_2","del_TS_3","del_TS_4","WTD_1_1_1","SWC_1_1_1","shf_Avg_2","shf_Avg_3")
for(col in req) if(col %in% names(df)) df[[col]] <- as.numeric(as.character(df[[col]])) else df[[col]] <- NA

df$is_flooded <- !is.na(df$WTD_1_1_1) & df$WTD_1_1_1 >= 0.005
Cs_dry <- rho_soil * (900 + 4190 * (((df$SWC_1_1_1 * 0.0951) + 49.107)/100))
s_soil <- ifelse(df$is_flooded, ((df$del_TS_2+df$del_TS_3)/2)*0.05*(1000*4190)/1800, ((df$del_TS_2+df$del_TS_3)/2)*0.05*Cs_dry/1800)
df$Dw_eff <- ifelse(df$is_flooded, pmin(df$WTD_1_1_1, 0.5), 0)
s_water <- df$del_TS_4 * df$Dw_eff * 1000 * 4190 / 1800
df$Gavg <- rowMeans(df[,c("shf_Avg_2","shf_Avg_3")], na.rm=TRUE) + s_soil + s_water

ggsave(file.path(fig_dir, "01_Final_Gavg.png"), ggplot(df, aes(time_join, Gavg)) + geom_line(color="red") + theme_bw(), width=10, height=5)
cat(paste0("Join Success: ", round(sum(!is.na(df$del_TS_4_new))/nrow(df)*100,1), "%\n"))