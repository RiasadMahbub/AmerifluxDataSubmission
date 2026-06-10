fig_dir <- "C:/Users/rbmahbub/Documents/RProjects/AmerifluxDataSubmission_LandscapeFlux/SoilHeatFluxCalculation/Figures/EachColumn"

way_dirs <- list(
  Way3 = "C:/Users/rbmahbub/Documents/RProjects/AmerifluxDataSubmission_LandscapeFlux/Data/ForLabResearachPurposeMoreColumns/Way3",
  Way4 = "C:/Users/rbmahbub/Documents/RProjects/AmerifluxDataSubmission_LandscapeFlux/Data/ForLabResearachPurposeMoreColumns/Way4"
)

wtd_files <- list(
  Way3 = list(file = "C:/Users/rbmahbub/Documents/RProjects/AmerifluxDataSubmission_LandscapeFlux/Data/InputLocalRawData/WTD/2024_way3_ECside.xlsx",
              sheet = "Way 3 EC Side"),
  
  Way4 = list(file = "C:/Users/rbmahbub/Documents/RProjects/AmerifluxDataSubmission_LandscapeFlux/Data/InputLocalRawData/WTD/2024_Masterfile_Way4_ECside.xlsx",
              sheet = "Way 4 - EC Side")
)

years <- c("2024", "2025")

process_wtd <- function(file_path, sheet_name) {
  
  wtd_raw <- read_excel(file_path, sheet = sheet_name, skip = 1)
  wtd_raw <- wtd_raw[-(1:2), ]
  
  wtd_raw$TIMESTAMP <- parse_excel_time(wtd_raw$TIMESTAMP)
  wtd_raw$T107_C_1cm_Avg <- as.numeric(as.character(wtd_raw$T107_C_1cm_Avg))
  
  wtd_clean <- wtd_raw %>%
    arrange(TIMESTAMP) %>%
    mutate(del_TS_4_new = T107_C_1cm_Avg - lag(T107_C_1cm_Avg)) %>%
    filter(!is.na(TIMESTAMP)) %>%
    select(TIMESTAMP, del_TS_4_new) %>%
    rename(time_join = TIMESTAMP)
  
  return(wtd_clean)
}

results <- list()

for(site in names(way_dirs)) {
  
  cat(paste0("\n================ PROCESSING ", site, " ================\n"))
  
  # Load WTD for that site
  wtd_clean <- process_wtd(
    wtd_files[[site]]$file,
    wtd_files[[site]]$sheet
  )
  
  for(year in years) {
    
    file_name <- paste0(site, "_", year, ".csv")
    file_path <- file.path(way_dirs[[site]], file_name)
    
    if(!file.exists(file_path)) {
      cat(paste0("Skipping: ", file_name, " (not found)\n"))
      next
    }
    
    cat(paste0("Processing: ", file_name, "\n"))
    
    df <- fread(file_path)
    df_proc <- prepare_and_merge(df, wtd_clean)
    
    out_dir <- file.path(fig_dir, paste0(site, "_", year))
    
    df_final <- calc_way3_flux_steps(
      df_proc,
      paste(site, year),
      out_dir
    )
    
    results[[paste(site, year, sep="_")]] <- df_final
  }
}