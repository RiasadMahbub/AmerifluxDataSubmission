# Set folder path
folder_path <- "C:/Users/rbmahbub/Documents/RProjects/AmerifluxDataSubmission_LandscapeFlux/Data/OutputLocalProcessedData_AFguidedSubmitted/Way3"
# List all CSV files in the folder
csv_files <- list.files(folder_path, pattern = "\\.csv$", full.names = TRUE)
# Loop over each file
for (file_path in csv_files) {
  # Read the CSV file
  df <- read.csv(file_path, stringsAsFactors = FALSE)
  # Check TIMESTAMP lengths
  bad_start_rows <- df[nchar(df$TIMESTAMP_START) != 12, ]
  bad_end_rows   <- df[nchar(df$TIMESTAMP_END) != 12, ]
  # Combine and remove duplicates
  bad_rows <- unique(rbind(bad_start_rows, bad_end_rows))
  # Print summary
  cat("\nFile:", basename(file_path), "\n")
  cat("  Total rows:                     ", nrow(df), "\n")
  cat("  Rows with invalid START time:   ", nrow(bad_start_rows), "\n")
  cat("  Rows with invalid END time:     ", nrow(bad_end_rows), "\n")
  cat("  Total unique bad rows reported: ", nrow(bad_rows), "\n")
  # Optional: View first few bad rows
  if (nrow(bad_rows) > 0) {
    print(head(bad_rows[, c("TIMESTAMP_START", "TIMESTAMP_END")]))
  }
}


# Set folder path to Way3
folder_path <- "C:/Users/rbmahbub/Documents/RProjects/AmerifluxDataSubmission_LandscapeFlux/Data/OutputLocalProcessedData_AFguidedSubmitted/Way4"
# List all CSV files in the folder
csv_files <- list.files(folder_path, pattern = "\\.csv$", full.names = TRUE)
# Loop over each file
for (file_path in csv_files) {
  # Read the CSV file
  df <- read.csv(file_path, stringsAsFactors = FALSE)
  # Check IMESTAMP lengths
  bad_start_rows <- df[nchar(df$TIMESTAMP_START) != 12, ]
  bad_end_rows   <- df[nchar(df$TIMESTAMP_END) != 12, ]
  # Combine and remove duplicates
  bad_rows <- unique(rbind(bad_start_rows, bad_end_rows))
  # Print summary
  cat("\nFile:", basename(file_path), "\n")
  cat("  Total rows:                     ", nrow(df), "\n")
  cat("  Rows with invalid START time:   ", nrow(bad_start_rows), "\n")
  cat("  Rows with invalid END time:     ", nrow(bad_end_rows), "\n")
  cat("  Total unique bad rows reported: ", nrow(bad_rows), "\n")
  # Optional: View first few bad rows
  if (nrow(bad_rows) > 0) {
    print(head(bad_rows[, c("TIMESTAMP_START", "TIMESTAMP_END")]))
  }
}