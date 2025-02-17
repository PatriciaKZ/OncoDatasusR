#' Merge Downloaded .csv Files Extracted from DATASUS
#'
#' This function reads all .csv files with data extracted from DATASUS by state and merges them into one comprehensive table.
#'
#' @param file_list Character vector. List of file names to be read.
#' @param output_dir Character. Directory to read the files from. If `NULL`, the function will use the 'datasus_files' folder created in the current working directory.
#' @param database Character. Acronym of the database for download. Acceptable values are 'AR', 'AQ', or 'RD'.
#' @param export_file_code Character. String used in the name of the output files from the `download_datasus()` function.
#' Use 'default' to include the ICD name in the file name. If more than one ICD is provided, they will be separated by an underscore ('_'). If no ICD is provided, the code for the file name will be 'all'.
#' @param icd_list Character. List of ICD codes used for data filtering. Default is `NA` to include all ICD codes.
#' @param silence Logical. If `TRUE`, suppresses console messages about reading and writing files. Default is `FALSE`.
#' @param first_year Numeric. Oldest year of the data extraction. This value is used for naming the saved file.
#' @param last_year Numeric. Most recent year of the data extraction. This value is used for naming the saved file.
#' @param rm_ufs_files Logical. If `TRUE`, removes the .csv files for each state after merging them into one table. Default is `TRUE`.
#' @return A merged .csv file containing data from all specified files.
#' @details
#' - The function searches for a folder named 'datasus_files' in the specified directory or in the current working directory if `output_dir` is `NULL`.
#' - The merged file is saved in the same directory as the source files with a standardized naming convention.
#' - Encodes patient CNS identifiers to base64 format for 'AQ' and 'AR' databases.
#' @export
#' @examples
#' \dontrun{
#' # Merge .csv files for database 'AR' and years 2010 to 2020
#' combined_data <- merge_datasus(
#'   file_list = c('AR_SP_C00_2010_2020.csv'),
#'   database = 'AR',
#'   export_file_code = 'C00',
#'   icd_list = c('C00'),
#'   first_year = 2010,
#'   last_year = 2020,
#'   silence = TRUE,
#'   rm_ufs_files = TRUE
#' )
#' }
merge_datasus <- function(file_list,
                          output_dir = NULL,
                          database,
                          export_file_code = "default",
                          icd_list = NA,
                          silence = FALSE,
                          first_year,
                          last_year,
                          rm_ufs_files = TRUE) {
  # Define output_dir
  if (is.null(output_dir)) {
    output_dir <- paste0(getwd(), "/datasus_files/")
  } else {
    output_dir <- paste0(output_dir, "/datasus_files/")
  }

  # Check if the output directory exists

  if (!dir.exists(output_dir)) {
    stop(paste0("Output directory does not exist: ", output_dir))
  }

  # Create a data.frame with data from all UFs
  final_df <- data.frame()

  for (file in file_list) {
    if (!silence)
      message(paste0("Reading: ", file))
    # Load data for each UF. All variables are defined as type 'character'
    df <- data.frame(data.table::fread(paste0(output_dir, file), colClasses = "character"))

    # Merge data.frames
    final_df <- dplyr::bind_rows(final_df, df)
  }

  if (export_file_code == "default") {
    if (all(!is.na(icd_list))) {
      export_file_code <- paste(icd_list, collapse = "_")
    } else {
      export_file_code <- "all"
    }
  }

  # Encode patient CNS to base64

  if (database %in% c("AQ", "AR")) {
    final_df$AP_CNSPCN_ENC64 <- sapply(final_df$AP_CNSPCN, function(x)
      RCurl::base64Encode(x)[1])
  }

  # Define file name
  final_file_name <- paste0(
    output_dir,
    database,
    "_",
    export_file_code,
    "_",
    as.character(first_year),
    "_",
    as.character(last_year),
    "_full.csv"
  )

  # Save file

  message(paste0("Writing: ", final_file_name))

  readr::write_csv(final_df, final_file_name)

  message("File saved successfully.")

  if (rm_ufs_files) {
    # Remove files for each UF
    invisible(file.remove(paste0(output_dir, file_list)))
  }
}
