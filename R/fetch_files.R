#' Fetch list of downloaded .csv files extracted from DATASUS
#'
#' This function searches for .csv files downloaded by state, ICD code, and year, and returns a list
#' of all files that match this string pattern, following the filename pattern of the `download_datasus()` function.
#'
#' @param output_dir Character. Directory where the function will read the files from. Default is the current working directory.
#' @param database Character. Acronym of the database for download. Acceptable values are 'AR', 'AQ', or 'RD'.
#' @param export_file_code Character. String used in the filename of the output files of the `download_datasus()` function. Default is 'default'.
#' @param icd_list Character. List of ICD codes used for data filtering.
#' @param first_year Integer. Oldest year of the database for extraction.
#' @param last_year Integer. Most recent year of the database for extraction.
#' @return A character vector containing the list of .csv files that were extracted from the DATASUS database.
#' @details
#' - The function searches for a folder named 'datasus_files' in the specified directory.
#' - If the `output_dir` parameter is `NULL`, the function will search for the 'datasus_files' folder in the current working directory.
#' @export
#' @examples
#' \dontrun{
#' # Fetch list of downloaded files for database 'AR', ICD 'C00', and years 2010 to 2020
#' file_list <- fetch_files(
#'   database = 'AR',
#'   export_file_code = 'C00',
#'   icd_list = c('C00'),
#'   first_year = 2010,
#'   last_year = 2020
#' )
#' print(file_list)
#' }
fetch_files <- function(output_dir = NULL,
                        database,
                        export_file_code = "default",
                        icd_list,
                        first_year,
                        last_year) {
  # Define path
  if (is.null(output_dir)) {
    output_dir <- paste0(getwd(), "/datasus_files")
  } else {
    output_dir <- paste0(output_dir, "/datasus_files")
  }

  # Check if the output directory exists
  if (!dir.exists(output_dir)) {
    stop(paste0("Output directory does not exist: ", output_dir))
  }

  if (export_file_code == "default")
    export_file_code <- paste0(icd_list, collapse = "_")

  # Get all downloaded files from the directory
  downloaded_uf_files_1 <- list.files(
    path = output_dir,
    pattern = paste0(
      database,
      "_.*\\_",
      export_file_code,
      "_",
      as.character(first_year),
      "_",
      as.character(last_year),
      ".csv"
    )
  )

  downloaded_uf_files_2 <- list.files(
    path = output_dir,
    pattern = paste0(
      database,
      "_.*\\_",
      export_file_code,
      "_",
      as.character(first_year),
      as.character(last_year),
      ".csv"
    )
  )

  downloaded_uf_files <- c(downloaded_uf_files_1, downloaded_uf_files_2)

  return(downloaded_uf_files)
}
