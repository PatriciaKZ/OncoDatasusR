#' Download and process DATASUS .dbc files
#'
#' This function extracts .dbc files from DATASUS, filters the data by provided ICD codes,
#' and saves the resulting table in .csv format in a local directory.
#'
#' @importFrom magrittr %>%
#' @param icd_list Character. List of ICD codes used for data filtering. Default is `NULL` to include all ICD codes.
#' @param fu_list Character. List of federative units (FUs) for extraction. Use 'all' to include all 27 federative units.
#' Default is 'all'.
#' @param selected_files Character. Vector containing the names of .dbc files to be extracted from the FTP server.
#' @param database Character. Acronym of the database for download. Acceptable values are 'AR', 'AQ', or 'RD'.
#' @param min_age Integer. Minimum age at the time of attendance for data filtering. Default is `0`.
#' @param max_age Integer. Maximum age at the time of attendance for data filtering. Default is `Inf`.
#' @param proc_code Character. Procedure code vector for data filtering. Default is `NA`.
#' @param export_file_code Character. String to be used for naming the output files. Use 'default' to include ICD name in
#' the file name. If there is more than one ICD, they will be separated by an underscore ('_').
#' @param output_dir Character. Output directory where .csv files will be saved. If `NULL`, the function will use the
#' current working directory. Default is `NULL`.
#' @return Saves .csv files in the folder named 'datasus_files', which is created in the output directory.
#' @details
#' - If `fu_list` is 'all', the function will download data from all 27 federative units of Brazil.
#' - The files will be saved into the 'datasus_files' folder created in the specified output directory.
#' @export
#' @examples
#' \dontrun{
#' # Download and process data for ICD 'C00', federative unit 'SP', and save in current directory
#' download_datasus(
#'   icd_list = 'C00',
#'   fu_list = 'SP',
#'   selected_files = c('ARSP0108.dbc', 'ARSP0208.dbc'),
#'   database = 'AR',
#'   min_age = 0,
#'   max_age = 100,
#'   proc_code = NA,
#'   export_file_code = 'default',
#'   output_dir = NULL
#' )
#' }
download_datasus <- function(icd_list = NULL,
                             fu_list = "all",
                             selected_files,
                             database,
                             min_age = 0,
                             max_age = Inf,
                             proc_code = NA,
                             export_file_code = "default",
                             output_dir = NULL) {
  # Error messages
  if (!database %in% c("AQ", "AR", "RD"))
    stop("Error: 'Unrecognized database. Use one of the following values: AR, AQ, or RD'")

  # Define output directory
  if (is.null(output_dir)) {
    output_dir <- paste0(getwd())
  }

  # Check if the output directory exists
  if (!dir.exists(output_dir)) {
    stop("Output directory does not exist.")
  }

  # Check if the datasus_files directory exists and create if it doesn't
  datasus_dir <- file.path(output_dir, "datasus_files")
  if (!dir.exists(datasus_dir)) {
    dir.create(datasus_dir)
  }


  # Define years
  FIRST_YEAR <- min(as.numeric(substr(selected_files, 5, 6)), na.rm = TRUE) +
    2000
  LAST_YEAR <- max(as.numeric(substr(selected_files, 5, 6)), na.rm = TRUE) +
    2000
  # Define icds
  if (length(icd_list) >
      1) {
    icd_list <- paste(icd_list, collapse = "|")
  } else {
    icd_list <- icd_list
  }
  # Define Federative units
  if (any(fu_list == "all")) {
    fu_list <- OncoDatasusR::ufs$sigla
  }
  # Definition of FTP folder path
  if (database %in% c("AR", "AQ"))
    FTP_URL = "ftp://ftp.datasus.gov.br/dissemin/publicos/SIASUS/200801_/Dados/"
  if (database %in% c("RD", "SP"))
    FTP_URL = "ftp://ftp.datasus.gov.br/dissemin/publicos/SIHSUS/200801_/Dados/"
  ## Download files and generate csv for each FU
  for (fu in fu_list) {
    # New data frame
    fu_df <- data.frame()
    # Filter only specific FU in file name
    fu_file_list <- selected_files[sapply(selected_files, function(x)
      substring(x, 3, 4) ==
        fu)]

    for (file_name in fu_file_list) {
      # Download file. '.retry()' is a internal function of package that retries the download in case of failure
      .retry(download.file(
        url = paste0(FTP_URL, file_name),
        destfile = file_name,
        mode = "wb"
      ))

      # Read file Filter only specific icds
      if (any(!is.null(icd_list))) {
        if (database %in% c("AR", "AQ"))
          temp_df <- read.dbc::read.dbc(file_name) %>%
            dplyr::filter(grepl(icd_list, AP_CIDPRI))
        if (database %in% c("RD", "SP"))
          temp_df <- read.dbc::read.dbc(file_name) %>%
            dplyr::filter(grepl(icd_list, DIAG_PRINC))
      } else {
        if (database %in% c("AR", "AQ"))
          temp_df <- read.dbc::read.dbc(file_name)
        if (database %in% c("RD", "SP"))
          temp_df <- read.dbc::read.dbc(file_name)
      }
      if (min_age != 0 | max_age != Inf) {
        if (database %in% c("AR", "AQ")) {
          temp_df <- temp_df %>%
            dplyr::mutate(
              AP_NUIDADE = as.numeric(as.character(AP_NUIDADE)),
              AP_COIDADE = as.numeric(as.character(AP_COIDADE)),
              IDADE_ANOS = case_when(
                AP_COIDADE <= 3 ~ 0,
                AP_COIDADE == 4 ~ AP_NUIDADE,
                AP_COIDADE ==
                  5 ~ AP_NUIDADE + 100
              )
            ) %>%
            dplyr::filter(IDADE_ANOS >= min_age) %>%
            dplyr::filter(IDADE_ANOS <= max_age)
        }
        if (database %in% c("RD", "SP")) {
          temp_df <- temp_df %>%
            dplyr::mutate(
              IDADE = as.numeric(as.character(IDADE)),
              COD_IDADE = as.numeric(as.character(COD_IDADE)),
              IDADE_ANOS = case_when(
                COD_IDADE <= 3 ~ 0,
                COD_IDADE == 4 ~ IDADE,
                COD_IDADE ==
                  5 ~ IDADE + 100
              )
            ) %>%
            dplyr::filter(IDADE_ANOS >= min_age) %>%
            dplyr::filter(IDADE_ANOS <= max_age)
        }
      }
      if (!is.na(proc_code)) {
        if (database %in% c("AR", "AQ"))
          temp_df <- temp_df %>%
            dplyr::filter(substr(as.character(AP_PRIPAL), 1, 2) ==
                            proc_code)
        if (database %in% c("RD", "SP"))
          temp_df <- temp_df %>%
            dplyr::filter(substr(as.character(PROC_REA), 1, 2) ==
                            proc_code)
      }
      # Append df
      fu_df <- dplyr::bind_rows(fu_df, temp_df)
      # Remove dbc file
      file.remove(file_name)
    }
    # Export FU csv
    if (nrow(fu_df) >
        0) {
      # If there is more than one CID, change symbol between them
      if (export_file_code == "default")
        export_file_code <- gsub("\\|", "_", icd_list)
      export_file_name <- paste0(
        datasus_dir,
        "/",
        database,
        "_",
        fu,
        "_",
        export_file_code,
        "_",
        as.character(FIRST_YEAR),
        "_",
        as.character(LAST_YEAR),
        ".csv"
      )
      readr::write_csv(fu_df, export_file_name)
    }
  }
}
