#' Get DATASUS Files
#'
#' This function automates the process of fetching file names from the DATASUS FTP server, downloading data by state,
#' converting from .dbc to .csv format, retrieving the list of .csv data files by state, and merging all datasets into one table.
#'
#' @param icd_list Character. List of ICD codes used for data filtering. Default is `NULL` to include all ICD codes.
#' @param fu_list Character. Vector containing the acronyms of the states for download. Use 'all' to download data from all 27 federative units of Brazil. Default is `"all"`.
#' @param export_file_code Character. If 'default', the function will use the default naming behavior.
#' @param database Character. Name of the database to be used. Acceptable values are 'AR', 'AQ', or 'RD'.
#' @param first_year Integer. The first year of the data interval to be downloaded.
#' @param last_year Integer. The last year of the data interval to be downloaded.
#' @param min_age Integer. Minimum age at the time of attendance for data filtering. Default is `0`.
#' @param max_age Integer. Maximum age at the time of attendance for data filtering. Default is `Inf`.
#' @param proc_code Character. Procedure code vector for data filtering. Default is `NA`.
#' @param output_dir Character. Working directory where files will be saved. If `NULL`, the function will use the 'datasus_files' folder in the current working directory.
#' @param rm_ufs_files Logical. If `TRUE`, the function will remove the .csv files for each state after merging them into one table. Default is `TRUE`.
#' @return
#' - .csv files saved in the 'datasus_files' folder within the output directory.
#' - A `data.frame` containing the merged table with data extracted from DATASUS.
#' @details
#' - If `fu_list` is 'all', the function will download data from all 27 federative units of Brazil.
#' - Files are saved in the 'datasus_files' folder, created in the specified or default output directory.
#' - This function is a wrapper for the following functions: `retrieve_filelist()`, `download_datasus()`, `fetch_files()`, and `merge_datasus()`.
#' @export
#' @examples
#' \dontrun{
#' # Download and merge data for ICD 'C00', database 'AR', and years 2010 to 2020
#' get_datasus(
#'   icd_list = 'C00',
#'   fu_list = 'all',
#'   export_file_code = 'default',
#'   database = 'AR',
#'   first_year = 2010,
#'   last_year = 2020,
#'   output_dir = NULL
#' )
#' }
get_datasus <- function(icd_list = NULL,
                        fu_list = "all",
                        export_file_code = "default",
                        database,
                        first_year,
                        last_year,
                        min_age = 0,
                        max_age = Inf,
                        output_dir = NULL,
                        proc_code = NA,
                        rm_ufs_files = TRUE) {
  # Error messages
  if (last_year < 2008)
    stop("Error: last_year must be equal to or greater than 2008")
  if (!database %in% c("AQ", "AR", "RD"))
    stop("Error: 'Unrecognized database. Use one of the following values: AR, AQ, or RD'")

  # List of FUs
  if (any(fu_list == "all")) {
    fu_list <- OncoDatasusR::ufs$sigla
  }

  dbc_filelist <- retrieve_filelist(database = database,
                                    first_year = first_year,
                                    last_year = last_year)

  download_datasus(
    icd_list = icd_list,
    fu_list = fu_list,
    selected_files = dbc_filelist,
    export_file_code = export_file_code,
    database = database,
    output_dir = output_dir
  )

  csv_filelist <- fetch_files(
    output_dir = output_dir,
    database = database,
    export_file_code = export_file_code,
    icd_list = icd_list,
    first_year = first_year,
    last_year = last_year
  )

  merge_datasus(
    file_list = csv_filelist,
    output_dir = output_dir,
    export_file_code = export_file_code,
    database = database,
    icd_list = icd_list,
    first_year = first_year,
    last_year = last_year,
    rm_ufs_files = rm_ufs_files
  )
}
