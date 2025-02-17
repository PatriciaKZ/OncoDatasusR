#' Retrieve a List of Filenames from a DATASUS FTP Server
#'
#' This function retrieves the names of files available for download on a DATASUS FTP server,
#' based on the provided database acronym, specified year range, and the maximum wait time to access the FTP.
#'
#' @param database Character. Acronym of the database for download. Acceptable values are 'AR', 'AQ', or 'RD'.
#' @param first_year Integer. The oldest year of the database for extraction. Default is 2008.
#' @param last_year Integer. The most recent year of the database for extraction. Default is Inf.
#' @return A Character vector containing the names of available files for download, filtered according to the provided parameters.
#' @export
#'
#' @examples
#' \dontrun{
#' # Retrieve file names from the 'AR' database, for the years 2010 to 2020
#' filelist <- retrieve_filelist(database = 'AR', first_year = 2010, last_year = 2020)
#' print(filelist)
#' }
retrieve_filelist <- function(database,
                              first_year = 2008,
                              last_year = Inf) {
  # Error messages
  if (last_year < 2008)
    stop("Error: last_year must be equal to or greater than 2008")
  if (!database %in% c("AQ", "AR", "RD"))
    stop("Error: 'Unrecognized database. Use one of the following values: AR, AQ, or RD'")

  # Define the path to the DATASUS FTP directory
  if (database %in% c("AR", "AQ")) {
    FTP_URL <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIASUS/200801_/Dados/"
  }
  if (database %in% c("RD", "SP")) {
    FTP_URL <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIHSUS/200801_/Dados/"
  }

  # {warning('Unrecognized database. Use one of the following values: AR, AQ,
  # or RD')}

  # Define the maximum wait time for access attempts
  options(timeout = 5000000)

  # Access and download data from the FTP. Get all files listed on the FTP
  all_files_string <- RCurl::getURL(FTP_URL, dirlistonly = TRUE)
  all_files_list <- strsplit(all_files_string, "\r\n")[[1]]

  # Filter only the selected database if the name starts with database
  selected_files <- all_files_list[grepl(paste0("^", database), all_files_list)]

  # Filter only the specified years - Filename pattern: '{DB}{FU}{YYMM}.dbc'
  selected_files <- selected_files[sapply(selected_files, function(x)
    as.numeric(substring(x, 5, 6)) >=
      first_year - 2000)]
  selected_files <- selected_files[sapply(selected_files, function(x)
    as.numeric(substring(x, 5, 6)) <=
      last_year - 2000)]

  # Filter only files ending with '.dbc'
  selected_files <- selected_files[sapply(selected_files, function(x)
    tolower(substr(selected_files, 10, 13)) ==
      "dbc")]

  # Alphabetically sort the list of files
  selected_files <- sort(selected_files)

  return(selected_files)
}
