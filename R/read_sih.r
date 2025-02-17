#' Read and Process SIH Records
#'
#' This function reads a file containing data from the Hospital Information System (SIH),
#' filtering records based on patient age, procedure code, and specified year range.
#' It optionally performs diagnostics of data quality and exports a frequency table
#' of procedures, including their descriptions.
#'
#' @importFrom magrittr %>%
#' @param dir Character. Path to the directory containing the SIH data file. If NULL, the function will search for the 'datasus_files' folder in the current working directory.
#' @param filename Character. Name of the file containing the SIH data.
#' @param min_age Numeric. Minimum age of patients during hospitalization.
#' @param max_age Numeric. Maximum age of patients during hospitalization.
#' @param proc_code Character. Two-digit procedure code to filter records. Default is NA (no filter applied).
#' @param select_variables Character. (Optional) Additional variables to include in the resulting table. Default is NA.
#' @param first_year Integer. Earliest year of hospitalizations to retain in the database.
#' @param last_year Integer. Most recent year of hospitalizations to retain in the database. Default is NA (no upper limit).
#' @param run_diagnostics Logical. If TRUE, performs diagnostics and attaches data quality analysis to the output. Default is TRUE.
#' @param silent Logical. If TRUE, suppresses console messages about processing steps. Default is FALSE.
#' @param export_procedure_table Logical. If TRUE, saves a .csv file containing a frequency table of procedures present in the database. Default is FALSE.
#' @param file_code Character (Optional). String used in naming the output file. Default is NA.
#' @param procedures_filename Character (Optional). Name of the file to save the frequency table of procedures. Default is NA.
#' @param group_icd Logical. If TRUE, groups procedures by ICD using the `icd_group_file_path`. Default is FALSE.
#' @param icd_group_file_path Character. Path and name of the file with ICD group mappings. The file must contain columns 'icd' and 'icd_group'. Default is NULL.
#'
#' @details
#' The function processes SIH data by filtering records based on age, procedure codes, and year of hospitalization. It generates diagnostic tables describing data quality, such as missing values, formatting errors, and inconsistencies in date fields. If `group_icd` is TRUE, the function expects an ICD group mapping file, which is used to group procedures in the frequency table output.
#'
#' The `first_year` and `last_year` parameters define the range of years for filtering records by admission year. The `proc_code` parameter can be used to filter specific procedure types. If `export_procedure_table` is TRUE, a procedure frequency table is saved to the specified directory.
#'
#' @return
#' - If `run_diagnostics` is TRUE, returns a list with the following elements:
#'   * `SIH`: A data frame with the filtered SIH data.
#'   * `diag_list`: A list containing diagnostic summaries:
#'       - `row_n`: Number of rows before and after filters.
#'       - `ICD_base_table`: Table summarizing ICD counts.
#'       - `admission_year_table`: Count of records by year of admission.
#'       - `procedure_table`: Frequency table of procedures with descriptions.
#'       - `error_table_all`: Table summarizing data quality errors.
#' - If `run_diagnostics` is FALSE, returns only the filtered SIH data as a data frame.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Read and process SIH data with diagnostics and procedure table export
#' result <- read_sih(
#'   dir = 'path/to/data',
#'   filename = 'sih_data.csv',
#'   min_age = 0,
#'   max_age = 100,
#'   proc_code = '02',
#'   select_variables = c('extra_var1', 'extra_var2'),
#'   first_year = 2008,
#'   last_year = 2020,
#'   run_diagnostics = TRUE,
#'   silent = FALSE,
#'   export_procedure_table = TRUE,
#'   file_code = 'SIH'
#' )
#' head(result$SIH)
#' head(result$diag_list)
#' }
read_sih <- function(dir = NULL,
                     filename,
                     min_age = 0,
                     max_age = Inf,
                     proc_code = NA,
                     select_variables = NA,
                     first_year = 2008,
                     last_year = NA,
                     run_diagnostics = TRUE,
                     silent = FALSE,
                     export_procedure_table = FALSE,
                     procedures_filename = NA,
                     file_code = NA,
                     group_icd = FALSE,
                     icd_group_file_path = NULL) {
  if (is.null(dir)) {
    dir <- paste0(getwd(), "/datasus_files/")
    if (!dir.exists(dir))
      stop("Error: The folder 'datasus_files' was not found in the current working directory.")
  } else {
    if (!dir.exists(dir)) {
      stop("Error: The directory does not exist.")
    }
  }

  if (group_icd) {
    if (file.exists(icd_group_file_path)) {
      icd_groups <- read.csv(
        icd_group_file_path,
        colClasses = c("character", "character"),
        encoding = "UTF-8"
      )
    } else {
      stop("Error: File with ICD grouping not found!")
    }
  }

  if (group_icd) {
    if (all(c("icd_group", "icd") %in%
            colnames(icd_groups))) {
      icd_groups <- icd_groups %>%
        dplyr::select(icd, icd_group)
    } else {
      stop("Error: The ICD grouping file must contain the columns 'icd' and 'icd_group'")
    }
  }


  # Read table
  SIH <- data.frame(data.table::fread(paste0(dir, filename), colClasses = "character"))

  # Replace empty values with NA
  SIH[SIH == ""] <- NA

  # Select variables
  variables <- c(
    "DIAG_PRINC",
    "DT_INTER",
    "NASC",
    "CEP",
    "SEXO",
    "PROC_REA",
    "IDADE",
    "COD_IDADE",
    "MUNIC_RES",
    "MUNIC_MOV"
  )

  # Include additional selected variables
  if (!is.na(select_variables))
    variables <- c(variables, select_variables)

  # Select only the defined variables and discard the others
  SIH <- SIH %>%
    dplyr::select(all_of(variables))

  # Procedures and ICD codes

  sigtap_procedures <- OncoDatasusR::sigtap_procedures
  icd_description <- OncoDatasusR::icd_description


  # Create age years primary CID variable and CEP5 (first 5 digits of CEP)
  SIH <- SIH %>%
    dplyr::mutate(
      IDADE = as.numeric(IDADE),
      COD_IDADE = as.numeric(COD_IDADE),
      IDADE_ANOS = dplyr::case_when(COD_IDADE <= 3 ~ 0, COD_IDADE == 4 ~ IDADE, COD_IDADE == 5 ~ IDADE +
                                      100),
      CID = substr(DIAG_PRINC, 1, 3),
      CEP5 = substr(CEP, 1, 5)
    )

  n_1 <- nrow(SIH)

  if (min_age != 0 | max_age != Inf) {
    SIH <- SIH %>%
      dplyr::filter(IDADE_ANOS >= min_age) %>%
      dplyr::filter(IDADE_ANOS <= max_age) %>%
      dplyr::select(-IDADE, -COD_IDADE)
  }

  n_2 <- nrow(SIH)

  if (!is.na(proc_code))
    SIH <- SIH %>%
    dplyr::filter(substr(as.character(PROC_REA), 1, 2) ==
                    proc_code)

  n_3 <- nrow(SIH)

  # Filter by year of admission
  SIH <- SIH %>%
    dplyr::mutate(ano_internacao = as.numeric(substr(DT_INTER, 1, 4))) %>%
    dplyr::filter(ano_internacao >= first_year)

  if (!is.na(last_year))
    SIH <- SIH %>%
    dplyr::filter(ano_internacao <= last_year)

  n_4 <- nrow(SIH)

  if (run_diagnostics) {
    diag_list <- list()
    row_n <- data.frame(
      n1 = n_1,
      n_age_trim = n_2,
      n_procedure_trim = n_3,
      n_year_trim = n_4
    )
    diag_list$row_n <- row_n
  }

  if (run_diagnostics) {
    suppressWarnings({
      ICD_base_table <- SIH %>%
        dplyr::count(DIAG_PRINC) %>%
        dplyr::arrange(DIAG_PRINC)


      ICD_base_table <- ICD_base_table %>%
        dplyr::left_join(icd_description, by = c(DIAG_PRINC = "cd_cid")) %>%
        dplyr::select(DIAG_PRINC, ds_cid, n) %>%
        dplyr::mutate(DIAG_PRINC = ifelse(
          nchar(DIAG_PRINC) >
            3,
          paste0(substr(DIAG_PRINC, 1, 3), ".", substr(DIAG_PRINC, 4, 4)),
          DIAG_PRINC
        )) %>%
        dplyr::mutate(DIAG_PRINC = ifelse(
          nchar(DIAG_PRINC) ==
            3,
          paste0(DIAG_PRINC, ".x"),
          DIAG_PRINC
        )) %>%
        dplyr::mutate(CID_principal = substr(DIAG_PRINC, 1, 3)) %>%
        dplyr::relocate(CID_principal)

      main_cid_total_tbl <- ICD_base_table %>%
        dplyr::group_by(CID_principal) %>%
        dplyr::summarise(total = sum(n))

      ICD_base_table <- ICD_base_table %>%
        dplyr::left_join(main_cid_total_tbl, by = c("CID_principal")) %>%
        dplyr::mutate(CID_principal = paste0(CID_principal, " (Total = ", total, ")")) %>%
        dplyr::select(-total)

      diag_list$ICD_base_table <- ICD_base_table

      # Count missing or incongruent values
      NA_column <- SIH %>%
        dplyr::select(-CID, -CEP5, -IDADE_ANOS, -ano_internacao) %>%
        dplyr::summarise_all( ~ (sum(is.na(.))))

      variable_names <- SIH %>%
        dplyr::select(-CID, -CEP5, -IDADE_ANOS, -ano_internacao) %>%
        names()

      error_table <- data.frame(matrix(0, 4, length(variable_names)))
      names(error_table) <- variable_names
      rownames(error_table) <- c("value_error",
                                 "date_error",
                                 "date_below_lim",
                                 "date_above_lim")

      if ("DIAG_PRINC" %in% variable_names)
        error_table$DIAG_PRINC[1] <- sum(!(
          grepl("^[A-Z][0-9]{2}$", SIH$DIAG_PRINC) |
            grepl("^[A-Z][0-9]{3}$", SIH$DIAG_PRINC)
        ))
      if ("PROC_REA" %in% variable_names)
        error_table$PROC_REA[1] <- sum(nchar(SIH$PROC_REA) !=
                                         10 | is.na(as.numeric(SIH$PROC_REA)))
      if ("DT_INTER" %in% variable_names) {
        admission_date <- lubridate::ymd(SIH$DT_INTER)
        error_table$DT_INTER[1] <- sum(nchar(SIH$DT_INTER) !=
                                         8 | is.na(as.numeric(SIH$DT_INTER)))
        error_table$DT_INTER[2] <- sum(is.na(admission_date))
        error_table$DT_INTER[3] <- sum(admission_date <= lubridate::ymd(paste0(first_year, "-01-01")))
        error_table$DT_INTER[4] <- sum(admission_date >= lubridate::ymd(paste0(last_year, "-12-31")))
      }

      if ("NASC" %in% variable_names) {
        birth_date <- lubridate::ymd(SIH$NASC)
        max_admission_date <- max(lubridate::ymd(SIH$DT_INTER), na.rm = TRUE)
        error_table$NASC[1] <- sum(nchar(SIH$NASC) !=
                                     8 | is.na(as.numeric(SIH$NASC)))
        error_table$NASC[2] <- sum(is.na(birth_date))
        error_table$NASC[3] <- sum(birth_date <= lubridate::ymd("1890-01-01"))
        error_table$NASC[4] <- sum(birth_date >= max_admission_date)
      }

      if ("CEP" %in% variable_names)
        error_table$CEP[1] <- sum(nchar(SIH$CEP) !=
                                    8 | is.na(as.numeric(SIH$CEP)))
      if ("SEXO" %in% variable_names)
        error_table$SEXO[1] <- sum(!(SIH$SEXO %in% c("1", "3")))

      error_table_all <- rbind(
        missing_values = NA_column,
        error_table,
        total = colSums(error_table) +
          NA_column
      )

      diag_list$error_table_all <- error_table_all

      # Count records by year of diagnosis and year of service
      admission_year_table <- SIH %>%
        dplyr::mutate(ano_internacao = substr(DT_INTER, 1, 4)) %>%
        dplyr::count(ano_internacao) %>%
        dplyr::arrange(ano_internacao)

      diag_list$admission_year_table <- admission_year_table

    })
  }

  # Create table with procedure counts
  procedure_table <- SIH %>%
    dplyr::count(PROC_REA, CID) %>%
    dplyr::arrange(desc(n)) %>%
    tidyr::pivot_wider(names_from = CID, values_from = n)

  # Reorganize the table according to the CID
  if (length(unique(SIH$CID)) >
      1) {
    procedure_table_all <- SIH %>%
      dplyr::count(PROC_REA) %>%
      dplyr::rename(Total = n)

    procedure_table <- procedure_table %>%
      dplyr::left_join(procedure_table_all, by = "PROC_REA")
  }

  # Include the description of the procedures
  procedure_table <- procedure_table %>%
    dplyr::left_join(sigtap_procedures, by = c(PROC_REA = "key")) %>%
    dplyr::relocate(PROC_REA, description) %>%
    dplyr::rename(procedure_code = PROC_REA) %>%
    data.frame()


  if (run_diagnostics)
    diag_list$procedure_table <- procedure_table

  if (export_procedure_table) {
    if (!is.na(procedures_filename)) {
      filename <- procedures_filename
    } else {
      if (is.na(file_code))
        file_code <- "SIH"
      filename <- paste0(file_code,
                         "_",
                         first_year,
                         "_",
                         last_year,
                         "_procedures.csv")
    }
    if (!silent)
      message(paste0("Writing: ", paste0(dir, filename)))
    readr::write_csv(procedure_table, file = paste0(dir, filename))
  }

  if (group_icd) {
    SIH <- SIH %>%
      dplyr::left_join(icd_groups, by = c(CID = "icd"))
  }
  if (run_diagnostics) {
    output <- list(SIH = SIH, diag_list = diag_list)
  } else {
    output <- SIH
  }


  return(output)
}
