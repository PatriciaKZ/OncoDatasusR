#' Merge SIH Records and Count Unique Patients
#'
#' This function filters and deduplicates patient records in a data frame containing SIH (Hospital Information System) data. 
#' It can optionally save the output to a .csv file.
#'
#' @importFrom magrittr %>%
#' @param sih_table DataFrame. Input data frame containing SIH data.
#' @param first_year Integer. The earliest year to include in the data. If `NULL`, the earliest year of attendance in the database will be used.
#' @param last_year Integer. The latest year to include in the data. If `NULL`, the latest year of attendance in the database will be used.
#' @param age_group Character. The age group to consider for filtering. Options are 'all' (default), 'adult', 'young_adult', and 'pediatric'.
#' @param sex Character. The sex to consider for filtering. If `NULL`, all records are considered. Options are 'M' and 'F'.
#' @param write Logical. If `TRUE`, saves the output to a .csv file. Default is `FALSE`.
#' @param output_dir Character. Directory path where the file will be saved. If `NULL`, the function will use the 'datasus_files' folder in the current working directory. Default is `NULL`.
#' @param filename Character. Name of the output file. Default is `NA`, which saves the file as "SIH_merged.csv".
#' @param silent Logical. If `TRUE`, suppresses console messages about file saving. Default is `FALSE`.
#' @param usecep Logical. If `TRUE`, uses the first 5 digits of the CEP (postal code) as part of the patient identification key. Default is `FALSE`.
#' @return DataFrame. A deduplicated data frame summarizing the count of unique patients.
#' @details 
#' - The function creates a unique patient identifier (`cod_paciente`) based on the provided birth date, sex, and optionally the first 5 digits of the CEP.
#' - If multiple records for the same patient are found, the record with the earliest admission date is retained.
#' - Deduplication behavior can be adjusted by toggling the `usecep` parameter.
#' - The `age_group` parameter specifies the age group to consider for filtering. The minimum and maximum ages for each group are:
#'      - 'all': 0-99 years;
#'      - 'adult': 18-99 years;
#'      - 'young_adult': 15-39 years;
#'      - 'pediatric': 0-18 years;
#' - If `write = TRUE`, the output file is saved in the specified or default directory.
#' @export
#' @examples
#' \dontrun{
#' # Merge SIH data and count unique patients, saving the result to a file
#' result <- merge_sih(
#'   sih_table = sih_data, 
#'   first_year = 2008,
#'   last_year = 2020,
#'   age_group = 'adult',
#'   sex  = NULL,
#'   write = TRUE, 
#'   output_dir = "path/to/save/", 
#'   filename = "sih_merged.csv",
#'   silent = FALSE, 
#'   usecep = TRUE
#' )
#' head(result)
#' }
merge_sih <- function(
    sih_table, first_year = NULL, last_year = NULL, age_group = "all", sex = NULL, write = FALSE, output_dir = NULL,
    filename = NA, silent = FALSE, usecep = FALSE, run_diagnostics = TRUE
) {

    if (is.null(output_dir)) {
        output_dir <- paste0(getwd(), "/exported_data")
        if(!dir.exists(output_dir)) dir.create(output_dir)
    } else {
        if(!dir.exists(output_dir)) { 
            stop ("Error: The directory does not exist.")
        } else {
            dir.create(paste0(output_dir, "/exported_data"))
            output_dir <- paste0(output_dir, "/exported_data")
        }
    }

      if(!age_group %in% c("all", "adult", "young_adult", "pediatric")) {
    stop(
      "Error: 'age_group' must be either 'all', 'adult', 'young_adult', or 'pediatric'."
    )
  }

    if (!is.null(sex)) {
    if (!sex %in% c("M", "F")) {
      stop("Error: 'sex' must be either 'M' or 'F'.")
    }
  }

    if (is.null(first_year))
    first_year <- min(sih_table$ano_internacao, na.rm = TRUE)

    if (is.null(last_year))
    last_year <- max(sih_table$ano_internacao, na.rm = TRUE)

      age_params <- list(
      all = list(min_age = 0, max_age = 99),
      adult = list(min_age = 18, max_age = 99),
      young_adult = list(min_age = 15, max_age = 39),
      pediatric = list(min_age = 0, max_age = 18)
      )

      age_param <- age_params[[age_group]]
  
  if (usecep) {
    SIH_merged <- sih_table %>%
      # Filter data up to 2021
      dplyr::mutate(ano_internacao = as.numeric(substr(DT_INTER, 1, 4))) %>%
      # Create column with admission date in date format
      dplyr::mutate(data_internacao = as.Date(as.character(DT_INTER), format = "%Y%m%d")) %>%
      # Transform SEXO column codes. 1 = Male ("M"), 3 = Female ("F")
      dplyr::mutate(SEXO = factor(SEXO, levels = c(1, 3), labels = c("M", "F"))) %>%
      # Create a ID patient column using birth date, sex and the first 5 digits of the CEP
      dplyr::mutate(cod_paciente = paste0(NASC, SEXO, CEP5)) %>%
      # Group data by birth date, sex, and CEP
      dplyr::group_by(cod_paciente) %>%
      # Reorder row sequence by birth date, sex, CEP, and admission date
      dplyr::arrange(NASC, SEXO, CEP5, desc(data_internacao)) %>%
      # Create a new column indicating the number of repeated records (by birth date, sex, and CEP)
      dplyr::mutate(num = 1:dplyr::n(), n_rep = dplyr::n()) %>%
      # Filter to keep only unique records, or if more than one record with the same NASC, SEXO, and CEP5, the one with the earliest admission date
      dplyr::filter(num == n_rep) %>%
      # Ungroup data
      dplyr::ungroup()
  } else {
    SIH_merged <- sih_table %>%
      # Filter data up to 2021
      dplyr::mutate(ano_internacao = as.numeric(substr(DT_INTER, 1, 4))) %>%
      # Create column with admission date in date format
      dplyr::mutate(data_internacao = as.Date(as.character(DT_INTER), format = "%Y%m%d")) %>%
      # Transform SEXO column codes. 1 = Male ("M"), 3 = Female ("F")
      dplyr::mutate(SEXO = factor(SEXO, levels = c(1, 3), labels = c("M", "F"))) %>%
      # Create a ID patient column using the birth date and sex
      dplyr::mutate(cod_paciente = paste0(NASC, SEXO)) %>%
      # Group data by birth date and sex
      dplyr::group_by(cod_paciente) %>%
      # Reorder row sequence by birth date, sex, and admission date
      dplyr::arrange(NASC, SEXO, desc(data_internacao)) %>%
      # Create a new column indicating the number of repeated records (by birth date and sex)
      dplyr::mutate(num = 1:dplyr::n(), n_rep = dplyr::n()) %>%
      # Filter to keep only unique records, or if more than one record with the same NASC and SEXO, the one with the earliest admission date
      dplyr::filter(num == n_rep) %>%
      # Ungroup data
      dplyr::ungroup()
  }

    if (run_diagnostics) {
    n_before <- nrow(SIH_merged)

    # Count unique patients
    SIH_unique_before <- length(unique(SIH_merged$cod_paciente))

    # Count number of records with a year outside the specified
    # interval
    lt_first_year_n <- SIH_merged %>%
      dplyr::select(ano_internacao) %>%
      dplyr::filter(ano_internacao < first_year) %>%
      nrow()
    gt_last_year_n <- SIH_merged %>%
      dplyr::select(ano_internacao) %>%
      dplyr::filter(ano_internacao > last_year) %>%
      nrow()
  }

    ## Select only the years of interest
  SIH_merged <- SIH_merged %>%
    dplyr::filter(ano_internacao >= first_year) %>%
    dplyr::filter(ano_internacao <= last_year)

  if (run_diagnostics) {
    n_after <- nrow(SIH_merged)
    SIH_unique_after <- length(unique(SIH_merged$cod_paciente))
  }

  if (run_diagnostics) {
    n_before_trim_age <- nrow(SIH_merged)
    lt_min_age <- sum(SIH_merged$IDADE_ANOS < age_param$min_age)
    gt_max_age <- sum(SIH_merged$IDADE_ANOS > age_param$max_age)
    n_after_trim_age <- nrow(SIH_merged) -
      lt_min_age - gt_max_age
  }

    SIH_merged <- SIH_merged %>%
    dplyr::filter(IDADE_ANOS >= age_param$min_age) %>%
    dplyr::filter(IDADE_ANOS <= age_param$max_age)

    if (run_diagnostics) {
    n_before_trim_sex = nrow(SIH_merged)
    rm_sex <- sum(SIH_merged$SEXO != sex)
  }

  if (!is.null(sex)) {
    SIH_merged <- SIH_merged %>%
      dplyr::filter(SEXO == sex)
  } else { sex = "both"}

  if (run_diagnostics) {
    n_after_trim_sex <- nrow(SIH_merged)
    final_patient_n <- length(unique(SIH_merged$cod_paciente))
    
    diag_list <- data.frame(
      n_before = n_before, SIH_unique_before = SIH_unique_before,
      lt_first_year_n = lt_first_year_n, gt_last_year_n = gt_last_year_n,
      n_after = n_after, SIH_unique_after = SIH_unique_after,
      n_before_trim_age = n_before_trim_age,
      lt_min_age = lt_min_age, gt_max_age = gt_max_age, n_after_trim_age = n_after_trim_age,
      selected_sex = sex, n_before_trim_sex = n_before_trim_sex,
      n_after_trim_sex = n_after_trim_sex, rm_sex = rm_sex, final_patient_n = final_patient_n
    )

    output <- list(SIH_table = SIH_merged, diag_list = diag_list)
  } else {
    output <- SIH_merged
  }

if (write) {
  if (!silent) {
    if (is.na(filename)) filename <- "SIH_merged.csv"
    message(paste0("Writing: ", file.path(output_dir, filename)))
  }
  
  write.csv2(SIH_merged, file.path(output_dir, filename))
}

  return(output)
}