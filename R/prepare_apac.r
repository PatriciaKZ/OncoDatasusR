#' Finalize Preparation of APAC Table
#'
#' This function performs the final preparation of the APAC table, including filtering records by the year of diagnosis,
#' patient age, and removing inconsistencies between the service date and the diagnosis date.
#'
#' @importFrom magrittr %>%
#' @param apac_table DataFrame. The APAC table containing patient data.
#' @param first_year Integer. The earliest year to include in the data. If `NULL`, the earliest year of attendance in the database will be used.
#' @param last_year Integer. The latest year to include in the data. If `NULL`, the latest year of attendance in the database will be used.
#' @param age_type Character. The type of age to consider for filtering. Options are 'age_diagnosis' (default) and 'age_attendance'.
#' @param age_group Character. The age group to consider for filtering. Options are 'all' (default), 'adult', 'young_adult', and 'pediatric'.
#' @param sex Character. The sex to consider for filtering. If `NULL`, all records are considered. Options are 'M' and 'F'.
#' @param write Logical. If `TRUE`, saves the processed table as a `.csv` file in the 'exported_data' folder. Default is `TRUE`.
#' @param filename Character. The name of the file to be saved.
#' @param output_dir Character. Directory where the 'exported_data' folder will be created. If `NULL`, it will be created in the current working directory.
#' @param run_diagnostics Logical. If `TRUE`, generates and saves diagnostic information about the data processing. Default is `TRUE`.
#' @param silent Logical. If `TRUE`, suppresses console messages about processing steps. Default is `FALSE`.
#' @return
#' - If `run_diagnostics = TRUE`: Returns a list with two elements:
#'   - `APAC_table`: The processed APAC table.
#'   - `diag_list`: A data frame with diagnostic information about the processing steps.
#' - If `run_diagnostics = FALSE`: Returns only the processed APAC table.
#' @details
#' - Filters records by year, patient age, and consistency between service and diagnosis dates.
#' - The `age_type` parameter determines whether to use the age at diagnosis or the age at attendance and year of diagnosis or year of attendance for filtering.
#' - The `age_group` parameter specifies the age group to consider for filtering. The minimum and maximum ages for each group are:
#'      - 'all': 0-99 years;
#'      - 'adult': 18-99 years;
#'      - 'young_adult': 15-39 years;
#'      - 'pediatric': 0-18 years;
#' - Diagnostic information includes counts of records removed at each processing step.
#' - Outputs can be saved as a `.csv` file if `write = TRUE`.
#' @export
#' @examples
#' \dontrun{
#' # Finalize preparation of APAC table with diagnostics and save the result
#' result <- prepare_apac(
#'   apac_table = apac_data,
#'   first_year = 2008,
#'   last_year = 2020,
#'   age_type = 'age_diagnosis',
#'   age_group = 'all',
#'   write = TRUE,
#'   filename = 'prepared_apac.csv',
#'   output_dir = 'path/to/save/',
#'   run_diagnostics = TRUE,
#'   silent = FALSE
#' )
#' head(result$APAC_table)
#' head(result$diag_list)
#' }
prepare_apac <- function(apac_table,
                         first_year = NULL,
                         last_year = NULL,
                         age_type = "age_diagnosis",
                         age_group = "all",
                         sex = NA,
                         write = TRUE,
                         filename,
                         output_dir = NULL,
                         run_diagnostics = TRUE,
                         silent = FALSE) {
  if (is.null(output_dir)) {
    output_dir <- paste0(getwd(), "/exported_data/")
    if (!dir.exists(output_dir))
      dir.create(output_dir)
  } else {
    if (!dir.exists(output_dir)) {
      stop("Error: The directory does not exist.")
    } else {
      if (!dir.exists(paste0(output_dir, "/exported_data/"))) {
        dir.create(paste0(output_dir, "/exported_data/"))
      }
      output_dir <- paste0(output_dir, "/exported_data/")
    }
  }

  if (!age_type %in% c("age_diagnosis", "age_attendance")) {
    stop("Error: 'age_type' must be either 'age_diagnosis' or 'age_attendance'.")
  }

  if (!age_group %in% c("all", "adult", "young_adult", "pediatric")) {
    stop("Error: 'age_group' must be either 'all', 'adult', 'young_adult', or 'pediatric'.")
  }

  if (!is.null(sex)) {
    if (!sex %in% c("M", "F")) {
      stop("Error: 'sex' must be either 'M' or 'F'.")
    }
  }

  if (is.null(first_year))
    first_year <- min(apac_table$ano_atendimento, na.rm = TRUE)

  if (is.null(last_year))
    last_year <- max(apac_table$ano_atendimento, na.rm = TRUE)

  age_params <- list(
    all = list(min_age = 0, max_age = 99),
    adult = list(min_age = 18, max_age = 99),
    young_adult = list(min_age = 15, max_age = 39),
    pediatric = list(min_age = 0, max_age = 18)
  )
  # Select the age parameters according to the age group

  age_param <- age_params[[age_group]]

  # Define year and age columns based on the selected age type

  age_col <- ifelse(age_type == "age_diagnosis",
                    "idade_diagnostico",
                    "idade_anos")

  year_col <- ifelse(age_type == "age_diagnosis",
                     "ano_diagnostico",
                     "ano_atendimento")

  if (run_diagnostics) {
    n_before <- nrow(apac_table)

    # Count unique patients
    APAC_unique_before <- length(unique(apac_table$cod_paciente))

    # Count number of records with a year outside the specified
    # interval
    lt_first_year_n <- apac_table %>%
      dplyr::select(.data[[year_col]]) %>%
      dplyr::filter(.data[[year_col]] < first_year) %>%
      nrow()
    gt_last_year_n <- apac_table %>%
      dplyr::select(.data[[year_col]]) %>%
      dplyr::filter(.data[[year_col]] > last_year) %>%
      nrow()
  }

  ## Select only the years of interest
  apac_table <- apac_table %>%
    dplyr::filter(.data[[year_col]] >= first_year) %>%
    dplyr::filter(.data[[year_col]] <= last_year)

  if (run_diagnostics) {
    n_after <- nrow(apac_table)
    APAC_unique_after <- length(unique(apac_table$cod_paciente))
  }

  # Count inconsistencies where the service date is earlier than the
  # diagnosis date

  apac_table <- apac_table %>%
    dplyr::mutate(dt_error = as.numeric(ano_atendimento) <
                    as.numeric(ano_diagnostico))

  if (run_diagnostics)
    dt_error_n <- sum(apac_table$dt_error)

  # Reorder rows and columns
  apac_table <- apac_table %>%
    dplyr::filter(!dt_error) %>%
    dplyr::relocate(
      cod_paciente,
      cid_prim,
      sexo,
      idade_anos,
      data_diagnostico,
      ano_diagnostico,
      data_atendimento,
      tratamento,
      cep_paciente
    ) %>%
    dplyr::arrange(cid_prim, data_atendimento)

  if (run_diagnostics) {
    n_before_trim_age <- nrow(apac_table)
    lt_min_age <- sum(apac_table[[age_col]] < age_param$min_age)
    gt_max_age <- sum(apac_table[[age_col]] > age_param$max_age)
    n_after_trim_age <- nrow(apac_table) -
      lt_min_age - gt_max_age
  }

  apac_table <- apac_table %>%
    dplyr::filter(.data[[age_col]] >= age_param$min_age) %>%
    dplyr::filter(.data[[age_col]] <= age_param$max_age)

  if (run_diagnostics) {
    n_before_trim_sex = nrow(apac_table)
    rm_sex <- sum(apac_table$sexo != sex)
  }

  if (!is.null(sex)) {
    apac_table <- apac_table %>%
      dplyr::filter(sexo == sex)
  } else {
    sex = "both"
  }

  if (run_diagnostics) {
    n_after_trim_sex <- nrow(apac_table)
    final_patient_n <- length(unique(apac_table$cod_paciente))

    diag_list <- data.frame(
      n_before = n_before,
      APAC_unique_before = APAC_unique_before,
      lt_first_year_n = lt_first_year_n,
      gt_last_year_n = gt_last_year_n,
      n_after = n_after,
      APAC_unique_after = APAC_unique_after,
      dt_error_n = dt_error_n,
      age_type = age_type,
      n_before_trim_age = n_before_trim_age,
      lt_min_age = lt_min_age,
      gt_max_age = gt_max_age,
      n_after_trim_age = n_after_trim_age,
      selected_sex = sex,
      n_before_trim_sex = n_before_trim_sex,
      n_after_trim_sex = n_after_trim_sex,
      rm_sex = rm_sex,
      final_patient_n = final_patient_n
    )

    output <- list(APAC_table = apac_table, diag_list = diag_list)
  } else {
    output <- apac_table
  }

  if (write) {
    if (!silent) {
      if (is.na(filename))
        filename <- "APAC_merged.csv"
      message(paste0("Writing: ", file.path(output_dir, filename)))
    }

    write.csv2(apac_table, file.path(output_dir, filename))
  }

  return(output)
}
