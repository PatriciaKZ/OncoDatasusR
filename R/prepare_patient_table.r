#' Prepare a Descriptive Table of Patient Characteristics
#'
#' Prepares a descriptive table of patient characteristics from an APAC or AIH data table.
#'
#' @importFrom magrittr %>%
#' @param df DataFrame. Table where each row represents a patient.
#' @param method Character. Identifies the database used to create the table. Acceptable values are `'APAC'` or `'AIH'`.
#' @param age_group Character. Specifies the age group division. Options are `'all'`, `'adult'`, `'young_adult'`, or `'pediatric'`. See **Details**.
#' @param split Character (optional). Variable to split the table. Options are `NULL`, `'icd'`, `'icd_group'`, `'procedure'`, or `'database'` (for AQ and AR). Default is `NULL`.
#' @param age_type Character. Type of age to be used in the table. Options are `'age_diagnosis'` or `'age_attendance'`. Default is `'age_diagnosis'`.
#' @details
#' The function returns counts and percentages of patients by age group, sex, region, and clinical staging,
#' as well as the mean and standard deviation of age for the entire population.
#'
#' **Age groups are defined as follows:**
#' - If `age_group = 'all'`: 0-17, 18-29, 30-49, 50-69, 70-99 years.
#' - If `age_group = 'adult'`: 18-29, 30-49, 50-69, 70-99 years.
#' - If `age_group = 'young_adult'`: 15-19, 20-24, 25-29, 30-34, 35-39 years.
#' - If `age_group = 'pediatric'`: 0-1, 2-5, 6-11, 12-18 years.
#'
#' In `age_type`, `'age_diagnosis'` uses calculated age at diagnosis, while `'age_attendance'` uses the age at the time of attendance registered in APAC and AIH files. Age at diagnosis is available only in APAC files.
#'
#' This function is a wrapper for internal functions `.\calculate_mean_sd()` and `.\frequency_table_summary()` used by the package.
#' @return DataFrame. A data frame containing the descriptive table of patients.
#' @export
#' @examples
#' \dontrun{
#' # Prepare a descriptive patient table with age groups 'all'
#' patient_table <- prepare_patient_table(
#'   df = apac_data,
#'   method = 'APAC',
#'   age_group = 'all',
#'   split = 'icd'
#' )
#' head(patient_table)
#' }
prepare_patient_table <- function(df,
                                  method = "APAC",
                                  age_group = "adult",
                                  age_type = "age_diagnosis",
                                  split = NULL) {
  if (!method %in% c("APAC", "AIH"))
    stop("Error: method must be 'APAC' or 'AIH'")
  if (!age_type %in% c("age_diagnosis", "age_attendance"))
    stop("Error: age_type must be 'age_diagnosis' or 'age_attendance'")
  if (!age_group %in% c("all", "adult", "young_adult", "pediatric"))
    stop("Error: age_group must be 'all', 'adult', 'young_adult', or 'pediatric'")
  if (method == "AIH" && age_type == "age_diagnosis")
    stop("Error: age_type must be 'age_attendance' for AIH")
  if (!is.null(split) &&
      !split %in% c("icd", "icd_group", "procedure", "database"))
    stop("Error: split must be NULL, 'icd', 'icd_group', 'procedure', or 'database'")
  if (!is.null(split) && split == "database" && method == "AIH")
    stop("Error: split 'database' is not available for AIH")

  # Lowercase column names to avoid conflicts with variable names
  names(df) <- tolower(names(df))

  # Age parameters of the age groups

  age_params <- list(
    all = list(
      breaks = c(-1, 17, 29, 49, 69, 99),
      labels = c("0-17", "18-29", "30-49", "50-69", "70-99"),
      min_age = 0,
      max_age = 99
    ),
    adult = list(
      breaks = c(17, 29, 49, 69, 99),
      labels = c("18-29", "30-49", "50-69", "70-99"),
      min_age = 18,
      max_age = 99
    ),
    young_adult = list(
      breaks = c(14, 19, 24, 29, 34, 39),
      labels = c("15-19", "20-24", "25-29", "30-34", "35-39"),
      min_age = 15,
      max_age = 39
    ),
    pediatric = list(
      breaks =  c(-1, 1, 5, 11, 18),
      labels = c("0-1", "2-5", "6-11", "12-18"),
      min_age = 0,
      max_age = 18
    )
  )

  # Select the age parameters according to the age group

  age_param <- age_params[[age_group]]

  # Setting the column names according to the method

  cod_paciente <- ifelse(method == "APAC", "cod_paciente", "cod_paciente")

  age_col <- ifelse(age_type == "age_diagnosis",
                    "idade_diagnostico",
                    "idade_anos")

  cod_mun <- ifelse(method == "APAC", "ap_munpcn", "munic_res")

  if (!is.null(split)) {
    if (split == "icd") {
      split <- ifelse(method == "APAC", "cid_prim", "cid")
    } else if (split == "icd_group") {
      split <- "icd_group"
    } else if (split == "procedure") {
      split <- ifelse(method == "APAC", "tratamento", "proc_rea")
    } else if (split == "database") {
      split <- "base"
    }
  }

  # Testing the existence of data with the specified maximum and minimum age
  df_nrow <- df[age_col] %>%
    setNames("age") %>%
    dplyr::filter(age >= age_param$min_age, age <= age_param$max_age) %>%
    nrow()

  if (df_nrow == 0) {
    stop("Error: There are no data with the specified maximum and minimum age.")
  }

  # Mean and standard deviation of age
  suppressWarnings({
    mean_sd <- df %>%
      .calculate_mean_sd(
        age_col = age_col,
        group_col = split,
        max_age = age_param$max_age,
        min_age = age_param$min_age
      )

    # Frequency Sex

    freq_sex <- df %>%
      .frequency_table_summary(
        id_pcn = cod_paciente,
        "sexo",
        age_col = age_col,
        group_col = split,
        max_age = age_param$max_age,
        min_age = age_param$min_age
      )

    # Frequency Region

    freq_region <- df %>%
      .frequency_table_summary(
        id_pcn = cod_paciente,
        "region",
        age_col = age_col,
        group_col = split,
        max_age = age_param$max_age,
        min_age = age_param$min_age,
        cod_mun = cod_mun
      )

    # Frequency Age Group

    freq_age_group <- df %>%
      .frequency_table_summary(
        id_pcn = cod_paciente,
        "age_group",
        age_col = age_col,
        group_col = split,
        max_age = age_param$max_age,
        min_age = age_param$min_age,
        age_breaks = age_param$breaks,
        age_labels = age_param$labels
      )

    if (method == "APAC") {
      # Frequency Clinical Stage
      freq_stage <- df %>%
        .frequency_table_summary(
          id_pcn = cod_paciente,
          "estadi",
          age_col = age_col,
          group_col = split,
          max_age = age_param$max_age,
          min_age = age_param$min_age
        )
    } else {
      freq_stage <- NULL
    }

    # Descriptive table

    patient_table <- dplyr::bind_rows(mean_sd, freq_age_group, freq_sex, freq_region, freq_stage)
  })
}
