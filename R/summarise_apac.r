#' Summarize APAC Records
#'
#' This function summarizes the APAC table data by patient code, including details about the diagnoses and treatments,
#' and performs diagnostic analyses to identify potential inconsistencies.
#'
#' @param apac_table Data.frame Data frame containing the APAC table to be used to summarize the information.
#' @param run_diagnostics Logical If TRUE, performs table diagnostic analyses and identifies potential inconsistencies.
#' @param age_type Character Type of age to be used in the analysis. Can be 'age_diagnosis' or 'age_attendance'. Default is 'age_diagnosis'.
#' @details
#' If the `run_diagnostics` argument is TRUE, the output will be a list where the first object is the summarized APAC table.
#' Additional elements of the list are as follows:
#' - `diagnostic_year_counts`: Counts the number of records with a diagnosis date earlier than 1998.
#' - `apac_per_patient`: Table with the frequency of APACs by patient code.
#' - `dt_table_diag`: Data frame with the count of the number of records with the same diagnosis date and year across different databases.
#' - `dt_diag_unique_patient`: Proportion of patients with unique diagnoses relative to the total (%).
#' - `dt_diag_unique_apac`: Proportion of records with unique diagnoses relative to the total (%).
#' - `n_before_cut_age`: Number of patients before filtering by age at diagnosis.
#' - `n_after_cut_age`: Number of patients after filtering by age at diagnosis.
#' - `dt_diag_error_tbl`: Table containing information about diagnosis inconsistencies.
#'
#' In `age_type`, 'age_diagnosis' uses calculated age at diagnosis, while 'age_attendance' uses the age at the time of attendance
#' registered in APAC and AIH files. Age at diagnosis is available only in APAC files.
#' @return
#' - If `run_diagnostics` is TRUE, returns a list containing the summarized APAC table and diagnostic analyses.
#' - If `run_diagnostics` is FALSE, returns the summarized APAC table.
#' @export
#' @examples
#' \dontrun{
#' # Summarize APAC data with diagnostic analyses
#' summarized_data <- summarise_apac(apac_table = apac_data, run_diagnostics = TRUE, max_age = 99)
#' head(summarized_data)
#' }
summarise_apac <- function(apac_table, run_diagnostics = TRUE) {
  ## Table with the frequency of APACs by patient code
  apac_per_patient <- apac_table %>%
    dplyr::group_by(cod_paciente) %>%
    dplyr::summarise(linhas = dplyr::n()) %>%
    dplyr::count(linhas)

  treatment_df <- apac_table %>%
    dplyr::arrange(cod_paciente) %>%
    dplyr::group_by(cod_paciente, Base, ano_diagnostico) %>%
    dplyr::summarise(n = dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(cod_paciente, ano_diagnostico) %>%
    dplyr::mutate(tratamento_ano_n = dplyr::n_distinct(Base)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      tratamento_ano = dplyr::case_when(
        tratamento_ano_n == 2 ~ "Chemo/Hormone/Immuno and Radiotherapy",
        tratamento_ano_n == 1 &
          Base == "AQ" ~ "Chemo/Hormone/Immuno",
        tratamento_ano_n == 1 & Base == "AR" ~ "Radiotherapy"
      )
    ) %>%
    dplyr::select(-tratamento_ano_n) %>%
    tidyr::pivot_wider(names_from = Base, values_from = n) %>%
    dplyr::mutate(AQ = tidyr::replace_na(AQ, 0)) %>%
    dplyr::mutate(AR = tidyr::replace_na(AR, 0)) %>%
    dplyr::group_by(cod_paciente) %>%
    dplyr::mutate(
      tratamento = ifelse(
        dplyr::n_distinct(tratamento_ano) >
          1,
        "Chemo/Hormone/Immuno and Radiotherapy",
        tratamento_ano
      )
    ) %>%
    dplyr::arrange(cod_paciente) %>%
    dplyr::ungroup()

  ## Calculates the earliest year of diagnosis and diagnosis date If
  ## the earliest year or diagnosis date is before 1998, choose the
  ## second earliest value

  # Creates a data.frame with distinct values for patient code, year
  # of diagnosis, and diagnosis date
  df_dataref <- apac_table %>%
    dplyr::select(cod_paciente, ano_diagnostico, data_diagnostico) %>%
    dplyr::arrange(cod_paciente, ano_diagnostico, data_diagnostico) %>%
    dplyr::distinct() %>%
    dplyr::group_by(cod_paciente) %>%
    dplyr::mutate(n = dplyr::n()) %>%
    dplyr::ungroup()

  # Selects only patients with more than one date and year of
  # diagnosis, and selects the second value if the year or diagnosis
  # date is from a date before 1998

  df_dataref_n2 <- df_dataref %>%
    dplyr::filter(n > 1) %>%
    dplyr::group_by(cod_paciente) %>%
    dplyr::mutate(
      ano_diagnostico_final = ifelse(
        ano_diagnostico <= 1998,
        dplyr::nth(ano_diagnostico, 2, order_by = ano_diagnostico),
        ano_diagnostico
      ),
      data_diagnostico_final = ifelse(
        ano_diagnostico <= 1998,
        dplyr::nth(data_diagnostico, 2, order_by = data_diagnostico),
        data_diagnostico
      ),
      dt_error = ifelse(ano_diagnostico <= 1998, TRUE, FALSE)
    ) %>%
    dplyr::select(-ano_diagnostico, -data_diagnostico) %>%
    dplyr::rename(ano_diagnostico = ano_diagnostico_final, data_diagnostico = data_diagnostico_final) %>%
    dplyr::slice(1)

  # Creates a final table of the selected diagnosis date and year
  # values
  df_dataref_final <- df_dataref %>%
    dplyr::filter(n == 1) %>%
    dplyr::mutate(dt_error = ifelse(ano_diagnostico <= 1998, TRUE, FALSE)) %>%
    rbind(df_dataref_n2) %>%
    dplyr::select(-n)

  ## Selects in the APAC table the row for each patient code and
  ## unifies with the earliest year and diagnosis date information
  apac_pruned <- apac_table %>%
    dplyr::select(-ano_diagnostico, -data_diagnostico) %>%
    dplyr::arrange(cod_paciente, data_atendimento) %>%
    dplyr::group_by(cod_paciente) %>%
    dplyr::mutate(apac_number = dplyr::n()) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::right_join(df_dataref_final, by = c("cod_paciente"))
  # Adds columns with treatment type information by patient and year
  # of diagnosis
  apac_new <- apac_pruned %>%
    dplyr::left_join(
      treatment_df %>%
        dplyr::select(cod_paciente, tratamento, tratamento_ano, ano_diagnostico),
      by = c("cod_paciente", "ano_diagnostico")
    ) %>%
    # Reorders the columns
    dplyr::relocate(
      cod_paciente,
      Base,
      ano_diagnostico,
      data_diagnostico,
      ano_atendimento,
      data_atendimento,
      tratamento_ano,
      tratamento
    ) %>%
    # Adds information on age in years, estimated year of birth, and
    # CEP5 (first 5 digits of the CEP) Converts age to years

    dplyr::mutate(
      idade = as.numeric(idade),
      cod_idade = as.numeric(cod_idade),
      idade_anos = dplyr::case_when(cod_idade <= 3 ~ 0, cod_idade == 4 ~ idade, cod_idade == 5 ~
                                      idade + 100),
      ano_atendimento = as.numeric(ano_atendimento),
      ano_nasc_calc = as.numeric(ano_atendimento - idade_anos),
      CEP5 = substr(cep_paciente, 1, 5)
    )
  # Calculated year of birth: as there is no date of birth, age is
  # used at the time of diagnosis and diagnosis date to calculate the
  # year of birth.  The calculated year of birth can have up to a
  # 1-year difference due to the calculation Creates a variable with
  # only the first 5 digits of the CEP


  # Counts the difference (inconsistencies) between diagnosis date
  # and diagnosis year between AQ and AR databases
  if (length(unique(apac_table$Base)) >
      1) {
    # This analysis will only run if AQ and AR databases are present
    # in the table
    dt_diag_min <- apac_table %>%
      dplyr::mutate(ano_diagnostico = as.numeric(ano_diagnostico)) %>%
      tidyr::drop_na(ano_diagnostico) %>%
      dplyr::filter(ano_diagnostico >= 1998) %>%
      dplyr::group_by(cod_paciente, Base) %>%
      dplyr::summarize(min_dt_diag = min(data_diagnostico)) %>%
      dplyr::ungroup() %>%
      dplyr::arrange(cod_paciente) %>%
      tidyr::pivot_wider(names_from = Base, values_from = min_dt_diag) %>%
      dplyr::mutate(ano_AQ = substr(AQ, 1, 4), ano_AR = substr(AR, 1, 4)) %>%
      dplyr::mutate(data_difere = AQ != AR,
                    ano_difere = ano_AQ != ano_AR)

    # Adds to the apac_new object the variables indicating whether
    # the diagnosis date and year differ between AQ and AR databases
    # for the same patient code
    apac_new <- apac_new %>%
      dplyr::left_join(dt_diag_min %>%
                         dplyr::select(cod_paciente, data_difere, ano_difere),
                       by = "cod_paciente")
  } else {
    # If only one of the databases is present, there is no way to
    # compare the diagnosis dates and years between the databases
    apac_new <- apac_new %>%
      dplyr::mutate(data_difere = FALSE, ano_difere = FALSE)
  }

  apac_new <- apac_new %>%
    # Calculates the difference between the year of service and the
    # year of diagnosis
    dplyr::mutate(
      dif = as.numeric(ano_atendimento) -
        as.numeric(ano_diagnostico),
      dif2 = ifelse(dif < 0, 0, dif),
      idade_diagnostico = idade_anos - dif2,
      idade_diagnostico = ifelse(idade_diagnostico < 0, idade_anos, idade_diagnostico)
    ) %>%
    dplyr::select(-dif, dif2)
  # If the year of service is less than the year of diagnosis, the
  # year of service will be considered as the year of diagnosis
  # Calculates the age during the year of diagnosis If the diagnosis
  # age is a negative value, the age during the first service is
  # considered This can occur due to an error in the registry of the
  # year of diagnosis

  # Checks for diagnosis date inconsistencies
  if (run_diagnostics) {
    if (length(unique(apac_table$Base)) >
        1) {
      # This analysis will only run if AQ and AR databases are
      # present in the table Creates a table counting divergences
      # between diagnosis dates between the two databases
      # dt_table_diag = dt_diag_min %>% dplyr::count(data_difere,
      # ano_difere) %>% dplyr::mutate(total = sum(n), p =
      # round(n/total*100,2))

      dt_table_diag <- apac_table %>%
        dplyr::group_by(cod_paciente) %>%
        dplyr::summarise(
          n_dt_diag = dplyr::n_distinct(data_diagnostico),
          n_apac = dplyr::n()
        ) %>%
        dplyr::mutate(n_dt_diag = ifelse(n_dt_diag >= 5, "5+", n_dt_diag)) %>%
        dplyr::group_by(n_dt_diag) %>%
        dplyr::summarise(
          n_patients = dplyr::n_distinct(cod_paciente),
          n_apac_total = sum(n_apac)
        ) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(
          p_patients = round(n_patients / sum(n_patients) *
                               100, 2),
          p_apac_total = round(n_apac_total / sum(n_apac_total) *
                                 100, 2)
        ) %>%
        dplyr::arrange(n_dt_diag)

      dt_diag_unique_patient <- dt_table_diag %>%
        dplyr::filter(n_dt_diag == 1) %>%
        dplyr::select(p_patients)
      dt_diag_unique_apac <- dt_table_diag %>%
        dplyr::filter(n_dt_diag == 1) %>%
        dplyr::select(p_apac_total)

      # unreplaced_date: amount of CNS codes with still inconsistent
      # dates (earlier or equal to 1998) after selecting the second
      # earliest value, as no consistent date was found for the
      # respective CNS.  dt_error: amount of CNS codes with
      # inconsistent dates (earlier or equal to 1998) where the
      # second earliest value was sought

      dt_diag_error_tbl <- apac_new %>%
        dplyr::mutate(unreplaced_date = ifelse(ano_diagnostico <= 1998, TRUE, FALSE)) %>%
        dplyr::count(dt_error, unreplaced_date)

      output <- list(
        APAC = apac_new,
        apac_per_patient = apac_per_patient,
        dt_table_diag = dt_table_diag,
        dt_diag_unique_patient = dt_diag_unique_patient,
        dt_diag_unique_apac = dt_diag_unique_apac,
        dt_diag_error_tbl = dt_diag_error_tbl
      )
    } else {
      output <- list(APAC = apac_new, apac_per_patient = apac_per_patient)
    }
  } else {
    output <- apac_new
  }

  return(output)
}
