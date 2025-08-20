#' Prepare the APAC Table for Identifying Possible Duplicates
#'
#' This function prepares the APAC table for evaluating possible duplicates by creating a unique patient key
#' using the first 5 digits of the ZIP code, primary CID, sex, date of diagnosis, and calculated year of birth.
#' The function also provides a diagnostic table with the count of NA values for the variables used in the key if required.
#'
#' @importFrom magrittr %>%
#' @param APAC_table DataFrame. The APAC data table to be processed.
#' @param run_diagnostics Logical. Indicates whether diagnostic information will be generated. Default is `TRUE`.
#' @details
#' - The function creates a potential key for identifying possible duplicates, composed of primary CID, sex,
#' the first 5 digits of the ZIP code, date of diagnosis, and the calculated year of birth.
#' - Missing values in the variables used for the key are replaced with a unique sequence for each missing value
#' to prevent them from being considered equal.
#' @return
#' - If `run_diagnostics = TRUE`: Returns a list with two elements:
#'   - `APAC_table`: The APAC data table after creating the unique patient key.
#'   - `key_var_NA_table`: A data frame with the count of NA values for the variables used in the key.
#' - If `run_diagnostics = FALSE`: Returns the APAC table with the unique patient key.
#' @export
#' @examples
#' \dontrun{
#' # Prepare APAC table for linkage with diagnostics
#' linkage_data <- prepare_linkage(
#'   APAC_table = apac_data,
#'   run_diagnostics = TRUE
#' )
#' head(linkage_data$APAC_table)
#' head(linkage_data$key_var_NA_table)
#' }
prepare_linkage <- function(APAC_table, run_diagnostics = TRUE) {
  # Creates key1 and key2
  lista_chave1 <- c("cid_prim", "sexo", "CEP5", "data_diagnostico")
  lista_chave2 <- c(lista_chave1, "ano_nasc_calc")

  # Adjust nulls: replaces NA with a unique sequential value for each missing
  # value to prevent them from being considered equal when applying the keys

  if (run_diagnostics) {
    key_var_NA_table <- APAC_table %>%
      dplyr::summarise(
        sexo_NA = sum(is.na(sexo)),
        CEP5_NA = sum(is.na(CEP5)),
        ano_nasc_calc_NA = sum(is.na(ano_nasc_calc)),
        total = dplyr::n()
      ) %>%
      dplyr::mutate(
        sexo_NA = paste0(round((total - sexo_NA) / total * 100, 0), "%"),
        CEP5_NA = paste0(round((total - CEP5_NA) / total * 100, 0), "%"),
        ano_nasc_calc_NA = paste0(
          round(
            (total - ano_nasc_calc_NA) /
              total *
              100,
            0
          ),
          "%"
        )
      ) %>%
      dplyr::select(-total)
  }

  # All patients with missing in CEP5 will have their values for CEP5
  # replaced by CEPNA1, CEP5_NA2, etc.
  for (var in lista_chave2) {
    if (sum(is.na(APAC_table[, var])) > 0) {
      APAC_table[is.na(APAC_table[, var]), var] <- paste0(
        var,
        "NA",
        1:sum(is.na(APAC_table[, var]))
      )
    }
  }

  # Applies key1 Creates a new variable whose values will be a string
  # containing all the information from key1
  APAC_table <- APAC_table %>%
    tidyr::unite(chave1, all_of(lista_chave1), remove = F)

  # Creates a new variable with the number of rows that each key has
  APAC_table <- APAC_table %>%
    dplyr::group_by(chave1) %>%
    dplyr::mutate(chave1_n = dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(desc(chave1_n), chave1) %>%
    dplyr::mutate(id = 1:nrow(.))

  if (run_diagnostics) {
    output <- list(APAC_table = APAC_table, key_var_NA_table = key_var_NA_table)
  } else {
    output <- APAC_table
  }

  return(output)
}
