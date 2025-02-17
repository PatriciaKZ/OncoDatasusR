#' Get Match List for Possible Duplicates
#'
#' This function identifies potential duplicate patient records in an APAC table based on a margin of error of plus or minus 1 year in the calculated birth year.
#'
#' @importFrom magrittr %>%
#' @param apac_table DataFrame. Table of patients that will be used for deduplication, containing patient information and calculated birth year.
#' @param silent Logical. If `TRUE`, the function will suppress warning messages when a patient code appears in more than one group of matches. Default is `FALSE`.
#' @param print_time Logical. If `TRUE`, the function will display the total processing time in the console. Default is `TRUE`.
#' @return DataFrame. A matrix indicating the rows of patients with the same key that were identified as potential duplicates.
#' @details
#' - The function uses the calculated birth year (`ano_nasc_calc`) to identify potential duplicate records with a margin of error of ±1 year, as the provided age might have been recorded before or after the patient's birthday.
#' - Each identifier (row position) should ideally appear in only one row of the final match matrix. If the same identifier appears in multiple rows, manual verification and adjustment are required to prevent duplicate records during deduplication steps, such as when using the `merge_cns()` function.
#' - A warning is displayed if a record appears in multiple groups, unless `silent = TRUE`.
#' @export
#' @examples
#' \dontrun{
#' # Identify potential duplicates
#' match_list <- get_match_list(apac_table = apac_data, silent = FALSE, print_time = TRUE)
#' head(match_list)
#' }
get_match_list <- function(apac_table,
                           silent = FALSE,
                           print_time = TRUE) {
  if (print_time)
    ini.time <- Sys.time()

  # Creates a vector with unique values of the key1
  chave_1_possiveis_duplicatas = apac_table %>%
    dplyr::filter(chave1_n > 1) %>%
    dplyr::select(chave1) %>%
    dplyr::distinct(chave1) %>%
    unlist()

  # Creates an integer variable for unique record counting
  match_list = data.frame()

  for (i in 1:length(chave_1_possiveis_duplicatas)) {
    # Creates a sub-table with only the records of each key with more than one record

    subAPAC = apac_table %>% dplyr::filter(chave1 == chave_1_possiveis_duplicatas[i])

    for (j in 1:nrow(subAPAC)) {
      # Checks if the sub-table does not have only two records (to avoid repetition
      # in comparison and optimize the code performance)

      if (!(j == 2 & nrow(subAPAC) == 2)) {
        # Verifies which records meet the age criteria (calculated birth year +/- 1)
        res = (
          subAPAC$ano_nasc_calc[j] == (subAPAC$ano_nasc_calc + 1) |
            subAPAC$ano_nasc_calc[j] == subAPAC$ano_nasc_calc       |
            subAPAC$ano_nasc_calc[j] == (subAPAC$ano_nasc_calc - 1)
        )

        # If another record meeting the above criteria is found, identifies the row (position) of the record to which it was compared
        if (sum(res, na.rm = T) > 1) {
          # Selects the id of the duplicate records, arranges in ascending order, and appends to the match list
          match_list <- subAPAC$id[which(res)] %>% sort() %>% t() %>% data.frame() %>% dplyr::bind_rows(match_list)

        }

      }
    }
  }

  # Removes duplicate rows
  match_list <- dplyr::distinct(match_list)

  # Checks if any record appears in more than one group
  # (if the frequency is greater than 1, verify manually)
  frequencia_registros <- unique(table(unlist(match_list)))

  if (length(frequencia_registros) > 1) {
    warning("Record appears in more than one group. Verify manually")
  } else {
    if (!silent) {
      message("All ok: each record appears in only one group.")
    }
  }

  if (print_time) {
    proc.time <- Sys.time() - ini.time
    message(paste0("Processing time: ", proc.time))
  }

  return(match_list)
}
