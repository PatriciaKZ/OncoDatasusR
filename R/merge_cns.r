#' Merge CNS Codes to Deduplicate Patient Records
#'
#' This function deduplicates patient records that may have been registered in the system with different CNS codes, using a match list matrix that indicates which rows correspond to the same patient.
#'
#' @importFrom magrittr %>%
#' @param apac_table DataFrame. Table containing the APAC database data, including patient records.
#' @param match_list DataFrame. Matrix describing the matches between patients, where each row corresponds to a group of potential duplicates.
#' @param run_diagnostics Logical. If `TRUE`, the function will return diagnostic information about the deduplication process, including the count of CNS codes per patient. Default is `FALSE`.
#' @return
#' - If `run_diagnostics = TRUE`, returns a list containing:
#'   - `APAC`: The deduplicated patient table.
#'   - `cns_count`: A table with the count of CNS codes per patient.
#' - If `run_diagnostics = FALSE`, returns only the deduplicated patient table as a DataFrame.
#' @details
#' - The function aggregates patient information by unifying duplicate records based on the match list.
#' - Fields such as age, staging, icd, and municipality are consolidated by selecting the first non-NA value in the sequence of service dates.
#' - The match list should be carefully curated to avoid erroneous matches or duplicates.
#' - Diagnostic information can help verify the deduplication process and identify cases requiring manual review.
#' @export
#' @examples
#' \dontrun{
#' # Merge CNS codes to deduplicate patient records with diagnostics
#' deduplicated_data <- merge_cns(
#'   apac_table = apac_data,
#'   match_list = match_list,
#'   run_diagnostics = TRUE
#' )
#' head(deduplicated_data$APAC)
#' head(deduplicated_data$cns_count)
#' }
merge_cns <- function(apac_table, match_list, run_diagnostics = FALSE) {
  if (run_diagnostics) {
    match_list$n_cns <- ncol(match_list) - apply(match_list, 1, function(x)
      sum(is.na(x)))
    multiple_cns_count <- match_list %>% dplyr::count(n_cns)

    cns_count <- apac_table %>%
      dplyr::count(chave1_n) %>%
      dplyr::filter(chave1_n == 1) %>%
      dplyr::rename(n_cns = chave1_n) %>%
      dplyr::bind_rows(multiple_cns_count)

    match_list <- match_list %>% dplyr::select(-n_cns)
  }

  # Creates a table to unify the duplicate records
  apac_table_dup <- data.frame()

  for (i in 1:nrow(match_list)) {
    # Selects ids with duplicates
    not_NA <- !is.na(match_list[i, ])
    ids <- unlist(match_list[i, not_NA])
    # Selects only records of ids with duplicates and creates a subtable

    sub_APAC <- apac_table[ids, ]
    # Aggregates information about treatment types.
    # If there is only one treatment type, keeps only the name of that treatment
    # If there are treatments "Chemo/Hormone/Immuno" and "Radiotherapy" in different rows,
    # returns "Chemo/Hormone/Immuno and Radiotherapy"

    sub_APAC <- sub_APAC %>% dplyr::arrange(data_atendimento)

    sub_APAC$tratamento <- ifelse(
      length(unique(sub_APAC$tratamento)) == 1,
      unique(sub_APAC$tratamento),
      "Chemo/Hormone/Immuno and Radiotherapy"
    )

    sub_APAC$n_cns <- nrow(sub_APAC)


    # If exists, record the first non-missing age value (NA), starting from the oldest service date
    if ("idade_anos" %in% colnames(sub_APAC))
      sub_APAC$idade_anos <- .first.non.na(sub_APAC$idade_anos)

    # If exists, record the first non-missing staging value (NA), starting from the oldest service date
    if ("AQ_ESTADI" %in% colnames(sub_APAC))
      sub_APAC$AQ_ESTADI <- .first.non.na(sub_APAC$AQ_ESTADI)

    # If exists, record the first non-missing municipality value (NA), starting from the oldest service date
    if ("AP_MUNPCN" %in% colnames(sub_APAC))
      sub_APAC$AP_MUNPCN <- .first.non.na(sub_APAC$AP_MUNPCN)

    if ("AP_UFMUN" %in% colnames(sub_APAC))
      sub_APAC$AP_UFMUN <- .first.non.na(sub_APAC$AP_UFMUN)

    # .first.non.na is a internal function of package that returns the first non-NA value found in the vector

    sub_APAC$APAC_number2 = sum(sub_APAC$apac_number)
    sub_APAC$min_diag <- min(sub_APAC$ano_diagnostico, na.rm = TRUE)
    sub_APAC$all_cns <- paste(sub_APAC$cod_paciente, collapse = " ")

    apac_table_dup <- sub_APAC %>%
      # Select only the first row
      dplyr::slice(1) %>%
      # Bind with the table of unified duplicate records
      dplyr::bind_rows(apac_table_dup)
  }

  # Bind records with potential duplicate matches found
  APAC_merged <- apac_table_dup %>%
    # With records with potential duplicates where matches were not found
    dplyr::bind_rows(apac_table[-sort(unlist(match_list)), ] %>% dplyr::filter(chave1_n > 1) %>% dplyr::mutate(n_cns = 1)) %>%
    # And with unique records, according to key 1 (CID, sex, CEP5, and ref_year)
    dplyr::bind_rows(apac_table %>% dplyr::filter(chave1_n == 1) %>% dplyr::rename(n_cns = chave1_n)) %>%
    # Remove columns that will not be used
    dplyr::select(-proc)

  if (run_diagnostics) {
    output <- list(APAC = APAC_merged, cns_count = cns_count)
  } else {
    output <- APAC_merged
  }

  return(output)
}
