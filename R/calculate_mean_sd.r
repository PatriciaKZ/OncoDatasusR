#' Calculate Mean and Standard Deviation
#'
#' This function calculates the mean and standard deviation of a specified column, optionally grouped by another column.
#'
#' @importFrom magrittr %>%
#' @param df DataFrame. Input data frame containing the data to be analyzed.
#' @param age_col Character. Name of the column containing age values.
#' @param max_age Numeric. Maximum age to include in the analysis. Must be specified.
#' @param min_age Numeric. Minimum age to include in the analysis. Must be specified.
#' @param group_col Character (optional). Name of the column for grouping data. If not provided, no grouping is performed.
#' @return A DataFrame containing the calculated mean and standard deviation of the specified column,
#' optionally grouped by the values in the grouping column. Includes additional formatting for display.
#' @details
#' - If `group_col` is provided, the output includes mean and standard deviation for each group.
#' - The function also computes a total summary of mean and standard deviation for all rows in the input data.
#' - This function is designed as an internal utility and is not intended for direct use outside its package context.
#' @noRd
#' @examples
#' # Example usage:
#' result <- .calculate_mean_sd(
#'   df = my_data,
#'   age_col = "age",
#'   max_age = 100,
#'   min_age = 18,
#'   group_col = "icd"
#' )
.calculate_mean_sd <- function(df, age_col, max_age, min_age, group_col = NULL) {
  # Check if the column `age_col` exists
  if (!(age_col %in% names(df))) {
    stop(
      paste(
        "The column",
        age_col,
        "was not found in the DataFrame. Please check the column name."
      )
    )
  }

  # Check the age parameters
  if (is.null(max_age) || is.null(min_age)) {
    stop("The 'max_age' and 'min_age' parameters must be provided.")
  }

  # Seleção e renomeação condicional das colunas
  columns_to_select <- c(age_col)
  if (!is.null(group_col)) {
    columns_to_select <- c(columns_to_select, group_col)
  }

  df <- df[columns_to_select] %>%
    setNames(c("age", if (!is.null(group_col))
      "group")) %>%
    dplyr::filter(age >= min_age, age <= max_age)

  # Cálculo de média e desvio padrão, com agrupamento condicional
  result <- df %>%
    {
      if (!is.null(group_col))
        dplyr::group_by(., group)
      else
        .
    } %>%
    dplyr::summarise(
      m = format(round(mean(age, na.rm = TRUE), 2), nsmall = 2),
      sd = format(round(sd(age, na.rm = TRUE), 2), nsmall = 2),
      .groups = "drop"  # Evita warnings de agrupamento
    ) %>%
    dplyr::mutate(sd = paste0("(", sd, ")"), APAC = paste(m, sd)) %>%
    dplyr::select(-m, -sd)

  # Calcular a média e o desvio padrão total (independente de `group_col`)
  total_summary <- df %>%
    dplyr::summarise(m = format(round(mean(age, na.rm = TRUE), 2), nsmall = 2),
                     sd = format(round(sd(age, na.rm = TRUE), 2), nsmall = 2)) %>%
    dplyr::mutate(sd = paste0("(", sd, ")"), APAC_total = paste(m, sd)) %>%
    dplyr::select(APAC_total)

  # If `group_col` is specified, add total column and apply `pivot_wider`
  if (!is.null(group_col)) {
    result <- result %>%
      dplyr::bind_cols(total_summary) %>%  # Adds the `APAC_total` column
      tidyr::pivot_wider(names_from = group, values_from = APAC) %>%
      dplyr::mutate(row_names = "Mean Age Years (SD)", variable = "Mean Age Years (SD)") %>%
      dplyr::rename(APAC = APAC_total)

    order_columns <- names(result) %>% sort()

    result <- result %>% dplyr::select(order_columns) %>%
      dplyr::relocate(row_names, .before = 1) %>%
      dplyr::relocate(variable, .after = "row_names") %>%
      dplyr::relocate(APAC, .after = dplyr::last_col())

  } else {
    # When `group_col` is not specified, add `row_names` and `variable`
    result <- result %>%
      dplyr::mutate(row_names = "Mean (SD)", variable = "Mean (SD)") %>%
      dplyr::relocate(row_names, .before = 1) %>%
      dplyr::relocate(variable, .after = "row_names")
  }

  return(result)
}
