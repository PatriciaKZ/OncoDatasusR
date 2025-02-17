#' Summary of Frequency Table
#'
#' This function generates a summary of a frequency table based on the specified parameters.
#'
#' @importFrom magrittr %>%
#' @param df DataFrame. Input data frame containing the data.
#' @param id_pcn Character. Name of the column containing unique identifiers.
#' @param var Character vector. List of variables to be included in the summary (e.g., "sexo", "age_group", "region", "estadi").
#' @param age_col Character. Name of the column containing age data.
#' @param group_col Character (optional). Name of the column for grouping data. Default is `NULL`.
#' @param max_age Numeric. Maximum age for filtering.
#' @param min_age Numeric. Minimum age for filtering.
#' @param cod_mun Character (optional). Name of the column containing municipality codes, required if "region" is included in `var`.
#' @param age_breaks Numeric vector (optional). Break points for age groups. Required if "age_group" is included in `var`.
#' @param age_labels Character vector (optional). Labels for age groups. Required if "age_group" is included in `var`.
#' @param replace_na_with_zero Logical. Whether to replace `NA` values with "0 (0.0%)" in the resulting table. Default is `TRUE`.
#' @return A DataFrame summarizing the frequency table, including counts and proportions, grouped by specified variables.
#' @details
#' - If `var` includes "region", `cod_mun` must be provided.
#' - If `var` includes "age_group", both `age_breaks` and `age_labels` must be specified.
#' - The resulting DataFrame includes formatted counts and proportions, optionally grouped by a specified column.
#' - This is an internal function used in the 'prepare_patient_table' function and is not intended for direct use by the user.
#' @noRd
#' @examples
#' # Example usage:
#' summary <- .frequency_table_summary(
#'   df = my_data,
#'   id_pcn = "patient_id",
#'   var = c("sexo", "age_group"),
#'   age_col = "age",
#'   max_age = 80,
#'   min_age = 18,
#'   age_breaks = c(17, 29, 49, 79),
#'   age_labels = c("18-29", "30-49", "50-79"),
#'   group_col = "region"
#' )
.frequency_table_summary <- function(df,
                                     id_pcn,
                                     var = c("sexo", "age_group", "region", "estadi"),
                                     age_col,
                                     group_col = NULL,
                                     max_age,
                                     min_age,
                                     cod_mun = NULL,
                                     age_breaks = NULL,
                                     age_labels = NULL,
                                     replace_na_with_zero = TRUE) {
  # Check if `group_col` exists in the DataFrame, if provided
  if (!is.null(group_col) && !(group_col %in% names(df))) {
    stop(
      paste(
        "The column",
        group_col,
        "was not found in the DataFrame. Please check the column name."
      )
    )
  }

  # Check if `cod_mun` is present when `region` is in variables
  if ("region" %in% var && is.null(cod_mun)) {
    stop("The column 'cod_mun' must be provided when 'region' is in the variable list.")
  }

  # Check if `age_col` is present in the DataFrame
  if (!(age_col %in% names(df))) {
    stop(
      paste(
        "The column",
        age_col,
        "was not found in the DataFrame. Please check the column name."
      )
    )
  }

  # Check age parameters
  if (is.null(max_age) || is.null(min_age)) {
    stop("The 'max_age' and 'min_age' parameters must be provided.")
  }

  # Check age group parameters
  if ("age_group" %in% var &&
      (is.null(age_breaks) || is.null(age_labels))) {
    stop(
      "The 'age_breaks' and 'age_labels' parameters must be provided when 'age_group' is in the variable list."
    )
  }

  # Prepare the DataFrame with specified columns
  columns_to_select <- c(id_pcn, age_col)
  columns_to_select <- c(columns_to_select, intersect(var, c("sexo", "estadi")))

  if ("region" %in% var) {
    columns_to_select <- c(columns_to_select, cod_mun)
  }
  if (!is.null(group_col)) {
    columns_to_select <- c(columns_to_select, group_col)
  }

  # Select ufs data.frame

  ufs <- OncoDatasusR::ufs

  # Select and rename columns for processing
  df <- df[columns_to_select] %>%
    setNames(c(
      "id",
      "age",
      if ("region" %in% var)
        "cod_mun",
      if ("sexo" %in% var)
        "variable",
      if ("estadi" %in% var)
        "variable",
      if (!is.null(group_col))
        "group"
    )) %>%
    dplyr::filter(age >= min_age, age <= max_age)

  n_total <- dplyr::n_distinct(df$id)

  # If the variable is 'age_group', apply age group categorization
  if ("age_group" %in% var) {
    df <- df %>%
      dplyr::mutate(
        variable = cut(age, breaks = age_breaks, labels = age_labels), include.lowest = TRUE) %>%
        dplyr::filter(!is.na(variable))  # Remove values outside the defined range
  }

  # If the variable is 'region', apply matching with `ufs`
  if ("region" %in% var) {
    df <- df %>%
      dplyr::mutate(cod = substr(cod_mun, 1, 2)) %>%
      merge(ufs, by = "cod", all.x = TRUE) %>%
      dplyr::mutate(variable = region) %>%
      dplyr::select(-cod)
  }

  # Calculate total per group if `group_col` is provided
  if (!is.null(group_col)) {
    df_totals <- df %>%
      dplyr::group_by(group) %>%
      dplyr::summarise(total_in_group = dplyr::n_distinct(id),
                       .groups = "drop")
  }

  # Group and calculate counts and proportions within each group
  result <- df %>%
    {
      if (!is.null(group_col)) {
        dplyr::group_by(., group, variable)
      } else {
        dplyr::group_by(., variable)
      }
    } %>%
    dplyr::summarise(n = dplyr::n_distinct(id), .groups = "drop") %>%
    {
      if (!is.null(group_col)) {
        dplyr::left_join(., df_totals, by = "group")
      } else {
        dplyr::mutate(., total_in_group = n_total)
      }
    } %>%
    dplyr::mutate(p = format(round((n / total_in_group) * 100, 1), nsmall = 1), APAC = paste(n, paste0("(", p, "%)"))) %>%
    dplyr::select(-n, -p, -total_in_group) %>%
    dplyr::mutate(
      row_names = dplyr::case_when(
        "age_group" %in% var ~ "Age Group (Years)",
        "sexo" %in% var ~ "Sex",
        "region" %in% var ~ "Region",
        "estadi" %in% var ~ "Clinical Stage"
      ),
      variable = dplyr::case_when(
        "sexo" %in% var ~ factor(
          variable,
          levels = c("F", "M"),
          labels = c("Female", "Male")
        ),
        "estadi" %in% var ~ dplyr::case_when(
          variable == "0" ~ "In Situ",
          variable == "1" ~ "I",
          variable == "2" ~ "II",
          variable == "3" ~ "III",
          variable == "4" ~ "IV",
          TRUE ~ "NI"
        ),
        TRUE ~ variable
      )
    ) %>%
    tidyr::drop_na(variable)

  # If `group_col` is specified, apply `pivot_wider`
  if (!is.null(group_col)) {
    result <- result %>%
      tidyr::pivot_wider(names_from = group, values_from = APAC)

    # Replace NAs with '0 (0.0%)' if `replace_na_with_zero` is TRUE
    if (replace_na_with_zero) {
      result <- result %>%
        dplyr::mutate(across(where(is.character), ~ tidyr::replace_na(., "0 (0.0%)")))
    }
  }

  # Create a total column outside parentheses for each row
  result <- result %>%
    dplyr::mutate(row_total = rowSums(dplyr::across(
      where(is.character), ~ as.numeric(gsub(" \\(.*\\)", "", .))
    ), na.rm = TRUE))

  # Calculate row percentage relative to `row_total`
  total_sum <- sum(result$row_total, na.rm = TRUE)


  result <- result %>%
    dplyr::mutate(
      row_percentage = format(round((
        row_total / total_sum
      ) * 100, 1), nsmall = 1),
      APAC = paste0(row_total, " (", row_percentage, "%)")
    ) %>%
    dplyr::select(-row_total, -row_percentage)

  order_columns <- names(result) %>% sort()

  result_final <- result %>%
    dplyr::select(order_columns) %>%
    dplyr::relocate(row_names, .before = 1) %>%
    dplyr::relocate(variable, .after = "row_names") %>%
    dplyr::relocate(APAC, .after = dplyr::last_col()) %>%

    return(result_final)
}
