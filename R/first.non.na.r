#' First Non-NA Value
#'
#' This function takes a vector as input and returns the first non-NA value found in the vector.
#' If all values in the vector are NA, the function returns NA.
#'
#' @param x Vector. The vector for which the function will be applied.
#' @return The first non-NA value found in the vector, or `NA` if all values are `NA`.
#' @details
#' - This function is used to select the first non-missing value of a variable.
#' - Commonly utilized within the `merge_cns` function as a helper.
#' @noRd
#' @examples
#' # Example usage:
#' vec <- c(NA, NA, 3, 4, NA)
#' first_non_na_value <- .first.non.na(vec)
#' print(first_non_na_value) # Output: 3
.first.non.na <- function(x) {
  if (all(is.na(x))) {
    return(NA)
  } else {
    return(na.omit(x)[1])
  }
}
