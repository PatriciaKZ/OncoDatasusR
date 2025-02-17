#' Auto Pack Rows
#'
#' This function automates the grouping of rows with the same name to enhance
#' data visualization in tables using the `kbl()` function from the `kableExtra` package in rMarkdown reports.
#'
#' @param x data.frame. Data table.
#' @param grouping_column char. Name of the column to be used for grouping the rows.
#' @param col.names char. Vector containing the names of the columns to be displayed in the generated markdown report.
#' @return A kableExtra object with rows grouped.
#' @details This function was designed to enhance the visualization of data quality analysis in rMarkdown reports.
#' For more details of use, see the vignettes in the package documentation.
#' This function is designed to be used with the `kbl()` function from the `kableExtra` package.
#' @importFrom dplyr select %>%
#' @importFrom kableExtra kbl pack_rows
#' @export
#' @examples
#' \dontrun{
#' data <- data.frame(Name = c('A', 'A', 'B', 'B', 'C'),
#'                    Value = c(1, 2, 3, 4, 5))
#' auto_pack_rows(data, 'Name', col.names = c('Value'))
#' }

auto_pack_rows <- function(x, grouping_column, col.names) {
  kbl_tbl <- x %>%
    select(-all_of(grouping_column)) %>%
    kbl(col.names = col.names)

  group_n <- x %>%
    select(all_of(grouping_column)) %>%
    unique() %>%
    nrow()

  for (i in 1:group_n) {
    group <- unique(x[, grouping_column])[i]
    first <- range(which(x[, grouping_column] == group))[1]
    last <- range(which(x[, grouping_column] == group))[2]
    kbl_tbl <- kbl_tbl %>%
      pack_rows(group, first, last)
  }

  return(kbl_tbl)
}
