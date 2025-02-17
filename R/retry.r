#' Retry Function Execution
#'
#' This function allows you to execute an expression repeatedly until it is successfully evaluated,
#' with the ability to detect and handle errors during the process.
#'
#' @param expr Expression to be repeatedly evaluated.
#' @param isError Function to detect errors. Default checks if the error belongs to the class "try-error".
#' @param maxErrors Integer. Maximum number of attempts before aborting the process.
#' @param increaseLagstep Integer. Number of attempts before increasing the wait time to 60 seconds.
#' @param increaseLagstep2 Integer. Number of attempts before increasing the wait time to 600 seconds.
#' @param sleep Numeric. Wait time in seconds between attempts.
#' @details This function is used to handle errors that may occur during the execution of downloading DATASUS files.
#' It is primarily used in the `download_datasus()` function.
#' @return The result of the successfully evaluated expression.
#' @noRd
#' @examples
#' \dontrun{
#' # Example usage
#' result <- retry({
#'   expression_that_might_fail()
#' }, maxErrors = 5)
#' print(result)
#' }
.retry <- function(expr,
                   isError = function(x)
                     "try-error" %in% class(x),
                   maxErrors = 264,
                   increaseLagstep = 60,
                   increaseLagstep2 = 120,
                   sleep = 1) {
  attempts <- 0
  retval <- try(eval(expr))

  while (isError(retval)) {
    attempts <- attempts + 1

    if (attempts >= maxErrors) {
      msg <- sprintf("retry: too many retries [[%s]]", capture.output(str(retval)))
      flog.fatal(msg)
      stop(msg)
    } else {
      msg <- sprintf(
        "retry: error in attempt %i/%i [[%s]]",
        attempts,
        maxErrors,
        capture.output(str(retval))
      )
      flog.error(msg)
      warning(msg)
    }

    if (attempts == increaseLagstep) {
      sleep <- 60
    }
    if (attempts == increaseLagstep2) {
      sleep <- 600
    }

    if (sleep > 0) {
      Sys.sleep(sleep)
    }

    retval <- try(eval(expr))
  }

  return(retval)
}
