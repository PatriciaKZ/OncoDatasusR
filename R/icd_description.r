#' @title ICD Description Data
#' @description This dataset contains the description of ICD-10 (International Classification of Diseases - 10th revision) codes.
#' @format A data frame with 14233 rows and 2 variables:
#' \describe{
#'   \item{cd_cid}{Character. ICD code.}
#'   \item{ds_cid}{Character. Description of the ICD code.}
#' }
#' @source World Health Organization (WHO) - International Classification of Diseases (ICD) 10th revision. 
#' Available at <http://www2.datasus.gov.br/cid10/V2008/cid10.htm>
#' @examples
#' # Example code on how to use the dataset
#' df <- OncoDatasusR::icd_description
#' head(df)
"icd_description"