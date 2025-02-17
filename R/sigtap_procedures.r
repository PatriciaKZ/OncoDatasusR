#' @title SIGTAP Procedures Data
#' @description This dataset contains the description of the procedures code in the SIGTAP database.
#' @format A data frame with 5494 rows and 2 variables:
#' \describe{
#'   \item{key}{Character. SIGTAP procedure code}
#'   \item{description}{Character. Description of the procedure}
#'   ...
#' }
#' @source SIGTAP - Sistema de Gerenciamento da Tabela de Procedimentos, Medicamentos e OPM do SUS. Competence year/month: 2024-11.
#' Available at <https://sigtap.datasus.gov.br/tabela-unificada/app/sec/inicio.jsp>
#' @examples
#' # Example code on how to use the dataset
#' df <- OncoDatasusR::sigtap_procedures
#' head(df)
"sigtap_procedures"