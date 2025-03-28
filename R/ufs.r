#' @title Federative Units Data
#' @description This dataset contains information about the Federative Units (UFs) in Brazil.
#' @format A data frame with 27 rows and five variables:
#' \describe{
#'   \item{cod}{Character. IBGE Code of the Federative Unit.}
#'   \item{nome}{Character. Description of the Federative Unit.}
#'   \item{sigla}{Character. Acronym of the Federative Unit.}
#'   \item{regiao}{Character. Region of the Federative Unit - Pt.}
#'   \item{region}{Character. Region of the Federative Unit - En.}
#' }
#' @source Adapted from the geobr package. 
#' @references Pereira, R.H.M.; Gonçalves, C.N.; Araujo, P.H.F. de; Carvalho, G.D.; Nascimento, I.; Arruda, R.A. de. (2019) geobr: an R package to easily access shapefiles of the Brazilian Institute of Geography and Statistics. GitHub repository - https://github.com/ipeaGIT/geobr.
#' @examples
#' # Example code on how to use the dataset
#' df <- OncoDatasusR::ufs
#' head(df)
"ufs"