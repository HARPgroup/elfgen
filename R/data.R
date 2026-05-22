#' elfdata for NHD Watershed HUC 0208020104
#'
#' An example watershed data frame derived from \code{elfdata()} using the HUC
#' '0208020104'
#'
#' @format ## `watershed.df`
#' A data frame with 81 rows and 15 columns:
#' \describe{
#'   \item{MAF}{Mean annual flow, as estimated from NHD estimation via \code{nhdPlusTools}}
#'   \item{Q01,Q02, Q03, Q04, Q05, Q06, Q07, Q08, Q09, Q10, Q11, Q12}{Monthly estimated mean flow derived from NHD estimation via \code{nhdPlusTools}}
#'   \item{NT.TOTAL.UNIQUE}{Total number of unique taxa}
#'   \item{watershed.code}{Watershed NHD Code}
#' }
#' @source elfdata(watershed.code = '0208020104', ichthy.localpath = tempdir(), use_cache = FALSE)
"watershed.df"
