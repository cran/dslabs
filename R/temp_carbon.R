#' Global temperature anomaly and carbon emissions, 1751-2018
#' 
#' Annual mean global temperature anomaly on land, sea and combined, 1880-2018. Annual global carbon emissions, 1751-2014.
#' 
#'
#' \itemize{
#'  \item year. Year (CE).
#'  \item temp_anomaly. Global annual mean temperature anomaly in degrees Celsius relative to the 20th century mean temperature. 1880-2018. 
#'  \item land_anomaly. Annual mean temperature anomaly on land in degrees Celsius relative to the 20th century mean temperature. 1880-2018.
#'  \item ocean_anomaly. Annual mean temperature anomaly over ocean in degrees Celsius relative to the 20th century mean temperature. 1880-2018.
#'  \item carbon_emissions. Annual carbon emissions in millions of metric tons of carbon. 1751-2014.
#' }
#'
#' @docType data
#'
#' @usage temp_carbon
#'
#' @format An object of class \code{"data.frame"}. 
#'
#' @keywords datasets
#'
#' @source NOAA and Boden, T.A., G. Marland, and R.J. Andres (2017) via CDIAC
#'
#' @examples
#' head(temp_carbon)
## NOAA: https://www.ncei.noaa.gov/access/monitoring/climate-at-a-glance/global/time-series/
## CDIAC: https://cdiac.ess-dive.lbl.gov/trends/emis/tre_glob_2014.html
"temp_carbon"