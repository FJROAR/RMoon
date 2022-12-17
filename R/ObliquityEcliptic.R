#' @title ObliquityEcliptic
#'
#' @description Estimation of the obliquity of ecliptic
#'
#' @param julianCent Numerical vector which represents a Julian converted in millenium
#' @param omega Numerical vecto which represents the Sun's true geometric longitude
#'
#' @return Epsilon (Obliquity of the Ecliptic), parameter needed for the Sun's Declination and Right Ascension
#'
#' @examples
#'
#' #JulianMil is computed at 12.04.1992, 00h 00m
#' library(RMoon)
#' MeanLong<- ObliqutiyEcliptic(juliancent = -0.077221081451, omega = 274.39607359)
#'
#' @references
#' Jean Meeus (1991), Astronomical Algorithms, ISBN 0-943396-35-2
#'
#' @export

ObliqutiyEcliptic <- function(juliancent, omega){

  epsilon0 <- 23 + 26 / 60 + 21.448 / 3600 -
    46.8150 * juliancent / 3600 - (0.00059 / 3600) * juliancent^2 +
    (0.00183/ 3600) * juliancent^3

  epsilon <- epsilon0 + 0.00256 * cos(k * omega)

  return(epsilon)
}
