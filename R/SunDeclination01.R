#' @title SunDeclination01
#'
#' @description Provides the Declination of the Sun in the version low
#' accuracy according to Chapter 24 of Astronomical Algorithms
#'
#' @param epsilon Numerical vector which represents the Obliquity of the Ecliptic
#' @param lambda Numerical vector which represents the Sun's apparent longitude
#'
#' @return Apparent declination of the Sun
#'
#' @examples
#'
#' #epsilon and lambda were computed at 12.04.1992, 00h 00m
#' library(RMoon)
#' SuncoordDec<- SunDeclination01(epsilon = 23.440491532, lambda = 22.33957501)
#'
#' @references
#' Jean Meeus (1991), Astronomical Algorithms, ISBN 0-943396-35-2
#'
#' @export


SunDeclination01 <- function(epsilon, lambda){

  k = pi / 180

  declination <- asin(sin(k*epsilon) * sin(k*lambda)) * (1/k)

  return(declination)

}
