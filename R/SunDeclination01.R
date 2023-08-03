#' @title SunDeclination01
#'
#' @description Provides the Declination of the Sun in the version low
#' accuracy according to Chapter 24 of Astronomical Algorithms
#'
#' @param epsilon Numerical vector which represents the Obliquity of the Ecliptic
#' @param lambda Numerical vector which represents the Sun's apparent longitude
#'
#' @return Vector of apparent Sun' declinations
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

  declination = numeric(length(lambda))

  for (i in c(1: length(declination))){

    declination[i] <- asin(sin(k*epsilon[i]) * sin(k*lambda[i])) * (1/k)

  }

  return(declination)

}
