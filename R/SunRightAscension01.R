#' @title SunRightAscension01
#'
#' @description Provides the Right Ascension of the Sun in the version low
#' accuracy according to Chapter 24 of Astronomical Algorithms
#'
#' @param epsilon Numerical vector which represents the eccentricity of the
#' Earth's orbit
#' @param lambda Numerical vector which represents the apparent longitude of the Sun,
#' referred to the true equinox of the date
#'
#'
#' @return List of Hours, Minutes and Seconds of apparent right ascensions of the Sun
#'
#' @examples
#'
#' #epsilon and lambda were computed at 12.04.1992, 00h 00m
#' library(RMoon)
#' SuncoordAsc<- SunRightAscension01(epsilon = 23.440491532, lambda = 22.33957501)
#'
#' @references
#' Jean Meeus (1991), Astronomical Algorithms, ISBN 0-943396-35-2
#'
#' @export


SunRightAscension01 <- function(epsilon, lambda){

  k = 3.141592654 / 180

  alpha_ <- atan2(cos(k*epsilon) * sin(k*lambda), cos(k*lambda)) * (1/k)

  if (alpha_ >= 0){

    alfaH <- alpha_ / 15
    alphaH <- as.integer(alfaH)
    aux <- alfaH - alphaH
    alfaM <- 60 * aux
    aux2 <- alfaM - as.integer(alfaM)
    alfaM <- as.integer(alfaM)
    alfaS <- round(aux2 * 60, 0)
  }


  if (alpha_ < 0){

    alpha_ = 360 + alpha_
    alfaH <- alpha_ / 15
    alphaH <- as.integer(alfaH)
    aux <- alfaH - alphaH
    alfaM <- 60 * aux
    aux2 <- alfaM - as.integer(alfaM)
    alfaM <- as.integer(alfaM)
    alfaS <- round(aux2 * 60, 0)
  }


  return(list(alphaH, alfaM, alfaS))

}
