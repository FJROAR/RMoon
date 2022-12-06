#' @title SunRightAscention01
#'
#' @description Provides the Right Ascention of the Sun in the version low
#' accurracy according to Chapter 24 of Astronomical Algorithms
#'
#' @param epsilon Numerical vector which represents the eccentricty of the
#' Earth's orbit
#' @param lambda Numerical vector which represents the apparent longitud of the Sun,
#' referred to the true equinnox of the date
#'
#'
#' @return Vector of Apparent Right Ascention of the Sun in Hour / Minutes /
#' Seconds
#'
#' @examples
#' #epsilon and lambda were computed at 12.04.1992, 00h 00m
#' Suncoord<- SunRightAscention01(epsilon = 23.440491532, lambda = 22.33957501)
#'
#' @references
#' Jean Meeus (1991), Astronomical Algorithms, ISBN 0-943396-35-2
#'
#' @export


SunRightAscention01 <- function(epsilon, lambda){

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


  return(c(alphaH, alfaM, alfaS))

}
