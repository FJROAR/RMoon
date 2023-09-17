#' @title SunRightAscension01
#'
#' @description Provides the Right Ascension of the Sun in the version low
#' accuracy according to Chapter 25 of Astronomical Algorithms
#'
#' @param epsilon Numerical vector which represents the obliquity of the
#' ecliptic
#' @param lambda Numerical vector which represents the apparent longitude of the Sun,
#' referred to the true equinox of the date
#'
#'
#' @return Vector of apparent Sun' right Ascensions
#'
#' @examples
#'
#' #epsilon and lambda were computed at 12.04.1992, 00h 00m
#' library(RMoon)
#' SuncoordAsc<- SunRightAscension01(epsilon = 23.440491532, lambda = 22.33957501)
#'
#' @references
#' Jean Meeus (1998), Astronomical Algorithms, 2nd Ed, ISBN 0-943396-61-1
#'
#' @export


SunRightAscension01 <- function(epsilon, lambda){

  k = pi / 180

  alpha = numeric(length(lambda))

  for (i in c(1: length(alpha))){

    alpha[i] <- atan2(cos(k*epsilon[i]) * sin(k*lambda[i]), cos(k*lambda[i])) * (1/k)

    if (alpha[i] < 0){

      alpha[i] = 360 + alpha[i]

    }

  }




  return(alpha)

}
