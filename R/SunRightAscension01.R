#' @title SunRightAscension01
#'
#' @description Provides the Right Ascension of the Sun in the version low
#' accuracy according to Chapter 24 of Astronomical Algorithms
#'
#' @param epsilon Numerical vector which represents the obliquity of the
#' ecliptic
#' @param lambda Numerical vector which represents the apparent longitude of the Sun,
#' referred to the true equinox of the date
#'
#'
#' @return Right Ascensions of the Sun
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

  k = pi / 180

  alpha <- atan2(cos(k*epsilon) * sin(k*lambda), cos(k*lambda)) * (1/k)

  if (alpha < 0){

    alpha = 360 + alpha

    }


  return(alpha)

}
