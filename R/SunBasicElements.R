#' @title SunBasicElements
#'
#' @description Provides some basic elements of the Sun position required in othe
#' functioin for computing the Sun Position according to Chapter 24 of Astronomical Algorithms
#'
#' @param juliancent Numerical vector which represents the Julian Day in centuries
#' with sufficient number of decimals (recommended at least 9)
#'
#' @return List of Theta (Sun's true longitude), Omega (Sun's true geometric longitude),
#' lambda (Sun's apparent longitude) and R (Sun's radiovector)
#'
#' @examples
#'
#' #epsilon and lambda were computed at 12.04.1992, 00h 00m
#' library(RMoon)
#' Sunbasics <- SunBasicElements(juliancent = -0.077221081451)
#'
#' @references
#' Jean Meeus (1991), Astronomical Algorithms, ISBN 0-943396-35-2
#'
#' @export


SunBasicElements <- function(juliancent){

  k = 3.141592654 / 180

  L <- (280.46646 + 36000.76983 * juliancent + 0.0003032 * juliancent^2 ) %% 360
  M <- (357.52911 + 35999.05029 * juliancent + 0.0000001537 * juliancent^2) %% 360
  e <- 0.016708634 - 0.000042037 * juliancent - 0.0000001267 * juliancent^2
  C <- (1.9146 - 0.004817 * juliancent - 0.000014 * juliancent^2) * sin(k *M) +
    (0.019993 - 0.000101 * juliancent) * sin(2 * k * M) +
    0.00029 * sin(3 * k * M)

  theta <- L + C
  v <- M + C

  R <- (1.000001018 * (1 - e ^ 2)) / (1 + e * cos(k * v))

  omega = 125.04 - 1934.136 * juliancent

  lambda <- theta - 0.00569 - 0.00478 * sin(k * omega)

  return(list(theta, omega, lambda, R))

}
