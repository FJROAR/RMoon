#' @title MoonBasicElements
#'
#' @description Provides some basic elements of the Moon position required in other
#' functions for computing the Moon Position according to Chapter 45 of Astronomical Algorithms
#'
#' @param juliancent Numerical vector which represents the Julian Day in centuries
#' with sufficient number of decimals (recommended at least 9)
#'
#' @return List of L (Moon's mean longitude), D (Moon's mean elongation), M (Sun's mean anomaly)
#' M_ (Moon's mean anomaly), F_ (Mean distance of the Moon from its ascending node),
#' and additonal arguments used to correct sum formulas A1, A2, A3 and E
#'
#' @examples
#'
#' library(RMoon)
#' Moonbasics <- MoonBasicElements(juliancent = -0.077221081451)
#'
#' @references
#' Jean Meeus (1991), Astronomical Algorithms, ISBN 0-943396-35-2
#'
#' @export


MoonBasicElements <- function(juliancent){

  L <- (218.3164591 +
           481267.88134236 * juliancent -
           0.0013268 * juliancent^2 +
          juliancent^3 / 538841 -
          juliancent^4 / 65194000) %% 360


  D <- (297.8502042 +
          445267.1115168 * juliancent -
          0.00163 * juliancent^2 +
          juliancent^3 / 545868 -
          juliancent^4 / 113065000) %% 360

  M <- (357.5291092 +
          35999.0502909 * juliancent -
          0.0001536 * juliancent^2 +
          juliancent^3 / 24490000) %% 360

  M_ <- (134.9634114 +
           477198.8676313 * juliancent +
           0.008997 * juliancent^2 +
           juliancent^3 / 69699 -
           juliancent^4 / 14712000) %% 360

  F_ <- (93.2720993 +
           483202.0175273 * juliancent -
           0.0034029 * juliancent^2 +
           juliancent^3 / 3526000 -
           juliancent^4 / 863310000) %% 360

  A1 <- (119.75 + 131.849 * juliancent) %% 360
  A2 <- (53.09 + 479264.29 * juliancent) %% 360
  A3 <- (313.45 + 481266.484 * juliancent) %% 360

  E <- 1 - 0.002516 * juliancent - 0.0000074 * juliancent^2
  #E_2 <- E^2

  return(list(L, D, M, M_, F_, A1, A2, A3, E))

}
