#' @title MoonBasicElements
#'
#' @description Provides some basic elements of the Moon position required in other
#' functions for computing the Moon Position according to Chapter 45 of Astronomical Algorithms
#'
#' @param juliancent Numerical vector which represents the Julian Day in centuries
#' with sufficient number of decimals (recommended at least 9)
#'
#' @return List of 3 vectors with the following elements:
#'
#' L (Moon's mean longitude), D (Moon's mean elongation), M (Sun's mean anomaly)
#' M' (Moon's mean anomaly), F' (Mean distance of the Moon from its ascending node),
#' and additonal arguments used to correct sum formulas A1, A2, A3 and E (additional
#' adjustment)
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

  L = numeric(length(juliancent))
  D = numeric(length(juliancent))
  M = numeric(length(juliancent))
  M_ = numeric(length(juliancent))
  F_ = numeric(length(juliancent))
  A1 = numeric(length(juliancent))
  A2 = numeric(length(juliancent))
  A3 = numeric(length(juliancent))
  E = numeric(length(juliancent))

  for (i in c(1: length(juliancent))){

    L[i] <- (218.3164591 +
            481267.88134236 * juliancent[i] -
            0.0013268 * juliancent[i]^2 +
            juliancent[i]^3 / 538841 -
            juliancent[i]^4 / 65194000) %% 360


    D[i] <- (297.8502042 +
            445267.1115168 * juliancent[i] -
            0.00163 * juliancent[i]^2 +
            juliancent[i]^3 / 545868 -
            juliancent[i]^4 / 113065000) %% 360

    M[i] <- (357.5291092 +
            35999.0502909 * juliancent[i] -
            0.0001536 * juliancent[i]^2 +
            juliancent[i]^3 / 24490000) %% 360

    M_[i] <- (134.9634114 +
             477198.8676313 * juliancent[i] +
             0.008997 * juliancent[i]^2 +
             juliancent[i]^3 / 69699 -
             juliancent[i]^4 / 14712000) %% 360

    F_[i] <- (93.2720993 +
             483202.0175273 * juliancent[i] -
             0.0034029 * juliancent[i]^2 +
             juliancent[i]^3 / 3526000 -
             juliancent[i]^4 / 863310000) %% 360

    A1[i] <- (119.75 + 131.849 * juliancent[i]) %% 360
    A2[i] <- (53.09 + 479264.29 * juliancent[i]) %% 360
    A3[i] <- (313.45 + 481266.484 * juliancent[i]) %% 360

    E[i] <- 1 - 0.002516 * juliancent[i] - 0.0000074 * juliancent[i]^2

  }


  return(list(L, D, M, M_, F_, A1, A2, A3, E))

}
