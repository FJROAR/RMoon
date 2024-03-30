#' @title MoonBasicElements
#'
#' @description Provides some basic elements of the Moon position required in other
#' functions for computing the Moon Position according to Chapter 47 of Astronomical Algorithms
#' 2nd Edition
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
#' Jean Meeus (1998), Astronomical Algorithms, 2nd Ed, ISBN 0-943396-61-1
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

    L[i] <- (218.3164477 +
            481267.88123421 * juliancent[i] -
            0.0015786 * juliancent[i]^2 +
            juliancent[i]^3 / 538841 -
            juliancent[i]^4 / 65194000) %% 360


    D[i] <- (297.8501921 +
            445267.1114034 * juliancent[i] -
            0.0018819 * juliancent[i]^2 +
            juliancent[i]^3 / 545868 -
            juliancent[i]^4 / 113065000) %% 360

    M[i] <- (357.5291092 +
            35999.0502909 * juliancent[i] -
            0.0001536 * juliancent[i]^2 +
            juliancent[i]^3 / 24490000) %% 360

    M_[i] <- (134.9633964 +
             477198.8675055 * juliancent[i] +
             0.0087414 * juliancent[i]^2 +
             juliancent[i]^3 / 69699 -
             juliancent[i]^4 / 14712000) %% 360

    F_[i] <- (93.2720950 +
             483202.0175233 * juliancent[i] -
             0.0036539 * juliancent[i]^2 +
             juliancent[i]^3 / 3526000 -
             juliancent[i]^4 / 863310000) %% 360

    A1[i] <- (119.75 + 131.849 * juliancent[i]) %% 360
    A2[i] <- (53.09 + 479264.29 * juliancent[i]) %% 360
    A3[i] <- (313.45 + 481266.484 * juliancent[i]) %% 360

    E[i] <- 1 - 0.002516 * juliancent[i] - 0.0000074 * juliancent[i]^2

  }


  return(list(L, D, M, M_, F_, A1, A2, A3, E))

}
