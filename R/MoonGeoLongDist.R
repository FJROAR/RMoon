#' @title MoonGeoLongDist
#'
#' @description Provides the Geocentric Longitud and distance of the Moon
#' according to Chapter 45 of Astronomical Algorithms
#'
#' @param L Elemental parameter: Moon's mean longitude
#' @param D Elemental parameter: Mean eleongation of the Moon
#' @param M Elemental parameter: Sun's mean anomaly
#' @param M_ Elemental parameter: Moon's mean anomaly
#' @param F_ Elemental parameter: Moon's argument of latitude
#' @param A1 Elemental parameter: Adjustments by Venus perturbation
#' @param A2 Elemental parameter: Adjustments by Jupiter perturbation
#' @param E Elemental parameter: Adjustments by Earth excentricity
#'
#'
#' @return Geocentric longitude of the Moon, distance between the centers of the Moon and Earth, and equatorial parallax
#'
#' @examples
#'
#' #epsilon and lambda were computed at 12.04.1992, 00h 00m
#' library(RMoon)
#' GeoLongDist<- MoonGeoLongDist(L  = 134.29018627,
#'                               D  = 113.84230856,
#'                               M  = 97.643513610,
#'                               M_ = 5.1508393327,
#'                               F_ = 219.88972622,
#'                               A1 = 109.56847763,
#'                               A2 = 123.78322533,
#'                               E  = 1.0001942441)
#'
#' @references
#' Jean Meeus (1991), Astronomical Algorithms, ISBN 0-943396-35-2
#'
#' @export


MoonGeoLongDist <- function(L, D, M, M_, F_, A1, A2, E){

  L <- L * 3.141592654 / 180
  D <- D * 3.141592654 / 180
  M <- M * 3.141592654 / 180
  M_ <- M_ * 3.141592654 / 180
  F_ <- F_ * 3.141592654 / 180
  A1 <- A1 * 3.141592654 / 180
  A2 <- A2 * 3.141592654 / 180

  df <- fread("../data/base/seriesdat1.csv")

  df$L_eccen <- ifelse(abs(df$M) == 1,
                       df$L_coeff * E,
                       ifelse(abs(df$M) == 2,
                              df$L_coeff* E_2,
                              df$L_coeff))

  df$R_eccen <- ifelse(abs(df$M) == 1,
                       df$R_coeff * E,
                       ifelse(abs(df$M) == 2,
                              df$R_coeff* E * E,
                              df$R_coeff))

  df$L_term <- df$L_coeff *
    as.numeric(sprintf("%.9f", sin(D * df$D + M * df$M + M_ * df$M_ + F_ * df$F)))

  df$R_term <- df$R_coeff *
    as.numeric(sprintf("%.9f", cos(D * df$D + M * df$M + M_ * df$M_ + F_ * df$F)))

  total_L_term <- sum(df$L_term)
  A1_m <- 3958 * sin(A1)
  L_F = 1962 * sin(L_ - F_)
  A2_m = 318 * sin(A2)

  total_L <- total_L_term + A1_m + L_F + A2_m
  total_R <- sum(df$R_term)

  lambda <- arguments(T_)[1] + total_L/1000000
  distance <- 385000.56 + total_R / 1000
  pi <- asin(6378.14/distance) * 180 / 3.141592654

  return (c(round(lambda, 4), distance, pi))

}
