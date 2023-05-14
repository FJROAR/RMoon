#' @title MoonGeoLat
#'
#' @description Provides the Geocentric Latitude of the Moon
#' according to Chapter 45 of Astronomical Algorithms
#'
#' @param L Elemental parameter: Moon's mean longitude
#' @param D Elemental parameter: Mean eleongation of the Moon
#' @param M Elemental parameter: Sun's mean anomaly
#' @param M_ Elemental parameter: Moon's mean anomaly
#' @param F_ Elemental parameter: Moon's argument of latitude
#' @param A1 Elemental parameter: Adjustments by Venus perturbation
#' @param A3 Elemental parameter: Adjustments by Jupiter perturbation
#' @param E Elemental parameter: Adjustments by Earth excentricity
#' @param E_2 Elemental parameter: Squared Adjustments by Earth excentricity
#'
#'
#' @return Geocentric latitude
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
#'                               A3 = 229.53,
#'                               E  = 1.0001942441
#'                               E_2 = 1.0001942441**2)
#'
#' @references
#' Jean Meeus (1991), Astronomical Algorithms, ISBN 0-943396-35-2
#'
#' @export


MoonGeoLat <- function(L, D, M, M_, F_, A1, A3, E, E_2){

  L_original = L
  L <- L * 3.141592654 / 180
  D <- D * 3.141592654 / 180
  M <- M * 3.141592654 / 180
  M_ <- M_ * 3.141592654 / 180
  F_ <- F_ * 3.141592654 / 180
  A1 <- A1 * 3.141592654 / 180
  A3 <- A3 * 3.141592654 / 180

  df = PeriodicLat


  df$B_eccen <- ifelse(abs(df$M) == 1,
                       df$B_coeff * as.numeric(sprintf("%.9f",E)),
                       ifelse(abs(df$M) == 2,
                              df$B_coeff * as.numeric(sprintf("%.9f",E_2)),
                              df$B_coeff))

  df$B_term <- df$B_coeff *
    as.numeric(sprintf("%.9f", sin(D * df$D + M * df$M + M_ * df$M_ + F_ * df$F)))

  total_B_term <- sum(df$B_term)
  L_m = -2235 * sin(L)
  A3_m = 382 * sin(A3)
  A1_F = 175 * sin(A1 - F_)
  A1_F2 = 175 * sin(A1 + F_)
  L_M_ = 127 * sin(L - M_)
  L_M_2 = 115 * sin(L + M_)

  total_B <- total_B_term + L_m + A3_m + A1_F + A1_F2 + L_M_ + L_M_2

  beta <- total_B / 1000000

  return(round(beta, 9))
}
