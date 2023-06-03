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
#' @return Vector of geocentric latitudes
#'
#' @examples
#'
#' #epsilon and lambda were computed at 12.04.1992, 00h 00m
#' library(RMoon)
#' GeoLat<- MoonGeoLat(L  = 134.29018627,
#'                     D  = 113.84230856,
#'                     M  = 97.643513610,
#'                     M_ = 5.1508393327,
#'                     F_ = 219.88972622,
#'                     A1 = 109.56847763,
#'                     A3 = 229.53,
#'                     E  = 1.0001942441)
#'
#' @references
#' Jean Meeus (1991), Astronomical Algorithms, ISBN 0-943396-35-2
#'
#' @export


MoonGeoLat <- function(L, D, M, M_, F_, A1, A3, E){

  L_original = L
  L <- L * 3.141592654 / 180
  D <- D * 3.141592654 / 180
  M <- M * 3.141592654 / 180
  M_ <- M_ * 3.141592654 / 180
  F_ <- F_ * 3.141592654 / 180
  A1 <- A1 * 3.141592654 / 180
  A3 <- A3 * 3.141592654 / 180

  E_2 = E **2

  # output vector
  beta_v = numeric(length(L))

  df = PeriodicLat

  for (i in c(1:(length(L)))){

    df$B_eccen <- ifelse(abs(df$M) == 1,
                       df$B_coeff * as.numeric(sprintf("%.9f",E[i])),
                       ifelse(abs(df$M) == 2,
                              df$B_coeff * as.numeric(sprintf("%.9f",E_2[i])),
                              df$B_coeff))

    df$B_term <- df$B_coeff *
      as.numeric(sprintf("%.9f", sin(D[i] * df$D + M[i] * df$M + M_[i] * df$M_ + F_[i] * df$F)))

    total_B_term <- sum(df$B_term)
    L_m = -2235 * sin(L[i])
    A3_m = 382 * sin(A3[i])
    A1_F = 175 * sin(A1[i] - F_[i])
    A1_F2 = 175 * sin(A1[i] + F_[i])
    L_M_ = 127 * sin(L[i] - M_[i])
    L_M_2 = -115 * sin(L[i] + M_[i])

    total_B <- total_B_term + L_m + A3_m + A1_F + A1_F2 + L_M_ + L_M_2

    beta <- total_B / 1000000
    beta_v[i] = beta
  }

  return(round(beta_v, 9))
}
