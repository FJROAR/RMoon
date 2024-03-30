#' @title MoonGeoLongDist
#'
#' @description Provides the Geocentric Longitude and Distance of the Moon
#' according to Chapter 47 of Astronomical Algorithms
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
#' @return List of geocentric longitudes of the Moon, distance between the centers
#' of the Moon and Earth, and equatorial parallax
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
#' Jean Meeus (1998), Astronomical Algorithms, 2nd Ed, ISBN 0-943396-61-1
#'
#' @export


MoonGeoLongDist <- function(L, D, M, M_, F_, A1, A2, E){

  L_original = L
  L <- L * 3.141592654 / 180
  D <- D * 3.141592654 / 180
  M <- M * 3.141592654 / 180
  M_ <- M_ * 3.141592654 / 180
  F_ <- F_ * 3.141592654 / 180
  A1 <- A1 * 3.141592654 / 180
  A2 <- A2 * 3.141592654 / 180

  # output vector
  lambda_v = numeric(length(L))
  distance_v = numeric(length(L))
  pi_v = numeric(length(L))

  for (i in c(1:(length(L)))){

    df = PeriodicLongDist


    R_eccen1  <- ifelse(abs(df$M) == 1,
                     E[i] * df$L_coeff,
                     ifelse(abs(df$M) == 2,
                            E[i] * E[i] * df$L_coeff,
                            df$L_coeff))

    R_eccen2  <- ifelse(abs(df$M) == 1,
                        E[i] * df$R_coeff,
                        ifelse(abs(df$M) == 2,
                               E[i] * E[i] * df$R_coeff,
                               df$R_coeff))

    L_term <- R_eccen1 *
      as.numeric(sprintf("%.9f", sin(D[i] * df$D + M[i] * df$M + M_[i] * df$M_ + F_[i] * df$F)))

    R_term <- R_eccen2 *
      as.numeric(sprintf("%.9f", cos(D[i] * df$D + M[i] * df$M + M_[i] * df$M_ + F_[i] * df$F)))

    total_L_term <- sum(L_term)
    A1_m <- 3958 * sin(A1[i])
    L_F = 1962 * sin(L[i] - F_[i])
    A2_m = 318 * sin(A2[i])

    total_L <- total_L_term + A1_m + L_F + A2_m
    total_R <- sum(R_term)

    lambda <- L_original[i] + total_L/1000000
    distance <- 385000.56 + total_R / 1000
    pi <- asin(6378.14/distance) * 180 / 3.141592654

    lambda_v[i] = lambda
    distance_v[i] = distance
    pi_v[i] = pi

  }

  return (list(lambda_v, distance_v, pi_v))
}
