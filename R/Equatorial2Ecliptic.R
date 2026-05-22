#' @title Equatorial2Ecliptic
#'
#' @description Conversion of Equatorial coordinates to Ecliptical
#'
#' @param RA Right Ascension (in hours)
#' @param Dec Declination (in degrees)
#' @param eclipT True Obliquity of the ecliptic (corrected by aberration and nutation)
#'
#'
#' @return Ecliptical coordinates in a list of 2 vectors:
#' lambda (longitude in degrees),
#' beta (latitude in degrees)
#'
#' @examples
#'
#' library(RMoon)
#' ecliptic <- Equatorial2Ecliptic(RA = 8.8932,
#'                                 Dec = 19.1825,
#'                                 eclipT = 23.440636)
#'
#' @references
#' Jean Meeus (1998), Astronomical Algorithms, 2nd Ed, ISBN 0-943396-61-1
#'
#' @export


Equatorial2Ecliptic <- function(RA, Dec, eclipT){

  lambda = numeric(length(RA))
  beta = numeric(length(RA))

  for (i in c(1:length(RA))) {

    alphaR <- RA[i] * 15 * pi / 180
    deltaR <- Dec[i] * pi / 180
    eclipR <- eclipT[i] * pi / 180

    a_lambda <- sin(alphaR) * cos(eclipR) + tan(deltaR) * sin(eclipR)
    b_lambda <- cos(alphaR)

    lambdaR <- atan(a_lambda / b_lambda)

    betaR <- asin(
      sin(deltaR) * cos(eclipR) -
        cos(deltaR) * sin(eclipR) * sin(alphaR)
    )

    lambdaDeg <- lambdaR * 180 / pi
    betaDeg <- betaR * 180 / pi

    if (lambdaDeg < 0) {
      lambdaDeg <- lambdaDeg + 360
    }

    lambda[i] <- lambdaDeg
    beta[i] <- betaDeg
  }

  return(list(lambda, beta))
}
