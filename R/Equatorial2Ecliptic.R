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
#'                                 Dec = 19.182,
#'                                 eclipT = 23.440636)
#'
#' @references
#' Jean Meeus (1998), Astronomical Algorithms, 2nd Ed, ISBN 0-943396-61-1
#'
#' @export


Ecliptic2Equatorial <- function(lambdaT, betaT, eclipT){

  RA = numeric(length(lambdaT))
  Declinacion = numeric(length(lambdaT))

  for (i in c(1:length(lambdaT))) {

    lambdaR <- lambdaT[i] * pi / 180
    betaR <- betaT[i] * pi / 180
    eclipR <- eclipT[i] * pi / 180

    a_alfa <- sin(lambdaR) * cos(eclipR) - tan(betaR) * sin(eclipR)
    b_alfa <- cos(lambdaR)

    tan_alfa = a_alfa / b_alfa

    a_delta <- sin(betaR) * cos(eclipR) + cos(betaR) * sin(eclipR) * sin(lambdaR)

    delta <- asin(a_delta) * 180 / pi

    alfa <- atan2(a_alfa, b_alfa)

    alfa <- (alfa * (180 / pi)) / 15

    RA[i] = alfa
    Declinacion[i] = delta

  }

  return(list(RA, Declinacion))
}
