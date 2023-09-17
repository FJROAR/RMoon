#' @title Ecliptic2Equatorial
#'
#' @description Conversion of ecliptical coordinates to Equatorial
#'
#' @param lambdaT Ecliptical or celestial longitude, measured from vernal equinnox
#' @param betaT Ecliptical or celestial latitude, positive if north, negative if south
#' @param eclipT True Obliquity of the ecliptic (corrected by aberration and nutation)
#'
#'
#' @return Equatorial coordinates in a list of 2 vectors:
#' RA or right ascension (in hours),
#' Declination (in degrees)
#'
#' @examples
#'
#'
#' library(RMoon)
#' equatorial <- Ecliptic2Equatorial(lambdaT = 133.162655,
#'                                   betaT = -3.229126,
#'                                   eclipT = 23.440636)
#'
#' @references
#' Jean Meeus (1998), Astronomical Algorithms, 2nd Ed, ISBN 0-943396-61-1
#'
#' @export


Ecliptic2Equatorial <- function(lambdaT, betaT, eclipT){

  RA = numeric(length(lambdaT))
  Declinacion = numeric(length(lambdaT))

  for (i in c(1: length(lambdaT)))

  {

    lambdaR <- lambdaT[i] * pi / 180
    betaR <- betaT[i] * pi / 180
    eclipR <- eclipT[i] * pi / 180

    a_alfa <- sin(lambdaR) * cos(eclipR) - tan(betaR) * sin(eclipR)
    b_alfa <- cos(lambdaR)
    tan_alfa = a_alfa / b_alfa

    a_delta <- sin(betaR) * cos(eclipR) + cos(betaR) * sin(eclipR) * sin(lambdaR)

    delta <- asin(a_delta) * 180 / pi

    alfa <- ifelse(b_alfa < 0,
                   pi + atan(tan_alfa),
                   ifelse(a_alfa < 0,
                          2 * pi + atan(tan_alfa),
                          atan(tan_alfa)))

    alfa <- (alfa * (180 / pi)) / 15

    RA[i] = alfa
    Declinacion[i] = delta

  }

  return(list(RA, Declinacion))

}
