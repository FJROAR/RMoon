#' @title Ecliptic2Ecuatorial
#'
#' @description Conversion of eclitical coordinates to Ecuatorial
#'
#' @param lambdaT Ecliptical or celestial longitude, measured from vernal equinnox
#' @param betaT Ecliptical or celestial latitude, positive if north, negative if south
#' @param eclipR True Obliquity of the ecliptic (corrected by aberration and nutation)
#'
#'
#' @return Ecuatorial coordinates in a vector of 7 components:
#' hours of alpha (or right ascencion),
#' minutes of alpha
#' seconds of alpha
#' sign of declination
#' degrees of delta (or declination)
#' minutes of delta
#' seconds of delta
#'
#' @examples
#'
#'
#' library(RMoon)
#' ecuatorial <- Ecliptic2Ecuatorial(lambda = 133.167265,
#'                                   beta = -3.2291226,
#'                                   epsilon = 23.440636)
#'
#' @references
#' Jean Meeus (1991), Astronomical Algorithms, ISBN 0-943396-35-2
#'
#' @export


Ecliptic2Ecuatorial <- function(lambdaT, betaT, eclipR){

  lambdaR <- lambdaT * 3.141592654 / 180
  betaR <- betaT * 3.141592654 / 180
  eclipR <- eclipR * 3.141592654 / 180

  a_alfa <- sin(lambdaR) * cos(eclipR) - tan(betaR) * sin(eclipR)
  b_alfa <- cos(lambdaR)
  tan_alfa = a_alfa / b_alfa

  a_delta <- sin(betaR) * cos(eclipR) + cos(betaR) * sin(eclipR) * sin(lambdaR)
  b_delta <- cos(asin(a_delta))
  tan_delta = a_delta / b_delta

  delta <- asin(a_delta) * 180 / 3.141592654
  alfa <- ifelse(b_alfa < 0,
                 3.141592654 + atan(tan_alfa),
                 ifelse(a_alfa < 0,
                        2 * 3.141592654 + atan(tan_alfa),
                        atan(tan_alfa)))

  alfa <- (alfa * (180 / 3.141592654)) / 15

  alfaG <- as.integer(alfa)
  aux <- alfa - alfaG
  alfaM <- 60 * aux
  aux2 <- alfaM - as.integer(alfaM)
  alfaM <- as.integer(alfaM)
  alfaS <- round(aux2 * 60, 0)


  if (delta >= 0){

    deltaG <- as.integer(delta)
    aux <- delta - deltaG
    deltaM <- 60 * aux
    aux2 <- deltaM - as.integer(deltaM)
    deltaM <- as.integer(deltaM)
    deltaS <- round(aux2 * 60, 0)
    signo = 1
  }


  if (delta < 0){

    absdelta <- abs(delta)
    deltaG <- as.integer(absdelta)
    aux <- absdelta - deltaG
    deltaM <- 60 * aux
    aux2 <- deltaM - as.integer(deltaM)
    deltaM <- as.integer(deltaM)
    deltaS <- round(aux2 * 60, 0)
    signo = -1
  }


  return(c(alfaG, alfaM, alfaS, signo, deltaG, deltaM, deltaS))

}
