#' @title MoonIlluminated
#'
#' @description Computes the fraction of Moon surface illuminated by the Sun
#' according to Chapter 48 of Astronomical Algorithms
#'
#' @param alpham Moon's right ascension in degrees
#' @param deltam Moon's declination in degrees
#' @param distm Distance Earth - Moon in Km
#' @param alphas Sun's right ascension in degrees
#' @param deltas Moon's declination in degrees
#' @param dists Distance Earth - Sun in AUs
#'
#' @return Vector of percentage of illuminated Moon's surface according to position of
#' the Moon and Sun for a date
#'
#' @examples
#'
#' #alpham, deltam, distm, alphas, deltas, dists were computed at 12.04.1992, 00h 00m

#' library(RMoon)

#' MoonSurfaceIlluminated <- MoonIlluminated(alpham  = 134.6885,
#'                                           deltam  = 13.7684,
#'                                           distm = 368410,
#'                                           alphas  = 20.6579,
#'                                           deltas = 8.6964,
#'                                           dists = 1.0024977)
#'
#' @references
#' Jean Meeus (1998), Astronomical Algorithms, 2nd Ed, ISBN 0-943396-61-1
#'
#' @export


MoonIlluminated <- function(alpham, deltam, distm, alphas, deltas, dists){

  k = pi / 180

  Illuminated = numeric(length(alpham))

  for (i in c(1: length(Illuminated))){

    cospsi = sin(deltas[i] * k) * sin(deltam[i] * k) +
      cos(deltas[i] * k) * cos(deltam[i] * k) * cos(alphas[i] * k - alpham[i] * k)

    psi = acos(cospsi)

    #cosi = -cos(psi)
    #j = acos(cosi)

    R = dists[i] * 149597869.4016

    tanj = (R * sin(psi) / (distm[i] - R * cos(psi)))
    j = atan(tanj)
    if (j < 0){j = j + pi}

    Illuminated[i] = (1 + cos(j)) / 2

  }

  return (Illuminated)

}
