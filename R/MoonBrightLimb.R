#' @title MoonBrightLimb
#'
#' @description Computes the fraction of Moon surface illuminated by the Sun
#' according to Chapter 48 of Astronomical Algorithms
#'
#' @param alpham Moon's right ascension in degrees
#' @param deltam Moon's declination in degrees
#' @param alphas Sun's right ascension in degrees
#' @param deltas Moon's declination in degrees
#'
#' @return Vector of the position angle of the Moon's bright limb of the midpoint
#' of the illuminated limb of the moon
#'
#' @examples
#'
#' #alpham, deltam, distm, alphas, deltas, dists were computed at 12.04.1992, 00h 00m
#' library(RMoon)
#' MoonBrightLimb <- MoonBrighLimb(alpham  = 134.6885,
#'                                deltam  = 13.7684,
#'                                alphas  = 20.6579,
#'                                deltas = 8.6964)
#'
#' @references
#' Jean Meeus (1991), Astronomical Algorithms, ISBN 0-943396-35-2
#'
#' @export


MoonBrightLimb <- function(alpham, deltam, alphas, deltas){

  k = pi / 180

  BrightLimb = numeric(length(alpham))

  for (i in c(1: length(BrightLimb))){

    BrightLimb[i] <- (1 / k) *
      (2 * pi +
         (atan2(cos(deltas[i] * k) * sin(alphas[i] * k - alpham[i] * k),
                sin(deltas[i] * k) * cos(deltam[i] * k) -
                  cos(deltas[i] * k) * sin(deltam[i] * k) * cos(alphas[i] * k - alpham[i] * k))))

    if (BrightLimb[i] > 2 * pi){BrightLimb[i] <- BrightLimb[i] - 2 * pi}

  }

  return (BrightLimb)

}
