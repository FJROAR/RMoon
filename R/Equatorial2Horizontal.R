#' @title Equatorial2Horizontal
#'
#' @description Conversion of Equatorial coordinates to Horizontal
#'
#' @param LocalHourAngleT Angle of the Local Hour (in hours)
#' @param DeclinationT Declination (in degrees)
#' @param LatitudeT The latitud of the observer (in degrees)
#'
#'
#' @return List of 2 vectors of degrees in horizontal coordinates:
#' Azimuth,
#' Height
#'
#' @examples
#'
#'
#' library(RMoon)
#' horizontal <- Equatorial2Horizontal(LocalHourAngleT = 4.29014,
#'                                   DeclinationT = -6.71989,
#'                                   LatitudeT = 38.92139)
#'
#' @references
#' Jean Meeus (1991), Astronomical Algorithms, ISBN 0-943396-35-2
#'
#' @export


Equatorial2Horizontal <- function(LocalHourAngleT, DeclinationT, LatitudeT){

  LocalHourAngleR <- 15 * LocalHourAngleT * 3.141592654 / 180
  DeclinationR <- DeclinationT * 3.141592654 / 180
  LatitudeR <- LatitudeT * 3.141592654 / 180

  #Azimuth

  Azimuth = atan2(sin(LocalHourAngleR), cos(LocalHourAngleR) *
                    sin(LatitudeR) - tan(DeclinationR) * cos(LatitudeR))

  Azimuth = 180 * Azimuth / 3.141592654
  Azimuth <- ifelse(Azimuth < 0, Azimuth + 360, Azimuth)

  #Height

  Height = asin(sin(LatitudeR) * sin(DeclinationR) +
                  cos(LatitudeR) * cos(DeclinationR) * cos(LocalHourAngleR))
  Height = 180 * Azimuth / 3141592654

  return(list(Azimuth, Height))

}
