#' @title Horizontal2Equatorial
#'
#' @description Conversion of Horizontal coordinates to Equatorial
#'
#' @param AzimuthT Azimuth (in degrees)
#' @param HeightT Height (in degrees)
#' @param LatitudeT The latitude of the observer (in degrees)
#'
#' @return List of 2 vectors:
#' LocalHourAngle (in degrees)
#' Declination (in degrees)
#'
#' @examples
#'
#' library(RMoon)
#' equatorial <- Horizontal2Equatorial(
#'   AzimuthT = 185.9468,
#'   HeightT = 44.18548,
#'   LatitudeT = 38.92139)
#'
#' @references
#' Based on Jean Meeus (1998), Astronomical Algorithms, 2nd Ed, ISBN 0-943396-61-1
#'
#' @export

Horizontal2Equatorial <- function(AzimuthT, HeightT, LatitudeT){

  # Ajuste desde Stellarium
  AzimuthT <- AzimuthT - 180

  if (AzimuthT < 0) {AzimuthT <- AzimuthT + 360}

  AzimuthR <- AzimuthT * 3.141592654 / 180
  HeightR <- HeightT * 3.141592654 / 180
  LatitudeR <- LatitudeT * 3.141592654 / 180

  # Declination

  Declination = asin(
    sin(HeightR) * sin(LatitudeR) +
      cos(HeightR) * cos(LatitudeR) * cos(AzimuthR)
  )

  Declination = 180 * Declination / 3.141592654

  DeclinationR <- Declination * 3.141592654 / 180

  # Local Hour Angle

  LocalHourAngle = atan2(
    sin(AzimuthR),
    cos(AzimuthR) * sin(LatitudeR) +
      tan(HeightR) * cos(LatitudeR)
  )

  LocalHourAngle = 180 * LocalHourAngle / 3.141592654

  LocalHourAngle <- ifelse(LocalHourAngle < 0,
                           LocalHourAngle + 360,
                           LocalHourAngle)

  return(list(LocalHourAngle, Declination))

}
