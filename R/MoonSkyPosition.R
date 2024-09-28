#' @title MoonSkyPosition
#'
#' @description Given a day and a geographical position, computes the Moon's
#' horizontal coordinates: Azimuth - Heigh plus Hour angle using the library
#' the Swiss tables provide by the library swephR
#'
#' @param year Vector of years in UT
#' @param month Vector of months in UT
#' @param day Vector of days in UT
#' @param hour Vector of hours in UT
#' @param minute Vector of minutes in UT
#' @param second Vector of seconds in UT
#' @param Long Longitude in degrees of the observer
#' @param Lat Latitud in degrees of the observer
#'
#' @return List of 3 vectors: Hour angle, Azimuth and Height in degrees
#'
#' @examples
#'
#' #epsilon and lambda were computed at 12.04.1992, 00h 00m
#' library(RMoon)
#' MoonSkyPosition(year = c(2024),
#'                 month = c(9),
#'                 day = c(27),
#'                 hour = c(2),
#'                 minute = c(0),
#'                 second = c(0),
#'                 Long = -3 + 37/60 + 40/3600,
#'                 Lat = 40 + 28/60 + 42/3600 )

#'
#' @references
#' Function created based on algorithms from Jean Meeus (1998), Astronomical Algorithms, 2nd Ed, ISBN 0-943396-61-1
#'
#' @export

MoonSkyPosition <- function(year, month, day, hour, minute, second,
                            Long,
                            Lat){

  #  year = c(2024, 2024, 2024)
  #  month = c(9, 9, 9)
  #  day = c(1, 1, 1)
  #  hour = c(19, 20, 21)
  #  minute = c(0, 0, 0)
  #  second = c(0, 0, 0)
  #  Long = -3 + 37/60 + 40/3600
  #  Lat = 40 + 28/60 + 42/3600

  library(swephR)

  n_dat = length(year)

  H <- vector("numeric", n_dat)
  Hor1 <- vector("numeric", n_dat)
  Hor2 <- vector("numeric", n_dat)

  for(i in c(1: n_dat)){

    Julday <- JulianDayMeeus(year[i], month[i], day[i],
                             hour[i], minute[i], second[i])[[2]]

    MoonBasics <- MoonBasicElements(Julday)
    MoonLongDist <- MoonGeoLongDist(MoonBasics[[1]],
                                    MoonBasics[[2]],
                                    MoonBasics[[3]],
                                    MoonBasics[[4]],
                                    MoonBasics[[5]],
                                    MoonBasics[[6]],
                                    MoonBasics[[7]],
                                    MoonBasics[[9]])

    MoonLat <- MoonGeoLat(MoonBasics[[1]],
                          MoonBasics[[2]],
                          MoonBasics[[3]],
                          MoonBasics[[4]],
                          MoonBasics[[5]],
                          MoonBasics[[6]],
                          MoonBasics[[8]],
                          MoonBasics[[9]])

    #To ecuatorian

    Ecliptica <- TrueObliqutiyEcliptic(Julday)
    EclNut <- Ecliptica[[1]] + NutationLong(Julday)

    Coordenadas <- Ecliptic2Equatorial(MoonLongDist[[1]] + NutationLong(Julday),
                                       MoonLat,
                                       Ecliptica[[1]])

    #Obtención de la hora en Greenwich


    gst <- swe_sidtime(JulianDayMeeus(year[i], month[i], day[i],
                                      hour[i], minute[i], second[i])[[1]])

    longitud_horas <- Long / 15

    lst <- gst + longitud_horas
    lst <- lst %% 24

    H[i] <- lst - Coordenadas[[1]]

    Hor1[i] <- Equatorial2Horizontal(H[i]*15,
                                     Coordenadas[[2]],
                                     Lat)[[1]]


    Hor2[i] <- Equatorial2Horizontal(H[i]*15,
                                     Coordenadas[[2]],
                                     Lat)[[2]]


  }


  return(list(H, Hor1, Hor2))

}
