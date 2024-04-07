#' @title JulianDayMeeus
#'
#' @description Provides the Julian Day using 5 parameter from year up to minutes
#' (seconds can be considered using decimal in the last parameter). This function has
#' been tested for BCE dates and takes into account dates previous to 05-10-1582 which
#' are in Julian calendar. Note that this function gives a non-sense day between
#' 05-10-1582 and 14-10-1582 which where drop from the Gregorian calendar. Only if
#' it is required 15-10-1582, 10 days must be rested to the result.
#'
#' @param year,month,day,hour,minute,seconds  Numerical vectors that take the temporal
#' reference of one or several days in UTM time
#'
#' @return List of 3 vectors containing the following elements:
#' (1) Julian day (2)  Julian century (3) Julian millenium
#'
#' @examples
#'
#' library(RMoon)
#' jlData<- JulianDayMeeus(year = 1992, month = 4, day = 12, hour = 0, minute = 0, second = 0)
#'
#' @references
#' Jean Meeus (1998), Astronomical Algorithms, 2nd Ed, ISBN 0-943396-61-1
#'
#' @export


JulianDayMeeus <- function(year, month, day, hour, minute, second){

  goodInteger <- function(number){

    for (i in c(1: length(number))){

      number[i] <- ifelse(number[i] < 0, as.integer(number[i]) - 1, as.integer(number[i]))

    }

    return(number)

  }

  Y <- ifelse(month <= 2, year - 1, year)

  M <- ifelse(month <= 2, month + 12, month)

  D <- day + hour / 24 + minute / (24 * 60) + second / (24 * 3600)


  A <- goodInteger(Y/100)

  B <- 2 - A + goodInteger(A / 4)

  JD <- goodInteger(365.25 * (Y + 4716)) +
    goodInteger(30.6001 *(M + 1)) +
    D + B - 1524.5

  JD <- ifelse(JD < 2299161.5, goodInteger(365.25 * (Y + 4716)) +
                 goodInteger(30.6001 *(M + 1)) +
                 D - 1524.5, JD)

  t <- (JD - 2451545) / 36525
  tmil = (JD - 2451545) / 365250


  return(list(JD, t, tmil))

}
