#' @title JuliantoGregorianDate
#'
#' @description Provides the date in year, month, day, hour, minute and second from a
#' positive julian day. Only this is valid for positive julian day according to the chapter
#' 7 of Astronomical Algorithms
#'
#' @param julianday  Numerical vectors that take the temporal
#' reference of one or several days in UTM time
#'
#' @return List of 6 vectors containing the following elements in Gregorian calendar:
#' (1) year (2)  month (3) day (4) hour (5) minute (6) second
#'
#' @examples
#'
#' library(RMoon)
#' DatesYMDhms <- JuliantoGregorianDate(julianday = 2436116.31)
#'
#' @references
#' Jean Meeus (1998), Astronomical Algorithms, 2nd Ed, ISBN 0-943396-61-1
#'
#' @export


JuliantoGregorianDate <- function(julianday){

  goodInteger <- function(number){

    for (i in c(1: length(number))){

      number[i] <- ifelse(number[i] < 0, as.integer(number[i]) - 1, as.integer(number[i]))

    }

    return(number)

  }

  Z <- as.integer(julianday + 0.5)
  F_ <- julianday + 0.5 - Z

  alfa = goodInteger((Z - 1867216.25)/36524.25)

  A <- ifelse(Z < 2299161, Z, Z + 1 + alfa - goodInteger(alfa/4))
  B <- A + 1524
  C <- goodInteger((B - 122.1) / 365.25)
  D <- goodInteger(365.25 * C)
  E <- goodInteger((B - D) / 30.6001)

  DAY <- B - D - goodInteger(30.6001 * E) + F_
  month <- ifelse(E < 14, E - 1, E - 13)
  year <- ifelse(month > 2, C - 4716, C - 4715)
  day <- as.integer(DAY)
  HOUR <- 24 * (DAY - day)
  hour <- as.integer(HOUR)
  MINUTE <- 60 * (HOUR - hour)
  minute <- as.integer(MINUTE)
  SECOND <- 60 * (MINUTE - minute)
  second <- round(SECOND, 2)

  return(list(year, month, day, hour, minute, second))

}
