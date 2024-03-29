#' @title JulianDay
#'
#' @description Provides the Julian Day using 5 parameter from year up to minutes
#' (seconds can be considered using decimal in the last parameter)
#'
#' @param year,month,day,hour,minute Numerical vectors that take the temporal
#' reference of one or several days in UTM time
#'
#' @return List of 4 vectors containing the following elements:
#' (1) Julian time (2) Julian day (epoch J2000.0) (3) Julian century
#' (4) Julian Millenium
#'
#' @examples
#'
#' library(RMoon)
#' jlData<- JulianDay(year = 1992, month = 4, day = 12, hour = 0, minute = 0, second = 0)
#'
#' @references
#' Jean Meeus (1998), Astronomical Algorithms, 2nd Ed, ISBN 0-943396-61-1
#'
#' @export


JulianDay <- function(year, month, day, hour, minute, second){

  dj2000 <- 367 * year -
    as.integer(7 * (year + as.integer((month + 9)/12))/4)+
    as.integer(275 * month / 9) +
    day - 730531.5 +
    (hour + minute/60 + second/3600)/24

  jd_ <- dj2000 + 2451545

  t <- dj2000 / 36525
  tmil = (jd_ - 2451545) / 365250

  return(list(dj2000, jd_, t, tmil))

}
