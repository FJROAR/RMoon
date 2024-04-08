#' @title WeekDayMeeus
#'
#' @description Provides the day of the week based on the calculation of the Julian
#' day according to the Chapter 7 of Astronomical Algorithms.
#'
#' @param year,month,day  Numerical vectors that take the temporal
#' reference of one or several days
#'
#' @return List of 1 vectors containing a number by week day. 0 = Sunday, 1 = Monday,
#' 2 = Tuesday, 3 = Wednesday, 4 = Thursday, 5 = Friday and 6 = Saturday
#'
#' @examples
#'
#' library(RMoon)
#' weekDay <- WeekDayMeeus(c(1492, 1582, 1582, 1582, 1976, 2024),
#'                 c(10, 10, 10, 10, 2, 4),
#'                 c(12, 15, 12, 4, 19, 29)
#'                 )
#'
#' @references
#' Jean Meeus (1998), Astronomical Algorithms, 2nd Ed, ISBN 0-943396-61-1
#'
#' @export

WeekDayMeeus <- function(year, month, day){

  weekDay <- (JulianDayMeeus(year, month, day, 0, 0, 0)[[1]] + 1.5 ) %% 7
  weekDay <- ifelse(year == 1582 &
                      month == 10 &
                      day %in% c(5:15), 5,
                    weekDay)

  return(weekDay)

}
