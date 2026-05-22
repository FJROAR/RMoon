#' @title MoonPeriApogeo
#'
#' @description Calculation about when the distance between Moon and Earth is a minimum
#' (perigeo) or a maximum (apogeo). This function has to be used at current day at gregorian
#' calendar
#'
#' @param decyear Vector of years in UT
#' @param month Vector of months in UT
#' @param day Vector of days in UT
#'
#' @return List of 2 vectors: Date when next perigeo-apogeo happens and equatorial parallax associated to them
#'
#'
#' @examples
#'
#' #epsilon and lambda were computed at 12.04.1992, 00h 00m
#' library(RMoon)
#' MoonPeriApogeo(year = c(1988),
#'                 month = c(10),
#'                 day = c(1)

#'
#' @references
#' Function created based on algorithms from Jean Meeus (1998), Astronomical Algorithms, 2nd Ed, ISBN 0-943396-61-1
#'
#' @export

MoonPeriApogeo <- function(year, month, day){

  #  year = c(1988, 2024, 2026)
  #  month = c(10, 9, 6)
  #  day = c(1, 1, 12)

  yearPart <- function(year, month, day) {

    fecha <- as.Date(sprintf("%d-%02d-%02d", year, month, day))
    inicio <- as.Date(sprintf("%d-01-01", year))
    fin    <- as.Date(sprintf("%d-12-31", year))

    dias_transcurridos <- as.numeric(fecha - inicio) + 1
    dias_totales       <- as.numeric(fin - inicio) + 1

    return(dias_transcurridos / dias_totales)
  }

  k1 = ((year + YearPart(year, month, day)) - 1999.97) * 13.2555

  nextinteger05 <- function(x) {

    x <- round(x, digits = 10)

    frac <- x - floor(x)

    is_integer05 <- (abs(frac) < 1e-9) | (abs(frac - 0.5) < 1e-9)

    go_to_5 <- frac > 0 & frac < 0.5 & !is_integer05

    go_to_integer <- frac > 0.5 & !is_integer05

    result <- floor(x)

    result[go_to_5]  <- result[go_to_5] + 0.5
    result[go_to_integer] <- result[go_to_integer] + 1

    result[is_integer05] <- x[is_integer05]

    return(result)
  }

  k <- nextinteger05(k1)

  jde <- 2451534.6698 + 27.55454989 * k -
    0.0006691 * t^2 -
    0.000001098 * t^3 +
    0.0000000052 * t^4












  return(list(H, Hor1, Hor2))

}
