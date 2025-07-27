#' @title MuslimtoGregorianDate
#'
#' @description Convert a Muslim date in a Gregorian date. This conversion has sense for
#' Muslim date greater or equal a year = 1, month = 1 and day = 1 which is equivalent
#' to 16 - 07 - 622. This function offers the julian day and gives information about
#' if the Muslim year used is a leap or normal year
#'
#' @param MuslimY  Numerical value for the year of a Muslin year
#' @param MuslimM  Numerical value for the month of a Muslin month
#' @param MuslimD  Numerical value for the day of a Muslin day
#'
#' @return List of 5 vectors containing the following elements in Gregorian calendar:
#' (1) year (2)  month (3) day (4) julian day (5) Information about leap or normal year
#'
#' @examples
#'
#' library(RMoon)
#' ListofGregorianDates <- MuslimtoGregorian(c(1,1421), c(1, 1), c(1, 1))
#'
#' @references
#' Jean Meeus (1998), Astronomical Algorithms, 2nd Ed, ISBN 0-943396-61-1
#'
#' @export


MuslimtoGregorian <- function(MuslimY, MuslimM, MuslimD){


  goodInteger <- function(number){

    for (i in c(1: length(number))){

      number[i] <- ifelse(number[i] < 0, as.integer(number[i]) - 1, as.integer(number[i]))

    }

    return(number)

  }

  year <- vector("numeric", length(MuslimY))
  month <- vector("numeric", length(MuslimY))
  day <- vector("numeric", length(MuslimY))
  julianD <- vector("numeric", length(MuslimY))
  leap <- vector("character", length(MuslimY))

  for (i in c(1, length(MuslimY))){

    N <- MuslimD[i] + goodInteger(29.5001 * (MuslimM[i] - 1) + 0.99)
    Q <- goodInteger(MuslimY[i] / 30)
    R <- MuslimY[i] %% 30
    A <- goodInteger((11 * R + 3) / 30)
    W <- 404 * Q + 354 * R + 208 + A
    Q1 <- goodInteger(W / 1461)
    Q2 <- W %% 1461
    G <- 621 + 4 * goodInteger(7 * Q + Q1)
    K <- goodInteger(Q2 / 365.2422)
    E <- goodInteger(365.2422 * K)
    J <- Q2 - E + N - 1
    X <- G + K

    if (J > 366 & X %% 4 == 0){
      J <- J - 366
      X <- X + 1}

    if (J > 365 & X %% 4 > 0){
      J <- J - 365
      X <- X + 1}

    JD <- goodInteger(365.25 * (X - 1)) + 1721423 + J
    alpha <- goodInteger((JD - 1867215.25) / 36534.35)
    beta <- ifelse(JD < 2299161, #15-10-1585
                   JD,
                   JD + 1 + alpha - goodInteger(alpha / 4))
    b <- beta + 1524
    c <- goodInteger((b - 122.1) / 365.25)
    d <- goodInteger(365.25 * c)
    e <- goodInteger((b - d) / 30.6001)

    day[i] <- b - d - goodInteger(30.6001 * e)
    month[i] <- ifelse(e < 14,
                       e - 1,
                       e - 13)
    year[i] <- ifelse(month[i] > 2,
                      c - 4716,
                      c - 4715)

    julianD[i] <- JD

    leap[i] <- ifelse((11 * R + 3) %% 30 > 18, "leap year", "common year")


  }

  return(list(year, month, day, julianD, leap))

}
