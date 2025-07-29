#' @title GregoriantoMuslimDate
#'
#' @description Convert a Gregorian date in a Muslim date. This conversion has sense for
#' Gregorian date greater or equal a year = 622, month = 7 and day = 16 which is equivalent
#' Before 1582-10-15 julian dates (non gregorian) are used as convection
#'
#' @param year  Numerical value for the year
#' @param month  Numerical value for the month
#' @param day  Numerical value for the day
#'
#' @return List of 3 vectors containing the following elements in Muslim calendar:
#' (1) MuslimY (2)  MuslimM (3) MuslimD
#'
#' @examples
#'
#' library(RMoon)
#' ListofMuslimDates <- GregoriantoMuslim(c(622, 2000, 1991), c(7, 4, 8), c(16, 6, 13))
#'
#' @references
#' Jean Meeus (1998), Astronomical Algorithms, 2nd Ed, ISBN 0-943396-61-1
#'
#' @export


GregoriantoMuslim <- function(year, month, day){

  goodInteger <- function(number){

    for (i in c(1: length(number))){

      number[i] <- ifelse(number[i] < 0, as.integer(number[i]) - 1, as.integer(number[i]))

    }

    return(number)

  }

  #year = c(622, 622)
  #month = c(7, 7)
  #day = c(16, 17)

  MuslimY <- vector("numeric", length(year))
  MuslimM <- vector("numeric", length(year))
  MuslimD <- vector("numeric", length(year))

  for(i in c(1: length(year))){

    if (month[i] < 3) {

      year[i] <- year[i] - 1
      month[i] <- month[i] + 12

    }

    alpha <- goodInteger(year[i] / 100)
    beta <- 2 - alpha + goodInteger(alpha / 4)

    b <- goodInteger(365.25 * year[i]) +
      goodInteger(30.6001 * (month[i] + 1)) +
      day[i] +
      1722519 +
      beta

    c <- goodInteger((b - 122.1) / 365.25)
    d <- goodInteger(365.25 * c)
    e <- goodInteger((b - d) / 30.6001)

    #En Juliano se hace el cálculo
    year2 <- as.character(year[i])
    month2 <- ifelse(nchar(month[i]) == 1,
                     paste0("0", month[i]),
                     as.character(month[i]))
    day2 <- ifelse(nchar(day[i]) == 1,
                   paste0("0", day[i]),
                   as.character(day[i]))

    if (as.numeric(paste0(year2, month2, day2)) > 15821015){

      #Se transforma de Gregoriano a Juliano
      day[i] <- b - d - goodInteger(30.6001 * e)
      month[i] <- ifelse(e < 14,
                         e - 1,
                         e - 13)
      year[i] <- ifelse(month[i] > 2,
                        c - 4716,
                        c - 4715)


    }

    W <- ifelse(year[i] %% 4 == 0, 1, 2)
    N <- goodInteger(275 * month[i] / 9) -
      W * goodInteger((month[i] + 9) / 12) +
      day[i] - 30
    A <- year[i] - 623
    B <- goodInteger(A / 4)
    C <- A %% 4
    C1 <- 365.2501 * C
    C2 <- goodInteger(C1)
    C2 <- ifelse(C1 - C2 > 0.5, C2 + 1, C2)
    D2 <- 1461 * B + 170 + C2
    Q <- goodInteger(D2 / 10631)
    R <- D2 %% 10631
    J <- goodInteger(R /354)
    K <- R %% 354
    O <- goodInteger((11 * J + 14) / 30)
    MuslimY[i] <- 30 * Q + J + 1
    JJ <- K - O + N - 1

    if (JJ > 354){

      #Distinguish betwween leap - common
      CL <- MuslimY[i] %% 30
      DL <- (11 * CL + 3) %% 30

      if(DL < 19){
        JJ <- JJ - 354
        MuslimY[i] <- MuslimY[i] + 1
      }

      if(DL > 18){
        JJ <- JJ - 355
        MuslimY[i] <- MuslimY[i] + 1
      }

      if(JJ == 0){
        JJ <- 355
        MuslimY[i] <- MuslimY[i] - 1
      }

      S <- goodInteger((JJ - 1) / 29.5)

      MuslimM[i] = 1 + S
      MuslimD[i] = goodInteger(JJ - 29.5 * S)

    }

    if(JJ == 355){

      MuslimM[i] = 12
      MuslimD[i] = 30

    }

  }

  return(list(MuslimY, MuslimM, MuslimD))

}
