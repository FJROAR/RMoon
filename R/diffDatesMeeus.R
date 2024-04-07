#' @title diffDatesMeeus
#'
#' @description This function computes the difference in Julian days, with decimals,
#' between 2 dates taking into account the restriction of the function
#' JulianDayMeeus(), this means that difference of days is computed considering
#' Julian Calendar all data before 1582-10-04 and Gregorian calendar all data after
#' 1582-10-15.
#'
#' @param Mdate1,Mdate2  Numerical matrix which need 6 columns (years, months,
#' days, hours, minutes, seconds) and the same number of rows to make the comparison
#'
#' @return A numeric vector giving a difference of days between each row of M1 and
#' M2 with decimals
#'
#' @examples
#'
#' library(RMoon)
#'
#' Mdate1 <-matrix(nrow = 2, ncol = 6)
#' Mdate2 <-matrix(nrow = 2, ncol = 6)
#'
#' Mdate1[1, ] <- c(1582, 10, 04, 0, 0, 0)
#' Mdate1[2, ] <- c(2000, 1, 1, 0, 0, 0)
#'
#' Mdate2[1, ] <- c(1582, 10, 15, 0, 0, 0)
#' Mdate2[2, ] <- c(2024, 4, 7, 0, 0, 0)
#'
#' dif_days <- DiffdaysMeeus(Mdate1, Mdate2)
#'
#' @references
#' Jean Meeus (1998), Astronomical Algorithms, 2nd Ed, ISBN 0-943396-61-1
#'
#' @export


DiffdaysMeeus <- function(Mdate1,
                          Mdate2){

  M1 <- Mdate1
  M2 <- Mdate2

  dfJ01 <- JulianDayMeeus(M1[,1], M1[,2], M1[,3], M1[,4], M1[,5], M1[,6])[[1]]
  dfJ02 <- JulianDayMeeus(M2[,1], M2[,2], M2[,3], M2[,4], M2[,5], M2[,6])[[1]]

  dfJ01 <- ifelse(M1[,1] == 1582 &
                    M1[,2] == 10 &
                    M1[,3] == 15,
                  dfJ01 - 10,
                  dfJ01)

  dfJ02 <- ifelse(M2[,1] == 1582 &
                    M2[,2] == 10 &
                    M2[,3] == 15,
                  dfJ02 - 10,
                  dfJ02)

  return(dfJ02 - dfJ01)

}
