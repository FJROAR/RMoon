#' @title SunRadioVectorEarth
#'
#' @description Estimation of the radio vector in UAs from Earth to Sun according to the VSOP87 theory
#'
#' @param julianmil Numerical vector which represents a Julian converted in millenium
#'
#' @return Distant Sun - Earth at that date
#'
#' @examples
#'
#' #JulianMil is computed at 12.04.1992, 00h 00m
#' library(RMoon)
#' MeanLong<- SunRadioVectorEarth(julianmil = -0.0077221081451)
#'
#' @references
#' Jean Meeus (1998), Astronomical Algorithms, 2nd Ed, ISBN 0-943396-61-1
#'
#' @export

SunRadioVectorEarth <- function(julianmil){

  df = EarthVSOP87

  df$A <- as.numeric(gsub(",", ".", df$A0))*10^9
  df$B <- as.numeric(gsub(",", ".", df$B))
  df$C <- as.numeric(gsub(",", ".", df$C))

  dfR0 <- df[which(df$serie_desc == "R0"),]
  R0 = dfR0$A * cos(dfR0$B + dfR0$C * julianmil)
  dfR1 <- df[which(df$serie_desc == "R1"),]
  R1 = dfR1$A * cos(dfR1$B + dfR1$C * julianmil)
  dfR2 <- df[which(df$serie_desc == "R2"),]
  R2 = dfR2$A * cos(dfR2$B + dfR2$C * julianmil)
  dfR3 <- df[which(df$serie_desc == "R3"),]
  R3 = dfR3$A * cos(dfR3$B + dfR3$C * julianmil)
  dfR4 <- df[which(df$serie_desc == "R4"),]
  R4 = dfR4$A * cos(dfR4$B + dfR4$C * julianmil)
  dfR5 <- df[which(df$serie_desc == "R5"),]
  R5 = dfR5$A * cos(dfR5$B + dfR5$C * julianmil)

  R = (sum(R0) +
         julianmil * sum(R1)  +
         julianmil^2 * sum(R2)  +
         julianmil^3 * sum(R3)  +
         julianmil^4 * sum(R4) +
         julianmil^4 * sum(R5)) / 10^9

  return (R)

}
