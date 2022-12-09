#' @title OrbitMeanLatEarth
#'
#' @description Estimation of the mean latitude of Earth's orbit according to the VSOP87 theory
#'
#' @param julianmil Numerical vector which represents a Julian converted in millenium
#'
#' @return Mean longitud of the Earth in its orbit at that date
#'
#' @examples
#'
#' #JulianMil is computed at 12.04.1992, 00h 00m
#' library(RMoon)
#' MeanLong<- OrbitMeanLatgEarth(julianmil = -0.0077221081451)
#'
#' @references
#' Jean Meeus (1991), Astronomical Algorithms, ISBN 0-943396-35-2
#'
#' @export

OrbitMeanLatEarth <- function(julianmil){

  df = EarthVSOP87

  df$A <- as.numeric(gsub(",", ".", df$A0))*10^9
  df$B <- as.numeric(gsub(",", ".", df$B))
  df$C <- as.numeric(gsub(",", ".", df$C))

  dfB0 <- df[which(df$serie_desc == "B0"),]
  B0 = dfB0$A * cos(dfB0$B + dfB0$C * julianmil)
  dfB1 <- df[which(df$serie_desc == "B1"),]
  B1 = dfB1$A * cos(dfB1$B + dfB1$C * julianmil)
  dfB2 <- df[which(df$serie_desc == "B2"),]
  B2 = dfB2$A * cos(dfB2$B + dfB2$C * julianmil)
  dfB3 <- df[which(df$serie_desc == "B3"),]
  B3 = dfB3$A * cos(dfB3$B + dfB3$C * julianmil)
  dfB4 <- df[which(df$serie_desc == "B4"),]
  B4 = dfB4$A * cos(dfB4$B + dfB4$C * julianmil)

  B = (sum(B0) +
         julianmil * sum(B1)  +
         julianmil^2 * sum(B2)  +
         julianmil^3 * sum(B3)  +
         julianmil^4 * sum(B4)) / 10^9

  return (B)

}
