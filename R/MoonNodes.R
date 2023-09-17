#' @title MoonNodes
#'
#' @description Determination of the instant when the Moon Eclipse reaches one of
#' its nodes according to Chapter 51 of Astronomical Algorithms
#'
#' @param day Date in format: "yyyy-mm-dd"
#' @param node Valid values: "ascending", "descending"
#'
#' @return List of vector of: dates after date given when the Moon reach the nearest
#' next chosen node.
#' If not valid value or another values are given at node parameter, the resultant
#' position will be referred for the following ascending node
#'
#' @examples
#'
#' library(RMoon)
#'
#' MoonEclipseElements <- MoonNodes("1987-05-15", "ascending")
#'
#' @references
#' Jean Meeus (1991), Astronomical Algorithms, ISBN 0-943396-35-2
#'
#' @export

MoonNodes <- function(day, node){

  day = as.Date(day)

  juliano <- vector("numeric", length(day))
  predDate <- vector("character", length(day))


  for (i in c(1: length(day))){

    anio = as.numeric(substr(day[i], 1, 4))
    first_day <- as.Date(paste0(anio, "-01-01"))
    past_days <- as.numeric(day[i] - first_day) + 1

    if ((anio %% 4 == 0 && anio %% 100 != 0) || anio %% 400 == 0) {

      partyear = round(past_days / 366 , 2)

    } else {

      partyear = round(past_days / 365 , 2)

    }

    kincr = 0

    if(node[i] == "descending"){

      kincr = 0.5
    }


    k = floor((anio + partyear - 2000.05) * 13.4223) + kincr

    Tcent = k / 1342.23

    JDE <- 2451565.1619 +
      27.212220817 * k +
      0.0002762 * Tcent^2 +
      0.000000021 * Tcent^3 -
      0.000000000088 * Tcent^4

    ref <-JulianDay(anio,
                    as.numeric(substr(day[i], 6, 7)),
                    as.numeric(substr(day[i], 9, 10)),
                    0,0,0)[[2]]

    if(JDE < ref){

      k = k + 1;

      Tcent = k / 1342.23

      JDE <- 2451565.1619 +
        27.212220817 * k +
        0.0002762 * Tcent^2 +
        0.000000021 * Tcent^3 -
        0.000000000088 * Tcent^4

    }

    E <- 1 - 0.002516 * Tcent - 0.0000074 * Tcent^2

    D <- ((183.638 + 331.73735682 * k + 0.0014852 * Tcent^2 +
      0.00000209 * Tcent^3 - 0.000000010 * Tcent^4) %% 360 ) * pi / 180

    M <- ((17.4006 + 26.8203725 * k -
             0.0001186 * Tcent^2 -
             0.00000006 * Tcent^3) %% 360 ) * pi / 180

    M_ <- ((38.3776 + 355.52747313 * k +
              0.0123499 * Tcent^2 +
              0.000014627 * Tcent^3 -
              0.000000069 * Tcent^4) %% 360 ) * pi / 180

    Om <- ((123.9767 - 1.44098956 * k +
              0.0020608 * Tcent^2 +
              0.00000214 * Tcent^3 -
              0.000000016 * Tcent^4) %% 360 ) * pi / 180

    Omg <- ((123.9767 - 1.44098956 * k +
              0.0020608 * Tcent^2 +
              0.00000214 * Tcent^3 -
              0.000000016 * Tcent^4) %% 360 )

    V <- ((299.75 + 132.85 * Tcent - 0.009173 * Tcent^2) %% 360 ) * pi / 180

    P <- ((Omg + 272.75 - 2.3 * Tcent) %% 360 ) * pi / 180


    correction = -0.4721 * sin(M_) - 0.1649 * sin(2 * D) -
                0.0868 * sin(2*D - M_) + 0.0084 * sin(2*D + M_) -
                0.0083 * sin(2*D - M) * E -
                0.0039 * sin(2*D - M - M_) * E +
                0.0034 * sin(2*M_) -
                0.0031 * sin(2*D - 2*M_) +
                0.0030 * sin(2*D + M) * E +
                0.0028 * sin(M - M_) * E +
                0.0026 * sin(M) * E +
                0.0025 * sin(4*D) + 0.0024 * sin(D) +
                0.0022 * sin(M + M_) * E +
                0.0017 * sin(Om) + 0.0014 * sin(4*D - M_) +
                0.0005 * sin(2*D + M - M_) * E +
                0.0004 * sin(2*D - M + M_) * E -
                0.0003 * sin(2*D - 2*M) * E +
                0.0003 * sin(4*D - M) * E +
                0.0003 * sin(V) + 0.0003 * sin (P)

    juliano[i] = JDE + correction

    base_1970 <- (juliano[i] - 2440587.5) * 24 * 3600
    predDate[i] = as.character(as.POSIXct(as.numeric(base_1970),
                                          origin = "1970-01-01", tz = "UTC"))

  }

  return(list(juliano, predDate))

}
