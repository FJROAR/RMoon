#' @title MoonPhasePrediction
#'
#' @description Prediction of the principal Moon's phases: new, full, first quarter,
#' last quarter, according to Chapter 49 of Astronomical Algorithms
#'
#' @param day Date in format: "yyyy-mm-dd"
#' @param phase Choice of the Moon's phase you have to choose between: "new",
#' "full", "firstquarter", "lastquarter"
#'
#' @return List of vector of julian days and dates at UTC
#'
#' @examples
#'
#' library(RMoon)
#'
#' MoonPhase <- MoonPhasePrediction("1977-02-15", "new")
#'
#' @references
#' Jean Meeus (1998), Astronomical Algorithms, 2nd Ed, ISBN 0-943396-61-1
#'
#' @export

MoonPhasePrediction <- function(day, phase){

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


    if (phase[i] == "new"){

      k = floor((anio + partyear - 2000) * 12.3685)

    }

    if (phase[i] == "firstquarter"){

      k = floor((anio + partyear - 2000) * 12.3685) + 0.25

    }

    if (phase[i] == "full"){

      k = floor((anio + partyear - 2000) * 12.3685) + 0.5

    }

    if (phase[i] == "lastquarter"){

      k = floor((anio + partyear - 2000) * 12.3685) + 0.75

    }

    Tcent = k / 1236.85

    JDE <- 2451550.09766 +
      29.530588861 * k +
      0.00015437 * Tcent^2 -
      0.000000150 * Tcent^3 +
      0.00000000073 * Tcent^4

    ref <-JulianDay(anio,
                    as.numeric(substr(day[i], 6, 7)),
                    as.numeric(substr(day[i], 9, 10)),
                    0,0,0)[[2]]

    if(JDE < ref){

      k = k + 1;

      Tcent = k / 1236.85

      JDE <- 2451550.09766 +
        29.530588861 * k +
        0.00015437 * Tcent^2 -
        0.000000150 * Tcent^3 +
        0.00000000073 * Tcent^4

    }


    E <- 1 - 0.002516 * Tcent - 0.0000074 * Tcent^2
    M <- ((2.5534 + 29.10535670 * k -
             0.0000015 * Tcent^2 -
             0.00000011 * Tcent^3) %% 360 ) * pi / 180

    M_ <- ((201.5643 + 385.81693528 * k +
              0.0107582 * Tcent^2 +
              0.00001238 * Tcent^3 -
              0.000000058 * Tcent^4) %% 360 ) * pi / 180

    F_ <- ((160.7108 + 390.67050284 * k -
              0.0016118 * Tcent^2 -
              0.00000227 * Tcent^3 +
              0.000000011 * Tcent^4) %% 360 ) * pi / 180

    Om <- ((124.7746 - 1.56375588 * k +
              0.0020672 * Tcent^2 +
              0.00000215 * Tcent^3) %% 360 ) * pi / 180

    A1 <- ((299.77 + 0.107408 * k - 0.009173 * Tcent^2) %% 360 ) * pi / 180
    A2 <- ((251.88 + 0.016321 * k) %% 360 ) * pi / 180
    A3 <- ((251.83 + 26.651886 * k) %% 360 ) * pi / 180
    A4 <- ((349.42 + 36.412478 * k) %% 360 ) * pi / 180
    A5 <- ((84.66 + 18.206239 * k) %% 360 ) * pi / 180
    A6 <- ((141.74 + 53.303771 * k) %% 360 ) * pi / 180
    A7 <- ((207.14 + 2.453732 * k) %% 360 ) * pi / 180
    A8 <- ((154.84 + 7.306860 * k) %% 360 ) * pi / 180
    A9 <- ((34.52 + 27.261239 * k) %% 360 ) * pi / 180
    A10 <- ((207.19 + 0.121824 * k) %% 360 ) * pi / 180
    A11 <- ((291.34 + 1.844379 * k) %% 360 ) * pi / 180
    A12 <- ((161.72 + 24.198154 * k) %% 360 ) * pi / 180
    A13 <- ((239.56 + 25.513099 * k) %% 360 ) * pi / 180
    A14 <- ((331.55 + 3.592518 * k) %% 360 ) * pi / 180


    if (phase[i] == "new") {

      correction = 0.00002 * sin(4 * M_) - 0.00002 * sin(3 * M_ + M) - 0.00002 * sin(M_ - M - 2 * F_) +
        0.00003 * sin(M_ - M + 2 * F_) - 0.00003 * sin(M_ + M + 2 * F_) +
        0.00003 * sin(2 * M_ + 2 * F_) + 0.00003 * sin(M_ + M - 2 * F_) + 0.00004 * sin(3 * M) +
        0.00004 * sin(2 * M_ - 2 * F_) - 0.00007 * sin(M_ + 2 * M) -0.00017 * sin(Om) -
        0.00024 * E * sin(2 * M_ - M) + 0.00038 * E * sin(M - 2 * F_) + 0.00042 * E * sin(M + 2 * F_) -
        0.00042 * sin( 3 * M_) + 0.00056 * E * sin(2 * M_ + M) - 0.00057 * sin(M_ + 2 * F_) -
        0.00111 * sin(M_ - 2 * F_) + 0.00208 * E * E * sin(2 * M) -0.00514 * E * sin(M_ + M) +
        0.00739 * E * sin(M_ - M) + 0.01039 * sin(2 * F_) + 0.01608 * sin(2*M_) +
        0.17241 * E * sin(M) + -0.40720 * sin(M_)

    } else if ((phase[i] == "firstquarter") || (phase[i] == "lastquarter")) {

      correction = -0.00002 * sin( 3 * M_ + M) +
        0.00002 * sin(M_ - M + 2 * F_) + 0.00002 * sin(2 * M_ - 2*F_) +
        0.00003 * sin(3 * M) + 0.00003 * sin(M_ + M - 2 * F_) +
        0.00004 * sin(M_ - 2 * M) - 0.00004 * sin(M_ + M + 2 * F_) +
        0.00004*sin(2 * M_ + 2 * F_) -0.00005 * sin(M_ - M - 2 * F_) -
        0.00017 * sin(Om) + 0.00027 * E * sin(2 * M_ + M) -
        0.00028 * E * E * sin(M_ + 2 * M) + 0.00032 * E * sin(M - 2 * F_) +
        0.00032 * E * sin(M + 2*F_) - 0.00034 * E * sin(2 * M_ - M) -
        0.00040 * sin(3 * M_) - 0.00070 * sin(M_ + 2 * F_) - 0.00180 * sin(M_ - 2 * F_) +
        0.00204* E * E * sin(2 * M) + 0.00454 * E * sin(M_ - M) +
        0.00804 * sin(2 * F_) + 0.00862 * sin(2 * M_) - 0.01183 * E * sin(M_ + M) +
        0.17172 * E * sin(M) - 0.62801 * sin(M_)

      W = 0.00306 - 0.00038 * E * cos(M) +
        0.00026 * cos(M_) -
        0.00002 * cos(M_ - M) +
        0.00002 * cos(M_ + M) +
        0.00002 * cos(2 * F_)

      if (phase[i] == "firstquarter"){
        correction = correction + W
      }

      else {

        correction  = correction - W
      }


    } else if (phase[i] == "full") {

      correction = 0.00002 * sin(4*M_) - 0.00002 * sin(3 * M_ + M) -
        0.00002 * sin(M_ - M - 2 * F_) + 0.00003 * sin(M_ - M + 2 * F_) -
        0.00003 * sin(M_ + M + 2 * F_) + 0.00003 * sin(2 * M_ + 2 * F_) +
        0.00003 * sin(M_ + M - 2 * F_) + 0.00004 * sin(3 * M) + 0.00004 * sin(2 * M_ - 2 * F_) -
        0.00007 * sin(M_ + 2 * M) - 0.00017 * sin(Om) -0.00024 * E * sin(2 * M_ - M) +
        0.00038 * E * sin(M - 2 * F_) + 0.00042 * E * sin(M + 2 * F_) - 0.00042 * sin(3*M_) +
        0.00056 * E * sin(2 * M_ + M) -0.00057 * sin(M_ + 2 * F_) -
        0.00111 * sin(M_ - 2 * F_) + 0.00209* E * E * sin(2 * M) -
        0.00514 * E * sin(M_ + M) + 0.00734 * E * sin(M_ - M) + 0.01043 * sin(2 * F_) +
        0.01614 * sin(2 * M_) + 0.17302 * E * sin(M) - 0.40614 * sin(M_);
    }


    correction2 <- 0.000325 * sin(A1) + 0.000165 * sin(A2) +
      0.000164 * sin(A3) + 0.000126 * sin(A4) +
      0.000110 * sin(A5) + 0.000062 * sin(A6) +
      0.000060 * sin(A7) +0.000056 * sin(A8) +
      0.000047 * sin(A9) + 0.000042 * sin(A10) +
      0.000040 * sin(A11) + 0.000037 * sin(A12) +
      0.000035 * sin(A13) + 0.000023 * sin(A14)

    juliano[i] <- JDE + correction + correction2

    base_1970 <- (juliano[i] - 2440587.5) * 24 * 3600
    predDate[i] = as.character(as.POSIXct(as.numeric(base_1970),
                                          origin = "1970-01-01", tz = "UTC"))

  }

  return(list(juliano, predDate))

}
