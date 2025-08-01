% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/GregoriantoMuslimDate.R
\name{GregoriantoMuslim}
\alias{GregoriantoMuslim}
\title{GregoriantoMuslimDate}
\usage{
GregoriantoMuslim(year, month, day)
}
\arguments{
\item{year}{Numerical value for the year}

\item{month}{Numerical value for the month}

\item{day}{Numerical value for the day}
}
\value{
List of 3 vectors containing the following elements in Muslim calendar:
(1) MuslimY (2)  MuslimM (3) MuslimD
}
\description{
Convert a Gregorian date in a Muslim date. This conversion has sense for
Gregorian date greater or equal a year = 622, month = 7 and day = 16 which is equivalent
Before 1582-10-15 julian dates (non gregorian) are used as convection
}
\examples{

library(RMoon)
ListofMuslimDates <- GregoriantoMuslim(c(622, 2000, 1991), c(7, 4, 8), c(16, 6, 13))

}
\references{
Jean Meeus (1998), Astronomical Algorithms, 2nd Ed, ISBN 0-943396-61-1
}
