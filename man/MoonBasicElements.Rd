% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MoonBasicElements.R
\name{MoonBasicElements}
\alias{MoonBasicElements}
\title{MoonBasicElements}
\usage{
MoonBasicElements(juliancent)
}
\arguments{
\item{juliancent}{Numerical vector which represents the Julian Day in centuries
with sufficient number of decimals (recommended at least 9)}
}
\value{
List of L (Moon's mean longitude), D (Moon's mean elongation), M (Sun's mean anomaly)
M_ (Moon's mean anomaly), F_ (Mean distance of the Moon from its ascending node),
and additonal arguments used to correct sum formulas A1, A2, A3 and E
}
\description{
Provides some basic elements of the Moon position required in other
functions for computing the Moon Position according to Chapter 45 of Astronomical Algorithms
}
\examples{

library(RMoon)
Moonbasics <- MoonBasicElements(juliancent = -0.077221081451)

}
\references{
Jean Meeus (1991), Astronomical Algorithms, ISBN 0-943396-35-2
}
