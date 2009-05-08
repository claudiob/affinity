\name{getCorrelation}
\alias{getCorrelation}

\title{Correlation among categories}
\description{
	Pearson's correlation coefficient among categories
}
\usage{
getCorrelation(...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{\dots}{Categories}
}
\details{
	Each instance has a fuzzy affinity value to each category; when these values are similar for two categories then the two categories are correlated.
	This function returns the Pearson' correlation coefficient among categories, to estimate whether a linear correlation between any two categories exist
}
\value{
}
\author{Claudio Baccigalupo}
\examples{
# getCorrelation(1:2)
}

\keyword{methods}
