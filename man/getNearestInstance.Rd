\name{getNearestInstance}
\alias{getNearestInstance}
\alias{printNearest}

\title{Nearest-neighbor instance}
\description{
	The nearest neighbors of an instance
}
\usage{
getNearestInstance(instanceId, howMany = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{instanceId}{The seed instance}
  \item{howMany}{How many neighbor instances to print}
}
\details{
Once a dataset has been loaded, each instance is represented as a fuzzy affinity vector. 
This function returns the instances with the nearest fuzzy affinity vector to the vector of the seed instance, where Euclidean distance is used as the distance measure. 
}
\value{
}
\author{Claudio Baccigalupo}
\examples{
# getNearestInstance(1)
}

\keyword{methods}
