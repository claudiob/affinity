\name{getBridgeInstances}
\alias{getBridgeInstances}

\title{Bridge Instances}
\description{
	The number of bridge instances between categories
}
\usage{
getBridgeInstances(categoriesId)
}

\arguments{
  \item{categoriesId}{The categories for which bridge instances should be printed}
}
\details{
	Bridge instances are instance whose highest centrality value is not for the category the instance belongs to.
}
\value{
}
\author{Claudio Baccigalupo}
\examples{
# getBridgeInstances(TRUE) # Returns the number of bridge instances for every category
}

\keyword{methods}
