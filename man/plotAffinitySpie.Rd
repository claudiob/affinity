\name{plotAffinitySpie}
\alias{plotAffinitySpie}
\alias{plotSpie}

\title{Spie affinity plot}
\description{
	A spie plot describing the affinity of one or two instances
}
\usage{
plotAffinitySpie(artistsId, genresId = getSortedCategories(1:8), plotCentrality = TRUE, plotPopularity = TRUE, plotSave = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{artistsId}{One or two instances to plot }
  \item{genresId}{Categories to plot}
  \item{plotCentrality}{Whether to plot centralities or affinities}
  \item{plotPopularity}{Whether to have sector areas depending on the popularity of each category}
  \item{plotSave}{Whether to save to graph as a local file}
}
\details{
	With one instance, the plot represents the affinity of instance to categories.
	With two instances, the plot is divided in two symmetric parts, to compare the affinity of the two instances to the same categories
}
\value{
}
\author{Claudio Baccigalupo}
\examples{
# plotAffinitySpie(1:2,1)
}

\keyword{methods}
