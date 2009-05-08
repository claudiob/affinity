\name{plotAffinity2D}
\alias{plotAffinity2D}

\title{Compare affinity to 2 categories}
\description{
	A 2D plot comparing the affinity values of instances to two categories
}
\usage{
plotAffinity2D(genresId, plotCentrality = FALSE, plotOthers = FALSE, plotNames = rep(FALSE, 2), plotColors = TRUE, plotSave = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{genresId}{Categories to plot}
  \item{plotCentrality}{Whether to plot affinities or centralities}
  \item{plotOthers}{Whether to plot only instances belonging to the two categories or every instance}
  \item{plotNames}{Whether to write instance labels over the plot}
  \item{plotColors}{Whether to plot the instances in a color representing their categories}
  \item{plotSave}{Whether to save the plot to a local file}
}
\details{
	This funcion plots a 2D graph with the two categories on the axis, and a point for each instance representing its fuzzy affinity values for those two categories.
}
\value{
}
\author{Claudio Baccigalupo}
\examples{
# plotAffinity2D(1,2)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{methods}
