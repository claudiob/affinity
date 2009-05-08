\name{affinities}
%\alias{affinities}
\alias{centralities}
\alias{printCentrality}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{affinities}
\description{
\code{affinities} is the matrix containing the fuzzy affinity values from instances to categories.

\code{centralities} is the matrix containing values between 0 and 100, describing the ranking of each affinity value for that category.

\code{printCentrality} prints the centrality of instace

}
\usage{
centralities(...)
printCentrality(instanceName, categoryName)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{\dots}{The instances and categories required}
\item{instanceName}{The instance whose centrality should be printed}
\item{categoryName}{The category whose centrality should be printed}
}

\value{
  The fuzzy affinity values for the instances and categories required.
}
\author{Claudio Baccigalupo}
\examples{
# centralities(1:3,1:3)
# printCentrality(1)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{methods}
