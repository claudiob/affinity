\name{getCategoryLabel}
%\alias{getCategoryLabel}
%\alias{getInstanceLabel}
%\alias{matchInstanceLabel}
%\alias{matchCategoryLabel}
\alias{getCompleteLabel}

\title{Labels}
\description{
The text labels of instances and categories
}
\usage{
getCompleteLabel(...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
	\item{...}{The instance to retrieve the text label for}
}
\details{
Once a dataset has been loaded, the actual names of the instances and categories can be retrieved with the \code{get} functions; to go back from the names to the instances and categories, use the \code{match} functions instead.

\code{getCompleteLabel} returns the name of an instance and the names of the categories that instance belongs to
}
\value{
}
\author{Claudio Baccigalupo}
\examples{
# getCategoryLabel(1) # Return the name of the first category
# matchCategoryLabel('Rock')
# getInstanceLabel(TRUE) # Return the names of every instance
# matchInstanceLabel('Madonna')
# getCompleteLabel(1)
}

\keyword{datasets}
