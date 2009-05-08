\name{categories}
%\alias{categories}
\alias{getCategory}
%\alias{instances}
\alias{getInstances}
\alias{instancesCategories}
\alias{categoriesData}
\alias{countInstances}
\alias{getSortedCategories}
\alias{printCategories}
\alias{plotCategories}
\alias{categoriesColors}

\title{Instances and categories}
\description{
List of loaded instances and categories.
}
\usage{
getCategory(...)
getInstances(...)
countInstances(...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{\dots}{The instances and categories required}
}
\details{
Once a dataset has been loaded, instances and categories are the matrices containing the relative data.
 
\code{getCategory} returns the list of categories that instance belongs to.

\code{getInstances} returns the list of instances belonging to a category.

\code{countInstances} returns the number of instances belonging to a category.

\code{getSortedCategories} returns the categories order by decreasing \code{countInstances}.

\code{printCategories} prints the list of categories.

\code{categoriesColors} returns a different color for each category.

\code{plotCategories} plots a pie representing the number of instances for each category, with the colors returned by \code{categoriesColors}.


}
\value{
}
\author{Claudio Baccigalupo}
\examples{
# getCategory(1)     # To which category does instance #1 belong to?
# getInstances(TRUE) # Which instances belong to each category? 
}

\keyword{datasets}
