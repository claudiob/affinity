\name{openstrands}
\alias{openstrands.listGenres}
\alias{openstrands.escapeXml}
\alias{openstrands.getArtistName}
\alias{openstrands.getGenreName}
\alias{openstrands.lookupArtists}
\alias{openstrands.matchArtistName}
\alias{openstrands.matchArtists}
\alias{openstrands.matchGenreName}
\alias{openstrands.subscriberId}
\title{OpenStrands functions}
\description{
  \code{openstrands.listGenres} returns the list of MyStrands genres using OpenStrands API

  \code{openstrands.escapeXml} escapes any string to call OpenStrands API with

  \code{openstrands.getArtistName} returns the name of an artist using OpenStrands API
  \code{openstrands.lookupArtists} returns the name of an artist using OpenStrands API

  \code{openstrands.getGenreName} returns the name of a genre using OpenStrands API
  \code{openstrands.matchGenreName} returns the ID of a genre given the name using OpenStrands API

  \code{openstrands.matchArtistName} returns the ID of an artist given the name using OpenStrands API
  \code{openstrands.matchArtists} returns the ID of an artist given the name using OpenStrands API

  \code{openstrands.subscriberId} prompts the user for a valid OpenStands ID


}
\usage{
openstrands.escapeXml(text)
openstrands.listGenres(genresId)
openstrands.getArtistName(...)
openstrands.lookupArtists(artistsId)
openstrands.getGenreName(...)
openstrands.matchGenreName(...)
openstrands.matchArtistName(...)
openstrands.matchArtists(artistsName = NULL)
openstrands.subscriberId()
}

\arguments{
\item{text}{The labels to match}
\item{artistsName}{The artist names to match}
\item{genresId}{The genres to lookup in OpenStrands}
\item{artistsId}{The artists to lookup in OpenStrands}
\item{artistsName}{The ID to match}

}
\details{
	To use OpenStands, an OpenStrands ID is required.
}
\value{
}
\references{http://www.mystrands.com/openstrands}
\author{Claudio Baccigalupo}
\examples{

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{methods}
