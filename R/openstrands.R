require("XML")

#########  [ OPENSTRANDS NAMES LOOKUP ]  ########


# Desc: Substitute in text the XML character entities [&amp; &apos; &gt; &lt; &quot;] with the corresponding [& ' > < "]
openstrands.escapeXml <- function(text) {
	map <- c("&amp;"="&", "&apos;"="'", "&gt;"=">", "&lt;"="<", "&quot;"='"')
	patterns <- names(map) 
	for (i in seq(along=map)) {text <- gsub(pattern=patterns[i], replacement=map[i], x=text) }
	return(text)
}

openstrands.subscriberId <- function() {
	# First try and read it from "openstrandsId.txt"
	if(length(find(".mystrandsSubscriberId")) == 0) {
		suppressWarnings(try(.mystrandsSubscriberId <<- scan("openstrandsId.txt", what="character", quiet=TRUE), silent=TRUE))
	}
	# Then prompt the user
	if(length(find(".mystrandsSubscriberId")) == 0) {
		id <- readline("Enter your OpenStrands API subscriber ID: ")
		if(id != "") {
			.mystrandsSubscriberId <<- id
			.mystrandsSubscriberId
		}
	}
	else .mystrandsSubscriberId
}

openstrands.listGenres <- function(genresId) {
	genres.URL		<- paste("http://www.mystrands.com/internalservices/list/genres?num=9999&subscriberId=", openstrands.subscriberId(), sep ="")
	genres.XMLTree	<- xmlRoot(xmlTreeParse(genres.URL))
	# Watch out the next as.character as it makes Rap return before R&amp;B
	unname(sapply(genres.XMLTree[as.numeric(as.character(genresId))], function(x) openstrands.escapeXml(toString(x[["GenreName"]][[1]]))))
}

openstrands.lookupArtists <- function(artistsId = NULL) {
	# if(missing(artistsId)) stop("No artist ID provided")
	# To do: split in many calls when length(artistsId) > 100
	artistsUrl			 <- paste("http://www.mystrands.com/internalservices/lookup/artists?subscriberId=", openstrands.subscriberId(),  sep ="")
	for(artistId in artistsId) artistsUrl <- paste(artistsUrl, "&id=", artistId, sep = "")
	artistsXmlTree		 <- xmlRoot(xmlTreeParse(artistsUrl))
	matchedNames 		 <- sapply(artistsXmlTree["SimpleArtist",all=TRUE], function(x) openstrands.escapeXml(toString(x[["ArtistName"]][[1]])))
	names(matchedNames)  <- sapply(artistsXmlTree["SimpleArtist",all=TRUE], function(x) xmlAttrs(x)["ArtistId"][[1]])
	matchedNames
}

openstrands.matchArtists <- function(artistsName = NULL) {
	# if(missing(artistsName)) stop("No artist name provided")
	artistsUrl			 <- paste("http://www.mystrands.com/internalservices/match/artists?subscriberId=", openstrands.subscriberId(),  sep ="")
	for(artistName in artistsName) artistsUrl <-  paste(artistsUrl, "&name=", URLencode(artistName), sep = "")
	artistsXmlTree		 <- xmlRoot(xmlTreeParse(artistsUrl))
	matchedNames		 <- sapply(artistsXmlTree["SimpleArtist",all=TRUE], function(x) openstrands.escapeXml(toString(x[["ArtistName"]][[1]])))
	names(matchedNames)  <- sapply(artistsXmlTree["SimpleArtist",all=TRUE], function(x) xmlAttrs(x)["ArtistId"][[1]])
	# Remove bracket text from artists names (e.g., "Death" for "Death (Rap)")
	matchedNames 		 <- gsub(" ?\\(.?*\\)", "", matchedNames)
	# Skip artists with no *exact* match (e.g., "Death" for "Death (Rap)")
	matchedNames 		 <- matchedNames[which(!is.na(match(matchedNames, artistsName)))]
	matchedNames
}



#########  [ LABELS RETRIEVAL ]  ########


openstrands.getGenreName <- function(...) {
	if(length(find(".mystrandsGenresNames")) == 0) {
		.mystrandsGenresNames			<<- array("", dim = c(length(categories())))
		.mystrandsGenresNames			<<- openstrands.listGenres(categories())
		names(.mystrandsGenresNames)	<<- categories()
	}
	.mystrandsGenresNames[c(...)]
}

# Desc: Return a genre given its 'label'
openstrands.matchGenreName <- function(...) {
	match(c(...), getCategoryLabel(TRUE))
}

# Desc: The name that identifies an artist, either from artists.names or retrieved with OpenStrands
# Note: Each name has to be retrieved individually since, differently from genres, OpenStrands does not allow listing of all the artists
openstrands.getArtistName <- function(...) {
	if(length(find(".mystrandsArtistsNames")) == 0) {
		.mystrandsArtistsNames 				<<- array(NA, dim = c(length(instances())))
		names(.mystrandsArtistsNames)		<<- instances()
	}
	missingIds						 <- na.omit(names(which(is.na(.mystrandsArtistsNames[c(...)]))))
	if(length(missingIds) > 0)  	    .mystrandsArtistsNames[missingIds]	<<- openstrands.lookupArtists(missingIds)
	.mystrandsArtistsNames[c(...)]
}

# Desc: Return an artist given its name, either from .mystrandsArtistsNames or retrieved with OpenStrands
openstrands.matchArtistName <- function(...) {
	if(length(find(".mystrandsArtistsNames")) == 0) {
		.mystrandsArtistsNames 				<<- array(NA, dim = c(length(instances())))
		names(.mystrandsArtistsNames)		<<- instances()
	}
	artistsNames					 <- c(...)
	missingNames				 	 <- na.omit(artistsNames[which(is.na(match(artistsNames, .mystrandsArtistsNames)))])
	if(length(missingNames) > 0) {
		matchedNames				 <- openstrands.matchArtists(missingNames)
		.mystrandsArtistsNames[names(matchedNames)]	<<- matchedNames	
	}
	match(artistsNames, .mystrandsArtistsNames)
}

