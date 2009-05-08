# This source file will generate the dataset mystrandsArtistsGenres.rda
# Contains the specific functions and dataset to use the affinity package to
# analyze the co-occurrences of artists and genres in a set of MyStrands playlists

# An example of execution:
#
# source("affinity.R")
# source("plots.R")
# source("openstrands.R")
# source("mystrandsGenres.R")
# runExamples()
# plotExamples()

suppressWarnings(rm(.categoriesCount, .categoriesSorted, .categoryAffinity, .categoryRanking, .instancesBridge, .categoryNearestNeighbor, .categoriesColors))

#########  [ INSTANCES AND CATEGORIES ]  ########


# Desc: The dataset containing, for each pair of movies, the number of playlists where they occur together at distance 0, 1, or 2
# NOTE: It is important that this file does not contain self co-occurrences!
cooccurrencesData <- function (...) {
    if (length(find("mystrandsArtistsCooccurrences")) == 0) {
        data(mystrandsArtistsCooccurrences)
        cat(sprintf("[DEBUG] Loaded %d co-occurrences.\n", dim(mystrandsArtistsCooccurrences)[1]))
    }
    mystrandsArtistsCooccurrences[...]
}


# Desc: The list of unique IDs used to identify artists 
instances <- function(...) {
	if(length(find(".mystrandsCooccurrentArtists")) == 0) {
		.mystrandsCooccurrentArtists <<- factor(unique(c(cooccurrencesData(TRUE,"artist1Id"),cooccurrencesData(TRUE,"artist1Id"))))	
		cat(sprintf("[DEBUG] Found %d unique instances.\n", length(.mystrandsCooccurrentArtists)))
	}
	.mystrandsCooccurrentArtists[...]
}

cooccurrences <- function(...) {
	if(length(find(".mystrandsArtistsCooccurrences")) == 0) {
		.mystrandsArtistsCooccurrences <<- matrix(rep(0L,length(instances())^2),nrow=length(instances()),ncol=length(instances()),dimnames=list(instances(),instances()))
		artist1Id						<- match(cooccurrencesData(TRUE,"artist1Id"), as.numeric(levels(instances()))[instances()])
		artist2Id						<- match(cooccurrencesData(TRUE,"artist2Id"), as.numeric(levels(instances()))[instances()])
		aggregatedValue				  	<- 25L*cooccurrencesData(TRUE,"d0") + 20L*cooccurrencesData(TRUE,"d1") + 16L*cooccurrencesData(TRUE,"d2")
		aggregatedValue	   			   ->> .mystrandsArtistsCooccurrences[cbind(artist1Id,artist2Id)]
		aggregatedValue	   			   ->> .mystrandsArtistsCooccurrences[cbind(artist2Id,artist1Id)]
		0L							   ->> diag(.mystrandsArtistsCooccurrences)
		cat(sprintf("[DEBUG] Loaded co-occurrence matrix.\n"))
		# Could add: rm(mystrandsArtistsCooccurrences) # The original data is not needed anymore from now on
	}
	.mystrandsArtistsCooccurrences[...]
}

categoriesData <- function(...) {
	if(length(find("mystrandsArtistsGenres")) == 0) {
		data(mystrandsArtistsGenres)
		mystrandsArtistsGenres$artistId		<<- match(mystrandsArtistsGenres$artistId, as.numeric(levels(instances()))[instances()])
		mystrandsArtistsGenres				<<- na.omit(mystrandsArtistsGenres)
		cat(sprintf("[DEBUG] Loaded %d instances/categories associations.\n", dim(mystrandsArtistsGenres)[1]))
		.mystrandsGenres					<<- factor(unique(mystrandsArtistsGenres$genreId))
		mystrandsArtistsGenres$genreId		<<- match(mystrandsArtistsGenres$genreId, as.numeric(levels(.mystrandsGenres))[.mystrandsGenres])
		cat(sprintf("[DEBUG] Found %d unique categories.\n", length(.mystrandsGenres)))
	}
	mystrandsArtistsGenres[...]
}

# Desc: The list of unique IDs used to identify genres 
categories <- function(...) {
	if(length(find(".mystrandsGenres")) == 0) {
		categoriesData(FALSE,FALSE)
	}
	.mystrandsGenres[...]
}

# Desc: The binary movies/genres matrix where ONLY ONE 1 is allowed on each column indicating the genre labels attached to the movie 
instancesCategories <- function(...) {
	if(length(find(".mystrandsArtistsGenres")) == 0) {
		.mystrandsArtistsGenres <<- matrix(rep(FALSE,length(instances())*length(categories())),nrow=length(instances()),ncol=length(categories()),dimnames=list(instances(),categories()))
		.mystrandsArtistsGenres[as.matrix(categoriesData(TRUE,c("artistId","genreId")))] <<- TRUE
		stopifnot(apply(.mystrandsArtistsGenres, 1, sum) == rep(1,length(instances()))) # Each artist has one genre
		# Could add: rm(mystrandsArtistsGenres) # The original data is not needed anymore from now on
	}
	.mystrandsArtistsGenres[...]
}


# 30.4.8 Apparently, if this is defined in affinity.R, then when I redefine it (overwriting this one) in netflixGenres.R, 
# this one is not totally hidden (runExamples keeps on calling this one). For this reason, I move it here and in mystrandsTags.R
affinities <- function(...) {
	if(length(find(".categoryAffinity")) == 0) {
		# Desc: The artists to genres association matrix, where each cell is an aggregation of the artist to artist associations from x to any artist of genre g
		instanceToCategory		 	<- cooccurrences() %*% ifelse(instancesCategories(), 1L, 0L)
		instanceToCategory.rowSum	<- apply(instanceToCategory, 1, sum, na.rm = TRUE)
		instanceToCategory.norm		<- instanceToCategory / instanceToCategory.rowSum
		# Desc: The symmetric artists to artists association matrix, adjusted to have values in [-1,1], independent from the artists' popularity
		cooccurrences.rowMeans 	 	<- apply(cooccurrences() + diag(NA,length(instances())), 1, mean, na.rm = TRUE)
		cooccurrences.rowNorm  	 	<- cooccurrences()  + diag(NA,length(instances())) - cooccurrences.rowMeans
		cooccurrences.rowMax	 	<- apply(cooccurrences.rowNorm,1, function(x) max(abs(x), na.rm = TRUE))
		cooccurrences.norm  	 	<- cooccurrences.rowNorm / cooccurrences.rowMax
		diag(cooccurrences.norm) 	<- 0L # By convention, auto-associations have the neutral value 0
		# Desc: The affinity of artist x to genre g, indicating whether many (resp., few) artists that co-occur with x belong (resp., do not belong) to genre g, adjusted to have values in [0,1]
		.categoryAffinity		   <<- ((cooccurrences.norm %*% instanceToCategory.norm) / length(instances()) + 1L) / 2
		cat(sprintf("[DEBUG] Loaded affinity matrix.\n"))
	}
	.categoryAffinity[...]
}

#########  [ LABELS RETRIEVAL ]  ########

getCategoryLabel 	<- function(...) openstrands.getGenreName(...)
matchCategoryLabel  <- function(...) openstrands.matchGenreName(...)
getInstanceLabel 	<- function(...) openstrands.getArtistName(...)
matchInstanceLabel 	<- function(...) openstrands.matchArtistName(...)



