# This source file will generate the dataset mystrandsArtistsTags.rda
# Contains the specific functions and dataset to use the affinity package to
# analyze the co-occurrences of artists and tags in a set of MyStrands playlists

# An example of execution:
#
# source("affinity.R")
# source("plots.R")
# source("openstrands.R")
# source("mystrandsTags.R")
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
	if(length(find("mystrandsArtistsTags")) == 0) {
		data(mystrandsArtistsTags)
		mystrandsArtistsTags$artistId	<<- match(mystrandsArtistsTags$artistId, as.numeric(levels(instances()))[instances()])
		mystrandsArtistsTags			<<- na.omit(mystrandsArtistsTags)
		cat(sprintf("[DEBUG] Loaded %d instances/categories associations.\n", dim(mystrandsArtistsTags)[1]))
		.mystrandsTags					<<- factor(unique(mystrandsArtistsTags$tagId))
		mystrandsArtistsTags$tagId		<<- match(mystrandsArtistsTags$tagId, as.numeric(levels(.mystrandsTags))[.mystrandsTags])
		cat(sprintf("[DEBUG] Found %d unique categories.\n", length(.mystrandsTags)))
	}
	mystrandsArtistsTags[...]
}

# Desc: The list of unique IDs used to identify tags 
categories <- function(...) {
	if(length(find(".mystrandsTags")) == 0) {
		categoriesData(FALSE,FALSE)
	}
	.mystrandsTags[...]
}

# Desc: The binary movies/tags matrix where MORE THAN ONE 1 is allowed on each column indicating the tag labels attached to the movie 
instancesCategories <- function(...) {
	if(length(find(".mystrandsArtistsTags")) == 0) {
		.mystrandsArtistsTags <<- matrix(rep(FALSE,length(instances())*length(categories())),nrow=length(instances()),ncol=length(categories()),dimnames=list(instances(),categories()))
		.mystrandsArtistsTags[as.matrix(categoriesData(TRUE,c("artistId","tagId")))] <<- TRUE
		# Could add: rm(mystrandsArtistsTags) # The original data is not needed anymore from now on
	}
	.mystrandsArtistsTags[...]
}


# 30.4.8 Apparently, if this is defined in affinity.R, then when I redefine it (overwriting this one) in netflixGenres.R, 
# this one is not totally hidden (runExamples keeps on calling this one). For this reason, I move it here and in mystrandsGenres.R
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

getInstanceLabel 	<- function(...) openstrands.getArtistName(...)
matchInstanceLabel 	<- function(...) openstrands.matchArtistName(...)

getCategoryLabel <- function(...) {
	if(length(find(".mystrandsTagsNames")) == 0) {
		data(mystrandsTagsNames)
		.mystrandsTagsNames 		<<- mystrandsTagsNames$tagName[match(categories(), mystrandsTagsNames$tagId)]
		names(.mystrandsTagsNames)  <<- categories()
	}
	.mystrandsTagsNames[...]
}

# Desc: Return a genre given its 'label'
matchCategoryLabel <- function(...) {
	match(c(...), getCategoryLabel())
}


