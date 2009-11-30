












suppressWarnings(rm(.categoriesCount, .categoriesSorted, .categoryAffinity, .categoryRanking, .instancesBridge, .categoryNearestNeighbor, .categoriesColors))

#########  [ INSTANCES AND CATEGORIES ]  ########


# Desc: The dataset containing, for each pair of movies, the number of playlists where they occur together at distance 0, 1, or 2
# NOTE: This functions is just for final users to have the whole data, but for cooccurrences it is not used, since we first have to filter which movies also have at least one genre
cooccurrencesData <- function(...) {
	if(length(find("netflixMoviesCooccurrences")) == 0) {
    data(netflixMoviesCooccurrences)
    cat(sprintf("[DEBUG] Loaded %d co-occurrences.\n", dim(netflixMoviesCooccurrencesFile)[1]))
	}
	netflixMoviesCooccurrences[...]
}


# Desc: The list of unique IDs used to identify movies 
instances <- function(...) {
	if(length(find(".netflixCooccurrentMovies")) == 0) {
		# 22.4.8 Attenzione, se un'istanza solo co-occorre con se' stessa, sminchia tutto! Non deve essere considerata!
		.netflixCooccurrentMovies <<- factor(unique(c(cooccurrencesData(TRUE,"movie1Id"),cooccurrencesData(TRUE,"movie2Id"))))	
#		To use less memory, replaced with:
#		.netflixCooccurrentMovies <<- factor(unique(c(unique(cooccurrencesData(TRUE,"movie1Id")),unique(cooccurrencesData(TRUE,"movie2Id")))))	
#   Or with even less memory, with:
#	a <- cooccurrencesData(1:2000000,"movie1Id")
# 	b <- unique(a)
#	a <- cooccurrencesData(2000001:4000000,"movie1Id")
#	b <- unique(c(b, unique(a)))
#	a <- cooccurrencesData(4000001:6000000,"movie1Id")
#	b <- unique(c(b, unique(a)))
#	a <- cooccurrencesData(6000001:9000000,"movie1Id")
#	b <- unique(c(b, unique(a)))
#	a <- cooccurrencesData(1:2000000,"movie2Id")
#	b <- unique(c(b, unique(a)))
#	a <- cooccurrencesData(2000001:4000000,"movie2Id")
#	b <- unique(c(b, unique(a)))
#	a <- cooccurrencesData(4000001:6000000,"movie2Id")
#	b <- unique(c(b, unique(a)))
#	a <- cooccurrencesData(6000001:9000000,"movie2Id")
#	b <- unique(c(b, unique(a)))
#  .netflixCooccurrentMovies <- factor(b)
	}
	.netflixCooccurrentMovies[...]
}


categoriesData <- function(...) {
	if(length(find("netflixMoviesGenres")) == 0) {
		data(netflixMoviesGenres)
		netflixMoviesGenres$movieId	<<- match(netflixMoviesGenres$movieId, as.numeric(levels(instances()))[instances()])
		netflixMoviesGenres			<<- na.omit(netflixMoviesGenres)
		.netflixGenres						<<- factor(unique(netflixMoviesGenres$genreId))
		netflixMoviesGenres$genreId	<<- match(netflixMoviesGenres$genreId, as.numeric(levels(.netflixGenres))[.netflixGenres])
	}
	netflixMoviesGenres[...]
}


# Desc: The list of unique IDs used to identify genres 
categories <- function(...) {
	if(length(find(".netflixGenres")) == 0) {
		categoriesData(FALSE,FALSE)
	}
	.netflixGenres[...]
}

# Desc: The binary movies/genres matrix where MORE THAN ONE 1 is allowed on each column indicating the genre labels attached to the movie 
instancesCategories <- function(...) {
	if(length(find(".netflixMoviesGenres")) == 0) {
		.netflixMoviesGenres <<- matrix(rep(as.integer(0),length(instances())*length(categories())),nrow=length(instances()),ncol=length(categories()),dimnames=list(instances(),categories()))
		.netflixMoviesGenres[as.matrix(categoriesData())] <<- as.integer(1)
		# The next line should be removed in production, to allow people to see the original data
		# rm(netflixMoviesGenres)
	}
	.netflixMoviesGenres[...]
}


# 21.4.8 Questa non va piu' bene con max.col, perche' un'istanza puo' avere piu' categorie!

# Questa versione e' perfetta quando lo chiamo con un solo argomento; con molti mi ritorna una lista di categories, 
# che poi non so ancora come usare bene, per es. per la getCategoryLabel
getCategory <- function(...) {
	apply(array(instancesCategories(...,TRUE), dim = c(length(...),length(categories())), dimnames=list(NULL,categories())), 1, function(x) names(x[x!=0]))
}

# 22.4.8 da spostare in affinity.r
getInstances <- function(...) {
	names(which(instancesCategories(TRUE,...) > 0))
}

#########  [ COOCCURRENCES ]  ########


cooccurrences <- function(...) {
	if(length(find(".netflixMoviesCooccurrences")) == 0) {
		.netflixMoviesCooccurrences	   <<- matrix(rep(as.integer(0),length(instances())^2),nrow=length(instances()),ncol=length(instances()),dimnames=list(instances(),instances()))
		movie1Id						<- match(cooccurrencesData(TRUE,"movie1Id"), as.numeric(levels(instances()))[instances()])
		movie2Id						<- match(cooccurrencesData(TRUE,"movie2Id"), as.numeric(levels(instances()))[instances()])
		cooccurrencesData(TRUE,"f")	   ->> .netflixMoviesCooccurrences[cbind(movie1Id,movie2Id)]
		cooccurrencesData(TRUE,"f")	   ->> .netflixMoviesCooccurrences[cbind(movie2Id,movie1Id)]
		as.integer(0)				   ->> diag(.netflixMoviesCooccurrences)
		# The next line should be removed in production, to allow people to see the original data
		# rm(netflixMoviesCooccurrences)
		# 22.4.8 With less memory:
#		movie1Id						<- match(cooccurrencesData(1:2000000,"movie1Id"), as.numeric(levels(instances()))[instances()])
#		movie2Id						<- match(cooccurrencesData(1:2000000,"movie2Id"), as.numeric(levels(instances()))[instances()])
#		cooccurrencesData(1:2000000,"f")	   ->> .netflixMoviesCooccurrences[cbind(movie1Id,movie2Id)]
#		cooccurrencesData(1:2000000,"f")	   ->> .netflixMoviesCooccurrences[cbind(movie2Id,movie1Id)]
#		movie1Id						<- match(cooccurrencesData(2000001:4000000,"movie1Id"), as.numeric(levels(instances()))[instances()])
#		movie2Id						<- match(cooccurrencesData(2000001:4000000,"movie2Id"), as.numeric(levels(instances()))[instances()])
#		cooccurrencesData(2000001:4000000,"f")	   ->> .netflixMoviesCooccurrences[cbind(movie1Id,movie2Id)]
#		cooccurrencesData(2000001:4000000,"f")	   ->> .netflixMoviesCooccurrences[cbind(movie2Id,movie1Id)]
#		movie1Id						<- match(cooccurrencesData(4000001:6000000,"movie1Id"), as.numeric(levels(instances()))[instances()])
#		movie2Id						<- match(cooccurrencesData(4000001:6000000,"movie2Id"), as.numeric(levels(instances()))[instances()])
#		cooccurrencesData(4000001:6000000,"f")	   ->> .netflixMoviesCooccurrences[cbind(movie1Id,movie2Id)]
#		cooccurrencesData(4000001:6000000,"f")	   ->> .netflixMoviesCooccurrences[cbind(movie2Id,movie1Id)]
#		movie1Id						<- match(cooccurrencesData(6000001:8000000,"movie1Id"), as.numeric(levels(instances()))[instances()])
#		movie2Id						<- match(cooccurrencesData(6000001:8000000,"movie2Id"), as.numeric(levels(instances()))[instances()])
#		cooccurrencesData(6000001:8000000,"f")	   ->> .netflixMoviesCooccurrences[cbind(movie1Id,movie2Id)]
#		cooccurrencesData(6000001:8000000,"f")	   ->> .netflixMoviesCooccurrences[cbind(movie2Id,movie1Id)]
#		movie1Id						<- match(cooccurrencesData(8000001:9000000,"movie1Id"), as.numeric(levels(instances()))[instances()])
#		movie2Id						<- match(cooccurrencesData(8000001:9000000,"movie2Id"), as.numeric(levels(instances()))[instances()])
#		cooccurrencesData(8000001:9000000,"f")	   ->> .netflixMoviesCooccurrences[cbind(movie1Id,movie2Id)]
#		cooccurrencesData(8000001:9000000,"f")	   ->> .netflixMoviesCooccurrences[cbind(movie2Id,movie1Id)]
#		as.integer(0)				   ->> diag(.netflixMoviesCooccurrences)
		
	}
	.netflixMoviesCooccurrences[...]
}



#########  [ LABELS RETRIEVAL ]  ########



getCategoryLabel <- function(...) {
	if(length(find("netflixGenresNames")) == 0) {
		data(netflixMoviesGenres)
	}
	netflixGenresNames[...]
}

# Desc: Return a genre given its 'label'
matchCategoryLabel <- function(...) {
	match(c(...), getCategoryLabel())
}


getInstanceLabel <- function(...) {
	if(length(find("netflixMoviesNames")) == 0) {
		data(netflixMoviesNames)
	}
	netflixMoviesNames[...]
}

# Desc: Return a genre given its 'label'
matchInstanceLabel <- function(...) {
	match(c(...), getInstanceLabel())
}



# TRY to rewrite affinities to use integer rather than double

affinitiesInt <- function(...) {
	if(length(find(".categoryAffinity")) == 0) {

		# [instanceToCategory] $P_x(g)$
		#instanceToCategory		 	<- matrix(as.integer(.netflixMoviesCooccurrences %*% instancesCategories()), nrow=length(instances()), dimnames=list(instances(),categories()))
		#instanceToCategory.rowSum	<- apply(instanceToCategory, 1, sum)
		#instanceToCategory			<- matrix(as.integer(round((instanceToCategory / instanceToCategory.rowSum)*(2^7))), nrow=length(instances()), dimnames=list(instances(),categories()))
		# 22.4.8 Ho dovuto riscriverlo per part2 cosi' (ma ora uso double!):
		instanceToCategory		 	<- matrix(.netflixMoviesCooccurrences %*% instancesCategories(), nrow=length(instances()), dimnames=list(instances(),categories()))
		instanceToCategory.rowSum	<- apply(instanceToCategory, 1, sum)
		instanceToCategory			<- matrix( instanceToCategory / instanceToCategory.rowSum, nrow=length(instances()), dimnames=list(instances(),categories()))
		
		for(i in 1:length(instances()) ) {
			# rowNorm <- .netflixMoviesCooccurrences[i,-i] - mean(.netflixMoviesCooccurrences[i,-i])
			# .netflixMoviesCooccurrences[i,-i] <<- as.integer(round((rowNorm / max(abs(rowNorm)))*(2^7)))       ######## <- funziona? o serve <<- ??
			# 22.4.8 Ho dovuto riscriverlo per part2 cosi' (senno' la somma di ogni riga non dava 0, ma ora uso double!):
			rowNorm <- .netflixMoviesCooccurrences[i,-i] - mean(.netflixMoviesCooccurrences[i,-i])
			.netflixMoviesCooccurrences[i,-i] <<- rowNorm / max(abs(rowNorm))       ######## <- funziona? o serve <<- ??

		}

		# Returns number between -2^30 and +20, hopefully stored as integer
		#.categoryAffinity		   <<- matrix(as.integer(round((.netflixMoviesCooccurrences %*% instanceToCategory)/length(instances))), nrow=length(instances()), dimnames=list(instances(),categories()))
		# 22.4.8 Ho dovuto riscriverlo per part2 cosi' (senno' la somma di ogni riga non dava 0, ma ora uso double!):
		.categoryAffinity		   <<- matrix( (.netflixMoviesCooccurrences %*% instanceToCategory)/length(instances), nrow=length(instances()), dimnames=list(instances(),categories()))
	}
	.categoryAffinity[...]
}

affinities <- function(...) {
#	(affinitiesInt(...)+2^14)/(2^15)
	affinitiesInt(...)
}

centralitiesInt <- function(...) {
	if(length(find(".categoryRanking")) == 0) .categoryRanking <<- apply(affinitiesInt(), 2, rank) * (100*(2^14)/length(instances()))
	.categoryRanking[...]
}

centralities <- function(...) {
	centralitiesInt(...)/(2^14)
}

getCorrelation	<- function(...) {cor(affinitiesInt(,c(...)))}


getBridgeInstances <- function(...) {
	if(length(find(".instancesBridge")) == 0) {
		getMostCentralCategory	<- function(...) {categories(max.col(matrix(centralitiesInt(...,TRUE), ncol=length(categories()))))}
		elements  				<- array(c(as.character(getCategory()), as.character(getMostCentralCategory())), dim=c(length(instances()),2))
		.instancesBridge	   <<- table(list(elements[,1], elements[,2]))
	}
	.instancesBridge[...]	
}

getCorrelation	<- function(...) {cor(affinities(TRUE,c(...)))}

# Print a movie with its genres
getCompleteLabel <- function(...) {
	a <- ""
	for(i in c(...)) a <- paste(a, getInstanceLabel(i), " (", toString(getCategoryLabel(getCategory(i))), ") ", sep="")
	a
}


# Examples !
# for(i in getSortedCategories(1:16)) print(sprintf("Core '%s' movies (%d) are: %s.", getCategoryLabel(i), countInstances(i), getCompleteLabel(getCoreInstances(i, 5))))
# for(i in 1:10) cat(sprintf("'%s' (%s)\n\t '%s' (%s).\n", getInstanceLabel(i), toString(getCategoryLabel(getCategory(i))), getInstanceLabel(getNearestInstance(i)), toString(getCategoryLabel(getCategory(getNearestInstance(i))))))
# cor(affinities(TRUE,getSortedCategories(1:16)))

