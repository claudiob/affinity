
# Desc: The list of categories where instances belong to
# NOTE: To fix: when called with multiple instances, returns a *list* of categories, which can be hardly used with getCategoryLabel
getCategory <- function(...) {
	apply(array(instancesCategories(...,TRUE), dim = c(length(...),length(categories())), dimnames=list(NULL,categories())), 1, function(x) unname(which(!is.na(x) & x==TRUE)))
}

# Desc: The list of instances belonging to a category
getInstances <- function(...) {
	unname(unlist(apply(matrix(instancesCategories(TRUE,...), nrow=length(instances()), dimnames=list(instances(), categories(...))), 2, which)))
}

# Desc: The list of instances belonging to a category
countInstances <- function(...) {
	if(length(find(".categoriesCount")) == 0) .categoriesCount <<- apply(instancesCategories(),2,sum)
	.categoriesCount[...]
}

# Desc: The list of categories with most instances
getSortedCategories <- function(...) {
	if(length(find(".categoriesSorted")) == 0) .categoriesSorted <<- match(names(sort(countInstances(),TRUE)), categories())
#	names(na.omit(.categoriesSorted[...]))
	.categoriesSorted[...]
}

# Desc: The name of an instance with the categories it belongs to
getCompleteLabel <- function(...) {
	result <- ""
	for(i in c(...)) result <- paste(result, getInstanceLabel(i), " (", toString(getCategoryLabel(getCategory(i))), ") ", sep="")
	result
}



#########  [ AFFINITY, CENTRALITY, CORE AND BRIDGE INSTANCES, CORRELATION  ]  ########

# Desc: $C_g(x)$ The percentage of artists whose genre affinity to g is lower or equal than the genre affinity of x to genre g
centralities <- function(...) {
	if(length(find(".categoryRanking")) == 0) .categoryRanking <<- apply(affinities(), 2, rank) * (100/length(instances()))
	.categoryRanking[...]
}

# Desc: The artists with the highest genre-centrality to a genre
getCoreInstances <- function(categoriesId, count = 5) {
	names(sort(centralities(TRUE,categoriesId),TRUE)[1:count])
}

# Desc: Number of artists with a genre g1 attached and with a genre g2 as the most central one
# Non ha senso con tutte le categorie, perche' mi dice se uno e' alto in children', che non mi interessa
getBridgeInstances <- function(categoriesId) {
	inst	 <- getInstances(categoriesId)
	instCat  <- getCategory(inst)
   	mostCat  <- categoriesId[{max.col(matrix(centralities(inst,categoriesId), ncol=length(categoriesId)))}]
	testCat  <- vector(length = length(inst))
	for(i in 1:length(inst)) testCat[i] <- !(mostCat[i] %in% instCat[[i]])
	element1 <- rep(mostCat[testCat], sapply(instCat[testCat], length))
	element2 <- unlist(instCat[testCat])
	# table(list(element1[element2 %in% categoriesId], element2[element2 %in% categoriesId]))
	# The previous line is correct, but prints the categories ID as rownames, which may cause confusion, so it's rewritten:
	table(list(as.character(categories(element1[element2 %in% categoriesId])), as.character(categories(element2[element2 %in% categoriesId]))))
}
   

# Desc: The Pearson correlation coefficient between categories
getCorrelation	<- function(...) {
	cor(affinities(TRUE,c(...)))
}

# Desc: The instances whose affinity vector has the closest distance to that of instanceId
getNearestInstance	<- function(instanceId, howMany = 1) {
	distances 		 <- dist(affinities( c(instanceId,1:(instanceId-1),(instanceId+1):length(instances())),TRUE))[1:(length(instances()) - 1)]
	names(distances) <- c(1:(instanceId-1),(instanceId+1):length(instances()))
	as.numeric(names(sort(distances)[1:howMany]))
}

#   Desc: Alternative NN function which requires more memory but can be called for multiple instances
#   getNearestInstances	<- function(...) {
#   	if(length(find(".categoryNearestNeighbor")) == 0) {
#   		affinity.distanceMatrix		     <- as.matrix(dist(affinities()))
#   		diag(affinity.distanceMatrix)    <- Inf
#   		.categoryNearestNeighbor	  	<<- apply(affinity.distanceMatrix, 1, which.min)
#   		names(.categoryNearestNeighbor) <<- instances()
#   	}
#   	.categoryNearestNeighbor[...]
#   }


printCore <- function(categoryName, howMany = 5) {
	getCompleteLabel(getCoreInstances(matchCategoryLabel(categoryName), howMany))
}

printCentrality <- function(instanceName, categoryName) {
	centralities(matchInstanceLabel(instanceName), matchCategoryLabel(categoryName))
}

printNearest <- function(instanceName, howMany = 5) {
	getCompleteLabel(getNearestInstance(matchInstanceLabel(instanceName), howMany))
}

printCategories <- function(range = TRUE) {
	unname(getCategoryLabel(getSortedCategories(range)))
}

runExamples <- function() {
	rndCategories	<- getSortedCategories(1:5)[trunc(runif(3)*5)+1]
	rndInstances	<- floor(runif(3, min=0, max=length(instances()))+1)

	# Print some core instances
	for(i in rndCategories) cat(sprintf("The core '%s' (%d) instances are: %s\n", getCategoryLabel(i), countInstances(i), getCompleteLabel(getCoreInstances(i, 5))))

	# Print some centrality values
	for(i in rndInstances) cat(sprintf("The highest centrality values for %s are: %s\n", getCompleteLabel(i), toString(paste( getCategoryLabel(names(sort(centralities(rndInstances[1],), TRUE)[1:3])), " (", format(sort(centralities(i,), TRUE)[1:3], digits=3), "%)", sep="" ))))
	
	# Print some NN instances
	for(i in 1:2) cat(sprintf("The nearest instances to %s are: %s\n", getCompleteLabel(rndInstances[i]), getCompleteLabel(getNearestInstance(rndInstances[i], 5))))

	# Print some bridge instances statistics
	bridgeMatrix <- getBridgeInstances(getSortedCategories(1:8))
	rownames(bridgeMatrix) <- getCategoryLabel(rownames(bridgeMatrix))
	colnames(bridgeMatrix) <- getCategoryLabel(colnames(bridgeMatrix))
	cat(sprintf("The number of bridge instances for categories %s is:\n", toString(getCategoryLabel(getSortedCategories(1:8)))))
	print(bridgeMatrix)

	# Print some categories correlation values
	corMatrix <- getCorrelation(getSortedCategories(1:8))
	rownames(corMatrix) <- colnames(corMatrix) <- getCategoryLabel(getSortedCategories(1:8))
	cat(sprintf("The correlation among categories %s is:\n", toString(rownames(corMatrix))))
	print(corMatrix, digits=1)

	# Show other examples
	cat(sprintf("# Get more categories running printCategories(range), e.g.: printCategories()\n"))
	cat(sprintf("# Get more core instances running printCore(categoryName, howMany), e.g.: printCore(\"%s\", 3)\n", getCategoryLabel(rndCategories[1])))
	cat(sprintf("# Get more centrality values running printCentrality(instanceName, categoryName), e.g.: printCentrality(\"%s\", \"%s\")\n", getInstanceLabel(rndInstances[1]), getCategoryLabel(rndCategories[2])))
	cat(sprintf("# Get more nearest instances running printNearest(instanceName, howMany), e.g.: printNearest(\"%s\", 3)\n", getInstanceLabel(rndInstances[3])))
}
