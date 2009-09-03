require("scatterplot3d")
require("grid")

#########  [ PLOT FUNCTIONS ]  ########


# Desc: A unique color code that identifies each genre in the plotted graph
# Note: The colors are associated according to the popularity of a genre; 8 have bright colors, 8 have opaque colors, the rest are grey
categoriesColors <- function(...) {
	if(length(find(".categoriesColors")) == 0) {
		.categoriesColors					 		   <<- array("#333333FF", dim = c(length(categories())))
		names(.categoriesColors)			 		   <<- categories()
		brightColors 					  	 	 	 	<- c("#AA00CCFF","#0000CCFF","#00AACCFF","#00CC88FF","#22CC00FF","#AAAA00FF","#CC2200FF","#CC0044FF")
		opaqueColors 					  	 	 	 	<- c("#D6C6E6FF","#C6D6E6FF","#C6E6E6FF","#C6E6D6FF","#D6E6C6FF","#E6E6C6FF","#E6C6C6FF","#E6C6E6FF")
		.categoriesColors[getSortedCategories(1:8)]	   <<- brightColors
		.categoriesColors[getSortedCategories(9:16)]   <<- opaqueColors
	}
	.categoriesColors[...]
}

# Desc: Plot the genre affinity of the specified artists to 3 different genres
# NOTE: Incomplete, title and labels missing, use this instead: http://addictedtor.free.fr/graphiques/RGraphGallery.php?graph=34
plotAffinity3D <- function(instancesId, threeCategoriesId) {
	# Check the number of objects and classes
	countObjects					<- length(threeCategoriesId)
	if(countObjects != 3) stop("Can only plot against three dimensions")
	R2 = affinities(instancesId, threeCategoriesId)
	lims = apply(R2, 2, function(x) max(abs(x - 0.5)))
	colnames(R2) = getCategoryLabel(threeCategoriesId)
	# To correct the axis labels, try adding: lab=c(3,3,3)
	#OSX# quartz(width=7,height=7) # Only for my own sake, should be removed to have cross-compatibility
	scatterplot3d(R2, pch = rank(instancesId), box = FALSE, angle = 30, grid = TRUE, xlim=c(0.5-lims[1],0.5+lims[1]), ylim=c(0.5-lims[2],0.5+lims[2]), zlim=c(0.5-lims[3],0.5+lims[3]), type="h",cex.symbols=c(2,2,2))
	# text(c(1,3.1,5.5), c(5.8,5.0,5.3),labels=getInstanceLabel(artistsId))
}

# [example plotAffinity3D]
## Not run: 
## plotAffinity3D(c("i1","i4"), c("c1","c2","c3"))

# Desc: Plot on a 2D graph the genre affinity to all the artists to 2 different genres
# NOTE: Should add instancesId as a parameter to specify which ones to plot
# NOTE: Should rename the parameters, e.g. others rather than plotOthers
plotAffinity2D <- function(genresId, plotCentrality = FALSE, plotOthers = FALSE, plotNames = rep(FALSE,2), plotColors = TRUE, plotSave = FALSE) {
	genresNames 	<- getCategoryLabel(genresId)
	artistsGenre1	<- which(instancesCategories(TRUE,genresId[1]) == 1)
	artistsGenre2	<- which(instancesCategories(TRUE,genresId[2]) == 1)
	# Plot
	plotFunction	<- if(plotCentrality)	centralities	else affinities
	plotTitle		<- if(plotCentrality)	"Genre-centrality"	else "Genre-affinity"
	plottedArtists <- if(plotOthers) 		TRUE				else c(artistsGenre1, artistsGenre2)

	# plottedCol		<- if(!plotColors)		1					else rgb(instancesCategories(plottedArtists,genresId) %*% t(col2rgb(categoriesColors(genresId),TRUE)), maxColorValue = 255)

	# 18.4.8 ATTENZIONE!! Questo plottedCol non va piu' bene dal momento che un'istanza puo' appartenere a piu' categorie!
	# In questo caso, di che colore lo si dipinge?! Idem per pch e cex, ma quelli almeno vengono piu' grandi
	# Per ora facciamo un colore in mezzo... anche se non ha molto senso
	plottedCol		<- if(!plotColors)		1					else rgb((instancesCategories(plottedArtists,genresId) %*% t(col2rgb(categoriesColors(genresId),TRUE))) %% 256, maxColorValue = 255)
	
	plottedPch		<- 3 - instancesCategories(plottedArtists,genresId[1])*2 - instancesCategories(plottedArtists,genresId[2])*1
	plottedCex		<- 0.4 + instancesCategories(plottedArtists,genresId[1])*0.5 + instancesCategories(plottedArtists,genresId[2])*0.5
	#OSX# quartz(width=7,height=7) # Only for my own sake, should be removed to have cross-compatibility
	plot(plotFunction(plottedArtists,genresId), pch=plottedPch, col=plottedCol, cex=plottedCex, xlab=genresNames[1], ylab=genresNames[2])
	# Title
	title(plotTitle, line = 2.3)
 	mtext(genresNames[1], line = 1, adj = 0.15, cex = 1.1, col = ifelse(plotColors, categoriesColors(genresId[1]), "black"))
	mtext("vs.", line = 1, cex = 1.1)
	mtext(genresNames[2], line = 1, adj = 0.85, cex = 1.1, col = ifelse(plotColors, categoriesColors(genresId[2]), "black"))
	# Text
	if(plotNames[1]) text(plotFunction(artistsGenre1,genresId), getInstanceLabel(artistsGenre1))		
	if(plotNames[2]) text(plotFunction(artistsGenre2,genresId), getInstanceLabel(artistsGenre2))		
	# Save
	#OSX# if(plotSave)     quartz.save(paste(genresNames[1], "_", genresNames[2], ".png", sep = ""))
}

# [example plotAffinity2D]
## Not run: 
## plotAffinity2D(c("c1","c2"))


# Desc: Plot a SPIE graph
# Inspired by http://www.cs.huji.ac.il/~feit/papers/Spie03TR.pdf and the A2R package
plotSpie <- function(values, amplitudes=NULL, minValue=NULL, meanValue=NULL, numticks=4, colors=rainbow(dim(values)[2]), title="Spie graph", save=FALSE) {
	# Check the number of objects and classes
	countObjects					<- dim(values)[1]
	countClasses					<- dim(values)[2]
	if(!(countObjects %in% c(1,2))) stop("Can only plot one or two objects")
	# Calculate the radium (defaultValue is for the ghost sector, minValue for the origin, radCenter for calculation only)
	defaultValue	<- if(is.null(meanValue)) (max(values) + min(values))/2	else meanValue
	radCenter		<- max( max(values) - defaultValue, defaultValue - min(values))
	minValue		<- if(is.null(minValue))  defaultValue - radCenter else minValue
	radium			<- if(is.null(minValue) || is.null(meanValue)) 2*radCenter else 2*(meanValue - minValue)
	# Draw the viewport
	#OSX# quartz(width=7,height=7) 	 # Only for my own sake, should be removed to have cross-compatibility
	grid.newpage()
	pushViewport(viewport(layout=grid.layout(1,1,respect=TRUE)))
	pushViewport(dataViewport(radium*c(-1.4,1.2), radium*c(-1.4,1.2), layout.pos.col=1, layout.pos.row=1))

	# Print the titles and set the angles and colors
	if(is.null(amplitudes)) amplitudes	<- rep(1, countClasses)
	angles								<- 2*pi*amplitudes/sum(amplitudes)
	names(angles) 						<- colnames(values)
	if(countObjects == 1) {
#		grid.text(paste(title,"of",rownames(values)[1]), 0, 1.2*radium, default.units="native", gp=gpar(cex=1.3))
		radii	<- values - minValue
		angles	<- cumsum(angles)
	} else {
		grid.text(paste(title,"comparison"), 0, 1.2*radium, default.units="native", gp=gpar(cex=1.3))
		grid.text(rownames(values)[1], -0.7*radium, -1.25*radium, default.units="native", gp=gpar(cex=1.1))
		grid.text(rownames(values)[2],  0.7*radium, -1.25*radium, default.units="native", gp=gpar(cex=1.1))
		radii	<- c(values[1,],rev(values[2,])) - minValue
		angles	<- cumsum(c(angles/2,rev(angles/2)))
		colors	<- c(colors,rev(colors))
	}
	# Add initial angle
	initialAngle	<- pi/2
	angles			<- angles + initialAngle

	for(i in 1:length(angles)) {
		# Draw 50 polygons per circle
		prevAngle 	<- if(i == 1) initialAngle else angles[i-1]
		theta 		<- seq(prevAngle, angles[i], length=50*(angles[i]-prevAngle))
	  	# Draw the ghost plot for the median value in the range
	  	grid.polygon(x = c(0, cos(theta)*(defaultValue-minValue) ,0), y = c(0, sin(theta)*(defaultValue-minValue) ,0), gp = gpar(fill=colors[i], lwd=0.4, lty=3, alpha=0.2), default.units="native")
		# Draw the real value for this sector
	  	grid.polygon(x = c(0, radii[i] * cos(theta),0), y = c(0, radii[i] * sin(theta),0), gp = gpar(fill=colors[i], lwd=1.5), default.units="native")
		# Note: To have different shading lines for left and right, substitute with this code:
	  	# grid.polygon(x = c(0, x[i,2] * ifelse(right,-1,1)*cos(theta),0), y = c(0, x[i,2] * sin(theta),0), gp = gpar(fill=ifelse(right,NA,col[i]), lwd=1.5), default.units="native")
		# if(right) for(j in theta[-c(1,length(theta))]) grid.lines(x = c(0, x[i,2] * ifelse(right,-1,1)*cos(j)), y = c(0, x[i,2] * sin(j)), gp = gpar(col=col[i], lwd=2), default.units="native")

		# Write the legend
	    angleAnn <- (angles[i] + prevAngle)/2
	    maxx 	 <- min(max(radii[i]+radium/7,radium/1.2), 3*radium)
		rot 	 <- (prevAngle+angles[i])*90/pi
	    grid.text(names(angles)[i], rot=rot, x=cos(angleAnn)*maxx, y=sin(angleAnn)*maxx, gp = gpar(col=colors[i], fill="white", lwd=2, fontsize=10), default.units="native")
	}
	
	# Plot lines and ticks
	if(countObjects == 2) grid.lines(c(0,0),radium*c(-1.2,1.05), default.units="native", gp=gpar(lty=2, lwd=2, alpha=0.5))
	
	if(!is.null(numticks)) {
		ticks = cumsum(rep(radium/numticks,numticks))
		grid.circle(x=0, y=0, r=ticks, gp=gpar(fill=NA, lty=3, lwd=0.5), default.units="native")
		for(i in ticks) {
			# Small bug: the percentage sign is not always required( ex: not for genre-affinity)
			grid.text( paste(format(i+minValue,digits=5), if((radium == 100) && (minValue == 0)) "%", sep="") , 0, i, gp=gpar(alpha=0.5, fill="white"), default.units="native")	
		  	# To have surrounding boxes, replace with:
	      	# st <- paste(i, "%", sep="")
	      	# grid.rect( x = unit(0, "native"), y = unit(i,"native"), width = 1.5*stringWidth(st), height = 1.5*stringHeight(st) , gp=gpar(fill="white", col=NA, alpha=0.2))
	      	# grid.text( st , 0, i, default.units="native")
		}  
	}
	upViewport(2)
	# Save
	#OSX# if(save) quartz.save(paste( rownames(values)[1], if(!is.na(rownames(values)[2])) paste(" vs", rownames(values)[2]), ".png", sep = ""))
}

# [example plotSpie]
## Not run: 
## plotSpie( matrix( affinities(c("i1","i2"),TRUE), nrow=2 ) )

# Desc: Plot on a Spie graph the centrality (or affinity) of one (or two) artists to a set of genres
plotAffinitySpie <- function(artistsId, genresId = getSortedCategories(1:8), plotCentrality = TRUE, plotPopularity = TRUE, plotSave = FALSE) {
	heights			<- if(plotCentrality) centralities(artistsId,genresId) else affinities(artistsId,genresId)
	title			<- if(plotCentrality) "Genre-centrality" else "Genre-affinity"
	# artistsLabels	<- paste(getInstanceLabel(artistsId), " (", getCategoryLabel(getCategory(artistsId)), ")", sep="")
	# 23.4.8 Since an instance can have more categories, the line has been changed to:
	artistsLabels	<- paste(getInstanceLabel(artistsId), "\n(", sapply(artistsId, function(x) toString(getCategoryLabel(getCategory(x)))), ")", sep="")
	genresLabels	<- getCategoryLabel(genresId)
	values			<- matrix(heights, ncol=length(genresId), dimnames=list(artistsLabels,genresLabels))
	amplitudes		<- if(plotPopularity) countInstances(genresId)	# If plotPopularity, then the amplitude of each sector represents is proportional to the popularity of that genre
	minValue		<- if(plotCentrality) 0 else NULL			# For affinity degree, let it calculate automatically
	meanValue		<- if(plotCentrality) 50 else 0.5
	numticks		<- 4
	colors			<- categoriesColors(genresId)
	plotSpie(values, amplitudes=amplitudes, minValue=minValue, meanValue=meanValue, numticks=numticks, colors=colors, title=title, save=plotSave)
}


plotCategories <- function(range = TRUE) {
	which = getSortedCategories(range)
	pie(countInstances(which), labels=getCategoryLabel(which), col=categoriesColors(which))
}

plotExamples <- function() {
	rndCategories	<- getSortedCategories(1:5)[trunc(runif(3)*5)+1]
	rndInstances	<- floor(runif(2, min=0, max=length(instances()))+1)

	# Plot the list of categories
  	readline("Press any key to plot a pie of category popularities...")
	plotCategories()

	# Plot a 3D affinity graph
  	readline(paste("Press any key to plot a 3D category affinity graph of", toString(getInstanceLabel(rndInstances)), " "))
	plotAffinity3D(rndInstances, rndCategories)

	# Plot a 2D affinity graph
  	readline(paste("Press any key to plot a comparative affinity graph of", toString(getCategoryLabel(rndCategories[1:2])), " "))
	plotAffinity2D(rndCategories[1:2])

	# Plot a 2D centrality graph
  	readline(paste("Press any key to plot a comparative centrality graph of", toString(getCategoryLabel(rndCategories[2:3])), " "))
	plotAffinity2D(rndCategories[2:3], plotCentrality = TRUE, plotOthers = TRUE)

	# Plot a comparative spie graph
	nearInstances <- c(rndInstances[1],getNearestInstance(rndInstances[1]))
  	readline(paste("Press any key to plot a comparative spie graph of", toString(getInstanceLabel(nearInstances)), " "))
	plotAffinitySpie(nearInstances)

	# Plot a descriptive spie graph
  	readline(paste("Press any key to plot a descriptive spie graph of", toString(getInstanceLabel(rndInstances[2])), " "))
	plotAffinitySpie(rndInstances[2])
	
	cat(sprintf("# Plot more categories running plotCategories(range), e.g.: plotCategories(1:8)\n"))
	cat(sprintf("# Plot more 3D affinity graphs running plotAffinity3D(instances, categories), e.g.: plotAffinity3D(matchInstanceLabel(%s),matchCategoryLabel(%s))\n", toString(paste("\"", getInstanceLabel(rndInstances), "\"", sep="")), toString(paste("\"", getCategoryLabel(rndCategories), "\"", sep=""))))
	cat(sprintf("# Plot more 2D affinity graphs running plotAffinity2D(categories), e.g.: plotAffinity2D(matchCategoryLabel(%s))\n", toString(paste("\"", getCategoryLabel(rndCategories[2:3]), "\"", sep=""))))
	cat(sprintf("# Plot more 2D centrality graphs running plotAffinity2D(categories, plotCentrality = TRUE), e.g.: plotAffinity2D(matchCategoryLabel(%s), plotCentrality = TRUE)\n", toString(paste("\"", getCategoryLabel(rndCategories[1:3]), "\"", sep=""))))
	cat(sprintf("# Plot more comparative spie graphs running plotAffinitySpie(twoInstances), e.g.: plotAffinitySpie(matchInstanceLabel(%s))\n", toString(paste("\"", getInstanceLabel(rndInstances), "\"", sep=""))))
	cat(sprintf("# Plot more descriptive spie graphs running plotAffinitySpie(oneInstance), e.g.: plotAffinitySpie(matchInstanceLabel(%s))\n", toString(paste("\"", getInstanceLabel(rndInstances[1]), "\"", sep=""))))
}


