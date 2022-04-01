#' Generate exploratory histograms, boxplots, and scatter plots for
#' all of the numeric variables in a dataframe.
#'
#' Dashed is mean; dotted is median.
#' @param data Dataframe to extract numeric variables from and plot.
#' @param colramp Colors to ramp between. Assumes red to green. Structure of c(first, second).
#' @param regions Number of regions to split the BinningCheck scatterplot into by color.
#' @return Files "ExplorationOutput_General.pdf" and "ExplorationOutput_BinningCheck.pdf" in the same directory as the code ran.
#' @export

generateExploratory.numeric <- function(data, colors = T, colramp = c("red", "green"), regions = 100) {

  #Fetch numeric variables.
	numerics_names <- names(data)[unlist(lapply(data, is.numeric))]

  #Open first file.
	pdf(file = "ExplorationOutput_General.pdf")
	par(mfrow = c(2, 2))

	#Populate first file.
	for (var in numerics_names) {
  	color <- sample(grDevices::colors()[grep('gr(a|e)y', grDevices::colors(), invert = T)], 1) #Random color for each set of graphs for distinctiveness.
  	hist(data[[var]], col = color, main = paste("Histogram of", var, ""), ylab = "", xlab = "")
  	boxplot(data[[var]], col = color, main = paste("Boxplot of", var, ""), ylab = "", xlab = "")
  }
	whyDoesDevOffReturnStuff <- dev.off()

	#Open second file.
	pdf(file = "ExplorationOutput_BinningCheck.pdf")
	par(mfrow = c(1, 1))

	#Establish color regions.
	pal <- colorRampPalette(colramp)
	regionColors <- pal(regions)

	#Populate second file.
	for (var in numerics_names) {

		sortedData <- sort(data[[var]], na.last = TRUE)

		dataColors <- cut(sortedData, regions, labels = F)
	  dataColors <- regionColors[dataColors]
	  dataColors[is.na(dataColors)] <- "#FFFFFF"

  	if(colors) {
  		plot(1:length(data[[var]]), sortedData,  main = paste("Binning Check for", var, ""), col = dataColors, ylab = "Value", xlab = "Observation # (Sorted)")}
	  else {plot(1:length(data[[var]]), sortedData,  main = paste("Binning Check for", var, ""), ylab = "Value", xlab = "Observation # (Sorted)")}

	  abline(h = mean(sortedData, na.rm = TRUE), lty = "dashed") #Mean
	  abline(h = median(sortedData, na.rm = TRUE), lty = "dotted") #Median

  }
	whyDoesDevOffReturnStuff <- dev.off()

}
