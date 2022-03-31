#' Generate exploratory histograms, boxplots, and scatter plots for
#' all of the numeric variables in a dataframe.
#'
#' @param data Dataframe to extract numeric variables from and plot.
#' @param colramp Colors to ramp between. Assumes red to green. Structure of c(first, second).
#' @return Files "ExplorationOutput_General.pdf" and "ExplorationOutput_BinningCheck.pdf" in the same directory as the code ran.
#' @export

generateExploratory.numeric <- function(data, colramp = c("red", "green")) {

	pdf(file = "ExplorationOutput_General.pdf")
	par(mfrow = c(2, 2))
	numerics_names <- names(data)[unlist(lapply(data, is.numeric))]
	for (var in numerics_names) {
  	color <- sample(grDevices::colors()[grep('gr(a|e)y', grDevices::colors(), invert = T)], 1) #Random color for each set of graphs for distinctiveness.
  	hist(data[[var]], col = color, main = paste("Histogram of", var, ""), ylab = "Value", xlab = "Observation # (Sorted)")
  	boxplot(data[var], col = color, main = paste("Boxplot of", var, ""), ylab = "Value", xlab = "Observation # (Sorted)")}
	dev.off()

	pal <- colorRampPalette(colramp)

	pdf(file = "ExplorationOutput_BinningCheck.pdf")
	par(mfrow = c(1, 1))
	pal <- colorRampPalette(c("red", "green"))
	for (var in numerics_names) {
		rampedColors <- pal(length(data[[var]]))
  	plot(1:length(data[[var]]), sort(data[[var]], na.last = TRUE),  main = paste("Binning Check for", var, ""), col = rampedColors, ylab = "Value", xlab = "Observation # (Sorted)")
	  abline(h = mean(data[[var]], na.rm = TRUE))}
	dev.off()

}