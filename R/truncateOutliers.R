#' Truncate outliers in entire dataset to 1.5*IQR standard.
#'
#' This accepts the *entire* dataset and returns the *entire* dataset with new variables.
#'
#' @param data The data to extract numeric variables from and subsequently truncate.
#' @param flagLevels The values of the "_OoB_Flag" variable in the format c(unmodified, too low, too high). Can be either numeric or character.
#' @return Dataframe with added "_OoB_Flag" and "_Truncated" variables.
#' @export
truncateOutliers <- function(data, flagLevels = c(0, 1, 2)) {
	numerics_names <- names(data)[unlist(lapply(data, is.numeric))]

	#This function returns a truncated value (if necessary).
	cutter <- function(x) {
	  if (is.na(x)) {NA}
		else if (x < floor) {floor}
		else if (x > ceiling) {ceiling}
		else {x}
	}

	#This function is used to build the OoB (Out of Bounds) flag variables.
	flagger <- function(x) {
	  if (is.na(x)) {flagLevels[1]}
		else if (x < floor) {flagLevels[2]}
		else if (x > ceiling) {flagLevels[3]}
		else {flagLevels[1]}
	}

	for (var in numerics_names) {
	  vec <- data[[var]]

	  IQR <- quantile(vec, 0.75, na.rm = T, names = F) - quantile(vec, 0.25, na.rm = T, names = F)
	  floor <- quantile(vec, 0.25, na.rm = T, names = F) - 1.5*IQR
	  ceiling <- quantile(vec, 0.75, na.rm = T, names = F) + 1.5*IQR

	  if (is.numeric(flagLevels)) {imputed_status <- vapply(vec, flagger, numeric(1))}
	  else if (is.character(flagLevels)) {imputed_status <- vapply(vec, flagger, character(1))}
	  else {stop("Flag levels must be numeric or character.")}

	  if (sum(imputed_status >= 1, na.rm = T) == flagLevels[1]) {next} #Don't actually impute if there's none to be done.

	  data[[paste(var, "_OoB_Flag", sep = "")]] <- factor(imputed_status, levels = c(flagLevels[1], flagLevels[2], flagLevels[3]))
	  data[[paste(var, "_Truncated", sep = "")]] <- vapply(vec, cutter, numeric(1))
	}

	data
}
