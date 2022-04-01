#' Impute missing variables across the *entire* dataset using MICE.
#'
#' This accepts the *entire* dataset and returns the *entire* dataset with new variables.
#' This does **not** preserve original values, as it only changes NAs, and those are encoded in "_Imputed_Flag" variables. No info is lost.
#'
#' @param data The data to extract numeric variables from and subsequently truncate.
#' @param meth The method to be passed to the mice() function.
#' @param flagLevels The values of the "_Imputed_Flag" variable in the format c(unmodified, was NA). Can be numeric, character, or logical.
#' @return Dataframe with added "_Imputed_Flag" variable and changed values.
#' @export
imputeMissing <- function(data, flagLevels = c(0, 1), miceMeth = "pmm", miceSeed = NA, miceM = 5, miceMaxit = 50) {
	na_names <- names(which(colSums(is.na(data)) > 0))
	numeric_names <- names(data)[unlist(lapply(data, is.numeric))]
	na_numeric_names <- intersect(na_names, numeric_names)

	for (name in na_numeric_names) { #For the variables which have NA values...
  	vec <- data[[name]] #Take them as raw vectors...
  	if (sum(is.na(vec)) == 0) {next} #Don't create an imputation flag if there weren't any values to be imputed. Otherwise...
  	willBeImputed <- ifelse(is.na(vec), flagLevels[2], flagLevels[1])
  	data[[paste(name, "_Imputed_Flag", sep = "")]] <- willBeImputed
  } #Create an imputation flag (name based on OG variables) variable.

	data[na_numeric_names] <- data[na_numeric_names] |> mice::mice(m = miceM, maxit = miceMaxit, meth = miceMeth, seed = miceSeed) |> mice::complete() #No flag variables have been used to generate imputations.

	data
}
