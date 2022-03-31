#' Runs summary functions on the supplied data with no additional arguments, returning as a list.
#' @param data The data to run the functions on.
#' @param funcs The functions to execute, formatted as a character vector of function names.
#' @return Named list of summary function return values.
#' @export
summaryFunctions <- function(data, funcs = c("mean", "median", "range", "IQR", "fivenum")) {
	returnList <- list()
	for (func in funcs) {
			returnList[[func]] <- do.call(func, list(data))
	}
	returnList
}
