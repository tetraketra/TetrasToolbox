#' Safely source a file.
#'
#' This function sources a file while installing and/or loading the specified packages,
#' all in one function. This reduces the need for multiple install.packages or library
#' calls during impromptu work. No extra error handling is included, so normal package
#' and source errors will be thrown if applicable.
#'
#' This was my first ever published function.
#' @param file The file to source.
#' @param packages The packages that must be installed and/or loaded to safely source the file.
#' @return Nothing.
#' @export
safeSource <- function(file, packages = NULL){

	if (!is.null(packages)) {
		for (p in packages) {
			if (p %in% rownames(installed.packages())) {
    		library(p, character.only = T) }
			else {
      	install.packages(p, dependencies = T)
      	library(p,character.only = T)
			}
		}
	}

	source(file)
}

