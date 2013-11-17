

#### functions for listing objects of interest in an environment ####

ls.MSImageSet <- function(envir=parent.frame()) {
	objectnames <- ls(envir=envir)
	which <- sapply(objectnames, function(x) isMSImageSet(get(x, envir)))
	if ( length(which) == 0 ) {
		character()
	} else {
		objectnames[which]
	}
}

ls.MSImageSegmentation <- function(envir=parent.frame()) {
	objectnames <- ls(envir=envir)
	which <- sapply(objectnames, function(x) Cardinal:::isMSImageSegmentation(get(x, envir)))
	if ( length(which) == 0 ) {
		character()
	} else {
		objectnames[which]
	}
}

ls.MSPeakFrame <- function(envir=parent.frame()) {
	objectnames <- ls(envir=envir)
	which <- sapply(objectnames, function(x) isMSPeakFrame(get(x, envir)))
	if ( length(which) == 0 ) {
		character()
	} else {
		objectnames[which]
	}
}

ls.ROI <- function(object, envir=parent.frame()) {
	objectnames <- ls(envir=envir)
	which <- sapply(objectnames, function(x) {
		x <- get(x, envir)
		is.logical(x) && length(x) == numPixels(object)
	} )
	if ( length(which) == 0 ) {
		character()
	} else {
		objectnames[which]
	}
}
