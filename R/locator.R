
#### functions for selection using locator() ####

locatorSelectPoint <- function(instance, ...) {
	px <- NULL
	while ( is.null(px) ) {
		selected <- locator(1)
		if ( within.parusr(selected) ) px <- selected
	}
	px
}

locatorSelectPixelCoord <- function(instance, ...) {
	object <- try(get(instance$objectname, envir=instance$objectenv), silent=TRUE)
	if ( inherits(object, "try-error") ) return(list(x=NA, y=NA))
	coord <- instance$parameters$coord
	sliceDimNames <- instance$parameters$sliceDimNames
	px <- NULL
	while ( is.null(px) ) {
		selected <- locator(1)
		coord[instance$parameters$sliceDimNames] <- round(unlist(selected))
		if ( within.parusr(selected) &&
			selected$x > 0 && selected$x <= max(coord(object)[sliceDimNames[[1]]]) &&
			selected$y > 0 && selected$y <= max(coord(object)[sliceDimNames[[2]]]) &&
			!is.na(pixels(object, coord=coord)) )
		{
			px <- selected
		}
	}
	px
}

within.parusr <- function(selected) {
	parusr <- par("usr")
	if ( is.null(selected) ) {
		FALSE
	} else {
		selected$x[[1]] > parusr[[1]] && selected$x[[1]] < parusr[[2]] &&
			selected$y[[1]] > parusr[[3]] && selected$y[[1]] < parusr[[4]]
	}
}

