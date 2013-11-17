
#### main GUI functions ####

CardinaliView <- function() {
	setupToolkit()
	instance <- newInstance("null", emptyenv())
	instance <- initializeInstance(instance)
	invisible(instance)
}

iView <- function(object) {
	setupToolkit()
	if ( !isMSImageSet(object) ) stop("'object' must be an 'MSImageSet'")
	instance <- newInstance(deparse(substitute(object)), objectenv=parent.frame())
	instance <- initializeInstance(instance)
	invisible(instance)
}

#### helper functions ####

setupToolkit <- function() {
	if ( is.null(getOption("guiToolkit")) ) {
		if ( require(gWidgetsRGtk2) && require(RGtk2) && require(cairoDevice) ) {
			options(guiToolkit="RGtk2")
		} else if ( require(gWidgetstcltk) && require(tcltk) ) {
			options(guiToolkit="tcltk")
		}
	}
	setupOutput()
}

setupOutput <- function() {
	if ( getOption("guiToolkit") == "tcltk" ) {
		assign("guiStart", tkProgressStart, envir=Cardinal:::.verboseState)
		assign("guiStop", tkProgressStop, envir=Cardinal:::.verboseState)
		assign("guiIncrement", tkProgressIncrement, envir=Cardinal:::.verboseState)
	} else if ( getOption("guiToolkit") == "RGtk2" ) {
		assign("guiStart", gtkProgressStart, envir=Cardinal:::.verboseState)
		assign("guiStop", gtkProgressStop, envir=Cardinal:::.verboseState)
		assign("guiIncrement", gtkProgressIncrement, envir=Cardinal:::.verboseState)
	}
	assign("guiMessage", iViewMessage, envir=Cardinal:::.verboseState)
}
