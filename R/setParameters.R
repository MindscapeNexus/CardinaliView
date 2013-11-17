
#### functions for setting parameters ####

setDefaults <- function(instance) {
	.guiState$busy <- TRUE
	object <- try(get(instance$objectname, envir=instance$objectenv), silent=TRUE)
	if ( inherits(object, "try-error") ) {
		.guiState$busy <- FALSE
		return(TRUE)
	}
	init.sliceDimNames <- object@metaData[["coordDimNames"]][c(1,2)]
	if ( length(object@metaData[["coordDimNames"]]) > 2 ) {
		init.fixCoord <- as.list(coord(object)[1,-c(1,2),drop=FALSE])
	} else {
		init.fixCoord <- list()
	}
	init.intensityRange <- c(min(object@spectra$spectra, na.rm=TRUE),
		max(object@spectra$spectra, na.rm=TRUE)) # range() uses too much memory
	setFeature(instance, 1)
	setMZRange(instance, range(mz(object)))
	setPixel(instance, 1)
	setCoordRange(instance, c(range(coord(object)[init.sliceDimNames[[1]]]),
		range(coord(object)[init.sliceDimNames[[2]]])))
	instance$parameters$sliceDimNames <- init.sliceDimNames
	instance$parameters$fixCoord <- init.fixCoord
	instance$parameters$minIntensity <- init.intensityRange[[1]]
	instance$parameters$maxIntensity <- init.intensityRange[[2]]
	setPlotIntensityRange(instance, range(intensities(object, pixel=1), na.rm=TRUE))
	setImageIntensityRange(instance, range(intensities(object, feature=1), na.rm=TRUE))
	.guiState$busy <- FALSE
	TRUE
}

setFeature <- function(instance, value) {
	.guiState$busy <- TRUE
	value <- as.integer(value)
	object <- try(get(instance$objectname, envir=instance$objectenv), silent=TRUE)
	if ( inherits(object, "try-error") || value < 1 || value > numPixels(object) ) {
		.guiState$busy <- FALSE
		return(FALSE)
	}
	feature <- value
	instance$parameters$feature <- feature
	mz <- mz(object)[[feature]]
	instance$parameters$mz <- mz
	if ( isExtant(instance$windows$main$topWidgets$plotControlWindow) ) {
		svalue(instance$windows$main$widgets$mass) <- round(mz, digits=2)
	} else {
		makePlotControls(instance)
	}
	if ( !instance$windows$main$image.options[["common intensity scale"]] ) {
		intensityRange <- range(intensities(object, mz=mz), na.rm=TRUE)
		setImageIntensityRange(instance, intensityRange)
	}
	.guiState$busy <- FALSE
	TRUE
}

setMZ <- function(instance, value) {
	.guiState$busy <- TRUE
	value <- as.numeric(value)
	object <- try(get(instance$objectname, envir=instance$objectenv), silent=TRUE)
	if ( inherits(object, "try-error") || is.na(value) ) {
		.guiState$busy <- FALSE
		return(FALSE)
	}
	feature <- features(object, mz=value)
	instance$parameters$feature <- feature
	mz <- mz(object)[[feature]]
	instance$parameters$mz <- mz
	if ( isExtant(instance$windows$main$topWidgets$plotControlWindow) ) {
		svalue(instance$windows$main$widgets$mass) <- round(mz, digits=2)
	} else {
		makePlotControls(instance)
	}
	if ( !instance$windows$main$image.options[["common intensity scale"]] ) {
		intensityRange <- range(intensities(object, mz=mz), na.rm=TRUE)
		setImageIntensityRange(instance, intensityRange)
	}
	.guiState$busy <- FALSE
	TRUE
}

setMZRange <- function(instance, value) {
	.guiState$busy <- TRUE
	value <- as.numeric(value)
	if ( is.na(value) || value[[1]] >= value[[2]] ) {
		.guiState$busy <- FALSE
		return(FALSE)
	}
	mzRange <- value
	instance$parameters$mzRange <- mzRange
	if ( isExtant(instance$windows$main$topWidgets$plotControlWindow) ) {
		svalue(instance$windows$main$widgets$massMin) <- round(mzRange[[1]], digits=2)
		svalue(instance$windows$main$widgets$massMax) <- round(mzRange[[2]], digits=2)
	} else {
		makePlotControls(instance)
	}
	.guiState$busy <- FALSE
	TRUE
}

setPixel <- function(instance, value) {
	.guiState$busy <- TRUE
	value <- as.integer(value)
	object <- try(get(instance$objectname, envir=instance$objectenv), silent=TRUE)
	if ( inherits(object, "try-error") || is.na(value) || value < 1 || value > numPixels(object) ) {
		.guiState$busy <- FALSE
		return(FALSE)
	}
	pixel <- value
	coord <- coord(object)[pixel,]
	instance$parameters$pixel <- pixel
	instance$parameters$coord <- coord
	if ( isExtant(instance$windows$main$topWidgets$imageControlWindow) ) {
		svalue(instance$windows$main$widgets$pixel) <- pixel
		svalue(instance$windows$main$widgets$coordX) <- coord[[instance$parameters$sliceDimNames[[1]]]]
		svalue(instance$windows$main$widgets$coordY) <- coord[[instance$parameters$sliceDimNames[[2]]]]
	} else {
		makeImageControls(instance)
	}
	if ( !instance$windows$main$plot.options[["common intensity scale"]] ) {
		intensityRange <- range(intensities(object, pixel=pixel), na.rm=TRUE)
		setPlotIntensityRange(instance, intensityRange)
	}
	.guiState$busy <- FALSE
	TRUE
}

setCoord <- function(instance, value) {
	.guiState$busy <- TRUE
	value <- lapply(value, as.integer)
	object <- try(get(instance$objectname, envir=instance$objectenv), silent=TRUE)
	if ( inherits(object, "try-error") || any(is.na(value)) || any(value < 1) ||
		any(mapply(`>`, value, lapply(coord(object), max))) ||
		is.na(pixels(object, coord=value) ) )
	{
		.guiState$busy <- FALSE
		return(FALSE)
	}
	coord <- value
	pixel <- pixels(object, coord=value)
	instance$parameters$pixel <- pixel
	instance$parameters$coord <- coord
	if ( isExtant(instance$windows$main$topWidgets$imageControlWindow) ) {
		svalue(instance$windows$main$widgets$pixel) <- pixel
		svalue(instance$windows$main$widgets$coordX) <- coord[[instance$parameters$sliceDimNames[[1]]]]
		svalue(instance$windows$main$widgets$coordY) <- coord[[instance$parameters$sliceDimNames[[2]]]]
	} else {
		makeImageControls(instance)
	}
	if ( !instance$windows$main$plot.options[["common intensity scale"]] ) {
		intensityRange <- range(intensities(object, pixel=pixel), na.rm=TRUE)
		setPlotIntensityRange(instance, intensityRange)
	}
	.guiState$busy <- FALSE
	TRUE
}

setCoordRange <- function(instance, value) {
	.guiState$busy <- TRUE
	value <- sapply(value, as.integer)
	if ( any(is.na(value)) || value[[1]] >= value[[2]] || value[[3]] >= value[[4]] ) {
		.guiState$busy <- FALSE
		return(FALSE)
	}
	instance$parameters$coordRange <- value
	.guiState$busy <- FALSE
	TRUE
}

setPlotIntensityRange <- function(instance, value) {
	.guiState$busy <- TRUE
	value <- sapply(value, as.numeric)
	if ( any(is.na(value)) ) {
		.guiState$busy <- FALSE
		return(FALSE)
	}
	intensityRange <- value
	instance$parameters$plot.intensityRange <- intensityRange
	if ( isExtant(instance$windows$main$topWidgets$plotControlWindow) ) {
		svalue(instance$windows$main$widgets$plotMinIntensity) <- intensityRange[[1]]
		svalue(instance$windows$main$widgets$plotMaxIntensity) <- intensityRange[[2]]
	} else {
		makePlotControls(instance)
	}
	.guiState$busy <- FALSE
	TRUE
}

setImageIntensityRange <- function(instance, value) {
	.guiState$busy <- TRUE
	value <- sapply(value, as.numeric)
	if ( any(is.na(value)) ) {
		.guiState$busy <- FALSE
		return(FALSE)
	}
	intensityRange <- value
	instance$parameters$image.intensityRange <- intensityRange
	if ( isExtant(instance$windows$main$topWidgets$imageControlWindow) ) {
		svalue(instance$windows$main$widgets$imageMinIntensity) <- intensityRange[[1]]
		svalue(instance$windows$main$widgets$imageMaxIntensity) <- intensityRange[[2]]
	} else {
		makeImageControls(instance)
	}
	.guiState$busy <- FALSE
	TRUE
}

setSliceDimNames <- function(instance, value) {
	FALSE
}

setFixCoord <- function(instance, value) {
	FALSE
}



