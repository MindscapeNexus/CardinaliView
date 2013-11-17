
#### handler functions for the main controls ####

makeSliceDimNames <- function(instance, object) {
	allSliceDimNames <- combn(object@metaData[["coordDimNames"]], 2)
	apply(allSliceDimNames, 2, function(n) paste(n, collapse=", "))
}

makeFixCoord <- function(instance, object) {
	whichDim <- which(object@metaData[["coordDimNames"]] %in% instance$parameters$sliceDimNames)
	allFixCoord <- coord(object)[-whichDim]
	allFixCoordOptions <- expand.grid(lapply(allFixCoord, unique))
	apply(allFixCoordOptions, 1, Cardinal:::formatCoord)
}

firstValidCoord <- function(instance, object) {
	fixCoord <- as.data.frame(as.list(instance$parameters$fixCoord))
	validCoord <- merge(fixCoord, coord(object), all.x=TRUE)
	validCoord <- validCoord[object@metaData[["coordDimNames"]]]
	validCoord[1,]
}

#### handler functions for the main controls ####

handlerKillWindow <- function(instance, windowname) {
	function(h, ...) {
		killWindow(instance, windowname)
	}
}

handlePlotCommonIntensity <- function(h, ...) {
	if ( .guiState$busy ) return()
	h$action$windows$main$plot.options[["common intensity scale"]] <- svalue(h$obj)
	if ( svalue(h$obj) ) {
		intensityRange <- c(h$action$parameters$minIntensity,
			h$action$parameters$maxIntensity)
		setPlotIntensityRange(h$action, intensityRange)
	} else {
		setPixel(h$action, h$action$parameters$pixel)
	}
	updatePlots(h$action)
}

handlePlotMinIntensity <- function(h, ...) {
	if ( .guiState$busy ) return()
	intensityRange <- h$action$parameters$plot.intensityRange
	intensityRange[[1]] <- svalue(h$obj)
	if ( setPlotIntensityRange(h$action, intensityRange) ) updatePlots(h$action)
}

handlePlotMaxIntensity <- function(h, ...) {
	if ( .guiState$busy ) return()
	intensityRange <- h$action$parameters$plot.intensityRange
	intensityRange[[2]] <- svalue(h$obj)
	if ( setPlotIntensityRange(h$action, intensityRange) ) updatePlots(h$action)
}

handleFilter <- function(h, ...) {
	if ( .guiState$busy ) return()
	h$action$windows$main$filter <- svalue(h$obj)
	updatePlots(h$action)
}

handleFilterWindow <- function(h, ...) {
	if ( .guiState$busy ) return()
	h$action$windows$main$filter.window <- svalue(h$obj)
	updatePlots(h$action)
}

handleMass <- function(h, ...) {
	if ( .guiState$busy ) return()
	if ( setMZ(h$action, svalue(h$obj)) ) updateAll(h$action)
}

handleMassMin <- function(h, ...) {
	if ( .guiState$busy ) return()
	mzRange <- h$action$parameters$mzRange
	mzRange[[1]] <- svalue(h$obj)
	if ( setMZRange(h$action, mzRange) ) updatePlots(h$action)
}

handleMassMax <- function(h, ...) {
	if ( .guiState$busy ) return()
	mzRange <- h$action$parameters$mzRange
	mzRange[[2]] <- svalue(h$obj)
	if ( setMZRange(h$action, mzRange) ) updatePlots(h$action)
}

handleMassPrev <- function(h, ...) {
	if ( .guiState$busy ) return()
	feature <- h$action$parameters$feature - 1
	if ( setFeature(h$action, feature) ) updateAll(h$action)
}

handleMassNext <- function(h, ...) {
	if ( .guiState$busy ) return()
	feature <- h$action$parameters$feature + 1
	if ( setFeature(h$action, feature) ) updateAll(h$action)
}

handleImageCommonIntensity <- function(h, ...) {
	if ( .guiState$busy ) return()
	h$action$windows$main$image.options[["common intensity scale"]] <- svalue(h$obj)
	if ( svalue(h$obj) ) {
		intensityRange <- c(h$action$parameters$minIntensity,
			h$action$parameters$maxIntensity)
		setImageIntensityRange(h$action, intensityRange)
	} else {
		setMZ(h$action, h$action$parameters$mz)
	}
	updateImages(h$action)
}

handleImageMinIntensity <- function(h, ...) {
	if ( .guiState$busy ) return()
	intensityRange <- h$action$parameters$image.intensityRange
	intensityRange[[1]] <- svalue(h$obj)
	if ( setImageIntensityRange(h$action, intensityRange) ) updateImages(h$action)
}

handleImageMaxIntensity <- function(h, ...) {
	if ( .guiState$busy ) return()
	intensityRange <- h$action$parameters$image.intensityRange
	intensityRange[[2]] <- svalue(h$obj)
	if ( setImageIntensityRange(h$action, intensityRange) ) updateImages(h$action)
}

handleContrast <- function(h, ...) {
	if ( .guiState$busy ) return()
	h$action$windows$main$contrast <- svalue(h$obj)
	updateImages(h$action)
}

handleSmoothing <- function(h, ...) {
	if ( .guiState$busy ) return()
	h$action$windows$main$smoothing <- svalue(h$obj)
	updateImages(h$action)
}

handleSmoothingWindow <- function(h, ...) {
	if ( .guiState$busy ) return()
	h$action$windows$main$smoothing.window <- svalue(h$obj)
	updateImages(h$action)
}

handleInterpolate <- function(h, ...) {
	if ( .guiState$busy ) return()
	h$action$windows$main$interpolate <- svalue(h$obj)
	updateImages(h$action)
}

handleInterpolateRes <- function(h, ...) {
	if ( .guiState$busy ) return()
	h$action$windows$main$interpolate.xres <- svalue(h$obj)
	updateImages(h$action)
}

handlePixel <- function(h, ...) {
	if ( .guiState$busy ) return()
	if ( setPixel(h$action, svalue(h$obj)) ) updateAll(h$action)
}

handleCoordX <- function(h, ...) {
	if ( .guiState$busy ) return()
	coord <- h$action$parameters$coord
	coord[[h$action$parameters$sliceDimNames[[1]]]] <- svalue(h$obj)
	if ( setCoord(h$action, coord) ) updateAll(h$action)
}

handleCoordY <- function(h, ...) {
	if ( .guiState$busy ) return()
	coord <- h$action$parameters$coord
	coord[[h$action$parameters$sliceDimNames[[2]]]] <- svalue(h$obj)
	if ( setCoord(h$action, coord) ) updateAll(h$action)
}

handleSelectMass <- function(h, ...) {
	if ( .guiState$busy ) return()
	active <- svalue(h$action$windows$main$widgets$plotActiveDevice)
	setDevice(h$action, active, "plot")
	repeat {
		mz <- locatorSelectPoint(h$action)$x
		if ( setMZ(h$action, mz) ) break
	}
	updateAll(h$action)
}

handleSelectPlotZoom <- function(h, ...) {
	if ( .guiState$busy ) return()
	active <- svalue(h$action$windows$main$widgets$plotActiveDevice)
	setDevice(h$action, active, "plot")
	px <- NULL
	for ( i in 1:2 ) {
		px[[i]] <- locatorSelectPoint(h$action)
		if ( h$action$windows$main$plot.options[["common intensity scale"]] ) {
			abline(v=px[[i]]$x, lty=4, lwd=0.5, col="blue")
			abline(h=px[[i]]$y, lty=4, lwd=0.5, col="blue")
		} else {
			abline(v=px[[i]]$x, lty=4, lwd=0.5, col="blue")
		}
	}
	mzRange <- sort(c(px[[1]]$x, px[[2]]$x))
	if ( h$action$windows$main$plot.options[["common intensity scale"]] ) {
		intensityRange <- sort(c(px[[1]]$y, px[[2]]$y))
	} else {
		intensityRange <- h$action$parameters$plot.intensityRange
	}
	if ( setMZRange(h$action, mzRange) && setPlotIntensityRange(h$action, intensityRange) ) {
		updatePlots(h$action)		
	}
}

handleBrowsePeaks <- function(h, ...) {
	if ( .guiState$busy ) return()
	object <- try(get(h$action$objectname, envir=h$action$objectenv), silent=TRUE)
	if ( inherits(object, "try-error") ) return()
	if ( h$action$windows$peaks$open ) {
		focus(h$action$windows$peaks$topWidgets$controlWindow) <- TRUE
	} else if ( nrow(object@peaks@peaks) > 0 ) {
		makePeaksControl(h$action)
		h$action$windows$peaks$open <- TRUE
		focus(h$action$windows$peaks$topWidgets$controlWindow) <- TRUE
	} else {
		galert(message="No peaks loaded.", title="Error")
	}
}

handlePeaksTable <- function(h, ...) {
	mz <- as.numeric(svalue(h$obj))
	setMZ(h$action, mz)
	updateAll(h$action)
}

handlerPeaksExport <- function(instance, peaks) {
	function(h, ...) {
		name <- gsub(pattern="\\.", replacement="_", x=instance$objectname)
		file <- gfile(text="Export peaks to CSV file", type="save",
			initialfilename=paste(name, "_peaks.csv", sep=""),
			filter=list("CSV files"=list(patterns="*.csv")))
		if ( is.na(file) ) return()
		if ( !grepl(pattern=".csv", x=file, ignore.case=TRUE) ) {
			file <- paste(file, ".csv", sep="")
		}
		tryCatch(write.csv(peaks@peaks, file=file),
			error=function(e) gmessage(message="Failed to save file.", title="Error"))
	}
}

handlePlotZoomIn <- function(h, ...) {
	if ( .guiState$busy ) return()
	mz <- h$action$parameters$mz
	mzRange <- h$action$parameters$mzRange
	mzRange <- (mzRange + mz) / 2
	if ( setMZRange(h$action, mzRange) ) updatePlots(h$action)
}

handlePlotZoomOut <- function(h, ...) {
	if ( .guiState$busy ) return()
	mz <- h$action$parameters$mz
	mzRange <- h$action$parameters$mzRange
	mzRange <- (2 * mzRange) - mz
	if ( setMZRange(h$action, mzRange) ) updatePlots(h$action)
}

handlePlotZoomFit <- function(h, ...) {
	if ( .guiState$busy ) return()
	object <- try(get(h$action$objectname, envir=h$action$objectenv), silent=TRUE)
	if ( inherits(object, "try-error") ) return()
	mzRange <- range(mz(object))
	intensityRange <- range(intensities(object, pixel=h$action$parameters$pixel), na.rm=TRUE)
	if ( setMZRange(h$action, mzRange) && setPlotIntensityRange(h$action, intensityRange) ) {
		updatePlots(h$action)
	}
}

handleImageZoomIn <- function(h, ...) {
	if ( .guiState$busy ) return()
	xy <- h$action$parameters$coord[h$action$parameters$sliceDimNames]
	xy <- rep(unlist(xy), each=2)
	coordRange <- h$action$parameters$coordRange
	coordRange <- (coordRange + xy) / 2
	if ( setCoordRange(h$action, coordRange) ) updateImages(h$action)
}

handleImageZoomOut <- function(h, ...) {
	if ( .guiState$busy ) return()
	xy <- h$action$parameters$coord[h$action$parameters$sliceDimNames]
	xy <- rep(unlist(xy), each=2)
	coordRange <- h$action$parameters$coordRange
	coordRange <- (2 * coordRange) - xy
	if ( setCoordRange(h$action, coordRange) ) updateImages(h$action)
}

handleImageZoomFit <- function(h, ...) {
	if ( .guiState$busy ) return()
	object <- try(get(h$action$objectname, envir=h$action$objectenv), silent=TRUE)
	if ( inherits(object, "try-error") ) return()
	sliceDimNames <- h$action$parameters$sliceDimNames
	coordRange <- c(range(coord(object)[sliceDimNames[[1]]]),
			range(coord(object)[sliceDimNames[[2]]]))
	intensityRange <- range(intensities(object, mz=h$action$parameters$mz), na.rm=TRUE)
	if ( setCoordRange(h$action, coordRange) && setImageIntensityRange(h$action, intensityRange) ) {
		updateImages(h$action)
	}
}

handleBrowseSlices <- function(h, ...) {
	if ( .guiState$busy ) return()
	object <- try(get(h$action$objectname, envir=h$action$objectenv), silent=TRUE)
	if ( inherits(object, "try-error") ) return()
	if ( h$action$windows$slices$open ) {
		focus(h$action$windows$slices$topWidgets$controlWindow) <- TRUE
	} else if ( ncol(coord(object)) > 2 ) {
		makeSlicesControl(h$action)
		h$action$windows$slices$open <- TRUE
		focus(h$action$windows$slices$topWidgets$controlWindow) <- TRUE
	} else {
		galert(message="Not enough dimensions.", title="Error")
	}
}

handleSelectPixel <- function(h, ...) {
	if ( .guiState$busy ) return()
	active <- svalue(h$action$windows$main$widgets$imageActiveDevice)
	setDevice(h$action, active, "image")
	coord <- h$parameters$coord
	repeat {
		coord[h$action$parameters$sliceDimNames] <- lapply(locatorSelectPixelCoord(h$action), round)
		if ( setCoord(h$action, coord) ) break
	}
	updateAll(h$action)
}

handleSelectImageZoom <- function(h, ...) {
	if ( .guiState$busy ) return()
	active <- svalue(h$action$windows$main$widgets$imageActiveDevice)
	setDevice(h$action, active, "image")
	px <- NULL
	for ( i in 1:2 ) {
		px[[i]] <- locatorSelectPixelCoord(h$action)
		abline(v=px[[i]]$x, lty=2, col="white")
		abline(h=px[[i]]$y, lty=2, col="white")
	}
	xRange <- sort(c(px[[1]]$x, px[[2]]$x))
	yRange <- sort(c(px[[1]]$y, px[[2]]$y))
	coordRange <- c(xRange, yRange)
	if ( setCoordRange(h$action, coordRange) ) updateImages(h$action)
}

handleSliceDimNames <- function(h, ...) {
	if ( .guiState$busy ) return()
	object <- try(get(h$action$objectname, envir=h$action$objectenv), silent=TRUE)
	if ( inherits(object, "try-error") ) return()
	coordDimNames <- object@metaData[["coordDimNames"]]
	allSliceDimNames <- combn(coordDimNames, 2)
	allsliceDimNamesOptions <- makeSliceDimNames(h$action, object)
	sliceDimNamesOption <- svalue(h$action$windows$slices$widgets$chooseSliceDimNames)
	whichCombo <- which(allsliceDimNamesOptions %in% sliceDimNamesOption)
	h$action$parameters$sliceDimNames <- allSliceDimNames[,whichCombo]
	whichDim <- which(coordDimNames %in% h$action$parameters$sliceDimNames)
	h$action$parameters$fixCoord <- as.list(coord(object)[1,-whichDim,drop=FALSE])
	delete(h$action$windows$slices$widgets$chooseFixCoordFrame,
		h$action$windows$slices$widgets$chooseFixCoord)
	h$action$windows$slices$widgets$chooseFixCoord <- gcombobox(items=makeFixCoord(h$action, object),
		container=h$action$windows$slices$widgets$chooseFixCoordFrame, expand=TRUE,
		handler=handleFixCoord,
		action=h$action)
	setCoord(h$action, firstValidCoord(h$action, object))
	handleImageZoomFit(h, ...)
	updatePlots(h$action)
}

handleFixCoord <- function(h, ...) {
	if ( .guiState$busy ) return()
	object <- try(get(h$action$objectname, envir=h$action$objectenv), silent=TRUE)
	if ( inherits(object, "try-error") ) return()
	whichDim <- which(object@metaData[["coordDimNames"]] %in% h$action$parameters$sliceDimNames)
	allFixCoord <- coord(object)[-whichDim]
	allFixCoord <- expand.grid(lapply(allFixCoord, unique))
	allFixCoordOptions <- makeFixCoord(h$action, object)
	fixCoordOption <- svalue(h$action$windows$slices$widgets$chooseFixCoord)
	whichCombo <- which(allFixCoordOptions %in% fixCoordOption)
	h$action$parameters$fixCoord <- as.list(allFixCoord[whichCombo,,drop=FALSE])
	setCoord(h$action, firstValidCoord(h$action, object))
	handleImageZoomFit(h, ...)
	updatePlots(h$action)
}

handleMainDestroy <- function(h, ...) {
	if ( !isExtant(h$action$windows$main$topWidgets$plotControlWindow) && !isExtant(h$action$windows$main$topWidgets$imageControlWindow) )
	{
		killInstance(h$action)
	}
}


