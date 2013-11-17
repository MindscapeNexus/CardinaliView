
#### functions for handling pre-processing ####

loadProcessedDataset <- function(instance, objectname, objectenv, newInstance=FALSE) {
	mz <- instance$parameters$mz
	pixel <- instance$parameters$pixel
	galert("Loading processed dataset...", title="Loading")
	Sys.sleep(0.1)
	if ( newInstance ) {
		instance <- newInstance(objectname, .userEnvironment)
		instance <- initializeInstance(instance)
	} else {
		instance <- pushInstance(instance, objectname, objectenv)
		setDefaults(instance)
	}
	setMZ(instance, mz)
	setPixel(instance, pixel)
	updateAll(instance)
}

updateSelectedPeaksList <- function(instance) {
	delete(instance$windows$selectPeaks$widgets$peaksFrame,
		instance$windows$selectPeaks$widgets$peaksChooser)
	instance$windows$selectPeaks$widgets$peaksChooser <- gcombobox(items=c("<none>",
		ls.MSPeakFrame(.userEnvironment)), container=instance$windows$selectPeaks$widgets$peaksFrame, expand=TRUE)
}

updateSelectedPeaksTable <- function(instance) {
	delete(instance$windows$selectPeaks$widgets$selectedPeaksFrame,
		instance$windows$selectPeaks$widgets$selectedPeaks)
	instance$windows$selectPeaks$widgets$selectedPeaks <- gtable(instance$windows$selectPeaks$peaks@peaks,
		chosencol=1, multiple=TRUE, container=instance$windows$selectPeaks$widgets$selectedPeaksFrame, expand=TRUE,
		handler=handleSelectPeaksTable,
		action=instance)
	delete(instance$windows$selectPeaks$widgets$numPeaksGroup,
		instance$windows$selectPeaks$widgets$numPeaks)
	instance$windows$selectPeaks$widgets$numPeaks <- glabel(text=paste("number of peaks:",
		nrow(instance$windows$selectPeaks$peaks@peaks)),
		container=instance$windows$selectPeaks$widgets$numPeaksGroup, expand=TRUE)
}

#### functions for handling pre-processing ####

handleNormalizeToTIC <- function(h, ...) {
	object <- try(get(h$action$objectname, envir=h$action$objectenv), silent=TRUE)
	if ( inherits(object, "try-error") ) {
		gmessage(message=paste("Could not find object '", h$action$objectname, "'.", sep=""), title="Error")
		return()
	}
	freezeGUI(TRUE)
	object <- standardizeTotalIonCurrent(object)
	freezeGUI(FALSE)
	assign(h$action$objectname, object, envir=h$action$objectenv)
	intensityRange <- c(min(object@spectra$spectra, na.rm=TRUE),
		max(object@spectra$spectra, na.rm=TRUE))
	h$action$parameters$minIntensity <- intensityRange[[1]]
	h$action$parameters$maxIntensity <- intensityRange[[2]]
	setPixel(h$action, h$action$parameters$pixel)
	setMZ(h$action, h$action$parameters$mz)
	updateAll(h$action)
}

handleRemoveNoiseMethod <- function(h, ...) {
	if ( h$action$windows$removeNoise$open ) {
		h$action$windows$removeNoise$method <- svalue(h$obj)
		updateProcessingPlots(h$action)
	}
}

handleRemoveNoiseWindow <- function(h, ...) {
	if ( h$action$windows$removeNoise$open ) {
		h$action$windows$removeNoise$window <- svalue(h$obj)
		updateProcessingPlots(h$action)
	}
}

handleRemoveNoiseProcess <- function(h, ...) {
	object <- try(get(h$action$objectname, envir=h$action$objectenv), silent=TRUE)
	if ( inherits(object, "try-error") ) {
		gmessage(message=paste("Could not find object '", h$action$objectname, "'.", sep=""), title="Error")
		return()
	}
	freezeGUI(TRUE)
	object <- removeNoise(object,
		method=h$action$windows$removeNoise$method,
		window=h$action$windows$removeNoise$window)
	freezeGUI(FALSE)
	newInstance <- svalue(h$action$windows$removeNoise$widgets$openNewInstance)
	objectname <- svalue(h$action$windows$removeNoise$widgets$newObjectName)
	assign(objectname, object, envir=.userEnvironment)
	killWindow(h$action, "removeNoise")
	loadProcessedDataset(h$action, objectname, .userEnvironment, newInstance=newInstance)
}

handleRemoveBaselineMethod <- function(h, ...) {
	if ( h$action$windows$removeBaseline$open ) {
		h$action$windows$removeBaseline$dots.method <- svalue(h$obj)
		updateProcessingPlots(h$action)
	}
}

handleRemoveBaselineChoose <- function(h, ...) {
	if ( h$action$windows$removeBaseline$open ) {
		h$action$windows$removeBaseline$dots.choose <- svalue(h$obj)
		updateProcessingPlots(h$action)
	}
}

handleRemoveBaselineBlocks <- function(h, ...) {
	if ( h$action$windows$removeBaseline$open ) {
		h$action$windows$removeBaseline$dots.blocks <- svalue(h$obj)
		updateProcessingPlots(h$action)
	}
}

handleRemoveBaselinePreserveLevel <- function(h, ...) {
	if ( h$action$windows$removeBaseline$open ) {
		h$action$windows$removeBaseline$dots.preserve.level <- svalue(h$obj)
		updateProcessingPlots(h$action)
	}
}

handleRemoveBaselineProcess <- function(h, ...) {
	object <- try(get(h$action$objectname, envir=h$action$objectenv), silent=TRUE)
	if ( inherits(object, "try-error") ) {
		gmessage(message=paste("Could not find object '", h$action$objectname, "'.", sep=""), title="Error")
		return()
	}
	freezeGUI(TRUE)
	object <- removeBaseline(object,
		method=h$action$windows$removeBaseline$method,
		interp1=h$action$windows$removeBaseline$dots.method,
		blocks=h$action$windows$removeBaseline$dots.blocks)
	freezeGUI(FALSE)
	newInstance <- svalue(h$action$windows$removeBaseline$widgets$openNewInstance)
	objectname <- svalue(h$action$windows$removeBaseline$widgets$newObjectName)
	assign(objectname, object, envir=.userEnvironment)
	killWindow(h$action, "removeBaseline")
	loadProcessedDataset(h$action, objectname, .userEnvironment, newInstance=newInstance)
}

handleDetectPeaksMethod <- function(h, ...) {
	if ( h$action$windows$detectPeaks$open ) {
		h$action$windows$detectPeaks$dots.method <- svalue(h$obj)
		updateProcessingPlots(h$action)
	}
}

handleDetectPeaksBlocks <- function(h, ...) {
	if ( h$action$windows$detectPeaks$open ) {
		h$action$windows$detectPeaks$dots.blocks <- svalue(h$obj)
		updateProcessingPlots(h$action)
	}
}

handleDetectPeaksSpan <- function(h, ...) {
	if ( h$action$windows$detectPeaks$open ) {
		h$action$windows$detectPeaks$dots.span <- svalue(h$obj)
		updateProcessingPlots(h$action)
	}
}

handleDetectPeaksSNR <- function(h, ...) {
	if ( h$action$windows$detectPeaks$open ) {
		h$action$windows$detectPeaks$dots.snr <- svalue(h$obj)
		updateProcessingPlots(h$action)
	}
}

handleDetectPeaksSpectrum <- function(h, ...) {
	if ( h$action$windows$detectPeaks$open ) {
		h$action$windows$detectPeaks$spectrum <- svalue(h$obj)
		updateProcessingPlots(h$action)
	}
}

handleDetectPeaksBrowse <- function(h, ...) {
	pixel <- switch(h$action$windows$detectPeaks$spectrum,
		"pixel"=1,
		"mean"=2,
		"max"=3)
	peaks <- detectPeaks(h$action$windows$detectPeaks$object, pixel=pixel,
		method=h$action$windows$detectPeaks$method,
		noise=h$action$windows$detectPeaks$dots.method,
		blocks=h$action$windows$detectPeaks$dots.blocks,
		span=h$action$windows$detectPeaks$dots.span,
		snr=h$action$windows$detectPeaks$dots.snr)[[1]]
	makeDetectPeaksTable(h$action, peaks)
}

handleDetectPeaksBatch <- function(h, ...) {
	if ( !h$action$windows$batchPeaks$open ) {
		makeBatchPeaksControl(h$action)
		h$action$windows$batchPeaks$open <- TRUE
	}
	focus(h$action$windows$batchPeaks$topWidgets$dialogWindow) <- TRUE
}

handleDetectPeaksSave <- function(h, ...) {
	pixel <- switch(h$action$windows$detectPeaks$spectrum,
		"pixel"=1,
		"mean"=2,
		"max"=3)
	peaks <- detectPeaks(h$action$windows$detectPeaks$object, pixel=pixel,
		method=h$action$windows$detectPeaks$method,
		noise=h$action$windows$detectPeaks$dots.method,
		blocks=h$action$windows$detectPeaks$dots.blocks,
		span=h$action$windows$detectPeaks$dots.span,
		snr=h$action$windows$detectPeaks$dots.snr)[[1]]
	peaksname <- svalue(h$action$windows$detectPeaks$widgets$newObjectName)
	assign(peaksname, peaks, envir=.userEnvironment)
	galert(message="An object of class 'MSPeakFrame' has been placed in the user environment.", title="Done")
	killWindow(h$action, "detectPeaks")
}

handleDetectPeaksTable <- function(h, ...) {
	mz <- as.numeric(svalue(h$obj))
	setMZ(h$action, mz)
	updateAll(h$action)
}

handlerDetectPeaksExport <- function(instance, peaks) {
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

handleBatchPeaksSearchPercent <- function(h, ...) {
	if ( h$action$windows$batchPeaks$open ) {
		h$action$windows$batchPeaks$search.percent <- svalue(h$obj)
	}
}

handleBatchPeaksMinfreqPercent <- function(h, ...) {
	if ( h$action$windows$batchPeaks$open ) {
		h$action$windows$batchPeaks$minfreq.percent <- svalue(h$obj)
	}
}

handleBatchPeaksAlignDiffMax <- function(h, ...) {
	if ( h$action$windows$batchPeaks$open ) {
		h$action$windows$batchPeaks$align.diff.max <- svalue(h$obj)
	}
}

handleBatchPeaksAlignDiffMax <- function(h, ...) {
	if ( h$action$windows$batchPeaks$open ) {
		h$action$windows$batchPeaks$align.units <- svalue(h$obj)
	}
}

handleBatchPeaksProcess <- function(h, ...) {
	object <- try(get(h$action$objectname, envir=h$action$objectenv), silent=TRUE)
	if ( inherits(object, "try-error") ) {
		gmessage(message=paste("Could not find object '", h$action$objectname, "'.", sep=""), title="Error")
		return()
	}
	pixel <- makeCombinedROI(h$action, "batchPeaks")
	if ( is.null(pixel) ) {
		pixel <- 1:numPixels(object)
	} else {
		pixel <- which(pixel)
	}
	peaksname <- svalue(h$action$windows$batchPeaks$widgets$newObjectName)
	search.percent <- h$action$windows$batchPeaks$search.percent
	search.length <- ceiling((search.percent / 100) * length(pixel))
	minfreq.percent <- h$action$windows$batchPeaks$minfreq.percent
	minfreq.length <- ceiling((minfreq.percent / 100) * search.length)
	which <- seq(from=1, to=length(pixel), length.out=search.length)
	freezeGUI(TRUE)
	peaklist <- detectPeaks(object, pixel=pixel[which], method="snr",
		noise=h$action$windows$detectPeaks$dots.method,
		blocks=h$action$windows$detectPeaks$dots.blocks,
		span=h$action$windows$detectPeaks$dots.span,
		snr=h$action$windows$detectPeaks$dots.snr)
	assign(paste(peaksname, "peaklist", sep=""), peaklist, envir=.userEnvironment)
	alignment <- alignPeaks(peaklist, object, method="diff",
		diff.max=h$action$windows$batchPeaks$align.diff.max,
		units=h$action$windows$batchPeaks$align.units)
	assign(paste(peaksname, "alignment", sep=""), alignment, envir=.userEnvironment)
	peaks <- mergePeaks(alignment, length.min=minfreq.length)
	assign(peaksname, peaks, envir=.userEnvironment)
	freezeGUI(FALSE)
	killWindow(h$action, "detectPeaks")
	killWindow(h$action, "batchPeaks")
	makeDetectPeaksTable(h$action, peaks)
	galert(message="An object of class 'MSPeakFrame' has been placed in the user environment.", title="Done")
}

handleSelectPeaksLoad <- function(h, ...) {
	peaksname <- svalue(h$action$windows$selectPeaks$widgets$peaksChooser)
	if ( peaksname == "<none>" ) return()
	peaks <- try(get(peaksname, envir=.userEnvironment), silent=TRUE)
	if ( inherits(peaks, "try-error") ) peaks <- Cardinal:::emptyMSPeakFrame()
	h$action$windows$selectPeaks$peaks <- peaks
	updateSelectedPeaksTable(h$action)
	updateProcessingPlots(h$action)
}

handleSelectPeaksPreview <- function(h, ...) {
	object <- try(get(h$action$objectname, envir=h$action$objectenv), silent=TRUE)
	if ( inherits(object, "try-error") ) {
		gmessage(message=paste("Could not find object '", h$action$objectname, "'.", sep=""), title="Error")
		return()
	}
	peaksname <- svalue(h$action$windows$selectPeaks$widgets$peaksChooser)
	if ( peaksname == "<none>" ) return()
	peaks <- try(get(peaksname, envir=.userEnvironment), silent=TRUE)
	if ( inherits(peaks, "try-error") ) peaks <- Cardinal:::emptyMSPeakFrame()
	if ( nrow(peaks@peaks) > 0 ) {
		setDevice(h$action, "main", "plot")
		peaks <- resampleSpectra(object, peaks=peaks, pixel=h$action$parameters$pixel)
		plot(peaks, pixel=1, col="red", add=TRUE)
	}
}

handleSelectPeaksAdd <- function(h, ...) {
	peaksname <- svalue(h$action$windows$selectPeaks$widgets$peaksChooser)
	if ( peaksname == "<none>" ) return()
	peaks <- try(get(peaksname, envir=.userEnvironment), silent=TRUE)
	if ( inherits(peaks, "try-error") ) peaks <- Cardinal:::emptyMSPeakFrame()
	h$action$windows$selectPeaks$peaks <- poolPeaks(h$action$windows$selectPeaks$peaks, peaks)
	updateSelectedPeaksTable(h$action)
	updateProcessingPlots(h$action)
}

handleSelectPeaksRemove <- function(h, ...) {
	peaks <- svalue(h$action$windows$selectPeaks$widgets$selectedPeaks)
	remove <- sapply(peaks, function(p) which(p == h$action$windows$selectPeaks$peaks$mz))
	h$action$windows$selectPeaks$peaks <- h$action$windows$selectPeaks$peaks[-remove,]
	updateSelectedPeaksTable(h$action)
	updateProcessingPlots(h$action)
}

handleSelectPeaksTable <- function(h, ...) {
	mz <- as.numeric(svalue(h$obj)[[1]])
	setMZ(h$action, mz)
	updateAll(h$action)
}

handleSelectPeaksSpectrum <- function(h, ...) {
	h$action$windows$selectPeaks$spectrum <- svalue(h$obj)
	updateProcessingPlots(h$action)
}

handleSelectPeaksSelect <- function(h, ...) {
	object <- h$action$windows$selectPeaks$object
	pixel <- 1
	galert(message="Select peak boundaries on an MS image and press ESC when done.", title="Select peaks")
	if ( getOption("guiToolkit") == "RGtk2" ) {
		for ( i in seq_along(h$action$windows) ) {
			for ( j in seq_along(h$action$windows[[i]]$gtkWidgets) ) {
				if ( grepl(pattern="plot", x=names(h$action$windows[[i]]$gtkWidgets[j])) ) {
					widget <- h$action$windows[[i]]$gtkWidgets[[j]]$child
					widget$getWindow()$setCursor(gdkCursorNew(GdkCursorType["hand1"]))
				}
			}
		}
		.guiState$nextCallback <- function() {
			loc <- list(x=grconvertX(.guiState$pxSelect$x, from="device", to="user"),
					y=grconvertY(.guiState$pxSelect$y, from="device", to="user"))
			bounds <- matrix(loc$x, ncol=2, byrow=TRUE)
			bounds <- apply(bounds, 1, sort)
			peaks <- Cardinal:::selectPeaksHelper(object, pixel=1,
				lbound=bounds[1,], ubound=bounds[2,])
			h$action$windows$selectPeaks$peaks <- poolPeaks(peaks,
				h$action$windows$selectPeaks$peaks)
			updateSelectedPeaksTable(h$action)
			updateProcessingPlots(h$action)
			NULL
		}
		.guiState$isSelectingPeakBounds <- TRUE
		.guiState$initiatingSelectPeakBounds <- TRUE
	} else {
		setDevice(h$action, svalue(h$action$windows$main$widgets$plotActiveDevice), "plot")
		peaks <- selectPeaks(object, pixel=1, silent=TRUE)
		h$action$windows$selectPeaks$peaks <- poolPeaks(peaks,
				h$action$windows$selectPeaks$peaks)
		updateSelectedPeaksTable(h$action)
		updateProcessingPlots(h$action)
	}
}

handleSelectPeaksSave <- function(h, ...) {
	peaksname <- svalue(h$action$windows$selectPeaks$widgets$newObjectName)
	assign(peaksname, h$action$windows$selectPeaks$peaks, envir=.userEnvironment)
	galert(message="An object of class 'MSPeakFrame' has been placed in the user environment.", title="Done")
	updateSelectedPeaksList(h$action)
}

handleSelectPeaksExport <- function(h, ...) {
	name <- gsub(pattern="\\.", replacement="_", x=h$action$objectname)
	file <- gfile(text="Export peaks to CSV file", type="save",
		initialfilename=paste(name, "_peaks.csv", sep=""),
		filter=list("CSV files"=list(patterns="*.csv")))
	if ( is.na(file) ) return()
	if ( !grepl(pattern=".csv", x=file, ignore.case=TRUE) ) {
		file <- paste(file, ".csv", sep="")
	}
	tryCatch(write.csv(h$action$windows$selectPeaks$peaks@peaks, file=file),
		error=function(e) gmessage(message="Failed to save file.", title="Error"))
}

handleBinPeaks <- function(h, ...) {
	if ( h$action$windows$bin$open ) {
		peaks <- try(get(svalue(h$obj), envir=.userEnvironment), silent=TRUE)
		if ( inherits(peaks, "try-error") ) peaks <- NA
		h$action$windows$bin$peaks <- peaks
		updateProcessingPlots(h$action)
	}
}

handleBinWidth <- function(h, ...) {
	if ( h$action$windows$bin$open ) {
		h$action$windows$bin$width <- as.numeric(svalue(h$obj))
		updateProcessingPlots(h$action)
	}
}

handleBinOffset <- function(h, ...) {
	if ( h$action$windows$bin$open ) {
		h$action$windows$bin$offset <- as.numeric(svalue(h$obj))
		updateProcessingPlots(h$action)
	}
}

handleBinProcess <- function(h, ...) {
	object <- try(get(h$action$objectname, envir=h$action$objectenv), silent=TRUE)
	if ( inherits(object, "try-error") ) {
		gmessage(message=paste("Could not find object '", h$action$objectname, "'.", sep=""), title="Error")
		return()
	}
	h$action$windows$bin$width <- as.numeric(svalue(h$action$windows$bin$widgets$width))
	h$action$windows$bin$offset <- as.numeric(svalue(h$action$windows$bin$widgets$offset))
	freezeGUI(TRUE)
	if ( isMSPeakFrame(h$action$windows$bin$peaks) ) {
		object <- binSpectra(object, peaks=h$action$windows$bin$peaks)
	} else {
		object <- binSpectra(object, width=h$action$windows$bin$width,
			offset=h$action$windows$bin$offset)
	}
	freezeGUI(FALSE)
	h$action$windows$bin$peaks <- NA
	newInstance <- svalue(h$action$windows$bin$widgets$openNewInstance)
	objectname <- svalue(h$action$windows$bin$widgets$newObjectName)
	assign(objectname, object, envir=.userEnvironment)
	killWindow(h$action, "bin")
	loadProcessedDataset(h$action, objectname, .userEnvironment, newInstance=newInstance)
}

handleResamplePeaks <- function(h, ...) {
	if ( h$action$windows$resample$open ) {
		peaks <- try(get(svalue(h$obj), envir=.userEnvironment), silent=TRUE)
		if ( inherits(peaks, "try-error") ) peaks <- NA
		h$action$windows$resample$peaks <- peaks
		updateProcessingPlots(h$action)
	}
}

handleResampleStep <- function(h, ...) {
	if ( h$action$windows$resample$open ) {
		h$action$windows$resample$step <- as.numeric(svalue(h$obj))
		updateProcessingPlots(h$action)
	}
}

handleResampleOffset <- function(h, ...) {
	if ( h$action$windows$resample$open ) {
		h$action$windows$resample$offset <- as.numeric(svalue(h$obj))
		updateProcessingPlots(h$action)
	}
}

handleResampleProcess <- function(h, ...) {
	object <- try(get(h$action$objectname, envir=h$action$objectenv), silent=TRUE)
	if ( inherits(object, "try-error") ) {
		gmessage(message=paste("Could not find object '", h$action$objectname, "'.", sep=""), title="Error")
		return()
	}
	h$action$windows$resample$step <- as.numeric(svalue(h$action$windows$resample$widgets$step))
	h$action$windows$resample$offset <- as.numeric(svalue(h$action$windows$resample$widgets$offset))
	freezeGUI(TRUE)
	if ( isMSPeakFrame(h$action$windows$resample$peaks) ) {
		object <- resampleSpectra(object, peaks=h$action$windows$resample$peaks)
	} else {
		object <- resampleSpectra(object, step=h$action$windows$resample$step,
			offset=h$action$windows$resample$offset)
	}
	freezeGUI(FALSE)
	h$action$windows$resample$peaks <- NA
	newInstance <- svalue(h$action$windows$resample$widgets$openNewInstance)
	objectname <- svalue(h$action$windows$resample$widgets$newObjectName)
	assign(objectname, object, envir=.userEnvironment)
	killWindow(h$action, "resample")
	loadProcessedDataset(h$action, objectname, .userEnvironment, newInstance=newInstance)
}

