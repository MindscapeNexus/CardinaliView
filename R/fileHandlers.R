
#### functions for file menu handlers ####

loadNewDataset <- function(instance, objectname, objectenv, newInstance=FALSE) {
	galert("Loading dataset...", title="Loading")
	Sys.sleep(0.1)
	if ( newInstance ) {
		killFileDialogs(instance)
		instance <- newInstance(objectname, .userEnvironment)
		instance <- initializeInstance(instance)
	} else {
		instance <- pushInstance(instance, objectname, objectenv)
		setDefaults(instance)
		updateAll(instance)
	}
}

killFileDialogs <- function(instance) {
	if ( !is.null(instance$windows$main$topWidgets$fileDialog) && isExtant(instance$windows$main$topWidgets$fileDialog) ) {
		dispose(instance$windows$main$topWidgets$fileDialog)
	}
	if ( getOption("guiToolkit") == "tcltk" ) {
		tcl("update")
	} else if ( getOption("guiToolkit") == "RGtk2" ) {
		gtkMainIterationDo(blocking=FALSE)
	}
}

#### functions for file menu handlers ####

handleLoadDataset <- function(h, ...) {
	newInstance <- svalue(h$action$windows$main$widgets$openNewInstance)
	objectname <- as.character(svalue(h$action$windows$main$widgets$datasetChooser))
	if ( length(objectname) == 0 || is.na(objectname) ) return()
	loadNewDataset(h$action, objectname, .userEnvironment, newInstance=newInstance)
}

handleLoadPeaks <- function(h, ...) {
	objectname <- as.character(svalue(h$action$windows$main$widgets$peaksChooser))
	if ( length(objectname) == 0 || is.na(objectname) ) return()
	peaks <- get(objectname, envir=.userEnvironment)
	object <- try(get(h$action$objectname, envir=h$action$objectenv), silent=TRUE)
	if ( inherits(object, "try-error") ) return()
	killFileDialogs(h$action)
	if ( !is.null(h$action$windows$peaks$topWidgets$controlWindow) && isExtant(h$action$windows$peaks$topWidgets$controlWindow) ) {
		dispose(h$action$windows$peaks$topWidgets$controlWindow)
	}
	object@peaks <- peaks
	assign(h$action$objectname, object, envir=h$action$objectenv)
	makePeaksControl(h$action)
	h$action$windows$peaks$open <- TRUE
	focus(h$action$windows$peaks$topWidgets$controlWindow) <- TRUE
}

handleLoadSegmentation <- function(h, ...) {
	objectname <- as.character(svalue(h$action$windows$main$widgets$segmentationChooser))
	if ( length(objectname) == 0 || is.na(objectname) ) return()
	h$action$windows$MSImageSegmentation$object <- get(objectname, envir=.userEnvironment)
	h$action$windows$MSImageSegmentation$objectname <- objectname
	killFileDialogs(h$action)
	makeMSImageSegmentationControl(h$action)
	setMSImageSegmentationOptions(h$action, plot.overlay=TRUE, image.mask.missing=TRUE)
	h$action$windows$MSImageSegmentation$open <- TRUE
	updateAnalysisPlots(h$action)
	updateAnalysisImages(h$action)
	focus(h$action$windows$MSImageSegmentation$topWidgets$controlWindow) <- TRUE
}

handleSaveDatasetToRDataFile <- function(h, ...) {
	name <- gsub(pattern="\\.", replacement="_", x=h$action$objectname)
	file <- gfile(text="Save dataset to R Data file", type="save",
		initialfilename=paste(name, ".RData", sep=""),
		filter=list("R Data files"=list(patterns=c("*.RData", "*.rda"))))
	if ( is.na(file) ) return()
	if ( !grepl(pattern=".RData|.rda", x=file, ignore.case=TRUE) ) {
		file <- paste(file, ".RData", sep="")
	}
	tryCatch(do.call(save, list(h$action$objectname, file=file, envir=.userEnvironment)),
		error=function(e) gmessage(message="Failed to save file.", title="Error"))
}

handleSaveSegmentationToRDataFile <- function(h, ...) {
	if ( !h$action$windows$MSImageSegmentation$open || is.null(h$action$windows$MSImageSegmentation$object) ) {
		gmessage(message="No 'MSImageSegmentation' object loaded", title="Error")
		return()
	}
	name <- gsub(pattern="\\.", replacement="_", x=h$action$objectname)
	file <- gfile(text="Save segmentation to R Data file", type="save",
		initialfilename=paste(name, "_segmentation.RData", sep=""),
		filter=list("R Data files"=list(patterns="*.RData")))
	if ( is.na(file) ) return()
	if ( !grepl(pattern=".RData|.rda", x=file, ignore.case=TRUE) ) {
		file <- paste(file, ".RData", sep="")
	}
	tryCatch(do.call(save, list(h$action$windows$MSImageSegmentation$objectname, file=file, envir=.userEnvironment)),
		error=function(e) gmessage(message="Failed to save file.", title="Error"))
}

handleImportFromRDataFile <- function(h, ...) {
	file <- gfile(text="Import dataset from R Data file", type="open",
		filter=list("R Data files"=list(patterns="*.RData")))
	if ( is.na(file) ) return()
	freezeGUI(TRUE)
	iViewMessage("Importing from R Data file...", precedes.progress.output=FALSE)
	tryCatch(load(file=file, envir=.userEnvironment),
		error=function(e) gmessage(message="Failed to load file.", title="Error"))
	freezeGUI(FALSE)
	makeLoadDatasetControl(h$action)
}

handleImportFromImzMLName <- function(h, ...) {
	filepath <- svalue(h$action$windows$main$widgets$importImzMLBrowser)
	file <- strsplit(filepath, split="/")[[1]]
	file <- file[[length(file)]]
	name <- strsplit(file, split=" |-|\\.")[[1]][[1]]
	dup <- grepl(name, ls(.userEnvironment))
	if ( any(dup) ) name <- paste(name, sum(dup), sep=".")
	svalue(h$action$windows$main$widgets$newObjectName) <- name
}

handleImportFromImzMLImport <- function(h, ...) {
	newInstance <- svalue(h$action$windows$main$widgets$openNewInstance)
	filepath <- svalue(h$action$windows$main$widgets$importImzMLBrowser)
	objectname <- svalue(h$action$windows$main$widgets$newObjectName)
	if ( is.na(filepath) || filepath == "Select a file..." ) {
		gmessage(message="Select a file.", title="Error")
		return()
	}
	if ( objectname == "" ) {
		gmessage(message="Provide a name for the object.", title="Error")
		return()
	}
	object <- NULL
	freezeGUI(TRUE)
	iViewMessage("Importing from imzML file...", precedes.progress.output=FALSE)
	tryCatch(object <- readImzML(file=filepath),
		error=function(e) gmessage(message="Failed to load file.", title="Error"))
	freezeGUI(FALSE)
	if ( !is.null(object) ) {
		assign(objectname, object, envir=.userEnvironment)
		loadNewDataset(h$action, objectname, .userEnvironment, newInstance=newInstance)
	}
}

handleImportFromAnalyze7.5Name <- function(h, ...) {
	filepath <- svalue(h$action$windows$main$widgets$importAnalyze7.5Browser)
	file <- strsplit(filepath, split="/")[[1]]
	file <- file[[length(file)]]
	name <- strsplit(file, split=" |-|\\.")[[1]][[1]]
	dup <- grepl(name, ls(.userEnvironment))
	if ( any(dup) ) name <- paste(name, sum(dup), sep=".")
	svalue(h$action$windows$main$widgets$newObjectName) <- name
}

handleImportFromAnalyze7.5Import <- function(h, ...) {
	newInstance <- svalue(h$action$windows$main$widgets$openNewInstance)
	filepath <- svalue(h$action$windows$main$widgets$importAnalyze7.5Browser)
	objectname <- svalue(h$action$windows$main$widgets$newObjectName)
	if ( is.na(filepath) || filepath == "Select a file..." ) {
		gmessage(message="Select a file.", title="Error")
		return()
	}
	if ( objectname == "" ) {
		gmessage(message="Provide a name for the object.", title="Error")
		return()
	}
	file <- strsplit(filepath, split="/|.hdr|.img|.t2m")[[1]]
	object <- NULL
	freezeGUI(TRUE)
	iViewMessage("Importing from Analyze 7.5 file...", precedes.progress.output=FALSE)
	tryCatch(object <- readAnalyze7.5(name=file[length(file)],
		folder=paste(file[-length(file)], collapse="/")),
		error=function(e) gmessage(message="Failed to load file.", title="Error"))
	freezeGUI(FALSE)
	if ( !is.null(object) ) {
		assign(objectname, object, envir=.userEnvironment)
		loadNewDataset(h$action, objectname, .userEnvironment, newInstance=newInstance)
	}
}

handleExportPlotOrImageDestroy <- function(h, ...) {
	if ( !is.null(h$action$windows$main$topWidgets$trellisDialog) && isExtant(h$action$windows$main$topWidgets$trellisDialog) ) {
		dispose(h$action$windows$main$topWidgets$trellisDialog)
	}
	if ( isExtant(h$action$windows$main$topWidgets$fileDialog) ) {
		dispose(h$action$windows$main$topWidgets$fileDialog)
	}
}

handleExportPlotExport <- function(h, ...) {
	filepath <- svalue(h$action$windows$main$widgets$exportPlotsBrowse)
	filetype <- svalue(h$action$windows$main$widgets$exportPlotsFiletype)
	if ( is.na(filepath) || filepath == "Select a file..." ) {
		gmessage(message="Select a file.", title="Error")
		return()
	}
	if ( !grepl(pattern=paste(".", filetype, sep=""), x=filepath, ignore.case=TRUE) ) {
		filepath <- paste(filepath, filetype, sep=".")
	}
	object <- try(get(h$action$objectname, envir=h$action$objectenv), silent=TRUE)
	if ( inherits(object, "try-error") ) return()
	device <- svalue(h$action$windows$main$widgets$exportPlotsDevice)
	width <- svalue(h$action$windows$main$widgets$exportPlotsWidth)
	height <- svalue(h$action$windows$main$widgets$exportPlotsHeight)
	pointsize <- svalue(h$action$windows$main$widgets$exportPlotsPointsize)
	freezeGUI(TRUE)
	updatePlots(h$action)
	if ( filetype == "pdf" ) {
		pdf(file=filepath, pointsize=pointsize)
	} else if ( filetype == "jpeg" ) {
		jpeg(filename=filepath, width=width, height=height, pointsize=pointsize)
	} else if ( filetype == "png" ) {
		png(filename=filepath, width=width, height=height, pointsize=pointsize)
	} else if ( filetype == "tiff" ) {
		tiff(filename=filepath, width=width, height=height, pointsize=pointsize)
	}
	outdev <- dev.cur()
	if ( device == "<trellis plot>" ) {
		iViewMessage("Exporting trellis plot...", precedes.progress.output=FALSE)
		pixel <- h$action$windows$main$trellis.pixel
		pixel <- eval(parse(text=paste("list(", pixel, ")")))
		if ( !all(sapply(pixel, is.numeric)) ) galert(message="Failed to save plot.", title="Error")
		print(trellisPlot(object, pixel=pixel, xlim=h$action$parameters$mzRange,
			ylim=h$action$parameters$plot.intensityRange,
			filter=h$action$windows$main$filter,
			window=h$action$windows$main$filter.window))
	} else {
		iViewMessage("Exporting plot...", precedes.progress.output=FALSE)
		setDevice(h$action, device, "plot")
		dev.copy(which=outdev)
	}
	dev.off(outdev)
	freezeGUI(FALSE)
	handleExportPlotOrImageDestroy(h, ...)
}

handleTrellisPlotOpen <- function(h, ...) {
	makeTrellisPlotControl(h$action)
}

handleTrellisPlotUse <- function(h, ...) {
	h$action$windows$main$trellis.pixel <- svalue(h$action$windows$main$widgets$trellisPlotPixels)
	delete(h$action$windows$main$widgets$exportPlotsDeviceFrame,
		h$action$windows$main$widgets$exportPlotsDevice)
	h$action$windows$main$widgets$exportPlotsDevice <- gcombobox(items=c("<trellis plot>",
		activeDevices(h$action, "plot")), container=h$action$windows$main$widgets$exportPlotsDeviceFrame, expand=TRUE)
	svalue(h$action$windows$main$widgets$exportPlotsDevice) <- "<trellis plot>"
	if ( !is.null(h$action$windows$main$topWidgets$trellisDialog) && isExtant(h$action$windows$main$topWidgets$trellisDialog) ) {
		dispose(h$action$windows$main$topWidgets$trellisDialog)
	}
}

handleExportImageExport <- function(h, ...) {
	filepath <- svalue(h$action$windows$main$widgets$exportImagesBrowse)
	filetype <- svalue(h$action$windows$main$widgets$exportImagesFiletype)
	if ( is.na(filepath) || filepath == "Select a file..." ) {
		gmessage(message="Select a file.", title="Error")
		return()
	}
	if ( !grepl(pattern=paste(".", filetype, sep=""), x=filepath, ignore.case=TRUE) ) {
		filepath <- paste(filepath, filetype, sep=".")
	}
	object <- try(get(h$action$objectname, envir=h$action$objectenv), silent=TRUE)
	if ( inherits(object, "try-error") ) return()
	device <- svalue(h$action$windows$main$widgets$exportImagesDevice)
	width <- svalue(h$action$windows$main$widgets$exportImagesWidth)
	height <- svalue(h$action$windows$main$widgets$exportImagesHeight)
	pointsize <- svalue(h$action$windows$main$widgets$exportImagesPointsize)
	freezeGUI(TRUE)
	updateImages(h$action)
	if ( filetype == "pdf" ) {
		pdf(file=filepath, pointsize=pointsize)
	} else if ( filetype == "jpeg" ) {
		jpeg(filename=filepath, width=width, height=height, pointsize=pointsize)
	} else if ( filetype == "png" ) {
		png(filename=filepath, width=width, height=height, pointsize=pointsize)
	} else if ( filetype == "tiff" ) {
		tiff(filename=filepath, width=width, height=height, pointsize=pointsize)
	}
	outdev <- dev.cur()
	if ( device == "<trellis image>" ) {
		iViewMessage("Exporting trellis image...", precedes.progress.output=FALSE)
		mz <- h$action$windows$main$trellis.mz
		mz <- eval(parse(text=paste("list(", mz, ")")))
		if ( !all(sapply(mz, is.numeric)) ) galert(message="Failed to save plot.", title="Error")
		print(trellisImage(object, mz=mz,
			sliceDimNames=h$action$parameters$sliceDimNames,
			fixCoord=h$action$parameters$fixCoord,
			xlim=h$action$parameters$coordRange[c(1,2)],
			ylim=h$action$parameters$coordRange[c(3,4)],
			zlim=h$action$parameters$image.intensityRange,
			contrast=h$action$windows$main$contrast,
			smoothing=h$action$windows$main$smoothing,
			window=h$action$windows$main$smoothing.window,
			interpolate=h$action$windows$main$interpolate,
			res=h$action$windows$main$interpolate.res))
	} else {
		iViewMessage("Exporting image...", precedes.progress.output=FALSE)
		setDevice(h$action, device, "image")
		dev.copy(which=outdev)
	}
	dev.off(outdev)
	freezeGUI(FALSE)
	handleExportPlotOrImageDestroy(h, ...)
}

handleTrellisImageOpen <- function(h, ...) {
	makeTrellisImageControl(h$action)
}

handleTrellisImageUse <- function(h, ...) {
	h$action$windows$main$trellis.mz <- svalue(h$action$windows$main$widgets$trellisImageMZ)
	delete(h$action$windows$main$widgets$exportImagesDeviceFrame,
		h$action$windows$main$widgets$exportImagesDevice)
	h$action$windows$main$widgets$exportImagesDevice <- gcombobox(items=c("<trellis image>",
		activeDevices(h$action, "image")), container=h$action$windows$main$widgets$exportImagesDeviceFrame, expand=TRUE)
	svalue(h$action$windows$main$widgets$exportImagesDevice) <- "<trellis image>"
	if ( !is.null(h$action$windows$main$topWidgets$trellisDialog) && isExtant(h$action$windows$main$topWidgets$trellisDialog) ) {
		dispose(h$action$windows$main$topWidgets$trellisDialog)
	}
}

