
#### functions for handling analysis ####

setMSImageSegmentationOptions <- function(instance, which=1, plot.overlay=FALSE,
	plot.climits=FALSE, plot.mode="centroids", image.overlay=FALSE, image.true.labels=FALSE,
	image.mask.missing=FALSE, image.mode="classes", transparency=0.5)
{
	instance$windows$MSImageSegmentation$which <- which
	instance$windows$MSImageSegmentation$plot.options$overlay <- plot.overlay
	svalue(instance$windows$MSImageSegmentation$widgets$plotOverlay) <- plot.overlay
	instance$windows$MSImageSegmentation$plot.options$climits <- plot.climits
	svalue(instance$windows$MSImageSegmentation$widgets$plotClimits) <- plot.climits
	instance$windows$MSImageSegmentation$plot.mode <- plot.mode
	svalue(instance$windows$MSImageSegmentation$widgets$plotMode) <- plot.mode
	instance$windows$MSImageSegmentation$image.options$overlay <- image.overlay
	svalue(instance$windows$MSImageSegmentation$widgets$imageOverlay) <- image.overlay
	instance$windows$MSImageSegmentation$image.options$true.labels <- image.true.labels
	svalue(instance$windows$MSImageSegmentation$widgets$imageTrueLabels) <- image.true.labels
	instance$windows$MSImageSegmentation$image.options$mask.missing <- image.mask.missing
	svalue(instance$windows$MSImageSegmentation$widgets$imageMaskMissing) <- image.mask.missing
	instance$windows$MSImageSegmentation$image.mode <- image.mode
	svalue(instance$windows$MSImageSegmentation$widgets$imageMode) <- image.mode
	instance$windows$MSImageSegmentation$transparency <- transparency
	svalue(instance$windows$MSImageSegmentation$widgets$transparency) <- transparency
}

#### functions for handling analysis ####

handlePCACompute <- function(h, ...) {
	object <- try(get(h$action$objectname, envir=h$action$objectenv), silent=TRUE)
	if ( inherits(object, "try-error") ) {
		gmessage(message=paste("Could not find object '", h$action$objectname, "'.", sep=""), title="Error")
		return()
	}
	method <- svalue(h$action$windows$PCA$widgets$method)
	ncomp <- svalue(h$action$windows$PCA$widgets$ncomp)
	standardize <- svalue(h$action$windows$PCA$widgets$standardize)
	freezeGUI(TRUE)
	h$action$windows$PCA$object <- PCA(object, method=method, ncomp=ncomp, standardize=standardize)
	freezeGUI(FALSE)
	name <- svalue(h$action$windows$PCA$widgets$newObjectName)
	h$action$windows$PCA$objectname <- name
	assign(name, h$action$windows$PCA$object, envir=.userEnvironment)
	killWindow(h$action, "PCA")
	window <- makePCAControl(h$action)
	h$action$windows$PCA$ncomp <-1
	h$action$windows$PCA$open <- TRUE
	updateAnalysisPlots(h$action)
	updateAnalysisImages(h$action)
	focus(window) <- TRUE
	galert(message="An object of class 'PCA' has been placed in the user environment.", title="Done")
}

handlePCAncomp <- function(h, ...) {
	if ( h$action$windows$PCA$open ) {
		h$action$windows$PCA$ncomp <- as.integer(svalue(h$obj))
		updateAnalysisPlots(h$action)
		updateAnalysisImages(h$action)
	}
}

handlePCAPlotEigenvalues <- function(h, ...) {
	object <- try(get(h$action$objectname, envir=h$action$objectenv), silent=TRUE)
	if ( inherits(object, "try-error") ) {
		gmessage(message=paste("Could not find object '", h$action$objectname, "'.", sep=""), title="Error")
		return()
	}
	dev.new(title=paste(h$action$name, "PCA eigenvalues", sep=" - "), width=5, height=5)
	comp <- 1:h$action$windows$PCA$object$ncomp
	eigen <- h$action$windows$PCA$object$sdev
	print(plot(comp, eigen, main=paste(object@metaData[["name"]], "PCA", sep=" - "),
		xlab="PC", ylab="eigenvalue", type="b"))
}

handleCompareROI <- function(h, ...) {
	labels <- makeSegmentedROI(h$action, "MSImageSegmentation")
	object <- try(get(h$action$objectname, envir=h$action$objectenv), silent=TRUE)
	if ( inherits(object, "try-error") ) {
		gmessage(message=paste("Could not find object '", h$action$objectname, "'.", sep=""), title="Error")
		return()
	}
	freezeGUI(TRUE)
	h$action$windows$MSImageSegmentation$object <- spatialClassify(object, labels=labels, r=0)
	freezeGUI(FALSE)
	name <- svalue(h$action$windows$MSImageSegmentation$widgets$newObjectName)
	h$action$windows$MSImageSegmentation$objectname <- name
	assign(name, h$action$windows$MSImageSegmentation$object, envir=.userEnvironment)
	killWindow(h$action, "MSImageSegmentation")
	window <- makeMSImageSegmentationControl(h$action)
	setMSImageSegmentationOptions(h$action, plot.overlay=TRUE, image.overlay=TRUE,
		image.true.labels=TRUE)
	h$action$windows$MSImageSegmentation$open <- TRUE
	updateAnalysisPlots(h$action)
	updateAnalysisImages(h$action)
	focus(window) <- TRUE
	galert(message="An object of class 'MSImageSegmentation' has been placed in the user environment.", title="Done")
}

handleSpatialClassify <- function(h, ...) {
	labels <- makeSegmentedROI(h$action, "MSImageSegmentation")
	object <- try(get(h$action$objectname, envir=h$action$objectenv), silent=TRUE)
	if ( inherits(object, "try-error") ) {
		gmessage(message=paste("Could not find object '", h$action$objectname, "'.", sep=""), title="Error")
		return()
	}
	r <- svalue(h$action$windows$MSImageSegmentation$widgets$r)
	s <- svalue(h$action$windows$MSImageSegmentation$widgets$s)
	s <- eval(parse(text=paste("c(", s, ")")))
	method <- svalue(h$action$windows$MSImageSegmentation$widgets$method)
	freezeGUI(TRUE)
	h$action$windows$MSImageSegmentation$object <- spatialSparseClassify(object,
		labels=labels, r=r, s=s, method=method)
	freezeGUI(FALSE)
	name <- svalue(h$action$windows$MSImageSegmentation$widgets$newObjectName)
	h$action$windows$MSImageSegmentation$objectname <- name
	assign(name, h$action$windows$MSImageSegmentation$object, envir=.userEnvironment)
	killWindow(h$action, "MSImageSegmentation")
	window <- makeMSImageSegmentationControl(h$action)
	setMSImageSegmentationOptions(h$action, plot.overlay=TRUE, image.mask.missing=TRUE)
	h$action$windows$MSImageSegmentation$open <- TRUE
	updateAnalysisPlots(h$action)
	updateAnalysisImages(h$action)
	focus(window) <- TRUE
	galert(message="An object of class 'MSImageSegmentation' has been placed in the user environment.", title="Done")
}

handleSpatialCluster <- function(h, ...) {
	object <- try(get(h$action$objectname, envir=h$action$objectenv), silent=TRUE)
	if ( inherits(object, "try-error") ) {
		gmessage(message=paste("Could not find object '", h$action$objectname, "'.", sep=""), title="Error")
		return()
	}
	r <- svalue(h$action$windows$MSImageSegmentation$widgets$r)
	k <- svalue(h$action$windows$MSImageSegmentation$widgets$k)
	k <- eval(parse(text=paste("c(", k, ")")))
	s <- svalue(h$action$windows$MSImageSegmentation$widgets$s)
	s <- eval(parse(text=paste("c(", s, ")")))
	method <- svalue(h$action$windows$MSImageSegmentation$widgets$method)
	freezeGUI(TRUE)
	h$action$windows$MSImageSegmentation$object <- spatialSparseCluster(object,
		r=r, k=k, s=s, method=method)
	freezeGUI(FALSE)
	name <- svalue(h$action$windows$MSImageSegmentation$widgets$newObjectName)
	h$action$windows$MSImageSegmentation$objectname <- name
	assign(name, h$action$windows$MSImageSegmentation$object, envir=.userEnvironment)
	killWindow(h$action, "MSImageSegmentation")
	window <- makeMSImageSegmentationControl(h$action)
	setMSImageSegmentationOptions(h$action, plot.overlay=TRUE)
	h$action$windows$MSImageSegmentation$open <- TRUE
	updateAnalysisPlots(h$action)
	updateAnalysisImages(h$action)
	focus(window) <- TRUE
	galert(message="An object of class 'MSImageSegmentation' has been placed in the user environment.", title="Done")
}


handleMSImageSegmentationWhich <- function(h, ...) {
	if ( h$action$windows$MSImageSegmentation$open ) {
		h$action$windows$MSImageSegmentation$which <- svalue(h$obj)
		updateAnalysisPlots(h$action)
		updateAnalysisImages(h$action)
	}
}

handleMSImageSegmentationPlotOverlay <- function(h, ...) {
	if ( h$action$windows$MSImageSegmentation$open ) {
		h$action$windows$MSImageSegmentation$plot.options[["overlay"]] <- svalue(h$obj)
		updateAnalysisPlots(h$action)
	}
}

handleMSImageSegmentationPlotClimits <- function(h, ...) {
	if ( h$action$windows$MSImageSegmentation$open ) {
		h$action$windows$MSImageSegmentation$plot.options[["climits"]] <- svalue(h$obj)
		updateAnalysisPlots(h$action)
	}
}

handleMSImageSegmentationPlotMode <- function(h, ...) {
	if ( h$action$windows$MSImageSegmentation$open ) {
		h$action$windows$MSImageSegmentation$plot.mode <- svalue(h$obj)
		updateAnalysisPlots(h$action)
	}
}

handleMSImageSegmentationImageOverlay <- function(h, ...) {
	if ( h$action$windows$MSImageSegmentation$open ) {
		h$action$windows$MSImageSegmentation$image.options[["overlay"]] <- svalue(h$obj)
		updateAnalysisImages(h$action)
	}
}

handleMSImageSegmentationImageTrueLabels <- function(h, ...) {
	if ( h$action$windows$MSImageSegmentation$open ) {
		h$action$windows$MSImageSegmentation$image.options[["true.labels"]] <- svalue(h$obj)
		updateAnalysisImages(h$action)
	}
}

handleMSImageSegmentationImageMaskMissing <- function(h, ...) {
	if ( h$action$windows$MSImageSegmentation$open ) {
		h$action$windows$MSImageSegmentation$image.options[["mask.missing"]] <- svalue(h$obj)
		updateAnalysisImages(h$action)
	}
}

handleMSImageSegmentationImageMode <- function(h, ...) {
	if ( h$action$windows$MSImageSegmentation$open ) {
		h$action$windows$MSImageSegmentation$image.mode <- svalue(h$obj)
		updateAnalysisImages(h$action)
	}
}

handleMSImageSegmentationTransparency <- function(h, ...) {
	if ( h$action$windows$MSImageSegmentation$open ) {
		h$action$windows$MSImageSegmentation$transparency <- svalue(h$obj)
		updateAnalysisPlots(h$action)
		updateAnalysisImages(h$action)
	}
}

handleMSImageSegmentationSummary <- function(h, ...) {
	makeMSImageSegmentationTable(h$action)
}

handleMSImageSegmentationPlotLikelihood <- function(h, ...) {
	dev.new(title=paste(h$action$name, "Segmentation likelihood", sep=" - "), width=5, height=5)
	print(likPlot(h$action$windows$MSImageSegmentation$object))
}

handleMSImageSegmentationTable <- function(h, ...) {
	mz <- as.numeric(svalue(h$obj))
	setMZ(h$action, mz)
	updateAll(h$action)
}

handlerMSImageSegmentationExport <- function(instance, summary) {
	function(h, ...) {
		name <- gsub(pattern="\\.", replacement="_", x=instance$objectname)
		file <- gfile(text="Export m/z list to CSV file", type="save",
			initialfilename=paste(name, "_segmentation_statistics.csv", sep=""),
			filter=list("CSV files"=list(patterns="*.csv")))
		if ( is.na(file) ) return()
		if ( !grepl(pattern=".csv", x=file, ignore.case=TRUE) ) {
			file <- paste(file, ".csv", sep="")
		}
		tryCatch(write.csv(summary, file=file),
			error=function(e) gmessage(message="Failed to save file.", title="Error"))
	}
}



