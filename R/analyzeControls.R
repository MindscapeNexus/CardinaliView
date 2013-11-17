
#### functions for making analysis controls ####

makePCADialog <- function(instance) {
	object <- try(get(instance$objectname, envir=instance$objectenv), silent=TRUE)
	if ( inherits(object, "try-error") ) return()
	window <- gwindow(title=paste(instance$objectname, "PCA", sep=" - "),
		width=100, height=100, visible=FALSE)
	group <- ggroup(horizontal=FALSE, container=window, expand=TRUE)
	# the method parameters
	frame <- gframe(text="method", horizontal=FALSE, container=group, expand=TRUE)
	instance$windows$PCA$widgets$method <- gcombobox(items=c("irlba",
		"svd", "nipals"), container=frame, expand=TRUE)
	subgroup <- ggroup(container=frame)
	glabel(text="number of components (ncomp)", container=subgroup)
	instance$windows$PCA$widgets$ncomp <- gspinbutton(from=1,
		to=numFeatures(object), by=1, container=subgroup, expand=TRUE)
	svalue(instance$windows$PCA$widgets$ncomp) <- 5
	subgroup <- ggroup(container=frame)
	instance$windows$PCA$widgets$standardize <- gcheckbox(text="standardize",
		checked=FALSE, container=subgroup, expand=TRUE)
	# name for the object
	subgroup <- ggroup(container=group)
	glabel(text="name of results", container=subgroup)
	instance$windows$PCA$widgets$newObjectName <- gedit(text=paste(instance$objectname,
		"PCA", sep="."), container=subgroup, expand=TRUE)
	# control buttons
	buttongroup <- ggroup(container=group)
	computeAction <- gaction(label="Compute", icon="plot",
		handler=handlePCACompute,
		action=instance)
	cancelAction <- gaction(label="Cancel", icon="cancel",
		handler=handlerKillWindow(instance, "PCA"))
	instance$windows$PCA$widgets$computeButton <- gbutton(action=computeAction,
		container=buttongroup, expand=TRUE)
	instance$windows$PCA$widgets$cancelButton <- gbutton(action=cancelAction,
		container=buttongroup, expand=TRUE)
	addHandlerDestroy(window, handler=handlerKillWindow(instance, "PCA"))
	instance$windows$PCA$topWidgets$dialogWindow <- window
	visible(window) <- TRUE
	window
}

makePCAControl <- function(instance) {
	object <- instance$windows$PCA$object
	window <- gwindow(title=paste(instance$objectname, "PCA", sep=" - "),
		width=100, height=100, visible=FALSE)
	group <- ggroup(horizontal=FALSE, container=window, expand=TRUE)
	frame <- gframe(text="component", horizontal=FALSE, container=group, expand=TRUE)
	instance$windows$PCA$widgets$which.ncomp <- gcombobox(items=1:object$ncomp,
		container=frame, expand=TRUE,
		handler=handlePCAncomp,
		action=instance)
	buttongroup <- ggroup(container=group)
	plotAction <- gaction(label="Plot eigenvalues", icon="plot",
		handler=handlePCAPlotEigenvalues,
		action=instance)
	closeAction <- gaction(label="Close", icon="cancel",
		handler=handlerKillWindow(instance, "PCA"))
	instance$windows$PCA$widgets$plotEigenvaluesButton <- gbutton(action=plotAction,
		container=buttongroup, expand=TRUE)
	instance$windows$PCA$widgets$closeButton <- gbutton(action=closeAction,
		container=buttongroup, expand=TRUE)
	addHandlerDestroy(window, handler=handlerKillWindow(instance, "PCA"))
	instance$windows$PCA$topWidgets$controlWindow <- window
	visible(window) <- TRUE
	window
}

makeCompareROIDialog <- function(instance) {
	object <- try(get(instance$objectname, envir=instance$objectenv), silent=TRUE)
	if ( inherits(object, "try-error") ) {
		gmessage(message=paste("Could not find object '", instance$objectname, "'.", sep=""), title="Error")
		return()
	}
	window <- gwindow(title=paste(instance$objectname, "Compare ROI", sep=" - "),
		width=100, height=100, visible=FALSE)
	# the available ROIs
	group <- ggroup(horizontal=FALSE, container=window, expand=TRUE)
	instance$windows$MSImageSegmentation$widgets$roiFrame <- gframe(text="available regions of interest", horizontal=FALSE,
		container=group, expand=TRUE)
	instance$windows$MSImageSegmentation$widgets$roiChooser <- gtable(items=data.frame("ROI"=ls.ROI(object, .userEnvironment)),
		multiple=TRUE, container=instance$windows$MSImageSegmentation$widgets$roiFrame, expand=TRUE)
	subgroup <- ggroup(container=group)
	glabel(text="(select multiple ROIs by holding down shift or ctrl)", container=subgroup, expand=TRUE)
	# name for the object
	subgroup <- ggroup(container=group)
	glabel(text="name of results", container=subgroup)
	instance$windows$MSImageSegmentation$widgets$newObjectName <- gedit(text=paste(instance$objectname,
		"compareROI", sep="."), container=subgroup, expand=TRUE)
	# control buttons
	buttongroup <- ggroup(container=group)
	compareAction <- gaction(label="Compare", icon="plot",
		handler=handleCompareROI,
		action=instance)
	cancelAction <- gaction(label="Close", icon="cancel", handler=function(h, ...) dispose(window))
	instance$windows$MSImageSegmentation$widgets$compareButton <- gbutton(action=compareAction,
		container=buttongroup, expand=TRUE)
	instance$windows$MSImageSegmentation$widgets$cancelButton <- gbutton(action=cancelAction,
		container=buttongroup, expand=TRUE)
	# make the window
	instance$windows$MSImageSegmentation$topWidgets$dialogWindow <- window
	visible(window) <- TRUE
	window
}

makeSpatialClassifyDialog <- function(instance) {
	object <- try(get(instance$objectname, envir=instance$objectenv), silent=TRUE)
	if ( inherits(object, "try-error") ) {
		gmessage(message=paste("Could not find object '", instance$objectname, "'.", sep=""), title="Error")
		return()
	}
	window <- gwindow(title=paste(instance$objectname, "Spatial classify", sep=" - "),
		width=100, height=100, visible=FALSE)
	# the available ROIs
	group <- ggroup(horizontal=FALSE, container=window, expand=TRUE)
	instance$windows$MSImageSegmentation$widgets$roiFrame <- gframe(text="available regions of interest", horizontal=FALSE,
		container=group, expand=TRUE)
	instance$windows$MSImageSegmentation$widgets$roiChooser <- gtable(items=data.frame("ROI"=ls.ROI(object, .userEnvironment)),
		multiple=TRUE, container=instance$windows$MSImageSegmentation$widgets$roiFrame, expand=TRUE)
	subgroup <- ggroup(container=group)
	glabel(text="(select multiple ROIs by holding down shift or ctrl)", container=subgroup, expand=TRUE)
	# method parameters
	frame <- gframe(text="method", horizontal=FALSE, container=group) #, expand=TRUE)
	instance$windows$MSImageSegmentation$widgets$method <- gcombobox(items=c("gaussian",
		"adaptive"), container=frame, # expand=TRUE,
		handler=NULL,
		action=instance)
	subgroup <- ggroup(container=frame)
	glabel(text="spatial smoothing radius (r)", container=subgroup)
	instance$windows$MSImageSegmentation$widgets$r <- gspinbutton(from=1,
		to=9, by=1, container=subgroup, expand=TRUE,
		handler=NULL,
		action=instance)
	svalue(instance$windows$MSImageSegmentation$widgets$r) <- 2
	subgroup <- ggroup(container=frame)
	glabel(text="shrinkage parameter (s)", container=subgroup)
	instance$windows$MSImageSegmentation$widgets$s <- gedit(text=2, width=10,
		container=subgroup, expand=TRUE,
		handler=NULL,
		action=instance)
	# warning
	subgroup <- ggroup(container=group)
	glabel(text="Note: it is strongly recommended to bin\nor resample the data before clustering.",
		container=subgroup, expand=TRUE)
	# name for the object
	subgroup <- ggroup(container=group)
	glabel(text="name of results", container=subgroup)
	instance$windows$MSImageSegmentation$widgets$newObjectName <- gedit(text=paste(instance$objectname,
		"spatialClassify", sep="."), container=subgroup, expand=TRUE)
	# control buttons
	buttongroup <- ggroup(container=group)
	classifyAction <- gaction(label="Classify", icon="plot",
		handler=handleSpatialClassify,
		action=instance)
	cancelAction <- gaction(label="Close", icon="cancel", handler=function(h, ...) dispose(window))
	instance$windows$MSImageSegmentation$widgets$classifyButton <- gbutton(action=classifyAction,
		container=buttongroup, expand=TRUE)
	instance$windows$MSImageSegmentation$widgets$cancelButton <- gbutton(action=cancelAction,
		container=buttongroup, expand=TRUE)
	# make the window
	instance$windows$MSImageSegmentation$topWidgets$dialogWindow <- window
	visible(window) <- TRUE
	window
}

makeSpatialClusterDialog <- function(instance) {
	object <- try(get(instance$objectname, envir=instance$objectenv), silent=TRUE)
	if ( inherits(object, "try-error") ) {
		gmessage(message=paste("Could not find object '", instance$objectname, "'.", sep=""), title="Error")
		return()
	}
	window <- gwindow(title=paste(instance$objectname, "Spatial cluster", sep=" - "),
		width=100, height=100, visible=FALSE)
	# method parameters
	group <- ggroup(horizontal=FALSE, container=window, expand=TRUE)
	frame <- gframe(text="method", horizontal=FALSE, container=group) #, expand=TRUE)
	instance$windows$MSImageSegmentation$widgets$method <- gcombobox(items=c("gaussian",
		"adaptive"), container=frame, # expand=TRUE,
		handler=NULL,
		action=instance)
	subgroup <- ggroup(container=frame)
	glabel(text="spatial smoothing radius (r)", container=subgroup)
	instance$windows$MSImageSegmentation$widgets$r <- gspinbutton(from=1,
		to=9, by=1, container=subgroup, expand=TRUE,
		handler=NULL,
		action=instance)
	svalue(instance$windows$MSImageSegmentation$widgets$r) <- 2
	subgroup <- ggroup(container=frame)
	glabel(text="number of clusters (k)", container=subgroup)
	instance$windows$MSImageSegmentation$widgets$k <- gedit(text=2, width=10,
		container=subgroup, expand=TRUE,
		handler=NULL,
		action=instance)
	subgroup <- ggroup(container=frame)
	glabel(text="shrinkage parameter (s)", container=subgroup)
	instance$windows$MSImageSegmentation$widgets$s <- gedit(text=2, width=10,
		container=subgroup, expand=TRUE,
		handler=NULL,
		action=instance)
	# warning
	subgroup <- ggroup(container=group)
	glabel(text="Note: it is strongly recommended to bin\nor resample the data before clustering.",
		container=subgroup, expand=TRUE)
	# name for the object
	subgroup <- ggroup(container=group)
	glabel(text="name of results", container=subgroup)
	instance$windows$MSImageSegmentation$widgets$newObjectName <- gedit(text=paste(instance$objectname,
		"spatialCluster", sep="."), container=subgroup, expand=TRUE)
	# control buttons
	buttongroup <- ggroup(container=group)
	clusterAction <- gaction(label="Cluster", icon="plot",
		handler=handleSpatialCluster,
		action=instance)
	cancelAction <- gaction(label="Close", icon="cancel", handler=function(h, ...) dispose(window))
	instance$windows$MSImageSegmentation$widgets$clusterButton <- gbutton(action=clusterAction,
		container=buttongroup, expand=TRUE)
	instance$windows$MSImageSegmentation$widgets$cancelButton <- gbutton(action=cancelAction,
		container=buttongroup, expand=TRUE)
	# make the window
	instance$windows$MSImageSegmentation$topWidgets$dialogWindow <- window
	visible(window) <- TRUE
	window
}

makeMSImageSegmentationControl <- function(instance) {
	object <- instance$windows$MSImageSegmentation$object
	window <- gwindow(title=paste(instance$objectname, "Image segmentation", sep=" - "),
		width=100, height=100, visible=FALSE)
	group <- ggroup(horizontal=FALSE, container=window, expand=TRUE)
	frame <- gframe(text="parameters", horizontal=FALSE, container=group, expand=TRUE)
	instance$windows$MSImageSegmentation$widgets$which <- gcombobox(items=names(object$classes),
		container=frame, expand=TRUE,
		handler=handleMSImageSegmentationWhich,
		action=instance)
	frame <- gframe(text="plot options", horizontal=FALSE, container=group, expand=TRUE)
	instance$windows$MSImageSegmentation$widgets$plotOverlay <- gcheckbox(text="overlay",
		checked=FALSE, container=frame,
		handler=handleMSImageSegmentationPlotOverlay,
		action=instance)
	instance$windows$MSImageSegmentation$widgets$plotClimits <- gcheckbox(text="climits",
		checked=FALSE, container=frame,
		handler=handleMSImageSegmentationPlotClimits,
		action=instance)
	subgroup <- ggroup(container=frame)
	glabel(text="mode", container=subgroup)
	instance$windows$MSImageSegmentation$widgets$plotMode <- gcombobox(items=c("centroids",
		"tstatistics"), container=subgroup, expand=TRUE,
		handler=handleMSImageSegmentationPlotMode,
		action=instance)
	frame <- gframe(text="image options", horizontal=FALSE, container=group)
	instance$windows$MSImageSegmentation$widgets$imageOverlay <- gcheckbox(text="overlay",
		checked=FALSE, container=frame,
		handler=handleMSImageSegmentationImageOverlay,
		action=instance)
	instance$windows$MSImageSegmentation$widgets$imageTrueLabels <- gcheckbox(text="true.labels",
		checked=FALSE, container=frame,
		handler=handleMSImageSegmentationImageTrueLabels,
		action=instance)
	instance$windows$MSImageSegmentation$widgets$imageMaskMissing <- gcheckbox(text="mask.missing",
		checked=FALSE, container=frame,
		handler=handleMSImageSegmentationImageMaskMissing,
		action=instance)
	subgroup <- ggroup(container=frame)
	glabel(text="mode", container=subgroup)
	instance$windows$MSImageSegmentation$widgets$imageMode <- gcombobox(items=c("classes",
		"probabilities"), container=subgroup, expand=TRUE,
		handler=handleMSImageSegmentationImageMode,
		action=instance)
	subgroup <- ggroup(container=group)
	glabel(text="transparency", container=subgroup)
	instance$windows$MSImageSegmentation$widgets$transparency <- gspinbutton(from=0.1,
		to=0.9, by=0.1, container=subgroup, expand=TRUE,
		handler=handleMSImageSegmentationTransparency,
		action=instance)
	svalue(instance$windows$MSImageSegmentation$widgets$transparency) <- 0.5
	buttongroup <- ggroup(container=group)
	summaryAction <- gaction(label="View statistics", icon="dataframe",
		handler=handleMSImageSegmentationSummary,
		action=instance)
	instance$windows$MSImageSegmentation$widgets$summaryButton <- gbutton(action=summaryAction,
		container=buttongroup, expand=TRUE)
	buttongroup <- ggroup(container=group)
	plotAction <- gaction(label="Plot likelihood", icon="plot",
		handler=handleMSImageSegmentationPlotLikelihood,
		action=instance)
	closeAction <- gaction(label="Close", icon="cancel",
		handler=handlerKillWindow(instance, "MSImageSegmentation"))
	instance$windows$MSImageSegmentation$widgets$plotButton <- gbutton(action=plotAction,
		container=buttongroup, expand=TRUE)
	instance$windows$MSImageSegmentation$widgets$closeButton <- gbutton(action=closeAction,
		container=buttongroup, expand=TRUE)
	addHandlerDestroy(window, handler=handlerKillWindow(instance, "MSImageSegmentation"))
	instance$windows$MSImageSegmentation$topWidgets$controlWindow <- window
	visible(window) <- TRUE
	window
}

makeMSImageSegmentationTable <- function(instance) {
	object <- instance$windows$MSImageSegmentation$object
	which <- instance$windows$MSImageSegmentation$which
	summary <- summary(object, n=Inf)[[which]]
	parameters <- names(object$classes[which])
	error <- object$MSEP[[which]]
	window <- gwindow(title=paste(instance$objectname, "Image segmentation", sep=" - "),
		width=100, height=100, visible=FALSE)
	group <- ggroup(horizontal=FALSE, container=window, expand=TRUE)
	subgroup <- ggroup(container=group, horizontal=FALSE)
	glabel(text=paste("parameters:", parameters), container=subgroup)
	if ( !is.null(error) ) {
		glabel(text=paste("mean squared error of prediction:", error), container=subgroup)
	}
	subgroup <- ggroup(container=group, expand=TRUE)
	gtable(summary, chosencol=1, container=subgroup, expand=TRUE,
		handler=handleMSImageSegmentationTable,
		action=instance)
	buttongroup <- ggroup(container=group)
	exportAction <- gaction(label="Export to CSV", icon="save",
		handler=handlerMSImageSegmentationExport(instance, summary),
		action=instance)
	closeAction <- gaction(label="Close", icon="cancel",
		handler=function(h, ...) dispose(window))
	exportButton <- gbutton(action=exportAction, container=buttongroup)
	closeButton <- gbutton(action=closeAction, container=buttongroup)
	visible(window) <- TRUE
	window
}


