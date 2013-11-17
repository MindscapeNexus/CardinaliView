
#### functions for making file menu control panels ####

makeLoadDatasetControl <- function(instance) {
	window <- gwindow(title=paste(instance$objectname, "Load dataset", sep=" - "),
		width=100, height=100, visible=FALSE)
	group <- ggroup(horizontal=FALSE, container=window)
	instance$windows$main$widgets$datasetChooser <- gtable(items=data.frame(MSImageSet=ls.MSImageSet(.userEnvironment)),
		container=group, expand=TRUE)
	subgroup <- ggroup(container=group)
	instance$windows$main$widgets$openNewInstance <- gcheckbox(text="open in new instance",
		checked=FALSE, container=subgroup, expand=TRUE)
	subgroup <- ggroup(container=group)
	loadAction <- gaction(label="Load", icon="open", handler=handleLoadDataset, action=instance)
	cancelAction <- gaction(label="Cancel", icon="cancel", handler=function(h, ...) dispose(window))
	instance$windows$main$widgets$loadDatasetLoad <- gbutton(action=loadAction,
		container=subgroup, expand=TRUE)
	instance$windows$main$widgets$loadDatasetCancel <- gbutton(action=cancelAction,
		container=subgroup, expand=TRUE)
	instance$windows$main$topWidgets$fileDialog <- window
	visible(window) <- TRUE
	window
}

makeLoadPeaksControl <- function(instance) {
	window <- gwindow(title=paste(instance$objectname, "Load peaks", sep=" - "),
		width=100, height=100, visible=FALSE)
	group <- ggroup(horizontal=FALSE, container=window)
	instance$windows$main$widgets$peaksChooser <- gtable(items=data.frame(MSPeakFrame=ls.MSPeakFrame(.userEnvironment)),
		container=group, expand=TRUE)
	subgroup <- ggroup(container=group)
	loadAction <- gaction(label="Load", icon="open",
		handler=handleLoadPeaks,
		action=instance)
	cancelAction <- gaction(label="Cancel", icon="cancel", handler=function(h, ...) dispose(window))
	instance$windows$main$widgets$loadPeaksLoad <- gbutton(action=loadAction,
		container=subgroup, expand=TRUE)
	instance$windows$main$widgets$loadPeaksCancel <- gbutton(action=cancelAction,
		container=subgroup, expand=TRUE)
	instance$windows$main$topWidgets$fileDialog <- window
	visible(window) <- TRUE
	window
}

makeLoadSegmentationControl <- function(instance) {
	window <- gwindow(title=paste(instance$objectname, "Load segmentation", sep=" - "),
		width=100, height=100, visible=FALSE)
	group <- ggroup(horizontal=FALSE, container=window)
	instance$windows$main$widgets$segmentationChooser <- gtable(items=data.frame(MSImageSegmentation=ls.MSImageSegmentation(.userEnvironment)),
		container=group, expand=TRUE)
	subgroup <- ggroup(container=group)
	loadAction <- gaction(label="Load", icon="open",
		handler=handleLoadSegmentation,
		action=instance)
	cancelAction <- gaction(label="Cancel", icon="cancel", handler=function(h, ...) dispose(window))
	instance$windows$main$widgets$loadSegmentationLoad <- gbutton(action=loadAction,
		container=subgroup, expand=TRUE)
	instance$windows$main$widgets$loadSegmentationCancel <- gbutton(action=cancelAction,
		container=subgroup, expand=TRUE)
	instance$windows$main$topWidgets$fileDialog <- window
	visible(window) <- TRUE
	window
}

makeImportFromImzMLControl <- function(instance) {
	window <- gwindow(title=paste(instance$objectname, "Import imzML", sep=" - "),
		width=100, height=100, visible=FALSE)
	group <- ggroup(horizontal=FALSE, container=window)
	# subgroup <- ggroup(container=group)
	# glabel(text="imzML converter", container=subgroup)
	# subgroup <- ggroup(container=group)
	# instance$windows$main$widgets$imzMLConverter <- gfilebrowse(container=subgroup, type="open",
	# 	filter=list("Java archive" = list(patterns=c("*.jar"))), quote=FALSE, expand=TRUE)
	# subgroup <- ggroup(container=group)
	# glabel(text="file", container=subgroup)
	subgroup <- ggroup(container=group)
	instance$windows$main$widgets$importImzMLBrowser <- gfilebrowse(container=subgroup, type="open",
		filter=list("imzML files" = list(patterns="*.imzML")), quote=FALSE, expand=TRUE,
		handler=handleImportFromImzMLName,
		action=instance)
	subgroup <- ggroup(container=group, expand=TRUE)
	glabel(text="object name", container=subgroup)
	instance$windows$main$widgets$newObjectName <- gedit(text="",
		container=subgroup, expand=TRUE)
	subgroup <- ggroup(container=group, horizontal=FALSE)
	instance$windows$main$widgets$openNewInstance <- gcheckbox(text="open in new instance",
		checked=FALSE, container=subgroup, expand=TRUE)
	buttongroup <- ggroup(container=group)
	importAction <- gaction(label="Import", icon="open",
		handler=handleImportFromImzMLImport,
		action=instance)
	cancelAction <- gaction(label="Cancel", icon="cancel", handler=function(h, ...) dispose(window))
	instance$windows$main$widgets$importImzMLImport <- gbutton(action=importAction,
		container=buttongroup, expand=TRUE)
	instance$windows$main$widgets$importImzMLCancel <- gbutton(action=cancelAction,
		container=buttongroup, expand=TRUE)
	instance$windows$main$topWidgets$fileDialog <- window
	visible(window) <- TRUE
	window
}

makeImportFromAnalyze7.5Control <- function(instance) {
	window <- gwindow(title=paste(instance$objectname, "Import Analyze 7.5", sep=" - "),
		width=100, height=100, visible=FALSE)
	group <- ggroup(horizontal=FALSE, container=window)
	# subgroup <- ggroup(container=group)
	# glabel(text="file", container=subgroup)
	subgroup <- ggroup(container=group)
	instance$windows$main$widgets$importAnalyze7.5Browser <- gfilebrowse(container=subgroup, type="open",
		filter=list("Analyze 7.5 files" = list(patterns="*.img")), quote=FALSE, expand=TRUE,
		handler=handleImportFromAnalyze7.5Name,
		action=instance)
	subgroup <- ggroup(container=group)
	glabel(text="object name", container=subgroup)
	instance$windows$main$widgets$newObjectName <- gedit(text="",
		container=subgroup, expand=TRUE)
	subgroup <- ggroup(container=group, horizontal=FALSE)
	instance$windows$main$widgets$openNewInstance <- gcheckbox(text="open in new instance",
		checked=FALSE, container=subgroup, expand=TRUE)
	buttongroup <- ggroup(container=group)
	importAction <- gaction(label="Import", icon="open",
		handler=handleImportFromAnalyze7.5Import,
		action=instance)
	cancelAction <- gaction(label="Cancel", icon="cancel", handler=function(h, ...) dispose(window))
	instance$windows$main$widgets$importAnalyze7.5Import <- gbutton(action=importAction,
		container=buttongroup, expand=TRUE)
	instance$windows$main$widgets$importAnalyze7.5Cancel <- gbutton(action=cancelAction,
		container=buttongroup, expand=TRUE)
	instance$windows$main$topWidgets$fileDialog <- window
	visible(window) <- TRUE
	window
}

makeExportPlotControl <- function(instance) {
	window <- gwindow(title=paste(instance$objectname, "Export plots", sep=" - "),
		width=100, height=100, visible=FALSE)
	group <- ggroup(horizontal=FALSE, container=window)
	instance$windows$main$widgets$exportPlotsDeviceFrame <- gframe(text="export from device",
		container=group, expand=TRUE)
	instance$windows$main$widgets$exportPlotsDevice <- gcombobox(items=activeDevices(instance,
		"plot"), container=instance$windows$main$widgets$exportPlotsDeviceFrame, expand=TRUE)
	frame <- gframe(text="file", container=group, expand=TRUE)
	instance$windows$main$widgets$exportPlotsBrowse <- gfilebrowse(container=frame, type="save",
		filter=list("All files" = list(patterns="*")), quote=FALSE, expand=TRUE)
	frame <- gframe(text="kind", horizontal=FALSE, container=group, expand=TRUE)
	instance$windows$main$widgets$exportPlotsFiletype <- gcombobox(items=c("pdf","jpeg","png","tiff"),
		container=frame, expand=TRUE)
	subgroup <- ggroup(container=frame)
	glabel(text="width", container=subgroup)
	instance$windows$main$widgets$exportPlotsWidth <- gedit(text=480, width=10,
		coerce.with=as.numeric, container=subgroup, expand=TRUE)
	subgroup <- ggroup(container=frame)
	glabel(text="height", container=subgroup)
	instance$windows$main$widgets$exportPlotsHeight <- gedit(text=480, width=10,
		coerce.with=as.numeric, container=subgroup, expand=TRUE)
	subgroup <- ggroup(container=frame)
	glabel(text="pointsize", container=subgroup)
	instance$windows$main$widgets$exportPlotsPointsize <- gedit(text=12, width=10,
		coerce.with=as.numeric, container=subgroup, expand=TRUE)
	buttongroup <- ggroup(container=group)
	trellisAction <- gaction(label="Trellis graphics", icon="evaluate",
		handler=handleTrellisPlotOpen,
		action=instance)
	instance$windows$main$widgets$exportPlotsTrellis <- gbutton(action=trellisAction,
		container=buttongroup, expand=TRUE)
	buttongroup <- ggroup(container=group)
	exportAction <- gaction(label="Export", icon="save",
		handler=handleExportPlotExport,
		action=instance)
	cancelAction <- gaction(label="Cancel", icon="cancel",
		handler=handleExportPlotOrImageDestroy,
		action=instance)
	instance$windows$main$widgets$exportPlotsExport <- gbutton(action=exportAction,
		container=buttongroup, expand=TRUE, icon="open")
	instance$windows$main$widgets$exportPlotsCancel <- gbutton(action=cancelAction,
		container=buttongroup, expand=TRUE)
	instance$windows$main$topWidgets$fileDialog <- window
	addHandlerDestroy(window, handler=handleExportPlotOrImageDestroy, action=instance)
	visible(window) <- TRUE
	window
}

makeTrellisPlotControl <- function(instance) {
	window <- gwindow(title=paste(instance$objectname, "Trellis plots", sep=" - "),
		width=400, height=100, visible=FALSE)
	group <- ggroup(horizontal=FALSE, container=window)
	if ( is.null(instance$windows$main$trellis.pixel) ) {
		pixels <- instance$parameters$pixel
	} else {
		pixels <- instance$windows$main$trellis.pixel
	}
	subgroup <- ggroup(container=group, expand=TRUE)
	glabel(text="List pixel indices separated by commas.", container=subgroup)
	subgroup <- ggroup(container=group, expand=TRUE)
	glabel(text="pixels", container=subgroup)
	instance$windows$main$widgets$trellisPlotPixels <- gedit(text=pixels, width=10,
		container=subgroup, expand=TRUE)
	buttongroup <- ggroup(container=group)
	useAction <- gaction(label="Use", icon="ok",
		handler=handleTrellisPlotUse,
		action=instance)
	cancelAction <- gaction(label="Cancel", icon="cancel", handler=function(h, ...) dispose(window))
	instance$windows$main$widgets$trellisPlotUse <- gbutton(action=useAction,
		container=buttongroup, expand=TRUE, icon="open")
	instance$windows$main$widgets$trellisPlotCancel <- gbutton(action=cancelAction,
		container=buttongroup, expand=TRUE)
	instance$windows$main$topWidgets$trellisDialog <- window
	visible(window) <- TRUE
	window
}

makeExportImageControl <- function(instance) {
	window <- gwindow(title=paste(instance$objectname, "Export images", sep=" - "),
		width=100, height=100, visible=FALSE)
	group <- ggroup(horizontal=FALSE, container=window)
	instance$windows$main$widgets$exportImagesDeviceFrame <- gframe(text="export from device",
		container=group, expand=TRUE)
	instance$windows$main$widgets$exportImagesDevice <- gcombobox(items=activeDevices(instance,
		"image"), container=instance$windows$main$widgets$exportImagesDeviceFrame, expand=TRUE)
	frame <- gframe(text="file", container=group, expand=TRUE)
	instance$windows$main$widgets$exportImagesBrowse <- gfilebrowse(container=frame, type="save",
		filter=list("All files" = list(patterns="*")), quote=FALSE, expand=TRUE)
	frame <- gframe(text="kind", horizontal=FALSE, container=group, expand=TRUE)
	instance$windows$main$widgets$exportImagesFiletype <- gcombobox(items=c("pdf","jpeg","png","tiff"),
		container=frame, expand=TRUE)
	subgroup <- ggroup(container=frame)
	glabel(text="width", container=subgroup)
	instance$windows$main$widgets$exportImagesWidth <- gedit(text=480, width=10,
		coerce.with=as.numeric, container=subgroup, expand=TRUE)
	subgroup <- ggroup(container=frame)
	glabel(text="height", container=subgroup)
	instance$windows$main$widgets$exportImagesHeight <- gedit(text=480, width=10,
		coerce.with=as.numeric, container=subgroup, expand=TRUE)
	subgroup <- ggroup(container=frame)
	glabel(text="pointsize", container=subgroup)
	instance$windows$main$widgets$exportImagesPointsize <- gedit(text=12, width=10,
		coerce.with=as.numeric, container=subgroup, expand=TRUE)
	buttongroup <- ggroup(container=group)
	trellisAction <- gaction(label="Trellis graphics", icon="evaluate",
		handler=handleTrellisImageOpen,
		action=instance)
	instance$windows$main$widgets$exportImagesTrellis <- gbutton(action=trellisAction,
		container=buttongroup, expand=TRUE)
	buttongroup <- ggroup(container=group)
	exportAction <- gaction(label="Export", icon="save",
		handler=handleExportImageExport,
		action=instance)
	cancelAction <- gaction(label="Cancel", icon="cancel",
		handler=handleExportPlotOrImageDestroy,
		action=instance)
	instance$windows$main$widgets$exportImagesExport <- gbutton(action=exportAction,
		container=buttongroup, expand=TRUE, icon="open")
	instance$windows$main$widgets$exportImagesCancel <- gbutton(action=cancelAction,
		container=buttongroup, expand=TRUE)
	instance$windows$main$topWidgets$fileDialog <- window
	addHandlerDestroy(window, handler=handleExportPlotOrImageDestroy, action=instance)
	visible(window) <- TRUE
	window
}

makeTrellisImageControl <- function(instance) {
	window <- gwindow(title=paste(instance$objectname, "Trellis images", sep=" - "),
		width=400, height=100, visible=FALSE)
	group <- ggroup(horizontal=FALSE, container=window)
	if ( is.null(instance$windows$main$trellis.mz) ) {
		mzs <- round(instance$parameters$mz, digits=2)
	} else {
		mzs <- instance$windows$main$trellis.mz
	}
	subgroup <- ggroup(container=group, expand=TRUE)
	glabel(text="List m/z values separated by commas.", container=subgroup)
	subgroup <- ggroup(container=group, expand=TRUE)
	glabel(text="m/z values", container=subgroup)
	instance$windows$main$widgets$trellisImageMZ <- gedit(text=mzs, width=10,
		container=subgroup, expand=TRUE)
	buttongroup <- ggroup(container=group)
	useAction <- gaction(label="Use", icon="ok",
		handler=handleTrellisImageUse,
		action=instance)
	cancelAction <- gaction(label="Cancel", icon="cancel", handler=function(h, ...) dispose(window))
	instance$windows$main$widgets$trellisImageUse <- gbutton(action=useAction,
		container=buttongroup, expand=TRUE, icon="open")
	instance$windows$main$widgets$trellisImageCancel <- gbutton(action=cancelAction,
		container=buttongroup, expand=TRUE)
	instance$windows$main$topWidgets$trellisDialog <- window
	visible(window) <- TRUE
	window
}

