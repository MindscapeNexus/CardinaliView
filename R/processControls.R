
#### functions for making pre-processing controls ####

makeRemoveNoiseControl <- function(instance) {
	window <- gwindow(title=paste(instance$objectname, "Remove noise", sep=" - "),
		width=100, height=100, visible=FALSE)
	group <- ggroup(horizontal=FALSE, container=window, expand=TRUE)
	# the method parameters
	frame <- gframe(text="method", horizontal=FALSE, container=group, expand=TRUE)
	instance$windows$removeNoise$widgets$method <- gcombobox(items=c("gaussian",
		"sgolay", "kaiser", "ma"), container=frame, expand=TRUE,
		handler=handleRemoveNoiseMethod,
		action=instance)
	subgroup <- ggroup(container=frame)
	glabel(text="window", container=subgroup)
	instance$windows$removeNoise$widgets$window <- gspinbutton(from=3,
		to=101, by=2, container=subgroup, expand=TRUE,
		handler=handleRemoveNoiseWindow,
		action=instance)
	svalue(instance$windows$removeNoise$widgets$window) <- instance$windows$removeNoise$window
	# name for the object
	subgroup <- ggroup(container=group)
	glabel(text="name of results", container=subgroup)
	instance$windows$removeNoise$widgets$newObjectName <- gedit(text=paste(instance$objectname,
		"denoised", sep="."), container=subgroup, expand=TRUE)
	# whether to open a new instance
	subgroup <- ggroup(container=group, horizontal=FALSE)
	instance$windows$removeNoise$widgets$openNewInstance <- gcheckbox(text="open in new instance",
			checked=FALSE, container=subgroup, expand=TRUE)
	# control buttons
	buttongroup <- ggroup(container=group)
	processAction <- gaction(label="Process", icon="ok",
		handler=handleRemoveNoiseProcess,
		action=instance)
	cancelAction <- gaction(label="Cancel", icon="cancel",
		handler=handlerKillWindow(instance, "removeNoise"))
	instance$windows$removeNoise$widgets$processButton <- gbutton(action=processAction,
		container=buttongroup, expand=TRUE)
	instance$windows$removeNoise$widgets$cancelButton <- gbutton(action=cancelAction,
		container=buttongroup, expand=TRUE)
	addHandlerDestroy(window, handler=handlerKillWindow(instance, "removeNoise"))
	instance$windows$removeNoise$topWidgets$dialogWindow <- window
	visible(window) <- TRUE
	window
}

makeRemoveBaselineControl <- function(instance) {
	window <- gwindow(title=paste(instance$objectname, "Remove baseline", sep=" - "),
		width=100, height=100, visible=FALSE)
	group <- ggroup(horizontal=FALSE, container=window, expand=TRUE)
	# the method parameters
	frame <- gframe(text="method", horizontal=FALSE, container=group, expand=TRUE)
	instance$windows$removeBaseline$widgets$interp1 <- gcombobox(items=c("linear",
		"nearest", "pchip", "cubic", "spline"), container=frame, expand=TRUE,
		handler=handleRemoveBaselineMethod,
		action=instance)
	svalue(instance$windows$removeBaseline$widgets$interp1) <- instance$windows$removeBaseline$dots.method
	subgroup <- ggroup(container=frame)
	glabel(text="choose", container=subgroup)
	instance$windows$removeBaseline$widgets$choose <- gcombobox(items=c("min",
		"median"), container=subgroup, expand=TRUE,
		handler=handleRemoveBaselineChoose,
		action=instance)
	svalue(instance$windows$removeBaseline$widgets$choose) <- instance$windows$removeBaseline$dots.choose
	subgroup <- ggroup(container=frame)
	glabel(text="blocks", container=subgroup)
	instance$windows$removeBaseline$widgets$blocks <- gspinbutton(from=100,
		to=1000, by=10, container=subgroup, expand=TRUE,
		handler=handleRemoveBaselineBlocks,
		action=instance)
	svalue(instance$windows$removeBaseline$widgets$blocks) <- instance$windows$removeBaseline$dots.blocks
	subgroup <- ggroup(container=frame)
	glabel(text="preserve.level", container=subgroup)
	instance$windows$removeBaseline$widgets$preserveLevel <- gspinbutton(from=0.1,
		to=2.0, by=0.1, container=subgroup, expand=TRUE,
		handler=handleRemoveBaselinePreserveLevel,
		action=instance)
	svalue(instance$windows$removeBaseline$widgets$preserveLevel) <- instance$windows$removeBaseline$dots.preserve.level
	# name for the object
	subgroup <- ggroup(container=group)
	glabel(text="name of results", container=subgroup)
	instance$windows$removeBaseline$widgets$newObjectName <- gedit(text=paste(instance$objectname,
		"nobaseline", sep="."), container=subgroup, expand=TRUE)
	# whether to open a new instance
	subgroup <- ggroup(container=group, horizontal=FALSE)
	instance$windows$removeBaseline$widgets$openNewInstance <- gcheckbox(text="open in new instance",
			checked=FALSE, container=subgroup, expand=TRUE)
	# control buttons
	buttongroup <- ggroup(container=group)
	processAction <- gaction(label="Process", icon="ok",
		handler=handleRemoveBaselineProcess,
		action=instance)
	cancelAction <- gaction(label="Cancel", icon="cancel",
		handler=handlerKillWindow(instance, "removeBaseline"))
	instance$windows$removeBaseline$widgets$processButton <- gbutton(action=processAction,
		container=buttongroup, expand=TRUE)
	instance$windows$removeBaseline$widgets$cancelButton <- gbutton(action=cancelAction,
		container=buttongroup, expand=TRUE)
	addHandlerDestroy(window, handler=handlerKillWindow(instance, "removeBaseline"))
	instance$windows$removeBaseline$topWidgets$dialogWindow <- window
	visible(window) <- TRUE
	window
}

makeDetectPeaksControl <- function(instance) {
	window <- gwindow(title=paste(instance$objectname, "Detect peaks", sep=" - "),
		width=100, height=100, visible=FALSE)
	group <- ggroup(horizontal=FALSE, container=window, expand=TRUE)
	# the method parameters
	frame <- gframe(text="method", horizontal=FALSE, container=group, expand=TRUE)
	instance$windows$detectPeaks$widgets$method <- gcombobox(items=c("sd", "mad",
		"adaptive-sd", "adaptive-mad", "limpic", "supersmoother"),
		container=frame, expand=TRUE,
		handler=handleDetectPeaksMethod,
		action=instance)
	svalue(instance$windows$detectPeaks$widgets$method) <- instance$windows$detectPeaks$dots.method
	subgroup <- ggroup(container=frame)
	glabel(text="blocks", container=subgroup)
	instance$windows$detectPeaks$widgets$blocks <- gspinbutton(from=10,
		to=500, by=5, container=subgroup, expand=TRUE,
		handler=handleDetectPeaksBlocks,
		action=instance)
	svalue(instance$windows$detectPeaks$widgets$blocks) <- instance$windows$detectPeaks$dots.blocks
	subgroup <- ggroup(container=frame)
	glabel(text="span", container=subgroup)
	instance$windows$detectPeaks$widgets$span <- gspinbutton(from=3,
		to=101, by=2, container=subgroup, expand=TRUE,
		handler=handleDetectPeaksSpan,
		action=instance)
	svalue(instance$windows$detectPeaks$widgets$span) <- instance$windows$detectPeaks$dots.span
	subgroup <- ggroup(container=frame)
	glabel(text="snr", container=subgroup)
	instance$windows$detectPeaks$widgets$snr <- gspinbutton(from=1,
		to=10, by=0.5, container=subgroup, expand=TRUE,
		handler=handleDetectPeaksSNR,
		action=instance)
	svalue(instance$windows$detectPeaks$widgets$snr) <- instance$windows$detectPeaks$dots.snr
	# which spectrum to use
	frame <- gframe(text="spectrum", horizontal=FALSE, container=group, expand=TRUE)
	instance$windows$detectPeaks$widgets$spectrum <- gcombobox(items=c("pixel",
		"mean", "max"), container=frame, expand=TRUE,
		handler=handleDetectPeaksSpectrum,
		action=instance)
	# name for the object
	subgroup <- ggroup(container=group)
	glabel(text="name of results", container=subgroup)
	instance$windows$detectPeaks$widgets$newObjectName <- gedit(text=paste(instance$objectname,
		"peaks", sep="."), container=subgroup, expand=TRUE)
	# control buttons
	buttongroup <- ggroup(container=group)
	browseAction <- gaction(label="Browse peaks", icon="dataframe",
		handler=handleDetectPeaksBrowse,
		action=instance)
	instance$windows$detectPeaks$widgets$browseButton <- gbutton(action=browseAction,
		container=buttongroup, expand=TRUE)
	buttongroup <- ggroup(container=group)
	batchAction <- gaction(label="Batch peak picking",
		handler=handleDetectPeaksBatch, icon="evaluate",
		action=instance)
	instance$windows$detectPeaks$widgets$batchButton <- gbutton(action=batchAction,
		container=buttongroup, expand=TRUE)
	buttongroup <- ggroup(container=group)
	saveAction <- gaction(label="Save", icon="ok",
		handler=handleDetectPeaksSave,
		action=instance)
	cancelAction <- gaction(label="Cancel", icon="cancel",
		handler=handlerKillWindow(instance, "detectPeaks"))
	instance$windows$detectPeaks$widgets$saveButton <- gbutton(action=saveAction,
		container=buttongroup, expand=TRUE)
	instance$windows$detectPeaks$widgets$cancelButton <- gbutton(action=cancelAction,
		container=buttongroup, expand=TRUE)
	addHandlerDestroy(window, handler=handlerKillWindow(instance, "detectPeaks"))
	instance$windows$detectPeaks$topWidgets$dialogWindow <- window
	visible(window) <- TRUE
	window
}

makeDetectPeaksTable <- function(instance, peaks) {
	window <- gwindow(title=paste(instance$objectname, "Detect peaks", sep=" - "),
		width=100, height=100, visible=FALSE)
	group <- ggroup(horizontal=FALSE, container=window, expand=TRUE)
	subgroup <- ggroup(container=group, expand=TRUE)
	gtable(peaks@peaks, chosencol=1, container=subgroup, expand=TRUE,
		handler=handleDetectPeaksTable,
		action=instance)
	subgroup <- ggroup(container=group)
	glabel(text=paste("number of peaks:", nrow(peaks@peaks)), container=subgroup, expand=TRUE)
	buttongroup <- ggroup(container=group)
	exportAction <- gaction(label="Export to CSV", icon="save",
		handler=handlerDetectPeaksExport(instance, peaks),
		action=instance)
	closeAction <- gaction(label="Close", icon="cancel",
		handler=function(h, ...) dispose(window))
	exportButton <- gbutton(action=exportAction, container=buttongroup)
	closeButton <- gbutton(action=closeAction, container=buttongroup)
	visible(window) <- TRUE
	window
}

makeBatchPeaksControl <- function(instance) {
	object <- try(get(instance$objectname, envir=instance$objectenv), silent=TRUE)
	if ( inherits(object, "try-error") ) {
		gmessage(message=paste("Could not find object '", instance$objectname, "'.", sep=""), title="Error")
		return()
	}
	window <- gwindow(title=paste(instance$objectname, "Peak picking", sep=" - "),
		width=100, height=100, visible=FALSE)
	# the available ROIs
	group <- ggroup(horizontal=FALSE, container=window, expand=TRUE)
	instance$windows$batchPeaks$widgets$roiFrame <- gframe(text="available regions of interest", horizontal=FALSE,
		container=group, expand=TRUE)
	instance$windows$batchPeaks$widgets$roiChooser <- gtable(items=data.frame("ROI"=ls.ROI(object, .userEnvironment)),
		multiple=TRUE, container=instance$windows$batchPeaks$widgets$roiFrame, expand=TRUE)
	subgroup <- ggroup(container=group)
	glabel(text="(select multiple ROIs by holding down shift or ctrl)", container=subgroup, expand=TRUE)
	# peak picking parameters
	frame <- gframe(text="peak picking", horizontal=FALSE, container=group, expand=TRUE)
	subgroup <- ggroup(container=frame)
	glabel(text="Search", container=subgroup)
	instance$windows$batchPeaks$widgets$searchPercent <- gspinbutton(from=5,
		to=100, by=5, container=subgroup, expand=TRUE)
	svalue(instance$windows$batchPeaks$widgets$searchPercent) <- instance$windows$batchPeaks$search.percent
	glabel(text="% of spectra in the considered region.", container=subgroup)
	subgroup <- ggroup(container=frame)
	glabel(text="Peaks must occur in at least", container=subgroup)
	instance$windows$batchPeaks$widgets$minfreqPercent <- gspinbutton(from=1,
		to=100, by=1, container=subgroup, expand=TRUE)
	svalue(instance$windows$batchPeaks$widgets$minfreqPercent) <- instance$windows$batchPeaks$minfreq.percent
	glabel(text="% of considered spectra.", container=subgroup)
	# alignment parameters
	frame <- gframe(text="alignment", horizontal=FALSE, container=group, expand=TRUE)
	subgroup <- ggroup(container=frame)
	glabel(text="Aligned peaks must be within", container=subgroup)
	instance$windows$batchPeaks$widgets$alignDiffMax <- gedit(text=200, width=10,
		coerce.with=as.numeric, container=subgroup, expand=TRUE)
	svalue(instance$windows$batchPeaks$widgets$alignDiffMax) <- instance$windows$batchPeaks$align.diff.max
	instance$windows$batchPeaks$widgets$alignUnits <- gcombobox(items=c("ppm",
		"mz"), container=subgroup, expand=TRUE)
	svalue(instance$windows$batchPeaks$widgets$alignUnits) <- instance$windows$batchPeaks$align.units
	# name for the object
	subgroup <- ggroup(container=group)
	glabel(text="name of results", container=subgroup)
	instance$windows$batchPeaks$widgets$newObjectName <- gedit(text=paste(instance$objectname,
		"peaks", sep="."), container=subgroup, expand=TRUE)
	# select peaks buttons
	buttongroup <- ggroup(container=group)
	processAction <- gaction(label="Process", icon="ok",
		handler=handleBatchPeaksProcess,
		action=instance)
	cancelAction <- gaction(label="Cancel", icon="cancel",
		handler=handlerKillWindow(instance, "batchPeaks"),
		action=instance)
	processButton <- gbutton(action=processAction, container=buttongroup, expand=TRUE)
	cancelButton <- gbutton(action=cancelAction, container=buttongroup, expand=TRUE)
	addHandlerDestroy(window, handler=handlerKillWindow(instance, "batchPeaks"))
	instance$windows$batchPeaks$topWidgets$dialogWindow <- window
	visible(window) <- TRUE
	window
}

makeSelectPeaksControl <- function(instance) {
	window <- gwindow(title=paste(instance$objectname, "Select peaks", sep=" - "),
		width=100, height=100, visible=FALSE)
	# available peaks
	group <- ggroup(horizontal=FALSE, container=window)
	instance$windows$selectPeaks$widgets$peaksFrame <- gframe(text="available peaks",
		container=group)
	buttongroup <- ggroup(container=instance$windows$selectPeaks$widgets$peaksFrame)
	previewAction <- gaction(label="Preview", icon="contour",
		handler=handleSelectPeaksPreview,
		action=instance)
	previewButton <- gbutton(action=previewAction, container=buttongroup, expand=TRUE)
	instance$windows$selectPeaks$widgets$peaksChooser <- gcombobox(items=c("<none>",
		ls.MSPeakFrame(.userEnvironment)), container=instance$windows$selectPeaks$widgets$peaksFrame, expand=TRUE)
	# available peaks buttons
	buttongroup <- ggroup(container=group)
	loadAction <- gaction(label="Load", icon="open",
		handler=handleSelectPeaksLoad,
		action=instance)
	addAction <- gaction(label="Add", icon="symbol_plus",
		handler=handleSelectPeaksAdd,
		action=instance)
	removeAction <- gaction(label="Remove", icon="symbol_cross",
		handler=handleSelectPeaksRemove,
		action=instance)
	loadButton <- gbutton(action=loadAction, container=buttongroup, expand=TRUE)
	addButton <- gbutton(action=addAction, container=buttongroup, expand=TRUE)
	removeButton <- gbutton(action=removeAction, container=buttongroup, expand=TRUE)
	# selected peaks
	instance$windows$selectPeaks$widgets$selectedPeaksFrame <- gframe(text="selected peaks",
		container=group, expand=TRUE)
	instance$windows$selectPeaks$widgets$selectedPeaks <- gtable(instance$windows$selectPeaks$peaks@peaks,
		chosencol=1, multiple=TRUE, container=instance$windows$selectPeaks$widgets$selectedPeaksFrame, expand=TRUE,
		handler=handleSelectPeaksTable,
		action=instance)
	instance$windows$selectPeaks$widgets$numPeaksGroup <- ggroup(container=group)
	instance$windows$selectPeaks$widgets$numPeaks <- glabel(text=paste("number of peaks:",
		nrow(instance$windows$selectPeaks$peaks@peaks)),
		container=instance$windows$selectPeaks$widgets$numPeaksGroup, expand=TRUE)
	subgroup <- ggroup(container=group)
	glabel(text="(select multiple peaks by holding down shift or ctrl)", container=subgroup, expand=TRUE)
	# name for the object
	subgroup <- ggroup(container=group)
	glabel(text="name of results", container=subgroup)
	instance$windows$selectPeaks$widgets$newObjectName <- gedit(text=paste(instance$objectname,
		"peaks", sep="."), container=subgroup, expand=TRUE)
	# which spectrum to use
	frame <- gframe(text="spectrum", container=group)
	buttongroup <- ggroup(container=frame)
	selectAction <- gaction(label="Select peaks", icon="lines",
		handler=handleSelectPeaksSelect,
		action=instance)
	selectButton <- gbutton(action=selectAction, container=buttongroup, expand=TRUE)
	instance$windows$selectPeaks$widgets$spectrum <- gcombobox(items=c("pixel",
		"mean", "max"), container=frame, expand=TRUE,
		handler=handleSelectPeaksSpectrum,
		action=instance)
	# select peaks buttons
	buttongroup <- ggroup(container=group)
	saveAction <- gaction(label="Save", icon="ok",
		handler=handleSelectPeaksSave,
		action=instance)
	exportAction <- gaction(label="Export to CSV", icon="save",
		handler=handleSelectPeaksExport,
		action=instance)
	cancelAction <- gaction(label="Cancel", icon="cancel",
		handler=handlerKillWindow(instance, "selectPeaks"),
		action=instance)
	saveButton <- gbutton(action=saveAction, container=buttongroup, expand=TRUE)
	exportButton <- gbutton(action=exportAction, container=buttongroup, expand=TRUE)
	cancelButton <- gbutton(action=cancelAction, container=buttongroup, expand=TRUE)
	addHandlerDestroy(window, handler=handlerKillWindow(instance, "selectPeaks"))
	instance$windows$selectPeaks$topWidgets$dialogWindow <- window
	visible(window) <- TRUE
	window
}

makeBinControl <- function(instance) {
	window <- gwindow(title=paste(instance$objectname, "Bin", sep=" - "),
		width=100, height=100, visible=FALSE)
	group <- ggroup(horizontal=FALSE, container=window, expand=TRUE)
	# the method parameters
	instance$windows$bin$widgets$peaksFrame <- gframe(text="peaks", horizontal=FALSE,
		container=group, expand=TRUE)
	instance$windows$bin$widgets$peaksChooser <- gcombobox(items=c("<none>", ls.MSPeakFrame(.userEnvironment)),
		container=instance$windows$bin$widgets$peaksFrame, expand=TRUE,
		handler=handleBinPeaks,
		action=instance)
	frame <- gframe(text="spectra", horizontal=FALSE, container=group, expand=TRUE)
	subgroup <- ggroup(container=frame)
	glabel(text="width", container=subgroup)
	instance$windows$bin$widgets$width <- gedit(text=1, width=10,
		coerce.with=as.numeric, container=subgroup, expand=TRUE,
		handler=handleBinWidth,
		action=instance)
	svalue(instance$windows$bin$widgets$width) <- instance$windows$bin$width
	subgroup <- ggroup(container=frame)
	glabel(text="offset", container=subgroup)
	instance$windows$bin$widgets$offset <- gedit(text=0, width=10,
		coerce.with=as.numeric, container=subgroup, expand=TRUE,
		handler=handleBinOffset,
		action=instance)
	svalue(instance$windows$bin$widgets$offset) <- instance$windows$bin$offset
	# name for the object
	subgroup <- ggroup(container=group)
	glabel(text="name of results", container=subgroup)
	instance$windows$bin$widgets$newObjectName <- gedit(text=paste(instance$objectname,
		"binned", sep="."), container=subgroup, expand=TRUE)
	# whether to open a new instance
	subgroup <- ggroup(container=group, horizontal=FALSE)
	instance$windows$bin$widgets$openNewInstance <- gcheckbox(text="open in new instance",
			checked=FALSE, container=subgroup, expand=TRUE)
	# control buttons
	buttongroup <- ggroup(container=group)
	processAction <- gaction(label="Process", icon="ok",
		handler=handleBinProcess,
		action=instance)
	cancelAction <- gaction(label="Cancel", icon="cancel",
		handler=handlerKillWindow(instance, "bin"))
	instance$windows$bin$widgets$processButton <- gbutton(action=processAction,
		container=buttongroup, expand=TRUE)
	instance$windows$bin$widgets$cancelButton <- gbutton(action=cancelAction,
		container=buttongroup, expand=TRUE)
	addHandlerDestroy(window, handler=handlerKillWindow(instance, "bin"))
	instance$windows$bin$topWidgets$dialogWindow <- window
	visible(window) <- TRUE
	window
}

makeResampleControl <- function(instance) {
	window <- gwindow(title=paste(instance$objectname, "Resample", sep=" - "),
		width=100, height=100, visible=FALSE)
	group <- ggroup(horizontal=FALSE, container=window, expand=TRUE)
	# the method parameters
	instance$windows$resample$widgets$peaksFrame <- gframe(text="peaks", horizontal=FALSE,
		container=group, expand=TRUE)
	instance$windows$resample$widgets$peaksChooser <- gcombobox(items=c("<none>", ls.MSPeakFrame(.userEnvironment)),
		container=instance$windows$resample$widgets$peaksFrame, expand=TRUE,
		handler=handleResamplePeaks,
		action=instance)
	frame <- gframe(text="spectra", horizontal=FALSE, container=group, expand=TRUE)
	subgroup <- ggroup(container=frame)
	glabel(text="step", container=subgroup)
	instance$windows$resample$widgets$step <- gedit(text=1, width=10,
		coerce.width=as.numeric, container=subgroup, expand=TRUE,
		handler=handleResampleStep,
		action=instance)
	svalue(instance$windows$resample$widgets$step) <- instance$windows$resample$step
	subgroup <- ggroup(container=frame)
	glabel(text="offset", container=subgroup)
	instance$windows$resample$widgets$offset <- gedit(text=0, width=10,
		coerce.width=as.numeric, container=subgroup, expand=TRUE,
		handler=handleResampleOffset,
		action=instance)
	svalue(instance$windows$resample$widgets$offset) <- instance$windows$resample$offset
	# name for the object
	subgroup <- ggroup(container=group)
	glabel(text="name of results", container=subgroup)
	instance$windows$resample$widgets$newObjectName <- gedit(text=paste(instance$objectname,
		"resampled", sep="."), container=subgroup, expand=TRUE)
	# whether to open a new instance
	subgroup <- ggroup(container=group, horizontal=FALSE)
	instance$windows$resample$widgets$openNewInstance <- gcheckbox(text="open in new instance",
			checked=FALSE, container=subgroup, expand=TRUE)
	# control buttons
	buttongroup <- ggroup(container=group)
	processAction <- gaction(label="Process", icon="ok",
		handler=handleResampleProcess,
		action=instance)
	cancelAction <- gaction(label="Cancel", icon="cancel",
		handler=handlerKillWindow(instance, "resample"))
	instance$windows$resample$widgets$processButton <- gbutton(action=processAction,
		container=buttongroup, expand=TRUE)
	instance$windows$resample$widgets$cancelButton <- gbutton(action=cancelAction,
		container=buttongroup, expand=TRUE)
	addHandlerDestroy(window, handler=handlerKillWindow(instance, "resample"))
	instance$windows$resample$topWidgets$dialogWindow <- window
	visible(window) <- TRUE
	window
}



