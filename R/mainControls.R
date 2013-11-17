
#### functions for making main control panels ####

makePlotControls <- function(instance) {
	window <- gwindow(title=paste(instance$name, "Mass spectrum", sep=" - "),
		width=100, height=100, visible=FALSE)
	gmenu(menulist=makeMenu(instance), container=window)
	group <- ggroup(horizontal=FALSE, container=window, expand=TRUE)
	# options
	frame <- gframe(text="options", container=group, expand=TRUE)
	instance$windows$main$widgets$plotCommonIntensity <- gcheckbox(text="common intensity scale",
		checked=FALSE, container=frame, expand=TRUE,
		handler=handlePlotCommonIntensity,
		action=instance)
	# intensity range
	frame <- gframe(text="intensity range", horizontal=FALSE, container=group, expand=TRUE)
	subgroup <- ggroup(container=frame)
	glabel(text="min", container=subgroup)
	instance$windows$main$widgets$plotMinIntensity <- gedit(text=instance$parameters$plot.intensityRange[[1]],
		coerce.with=as.numeric, width=10, container=subgroup, expand=TRUE,
		handler=handlePlotMinIntensity,
		action=instance)
	subgroup <- ggroup(container=frame)
	glabel(text="max", container=subgroup)
	instance$windows$main$widgets$plotMaxIntensity <- gedit(text=instance$parameters$plot.intensityRange[[2]],
		coerce.with=as.numeric, width=10, container=subgroup, expand=TRUE,
		handler=handlePlotMaxIntensity,
		action=instance)
	# smoothing filter
	frame <- gframe(text="filter", horizontal=FALSE, container=group, expand=TRUE)
	instance$windows$main$widgets$filter <- gcombobox(items=c("none",
		"gaussian", "sgolay", "kaiser", "ma"), container=frame, expand=TRUE,
		handler=handleFilter,
		action=instance)
	subgroup <- ggroup(container=frame)
	glabel(text="window", container=subgroup)
	instance$windows$main$widgets$filterWindow <- gspinbutton(from=3,
		to=101, by=2, value=instance$windows$main$filter.window,
		container=subgroup, expand=TRUE,
		handler=handleFilterWindow,
		action=instance)
	# mass range
	frame <- gframe(text="mass range", horizontal=FALSE, container=group, expand=TRUE)
	subgroup <- ggroup(container=frame)
	glabel(text="mass", container=subgroup)
	instance$windows$main$widgets$mass <- gedit(text=round(instance$parameters$mz, digits=2),
		coerce.with=as.numeric, width=10, container=subgroup, expand=TRUE,
		handler=handleMass,
		action=instance)
	subgroup <- ggroup(container=frame)
	glabel(text="min", container=subgroup)
	instance$windows$main$widgets$massMin <- gedit(text=round(instance$parameters$mzRange[[1]], digits=2),
		coerce.with=as.numeric, width=10, container=subgroup, expand=TRUE,
		handler=handleMassMin,
		action=instance)
	subgroup <- ggroup(container=frame)
	glabel(text="max", container=subgroup)
	instance$windows$main$widgets$massMax <- gedit(text=round(instance$parameters$mzRange[[2]], digits=2),
		coerce.with=as.numeric, width=10, container=subgroup, expand=TRUE,
		handler=handleMassMax,
		action=instance)
	subgroup <- ggroup(container=frame, expand=TRUE)
	instance$windows$main$widgets$massPrev <- gbutton(text="< Prev",
		container=subgroup, expand=TRUE,
		handler=handleMassPrev,
		action=instance)
	instance$windows$main$widgets$massNext <- gbutton(text="Next >",
		container=subgroup, expand=TRUE,
		handler=handleMassNext,
		action=instance)
	subgroup <- ggroup(container=frame, expand=TRUE)
	instance$windows$main$widgets$browsePeaks <- gbutton(text="Browse peaks",
		container=subgroup, expand=TRUE,
		handler=handleBrowsePeaks,
		action=instance)
	# zoom buttons
	buttongroup <- gframe(text="zoom", horizontal=TRUE, container=group, expand=TRUE)
	instance$windows$main$widgets$plotZoomOut <- gbutton(text="-",
		container=buttongroup, expand=TRUE,
		handler=handlePlotZoomOut,
		action=instance)
	instance$windows$main$widgets$plotZoomFit <- gbutton(text="100%",
		container=buttongroup, expand=TRUE,
		handler=handlePlotZoomFit,
		action=instance)
	instance$windows$main$widgets$plotZoomIn <- gbutton(text="+",
		container=buttongroup, expand=TRUE,
		handler=handlePlotZoomIn,
		action=instance)
	# selection buttons
	if ( getOption("guiToolkit") != "RGtk2" ) {
		instance$windows$main$widgets$plotDeviceFrame <- frame <- gframe(text="active device",
			horizontal=FALSE, container=group, expand=TRUE)
		instance$windows$main$widgets$plotActiveDevice <- gcombobox(items=activeDevices(instance, "plot"),
			container=instance$windows$main$widgets$plotDeviceFrame, expand=TRUE)
		buttongroup <- ggroup(horizontal=FALSE, container=group, expand=TRUE)
		instance$windows$main$widgets$selectMass <- gbutton(text="Select mass",
			container=buttongroup, expand=TRUE,
			handler=handleSelectMass,
			action=instance)
		instance$windows$main$widgets$selectMassRange <- gbutton(text="Select zoom region",
			container=buttongroup, expand=TRUE,
			handler=handleSelectPlotZoom,
			action=instance)
	}
	addHandlerDestroy(window, handler=handleMainDestroy, action=instance)
	tryWindowPlacement(instance, window, "plot")
	instance$windows$main$topWidgets$plotControlWindow <- window
	visible(window) <- TRUE
	window
}

makeImageControls <- function(instance) {
	window <- gwindow(title=paste(instance$name, "Ion image", sep=" - "),
		width=100, height=100, visible=FALSE)
	gmenu(menulist=makeMenu(instance), container=window)
	group <- ggroup(horizontal=FALSE, container=window, expand=TRUE)
	# options
	frame <- gframe(text="options", container=group, expand=TRUE)
	instance$windows$main$widgets$imageCommonIntensity <- gcheckbox(text="common intensity scale",
		checked=FALSE, container=frame, expand=TRUE,
		handler=handleImageCommonIntensity,
		action=instance)
	# intensity range
	frame <- gframe(text="intensity range", horizontal=FALSE, container=group, expand=TRUE)
	subgroup <- ggroup(container=frame)
	glabel(text="min", container=subgroup)
	instance$windows$main$widgets$imageMinIntensity <- gedit(text=instance$parameters$image.intensityRange[[1]],
		coerce.with=as.numeric, width=10, container=subgroup, expand=TRUE,
		handler=handleImageMinIntensity,
		action=instance)
	subgroup <- ggroup(container=frame)
	glabel(text="max", container=subgroup)
	instance$windows$main$widgets$imageMaxIntensity <- gedit(text=instance$parameters$image.intensityRange[[2]],
		coerce.with=as.numeric, width=10, container=subgroup, expand=TRUE,
		handler=handleImageMaxIntensity,
		action=instance)
	# contrast enhancement
	frame <- gframe(text="contrast", horizontal=FALSE, container=group, expand=TRUE)
	instance$windows$main$widgets$contrast <- gcombobox(items=c("none",
		"suppress", "histeq"), container=frame, expand=TRUE,
		handler=handleContrast,
		action=instance)
	# smoothing filter
	frame <- gframe(text="smoothing", horizontal=FALSE, container=group, expand=TRUE)
	instance$windows$main$widgets$smoothing <- gcombobox(items=c("none",
		"gaussian", "adaptive"), container=frame, expand=TRUE,
		handler=handleSmoothing,
		action=instance)
	subgroup <- ggroup(container=frame)
	glabel(text="window", container=subgroup)
	instance$windows$main$widgets$smoothingWindow <- gspinbutton(from=3,
		to=11, by=2, value=instance$windows$main$smoothing.window,
		container=subgroup, expand=TRUE,
		handler=handleSmoothingWindow,
		action=instance)
	# interpolation
	frame <- gframe(text="interpolate", horizontal=FALSE, container=group, expand=TRUE)
	instance$windows$main$widgets$interpolate <- gcombobox(items=c("none",
		"bilinear"), container=frame, expand=TRUE,
		handler=handleInterpolate,
		action=instance)
	subgroup <- ggroup(container=frame)
	glabel(text="xres", container=subgroup)
	instance$windows$main$widgets$interpolateRes <- gspinbutton(from=2,
		to=9, by=1, value=instance$windows$main$interpolate.xres,
		container=subgroup, expand=TRUE,
		handler=handleInterpolateRes,
		action=instance)
	# coordinate limits
	frame <- gframe(text="coordinates", horizontal=FALSE, container=group, expand=TRUE)
	subgroup <- ggroup(container=frame)
	glabel(text="pixel", container=subgroup)
	instance$windows$main$widgets$pixel <- gedit(text=instance$parameters$pixel,
		coerce.with=as.numeric, width=10, container=subgroup, expand=TRUE,
		handler=handlePixel,
		action=instance)
	subgroup <- ggroup(container=frame)
	glabel(text=instance$parameters$sliceDimNames[[1]], container=subgroup)
	instance$windows$main$widgets$coordX <- gedit(text=instance$parameters$coord[[instance$parameters$sliceDimNames[[1]]]],
		coerce.with=as.numeric, width=10, container=subgroup, expand=TRUE,
		handler=handleCoordX,
		action=instance)
	subgroup <- ggroup(container=frame)
	glabel(text=instance$parameters$sliceDimNames[[2]], container=subgroup)
	instance$windows$main$widgets$coordY <- gedit(text=instance$parameters$coord[[instance$parameters$sliceDimNames[[1]]]],
		coerce.with=as.numeric, width=10, container=subgroup, expand=TRUE,
		handler=handleCoordY,
		action=instance)
	subgroup <- ggroup(container=frame, expand=TRUE)
	instance$windows$main$widgets$browseSlices <- gbutton(text="Browse slices",
		container=subgroup, expand=TRUE,
		handler=handleBrowseSlices,
		action=instance)
	# zoom buttons
	buttongroup <- gframe(text="zoom", horizontal=TRUE, container=group, expand=TRUE)
	instance$windows$main$widgets$imageZoomOut <- gbutton(text="-",
		container=buttongroup, expand=TRUE,
		handler=handleImageZoomOut,
		action=instance)
	instance$windows$main$widgets$imageZoomFit <- gbutton(text="100%",
		container=buttongroup, expand=TRUE,
		handler=handleImageZoomFit,
		action=instance)
	instance$windows$main$widgets$imageZoomIn <- gbutton(text="+",
		container=buttongroup, expand=TRUE,
		handler=handleImageZoomIn,
		action=instance)
	# selection buttons
	if ( getOption("guiToolkit") != "RGtk2" ) {
		instance$windows$main$widgets$imageDeviceFrame <- frame <- gframe(text="active device",
			horizontal=FALSE, container=group, expand=TRUE)
		instance$windows$main$widgets$imageActiveDevice <- gcombobox(items=activeDevices(instance, "image"),
			container=instance$windows$main$widgets$imageDeviceFrame, expand=TRUE)
		buttongroup <- ggroup(horizontal=FALSE, container=group, expand=TRUE)
		instance$windows$main$widgets$selectPixel <- gbutton(text="Select pixel",
			container=buttongroup, expand=TRUE,
			handler=handleSelectPixel,
			action=instance)
		instance$windows$main$widgets$selectCoordRange <- gbutton(text="Select zoom region",
			container=buttongroup, expand=TRUE,
			handler=handleSelectImageZoom,
			action=instance)
	}
	addHandlerDestroy(window, handler=handleMainDestroy, action=instance)
	tryWindowPlacement(instance, window, "image")
	instance$windows$main$topWidgets$imageControlWindow <- window
	visible(window) <- TRUE
	window
}

makePeaksControl <- function(instance) {
	object <- try(get(instance$objectname, envir=instance$objectenv), silent=TRUE)
	if ( inherits(object, "try-error") ) return()
	peaks <- object@peaks
	window <- gwindow(title=paste(instance$objectname, "Peaks", sep=" - "),
		width=100, height=100, visible=FALSE)
	group <- ggroup(horizontal=FALSE, container=window, expand=TRUE)
	subgroup <- ggroup(container=group, expand=TRUE)
	instance$windows$peaks$widgets$table <- gtable(peaks@peaks, chosencol=1,
		container=subgroup, expand=TRUE,
		handler=handlePeaksTable,
		action=instance)
	subgroup <- ggroup(container=group)
	glabel(text=paste("number of peaks:", nrow(peaks@peaks)), container=subgroup, expand=TRUE)
	buttongroup <- ggroup(container=group)
	exportAction <- gaction(label="Export to CSV", icon="save",
		handler=handlerPeaksExport(instance, peaks),
		action=instance)
	closeAction <- gaction(label="Close", icon="cancel",
		handler=handlerKillWindow(instance, "peaks"))
	instance$windows$peaks$widgets$exportButton <- gbutton(action=exportAction, container=buttongroup)
	instance$windows$peaks$widgets$closeButton <- gbutton(action=closeAction, container=buttongroup)
	addHandlerDestroy(window, handler=handlerKillWindow(instance, "peaks"))
	instance$windows$peaks$topWidgets$controlWindow <- window
	visible(window) <- TRUE
	window
}

makeSlicesControl <- function(instance) {
	object <- try(get(instance$objectname, envir=instance$objectenv), silent=TRUE)
	if ( inherits(object, "try-error") ) return()
	window <- gwindow(title=paste(instance$objectname, "Slices", sep=" - "),
		width=200, height=100, visible=FALSE)
	group <- ggroup(horizontal=FALSE, container=window, expand=TRUE)
	instance$windows$slices$widgets$chooseSliceDimNamesFrame <- gframe(text="sliced dimensions", container=group)
	instance$windows$slices$widgets$chooseSliceDimNames <- gcombobox(items=makeSliceDimNames(instance, object),
		container=instance$windows$slices$widgets$chooseSliceDimNamesFrame, expand=TRUE,
		handler=handleSliceDimNames,
		action=instance)
	instance$windows$slices$widgets$chooseFixCoordFrame <- gframe(text="fixed coordinates", container=group)
	instance$windows$slices$widgets$chooseFixCoord <- gcombobox(items=makeFixCoord(instance, object),
		container=instance$windows$slices$widgets$chooseFixCoordFrame, expand=TRUE,
		handler=handleFixCoord,
		action=instance)
	addHandlerDestroy(window, handler=handlerKillWindow(instance, "slices"))
	instance$windows$slices$topWidgets$controlWindow <- window
	visible(window) <- TRUE
	window
}

