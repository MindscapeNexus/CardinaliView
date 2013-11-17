
#### functions for ROI selection ####

makeROIControl <- function(instance) {
	object <- try(get(instance$objectname, envir=instance$objectenv), silent=TRUE)
	if ( inherits(object, "try-error") ) {
		gmessage(message=paste("Could not find object '", instance$objectname, "'.", sep=""), title="Error")
		return()
	}
	window <- gwindow(title=paste(instance$objectname, "Select regions of interest", sep=" - "),
		width=300, height=200, visible=FALSE)
	# the available ROIs
	group <- ggroup(horizontal=FALSE, container=window, expand=TRUE)
	instance$windows$roi$widgets$roiFrame <- gframe(text="available regions of interest", horizontal=FALSE,
		container=group, expand=TRUE)
	instance$windows$roi$widgets$roiChooser <- gtable(items=data.frame("ROI"=ls.ROI(object, .userEnvironment)),
		multiple=TRUE, container=instance$windows$roi$widgets$roiFrame, expand=TRUE)
	# the name for the new ROI
	subgroup <- ggroup(container=group)
	glabel(text="(select multiple ROIs by holding down shift or ctrl)", container=subgroup, expand=TRUE)
	subgroup <- ggroup(container=group)
	glabel(text="ROI name", container=subgroup)
	instance$windows$roi$widgets$newROIName <- gedit(text=paste(instance$objectname,
		"ROI", length(ls.ROI(object, .userEnvironment)) + 1, sep="."), container=subgroup, expand=TRUE)
	# whether to select pixel-by-pixel or a polygon
	subgroup <- ggroup(container=group, horizontal=FALSE)
	instance$windows$roi$widgets$selectPixelByPixel <- gcheckbox(text="select pixel-by-pixel",
		checked=FALSE, container=subgroup, expand=TRUE)
	# control buttons
	buttongroup <- ggroup(container=group)
	selectAction <- gaction(label="Select ROI", icon="lines",
		handler=handleSelectROI,
		action=instance)
	previewAction <- gaction(label="Preview", icon="contour",
		handler=handlePreviewROI,
		action=instance)
	cancelAction <- gaction(label="Close", icon="cancel",
		handler=handlerKillWindow(instance, "roi"))
	instance$windows$roi$widgets$selectROIButton <- gbutton(action=selectAction,
		container=buttongroup, expand=TRUE)
	instance$windows$roi$widgets$previewROIButton <- gbutton(action=previewAction,
		container=buttongroup, expand=TRUE)
	instance$windows$roi$widgets$cancelROIButton <- gbutton(action=cancelAction,
		container=buttongroup, expand=TRUE)
	# make the window
	addHandlerDestroy(window, handler=handlerKillWindow(instance, "roi"))
	instance$windows$roi$topWidgets$dialogWindow <- window
	visible(window) <- TRUE
	window
}

makeCropControl <- function(instance) {
	object <- try(get(instance$objectname, envir=instance$objectenv), silent=TRUE)
	if ( inherits(object, "try-error") ) {
		gmessage(message=paste("Could not find object '", instance$objectname, "'.", sep=""), title="Error")
		return()
	}
	window <- gwindow(title=paste(instance$objectname, "Crop", sep=" - "),
		width=300, height=200, visible=FALSE)
	# the available ROIs
	group <- ggroup(horizontal=FALSE, container=window, expand=TRUE)
	instance$windows$crop$widgets$roiFrame <- gframe(text="available regions of interest", horizontal=FALSE,
		container=group, expand=TRUE)
	instance$windows$crop$widgets$roiChooser <- gtable(items=data.frame("ROI"=ls.ROI(object, .userEnvironment)),
		multiple=TRUE, container=instance$windows$crop$widgets$roiFrame, expand=TRUE)
	subgroup <- ggroup(container=group)
	glabel(text="(select multiple ROIs by holding down shift or ctrl)", container=subgroup, expand=TRUE)
	# the mass range
	frame <- gframe(text="mass range", container=group, expand=TRUE)
	subgroup <- ggroup(container=frame, expand=TRUE)
	glabel(text="min", container=subgroup)
	instance$windows$crop$widgets$massMin <- gedit(text=round(min(mz(object)), digits=2),
		coerce.with=as.numeric, width=10, container=subgroup, expand=TRUE)
	subgroup <- ggroup(container=frame, expand=TRUE)
	glabel(text="max", container=subgroup)
	instance$windows$crop$widgets$massMax <- gedit(text=round(max(mz(object)), digits=2),
		coerce.with=as.numeric, width=10, container=subgroup, expand=TRUE)
	# name for the object
	subgroup <- ggroup(container=group)
	glabel(text="name of results", container=subgroup)
	instance$windows$crop$widgets$newObjectName <- gedit(text=paste(instance$objectname,
		"cropped", sep="."), container=subgroup, expand=TRUE)
	# whether to open a new instance
	subgroup <- ggroup(container=group, horizontal=FALSE)
	instance$windows$crop$widgets$openNewInstance <- gcheckbox(text="open in new instance",
			checked=FALSE, container=subgroup, expand=TRUE)
	# control buttons
	buttongroup <- ggroup(container=group)
	cropAction <- gaction(label="Crop", icon="cut",
		handler=handleCrop,
		action=instance)
	cancelAction <- gaction(label="Close", icon="cancel",
		handler=handlerKillWindow(instance, "crop"))
	instance$windows$crop$widgets$cropButton <- gbutton(action=cropAction,
		container=buttongroup, expand=TRUE)
	instance$windows$crop$widgets$cancelButton <- gbutton(action=cancelAction,
		container=buttongroup, expand=TRUE)
	# make the window
	addHandlerDestroy(window, handler=handlerKillWindow(instance, "crop"))
	instance$windows$crop$topWidgets$dialogWindow <- window
	visible(window) <- TRUE
	window
}

