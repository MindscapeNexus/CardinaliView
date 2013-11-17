
#### functions for editing ####

updateROIlists <- function(instance) {
	object <- try(get(instance$objectname, envir=instance$objectenv), silent=TRUE)
	if ( inherits(object, "try-error") ) return()
	for ( i in seq_along(instance$windows) ) {
		if ( any(names(instance$windows[[i]]$widgets) %in% c("roiFrame", "roiChooser"))
			&& isExtant(instance$windows[[i]]$widgets$roiFrame)
			&& isExtant(instance$windows[[i]]$widgets$roiChooser) )
		{
			delete(instance$windows[[i]]$widgets$roiFrame,
				instance$windows[[i]]$widgets$roiChooser)
			instance$windows[[i]]$widgets$roiChooser <- gtable(items=data.frame("ROI"=ls.ROI(object, .userEnvironment)),
				multiple=TRUE, container=instance$windows[[i]]$widgets$roiFrame, expand=TRUE)
		}
	}
}

makeSegmentedROI <- function(instance, windowname) {
	objectnames <- svalue(instance$windows[[windowname]]$widgets$roiChooser)
	if ( length(objectnames) < 1 || any(is.na(objectnames)) ) {
		return(NULL)
	} else {
		objectnames <- as.character(objectnames)
	}
	rois <- list()
	for ( i in seq_along(objectnames) ) {
		rois[[i]] <- get(objectnames[[i]], envir=.userEnvironment)
	}
	multiple <- length(objectnames) > 1
	if ( multiple ) {
		labels <- objectnames
	} else {
		labels <- c(objectnames, paste("NOT", objectnames))
	}
	roi <- do.call(roiBind, c(rois, list(labels=labels, na.replace=!multiple)))
	roi
}

makeCombinedROI <- function(instance, windowname) {
	objectnames <- svalue(instance$windows[[windowname]]$widgets$roiChooser)
	if ( length(objectnames) < 1 || any(is.na(objectnames)) ) {
		return(NULL)
	} else {
		objectnames <- as.character(objectnames)
	}
	rois <- list()
	for ( i in seq_along(objectnames) ) {
		rois[[i]] <- get(objectnames[[i]], envir=.userEnvironment)
	}
	crop <- logical(length(rois[[1]]))
	for ( i in seq_along(rois) ) {
		crop[rois[[i]]] <- TRUE
	}
	crop
}

#### functions for editing ####

handleFlipHorizontal <- function(h, ...) {
	object <- try(get(h$action$objectname, envir=h$action$objectenv), silent=TRUE)
	if ( inherits(object, "try-error") ) return()
	object <- flipHorizontal(object, sliceDimNames=h$action$parameters$sliceDimNames)
	assign(h$action$objectname, object, envir=h$action$objectenv)
	setPixel(h$action, h$action$parameters$pixel)
	updateImages(h$action)
}

handleFlipVertical <- function(h, ...) {
	object <- try(get(h$action$objectname, envir=h$action$objectenv), silent=TRUE)
	if ( inherits(object, "try-error") ) return()
	object <- flipVertical(object, sliceDimNames=h$action$parameters$sliceDimNames)
	assign(h$action$objectname, object, envir=h$action$objectenv)
	setPixel(h$action, h$action$parameters$pixel)
	updateImages(h$action)
}

handleRotateLeft <- function(h, ...) {
	object <- try(get(h$action$objectname, envir=h$action$objectenv), silent=TRUE)
	if ( inherits(object, "try-error") ) return()
	object <- rotateLeft(object, sliceDimNames=h$action$parameters$sliceDimNames)
	assign(h$action$objectname, object, envir=h$action$objectenv)
	setPixel(h$action, h$action$parameters$pixel)
	updateImages(h$action)
}

handleRotateRight <- function(h, ...) {
	object <- try(get(h$action$objectname, envir=h$action$objectenv), silent=TRUE)
	if ( inherits(object, "try-error") ) return()
	object <- rotateRight(object, sliceDimNames=h$action$parameters$sliceDimNames)
	assign(h$action$objectname, object, envir=h$action$objectenv)
	setPixel(h$action, h$action$parameters$pixel)
	updateImages(h$action)
}

handleSelectROI <- function(h, ...) {
	object <- try(get(h$action$objectname, envir=h$action$objectenv), silent=TRUE)
	if ( inherits(object, "try-error") ) {
		gmessage(message=paste("Could not find object '", h$action$objectname, "'.", sep=""), title="Error")
		return()
	}
	name <- svalue(h$action$windows$roi$widgets$newROIName)
	galert(message="Select the region of interest on an MS image and press ESC when done.", title="Select an ROI")
	if ( getOption("guiToolkit") == "RGtk2" ) {
		for ( i in seq_along(h$action$windows) ) {
			for ( j in seq_along(h$action$windows[[i]]$gtkWidgets) ) {
				if ( grepl(pattern="image", x=names(h$action$windows[[i]]$gtkWidgets[j])) ) {
					widget <- h$action$windows[[i]]$gtkWidgets[[j]]$child
					widget$getWindow()$setCursor(gdkCursorNew(GdkCursorType["hand1"]))
				}
			}
		}
		.guiState$nextCallback <- function() {
			loc <- list(x=grconvertX(.guiState$pxSelect$x, from="device", to="user"),
					y=grconvertY(.guiState$pxSelect$y, from="device", to="user"))
			if ( svalue(h$action$windows$roi$widgets$selectPixelByPixel) ) {
				roi <- Cardinal:::selectPixelsHelper(loc, object=object,
					sliceDimNames=h$action$parameters$sliceDimNames,
					fixCoord=h$action$parameters$fixCoord)
			} else {
				roi <- Cardinal:::selectROIHelper(loc, object=object,
					sliceDimNames=h$action$parameters$sliceDimNames,
					fixCoord=h$action$parameters$fixCoord)
			}
			assign(name, roi, envir=.userEnvironment)
			svalue(h$action$windows$roi$widgets$newROIName) <- paste(h$action$objectname,
				"ROI", length(ls.ROI(object, .userEnvironment)) + 1, sep=".")
			updateROIlists(h$action)
			NULL
		}
		if ( svalue(h$action$windows$roi$widgets$selectPixelByPixel) ) {
			.guiState$isSelectingPoints <- TRUE
			.guiState$initiatingSelectPoints <- TRUE
		} else {
			.guiState$isSelectingShape <- TRUE
			.guiState$initiatingSelectShape <- TRUE
		}
	} else {
		setDevice(h$action, svalue(h$action$windows$main$widgets$imageActiveDevice), "image")
		if ( svalue(h$action$windows$roi$widgets$selectPixelByPixel) ) {
			roi <- selectPixels(object, sliceDimNames=h$action$parameters$sliceDimNames,
				fixCoord=h$action$parameters$fixCoord, silent=TRUE)
		} else {
			roi <- selectROI(object, sliceDimNames=h$action$parameters$sliceDimNames,
				fixCoord=h$action$parameters$fixCoord, silent=TRUE)
		}
		assign(name, roi, envir=.userEnvironment)
		svalue(h$action$windows$roi$widgets$newROIName) <- paste(h$action$objectname,
			"ROI", length(ls.ROI(object, .userEnvironment)) + 1, sep=".")
		updateROIlists(h$action)
	}
}

handlePreviewROI <- function(h, ...) {
	roi <- makeSegmentedROI(h$action, "roi")
	if ( is.null(roi) ) return()
	object <- try(get(h$action$objectname, envir=h$action$objectenv), silent=TRUE)
	if ( inherits(object, "try-error") ) {
		gmessage(message=paste("Could not find object '", h$action$objectname, "'.", sep=""), title="Error")
		return()
	}
	updateImages(h$action)
	setDevice(h$action, "main", "image")
	image(object, values=roi, col=rainbow(length(levels(roi)), alpha=0.5),
		breaks=seq(0, length(levels(roi))) + 0.5,
		sliceDimNames=h$action$parameters$sliceDimNames,
		fixCoord=h$action$parameters$fixCoord, add=TRUE)
	legend("topright", legend=levels(roi), fill=rainbow(length(levels(roi))),
		bg=rgb(1, 1, 1, 0.75))
}

handleCrop <- function(h, ...) {
	object <- try(get(h$action$objectname, envir=h$action$objectenv), silent=TRUE)
	if ( inherits(object, "try-error") ) {
		gmessage(message=paste("Could not find object '", h$action$objectname, "'.", sep=""), title="Error")
		return()
	}
	pixel <- makeCombinedROI(h$action, "crop")
	if ( is.null(pixel) ) pixel <- 1:numPixels(object)
	mz <- as.numeric(c(svalue(h$action$windows$crop$widgets$massMin),
		svalue(h$action$windows$crop$widgets$massMax)))
	if ( any(is.na(mz)) ) return()
	object <- crop(object, pixel=pixel, mz=mz)
	newInstance <- svalue(h$action$windows$crop$widgets$openNewInstance)
	objectname <- svalue(h$action$windows$crop$widgets$newObjectName)
	objectenv <- .userEnvironment
	assign(objectname, object, envir=objectenv)
	dispose(h$action$windows$crop$topWidgets$dialogWindow)
	loadNewDataset(h$action, objectname, objectenv, newInstance=newInstance)
}

