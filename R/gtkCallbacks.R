
#### functions for Gtk+ widget callback functions ####

gtkClickOrDrag <- function(widget, event, data) {
	setDevice(data$instance, data$windowname, data$devicename)
	.guiState$initiatingButtonPress <- TRUE
	if ( grepl(pattern="plot", x=data$devicename) ) {
		type <- "plot"
	} else if ( grepl(pattern="image", x=data$devicename) ) {
		type <- "image"
	}
	if ( type == "plot" ) {
		if ( event$type == GdkEventType["2button-press"] ) {
			return(gtkDoubleClickPlot(widget, event, data))
		} else if ( isTRUE(.guiState$isSelectingShape) ) {
			return(TRUE)
		} else if ( isTRUE(.guiState$isSelectingPoints) ) {
			return(TRUE)
		} else if ( isTRUE(.guiState$initiatingSelectPeakBounds) ) {
			return(gtkSelectPeakBounds(widget, event, data))
		} else if ( isTRUE(.guiState$isSelectingPeakBounds) ) {
			gtkDrawClickOrDrag(widget, event, shape="vline", style="on-off-dash",  fgcolor="blue")
			return(TRUE)
		} else {
			return(gtkClickOrDragPlot(widget, event, data))
		}
	} else if ( type == "image" ) {
		if ( event$type == GdkEventType["2button-press"] ) {
			return(gtkDoubleClickImage(widget, event, data))
		} else if ( isTRUE(.guiState$initiatingSelectShape) ) {
			return(gtkSelectShape(widget, event, data))
		} else if ( isTRUE(.guiState$isSelectingShape) ) {
			gtkDrawClickOrDrag(widget, event, shape="line", style="double-dash", fgcolor="black")
			return(TRUE)
		} else if ( isTRUE(.guiState$initiatingSelectPoints) ) {
			return(gtkSelectPoints(widget, event, data))
		} else if ( isTRUE(.guiState$isSelectingPoints) ) {
			gtkDrawClickOrDrag(widget, event, shape="none", style="solid")
			return(TRUE)
		} else if ( isTRUE(.guiState$isSelectingPeakBounds) ) {
			return(TRUE)
		} else {
			return(gtkClickOrDragImage(widget, event, data))
		}
	}
	TRUE
}

gtkClickOrDragPlot <- function(widget, event, data) {
	if ( data$instance$windows$main$plot.options[["common intensity scale"]] ) {
		px <- gtkDrawClickOrDrag(widget, event, shape="box",
			style="double-dash", fgcolor="blue")
	} else {
		px <- gtkDrawClickOrDrag(widget, event, shape="vline",
			style="on-off-dash", fgcolor="blue")
	}
	xyStart <- list(x=grconvertX(px$start$x, from="device", to="user"),
		y=grconvertY(px$start$y, from="device", to="user"))
	xyEnd <- list(x=grconvertX(px$end$x, from="device", to="user"),
		y=grconvertY(px$end$y, from="device", to="user"))
	object <- try(get(data$instance$objectname, envir=data$instance$objectenv), silent=TRUE)
	if ( inherits(object, "try-error") ) return(TRUE)
	mzRange <- sort(c(xyStart$x, xyEnd$x))
	Sys.sleep(0.25) # in case of double-clicks
	if ( !.guiState$initiatingButtonPress ) return(TRUE)
	if ( abs(diff(features(object, mz=mzRange))) > 1 ) {
		setMZRange(data$instance, mzRange)
		if ( data$instance$windows$main$plot.options[["common intensity scale"]] ) {
			intensityRange <- sort(c(xyStart$y, xyEnd$y))
			setPlotIntensityRange(data$instance, intensityRange)
		}
		updatePlots(data$instance)
	} else {
		setMZ(data$instance, mzRange[[1]])
		updateAll(data$instance)
	}
	TRUE
}

gtkClickOrDragImage <- function(widget, event, data) {
	px <- gtkDrawClickOrDrag(widget, event, shape="box", style="double-dash",
		fgcolor="white", bgcolor="black")
	xyStart <- list(x=grconvertX(px$start$x, from="device", to="user"),
		y=grconvertY(px$start$y, from="device", to="user"))
	xyEnd <- list(x=grconvertX(px$end$x, from="device", to="user"),
		y=grconvertY(px$end$y, from="device", to="user"))
	coordRange <- round(c(sort(c(xyStart$x, xyEnd$x)), sort(c(xyStart$y, xyEnd$y))))
	Sys.sleep(0.25) # in case of double-clicks
	if ( !.guiState$initiatingButtonPress ) return(TRUE)
	if ( abs(diff(coordRange[c(1,2)])) > 1 && abs(diff(coordRange[c(3,4)])) > 1 ) {
		setCoordRange(data$instance, coordRange)
		updateImages(data$instance)
	} else {
		coord <- data$instance$parameters$coord
		coord[data$instance$parameters$sliceDimNames] <- coordRange[c(1,3)]
		setCoord(data$instance, coord)
		updateAll(data$instance)
	}
	TRUE
}

gtkDoubleClickPlot <- function(widget, event, data) {
	.guiState$initiatingButtonPress <- FALSE
	h <- list(action=data$instance)
	handlePlotZoomFit(h)
	TRUE
}

gtkDoubleClickImage <- function(widget, event, data) {
	.guiState$initiatingButtonPress <- FALSE
	h <- list(action=data$instance)
	handleImageZoomFit(h)
	TRUE
}

gtkSelectShape <- function(widget, event, data) {
	px <- gtkDrawClickOrDrag(widget, event, shape="line", style="double-dash",
		fgcolor="black", bgcolor="white")
	.guiState$initiatingSelectShape <- FALSE
	.guiState$pxSelect <- list(x=c(px$start$x, px$end$x), y=c(px$start$y, px$end$y))
	xy <- list(x=grconvertX(.guiState$pxSelect$x, from="device", to="user"),
		y=grconvertY(.guiState$pxSelect$y, from="device", to="user"))
	lines(xy$x, xy$y, pch=20, type="b", col="white", lwd=1.5)
	widget$getWindow()$setCursor(gdkCursorNew(GdkCursorType["hand1"]))
	pressAnyKey <- function(widget, event, data) {
		.guiState$isSelectingShape <- FALSE
	    TRUE
	}
	gdkgc <- gdkGCNew(widget$window)
	gdkgc$setRgbFgColor(gdkColorParse("black")$color)
	gdkgc$setRgbBgColor(gdkColorParse("white")$color)
    gdkgc$setLineAttributes(line.width=1, line.style=GdkLineStyle["double-dash"],
    	cap.style=GdkCapStyle["butt"], join.style=GdkJoinStyle["miter"])
    gdkgc$setDashes(c(8, 4))
	escapeSignal <- gSignalConnect(widget$parent, "key-press-event", pressAnyKey, data=data)
	repeat {
		if ( !.guiState$isSelectingShape ) {
			points(0, 0, type="n") # plotting resets the cursor
			break
		}
		if ( !.guiState$isDragging ) {
			pxCurrent <- widget$window$getPointer()
			if ( tail(.guiState$pxSelect$x, 1) != .guiState$pxEnd$x || tail(.guiState$pxSelect$y, 1) != .guiState$pxEnd$y ) {
				.guiState$pxSelect$x <- c(.guiState$pxSelect$x, .guiState$pxStart$x, .guiState$pxEnd$x)
				.guiState$pxSelect$y <- c(.guiState$pxSelect$y, .guiState$pxStart$y, .guiState$pxEnd$y)
				xy <- list(x=grconvertX(.guiState$pxSelect$x, from="device", to="user"),
					y=grconvertY(.guiState$pxSelect$y, from="device", to="user"))
				lines(xy$x, xy$y, pch=20, type="b", col="white", lwd=1.5)
				widget$getWindow()$setCursor(gdkCursorNew(GdkCursorType["hand1"]))
			}
			if ( pxCurrent$x != .guiState$pxEnd$x || pxCurrent$y != .guiState$pxEnd$y ) {
				widget$window$invalidateRect(invalidate.children=FALSE)
				gdkDrawLine(widget$window, gc=gdkgc,
					x1=.guiState$pxEnd$x, y1=.guiState$pxEnd$y,
					x2=pxCurrent$x, y2=pxCurrent$y)
			}
		}
		gdkWindowProcessAllUpdates()
		while (gtkEventsPending()) gtkMainIterationDo(blocking=FALSE)
	}
	gSignalHandlerDisconnect(widget$parent, escapeSignal)
	widget$window$invalidateRect(invalidate.children=FALSE)
	x <- .guiState$pxSelect$x
	y <- .guiState$pxSelect$y
	xyUnique <- !(duplicated(x) & duplicated(y))
	.guiState$pxSelect <- list(x=x[xyUnique], y=y[xyUnique])
	.guiState$nextCallback <- .guiState$nextCallback()
	TRUE
}

gtkSelectPoints <- function(widget, event, data) {
	px <- gtkDrawClickOrDrag(widget, event, shape="none", style="solid")
	.guiState$initiatingSelectPoints <- FALSE
	.guiState$pxSelect <- list(x=px$end$x, y=px$end$y)
	xy <- list(x=grconvertX(.guiState$pxSelect$x, from="device", to="user"),
		y=grconvertY(.guiState$pxSelect$y, from="device", to="user"))
	points(xy$x, xy$y, pch=4, lwd=1, col="white")
	widget$getWindow()$setCursor(gdkCursorNew(GdkCursorType["hand1"]))
	pressAnyKey <- function(widget, event, data) {
		.guiState$isSelectingPoints <- FALSE
	    TRUE
	}
	escapeSignal <- gSignalConnect(widget$parent, "key-press-event", pressAnyKey, data=data)
	repeat {
		if ( !.guiState$isSelectingPoints ) {
			points(0, 0, type="n") # plotting resets the cursor
			break
		}
		if ( !.guiState$isDragging ) {
			pxCurrent <- widget$window$getPointer()
			if ( tail(.guiState$pxSelect$x, 1) != .guiState$pxEnd$x || tail(.guiState$pxSelect$y, 1) != .guiState$pxEnd$y ) {
				.guiState$pxSelect$x <- c(.guiState$pxSelect$x, .guiState$pxEnd$x)
				.guiState$pxSelect$y <- c(.guiState$pxSelect$y, .guiState$pxEnd$y)
				xy <- list(x=grconvertX(.guiState$pxSelect$x, from="device", to="user"),
					y=grconvertY(.guiState$pxSelect$y, from="device", to="user"))
				points(xy$x, xy$y, pch=4, lwd=1, col="white")
				widget$getWindow()$setCursor(gdkCursorNew(GdkCursorType["hand1"]))
			}
		}
		gdkWindowProcessAllUpdates()
		while (gtkEventsPending()) gtkMainIterationDo(blocking=FALSE)
	}
	widget$getWindow()$setCursor(gdkCursorNew(GdkCursorType["crosshair"]))
	gSignalHandlerDisconnect(widget$parent, escapeSignal)
	.guiState$nextCallback <- .guiState$nextCallback()
	TRUE
}

gtkSelectPeakBounds <- function(widget, event, data) {
	px <- gtkDrawClickOrDrag(widget, event, shape="vline", style="on-off-dash", 
		fgcolor="blue")
	.guiState$initiatingSelectPeakBounds <- FALSE
	wasDrag <- px$start$x != px$end$x
	if ( wasDrag ) {
		.guiState$pxSelect <- list(x=c(px$start$x, px$end$x), y=c(px$start$y, px$end$y))
	} else {
		.guiState$pxSelect <- list(x=px$end$x, y=px$end$y)
	}
	xy <- list(x=grconvertX(.guiState$pxSelect$x, from="device", to="user"),
		y=grconvertY(.guiState$pxSelect$y, from="device", to="user"))
	abline(v=xy$x, col="blue", lty=4, lwd=0.5)
	if ( wasDrag ) {
		bounds <- sort(xy$x)
		rect(bounds[[1]], par("usr")[[3]], bounds[[2]], par("usr")[[4]], col=rgb(0, 0, 1, 0.5))
	}
	widget$getWindow()$setCursor(gdkCursorNew(GdkCursorType["hand1"]))
	pressAnyKey <- function(widget, event, data) {
		.guiState$isSelectingPeakBounds <- FALSE
	    TRUE
	}
	gdkgc <- gdkGCNew(widget$window)
	gdkgc$setRgbFgColor(gdkColorParse("blue")$color)
	gdkgc$setRgbBgColor(gdkColorParse("white")$color)
    gdkgc$setLineAttributes(line.width=1, line.style=GdkLineStyle["on-off-dash"],
    	cap.style=GdkCapStyle["butt"], join.style=GdkJoinStyle["miter"])
    gdkgc$setDashes(c(8, 4))
	escapeSignal <- gSignalConnect(widget$parent, "key-press-event", pressAnyKey, data=data)
	repeat {
		if ( !.guiState$isSelectingPeakBounds ) {
			points(0, 0, type="n") # plotting resets the cursor
			break
		}
		if ( !.guiState$isDragging ) {
			pxCurrent <- widget$window$getPointer()
			if ( tail(.guiState$pxSelect$x, 1) != .guiState$pxEnd$x || tail(.guiState$pxSelect$y, 1) != .guiState$pxEnd$y ) {
				doubleSelect <- FALSE
				wasDrag <- .guiState$pxStart$x != .guiState$pxEnd$x
				if ( wasDrag ) {
					if ( length(.guiState$pxSelect$x) %% 2 == 0 ) {
						.guiState$pxSelect$x <- c(.guiState$pxSelect$x, .guiState$pxStart$x, .guiState$pxEnd$x)
						.guiState$pxSelect$y <- c(.guiState$pxSelect$y, .guiState$pxStart$y, .guiState$pxEnd$y)
					} else {
						doubleSelect <- TRUE
						.guiState$pxSelect$x <- c(.guiState$pxSelect$x, .guiState$pxStart$x,
							.guiState$pxStart$x, .guiState$pxEnd$x)
						.guiState$pxSelect$y <- c(.guiState$pxSelect$y, .guiState$pxStart$y,
							.guiState$pxStart$y, .guiState$pxEnd$y)
					}
				} else {
					.guiState$pxSelect$x <- c(.guiState$pxSelect$x, .guiState$pxEnd$x)
					.guiState$pxSelect$y <- c(.guiState$pxSelect$y, .guiState$pxEnd$y)	
				}
				xy <- list(x=grconvertX(.guiState$pxSelect$x, from="device", to="user"),
					y=grconvertY(.guiState$pxSelect$y, from="device", to="user"))
				abline(v=xy$x, col="blue", lty=4, lwd=0.5)
				if ( length(xy$x) %% 2 == 0 ) {
					if ( doubleSelect ) {
						bounds <- range(tail(xy$x, 4))
					} else {
						bounds <- sort(tail(xy$x, 2))
					}
					rect(bounds[[1]], par("usr")[[3]], bounds[[2]], par("usr")[[4]],
						col=rgb(0, 0, 1, 0.5))
				}
				widget$getWindow()$setCursor(gdkCursorNew(GdkCursorType["hand1"]))
			}
			if ( pxCurrent$x != .guiState$pxEnd$x || pxCurrent$y != .guiState$pxEnd$y ) {
				widget$window$invalidateRect(invalidate.children=FALSE)
				gdkDrawLine(widget$window, gc=gdkgc,
					x1=pxCurrent$x, y1=1,
					x2=pxCurrent$x, y2=dev.size("px")[[2]])
			}
		}
		gdkWindowProcessAllUpdates()
		while (gtkEventsPending()) gtkMainIterationDo(blocking=FALSE)
	}
	gSignalHandlerDisconnect(widget$parent, escapeSignal)
	widget$window$invalidateRect(invalidate.children=FALSE)
	x <- .guiState$pxSelect$x
	y <- .guiState$pxSelect$y
	xyUnique <- !(duplicated(x) & duplicated(y))
	.guiState$pxSelect <- list(x=x[xyUnique], y=y[xyUnique])
	.guiState$nextCallback <- .guiState$nextCallback()
	TRUE
}

