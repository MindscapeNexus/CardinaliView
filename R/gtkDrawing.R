
#### functions for Gtk+ drawing on the plotting area ####

gtkDrawClickOrDrag <- function(widget, event, shape=c("none", "box", "line", "vline", "hline"),
	fgcolor="black", bgcolor="white", style=c("solid", "on-off-dash", "double-dash"))
{
	.guiState$pxStart <- list(x=event$x, y=event$y)
	.guiState$pxDrag <- .guiState$pxStart
	.guiState$pxEnd <- .guiState$pxStart
	.guiState$isDragging <- TRUE
	releaseButton <- function(widget, event, data) {
		.guiState$isDragging <- FALSE
	    .guiState$pxEnd <- list(x=event$x, y=event$y)
	    TRUE
	}
	gdkgc <- gdkGCNew(widget$window)
	gdkgc$setRgbFgColor(gdkColorParse(fgcolor)$color)
	gdkgc$setRgbBgColor(gdkColorParse(bgcolor)$color)
    gdkgc$setLineAttributes(line.width=1, line.style=GdkLineStyle[style],
    	cap.style=GdkCapStyle["butt"], join.style=GdkJoinStyle["miter"])
    gdkgc$setDashes(c(8, 4))
	releaseSignal <- gSignalConnect(widget, "button-release-event", releaseButton, data=data)
	repeat {
		if ( !.guiState$isDragging ) break
		pxCurrent <- widget$window$getPointer()
		if ( pxCurrent$x != .guiState$pxEnd$x || pxCurrent$y != .guiState$pxEnd$y) {
			.guiState$pxDrag <- list(x=c(.guiState$pxDrag$x, pxCurrent$x),
				y=c(.guiState$pxDrag$y, pxCurrent$y))
			widget$window$invalidateRect(invalidate.children=FALSE)
			.guiState$pxEnd <- pxCurrent
		}
		pxDraw <- list(x=c(.guiState$pxStart$x, pxCurrent$x),
			y=c(.guiState$pxStart$y, pxCurrent$y))
		if ( shape == "box" ) {
			gdkDrawRectangle(widget$window, gc=gdkgc,
				filled=FALSE, x=min(pxDraw$x), min(pxDraw$y),
				width=abs(diff(pxDraw$x)), height=abs(diff(pxDraw$y)))
		} else if ( shape == "line" ) {
			gdkDrawLine(widget$window, gc=gdkgc,
				x1=pxDraw$x[[1]], y1=pxDraw$y[[1]],
				x2=pxDraw$x[[2]], y2=pxDraw$y[[2]])
		} else if ( shape == "vline" ) {
			gdkDrawLine(widget$window, gc=gdkgc,
				x1=pxDraw$x[[1]], y1=1,
				x2=pxDraw$x[[1]], y2=dev.size("px")[[2]])
			gdkDrawLine(widget$window, gc=gdkgc,
				x1=pxDraw$x[[2]], y1=1,
				x2=pxDraw$x[[2]], y2=dev.size("px")[[2]])
		} else if ( shape == "hline" ) {
			gdkDrawLine(widget$window, gc=gdkgc,
				x1=1, y1=pxDraw$y[[1]],
				x2=dev.size("px")[[1]], y1=pxDraw$y[[1]])
			gdkDrawLine(widget$window, gc=gdkgc,
				x1=1, y1=pxDraw$y[[2]],
				x2=dev.size("px")[[1]], y1=pxDraw$y[[2]])
		}
		gdkWindowProcessAllUpdates()
		while (gtkEventsPending()) gtkMainIterationDo(blocking=FALSE)
	}
	gSignalHandlerDisconnect(widget, releaseSignal)
	widget$window$invalidateRect(invalidate.children=FALSE)
	list(start=.guiState$pxStart, drag=.guiState$pxDrag, end=.guiState$pxEnd)
}





