
#### Class for an ion image plot ####
## ----------------------------------
.iViewIonImage <- setRefClass("iViewIonImage",
	contains = "iViewGraphics",
	methods = list(
		initialize = function(...) {
			callSuper(...)
			plist$feature <<- numeric(1)
			plist$x <<- numeric(1)
			plist$y <<- numeric(1)
			plist$x.min <<- numeric(1)
			plist$x.max <<- numeric(1)
			plist$y.min <<- numeric(1)
			plist$y.max <<- numeric(1)
			plist$img.intensity.min <<- numeric(1)
			plist$img.intensity.max <<- numeric(1)
			handlers$ANY <<- addHandlerChanged(
				obj=interface,
				handler=.changed.IonImage,
				action=.self)
		},
		refresh = function(...) {
			callSuper(...)
			.plot.IonImage(plist)
		}))

.changed.IonImage <- function(h, ...) {
	# NEED TO FIX TO CHANGE PIXEL AT THE SAME TIME!
	if ( abs(diff(h$x)) < 1 || abs(diff(h$y)) < 1 ) {
		x <- h$x[[1]]
		y <- h$y[[1]]
		elt <- h$action$findParent("CardinaliView")
		if ( elt$plist$pixel.linked ) {
			elt <- elt$findParent("CardinaliViewGroup")
			elt$update(x=x, y=y,
				with.properties=c(pixel.linked=TRUE))
		} else {
			elt$update(x=x, y=y)
		}
	} else {
		x.min <- min(as.integer(h$x))
		x.max <- max(as.integer(h$x))
		y.min <- min(as.integer(h$y))
		y.max <- max(as.integer(h$y))
		elt <- h$action$findParent("CardinaliView")
		if ( elt$plist$img.zoom.linked ) {
			elt <- elt$findParent("CardinaliViewGroup")
			elt$update(x.min=x.min, x.max=x.max,
				y.min=y.min, y.max=y.max,
				with.properties=c(img.zoom.linked=TRUE))
		} else {
			elt$update(x.min=x.min, x.max=x.max,
				y.min=y.min, y.max=y.max)
		}
	}
}
