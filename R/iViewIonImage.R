
#### Class for an ion image plot ####
## ----------------------------------
.iViewIonImage <- setRefClass("iViewIonImage",
	contains = "iViewGraphics",
	methods = list(
		initialize = function(...) {
			uuid <<- Cardinal:::uuid()
			interface <<- ggraphics(...)
			plist$dataset <<- character(1)
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
		refresh = function() {
			callSuper()
			.plot.IonImage(plist)
		}))

.changed.IonImage <- function(h, ...) {
	object <- try(get(h$action$plist$dataset, envir=globalenv()), silent=TRUE)
	if ( !is(object, "MSImageSet") ) return()
	if ( abs(diff(h$x)) < 1 || abs(diff(h$y)) < 1 ) {
		x <- as.integer(h$x[[1]])
		y <- as.integer(h$y[[1]])
		pixel <- pixels(object, x=x, y=y)
		ms.intensity.min <- min(spectra(object)[,pixel])
		ms.intensity.max <- max(spectra(object)[,pixel])
		elt <- h$action$findParent("iViewGroup")
		if ( elt$plist$pixel.linked ) {
			tmp <- elt$findParent("iViewTab")
			tmp$update(x=x, y=y, pixel=pixel,
				with.properties=c(pixel.linked=TRUE),
				blocking=TRUE)
		} else {
			elt$update(x=x, y=y, pixel=pixel,
				blocking=TRUE)
		}
		if ( elt$plist$ms.intensity.zoom.linked ) {
			tmp <- elt$findParent("iViewTab")
			tmp$update(ms.intensity.min=ms.intensity.min,
				ms.intensity.max=ms.intensity.max,
				with.properties=c(ms.intensity.zoom.linked=TRUE))
		} else {
			elt$update(ms.intensity.min=ms.intensity.min,
				ms.intensity.max=ms.intensity.max)
		}
	} else {
		x.min <- min(as.integer(h$x))
		x.max <- max(as.integer(h$x))
		y.min <- min(as.integer(h$y))
		y.max <- max(as.integer(h$y))
		elt <- h$action$findParent("iViewGroup")
		if ( elt$plist$img.zoom.linked ) {
			elt <- elt$findParent("iViewTab")
			elt$update(x.min=x.min, x.max=x.max,
				y.min=y.min, y.max=y.max,
				with.properties=c(img.zoom.linked=TRUE))
		} else {
			elt$update(x.min=x.min, x.max=x.max,
				y.min=y.min, y.max=y.max)
		}
	}
}
