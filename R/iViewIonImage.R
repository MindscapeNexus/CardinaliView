
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
			dirty <<- FALSE
			handlers$ANY <<- addHandlerChanged(
				obj=interface,
				handler=.changed.IonImage,
				action=.self)
		},
		refresh = function(force=FALSE) {
			callSuper(force)
			if ( dirty || force )
				.plot.IonImage(.self)
		}))

.update.pixel <- function(elt, pixel, x, y) {
	if ( length(pixel) != 1 ) return()
	object <- try(get(elt$plist$dataset, envir=globalenv()), silent=TRUE)
	ms.intensity.min <- min(spectra(object)[,pixel])
	ms.intensity.max <- max(spectra(object)[,pixel])
	elt <- elt$findParent("iViewGroup")
	if ( elt$plist$pixel.linked ) {
		elt$findParent("iViewTab")$update(
			x=x, y=y, pixel=pixel,
			ms.intensity.min=ms.intensity.min,
			ms.intensity.max=ms.intensity.max,
			with.properties=c(pixel.linked=TRUE))
	} else {
		elt$update(x=x, y=y, pixel=pixel,
			ms.intensity.min=ms.intensity.min,
			ms.intensity.max=ms.intensity.max)
	}
}

.changed.IonImage <- function(h, ...) {
	object <- try(get(h$action$plist$dataset, envir=globalenv()), silent=TRUE)
	if ( !is(object, "MSImageSet") ) return()
	if ( abs(diff(h$x)) < 1 || abs(diff(h$y)) < 1 ) {
		x <- round(h$x[[1]])
		y <- round(h$y[[1]])
		pixel <- pixels(object, x=x, y=y)
		.update.pixel(h$action, pixel=pixel, x=x, y=y)
	} else {
		x.min <- min(round(h$x))
		x.max <- max(round(h$x))
		y.min <- min(round(h$y))
		y.max <- max(round(h$y))
		elt <- h$action$findParent("iViewGroup")
		elt$update(x.min=x.min, x.max=x.max,
			y.min=y.min, y.max=y.max)
	}
}

