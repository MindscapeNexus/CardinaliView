
#### Class for a mass spectrum plot ####
## -------------------------------------
.iViewMassSpectrum <- setRefClass("iViewMassSpectrum",
	contains = "iViewGraphics",
	methods = list(
		initialize = function(...) {
			uuid <<- Cardinal:::uuid()
			interface <<- ggraphics(...)
			plist$dataset <<- character(1)
			plist$pixel <<- numeric(1)
			plist$mz <<- numeric(1)
			plist$mz.min <<- numeric(1)
			plist$mz.max <<- numeric(1)
			plist$ms.intensity.min <<- numeric(1)
			plist$ms.intensity.max <<- numeric(1)
			dirty <<- FALSE
			handlers$ANY <<- addHandlerChanged(
				obj=interface,
				handler=.changed.MassSpectrum,
				action=.self)
		},
		refresh = function(force=FALSE) {
			callSuper(force)
			if ( dirty || force )
				.plot.MassSpectrum(.self)
		}))

.update.feature <- function(elt, feature, mz) {
	if ( length(feature) != 1 ) return()
	object <- try(get(elt$plist$dataset, envir=globalenv()), silent=TRUE)
	img.intensity.min <- min(spectra(object)[feature,])
	img.intensity.max <- max(spectra(object)[feature,])
	elt <- elt$findParent("iViewGroup")
	if ( elt$plist$feature.linked ) {
		elt$findParent("iViewTab")$update(
			mz=mz, feature=feature,
			img.intensity.min=img.intensity.min,
			img.intensity.max=img.intensity.max,
			with.properties=c(feature.linked=TRUE))
	} else {
		elt$update(mz=mz, feature=feature,
			img.intensity.min=img.intensity.min,
			img.intensity.max=img.intensity.max)

	}
}

.changed.MassSpectrum <- function(h, ...) {
	object <- try(get(h$action$plist$dataset, envir=globalenv()), silent=TRUE)
	if ( !is(object, "MSImageSet") ) return()
	if ( abs(diff(h$x)) < 1e-6 || abs(diff(h$y)) < 1e-6 ) {
		mz <- round(h$x[[1]], digits=4)
		feature <- features(object, mz=mz)
		.update.feature(h$action, feature=feature, mz=mz)
	} else {
		mz.min <- min(round(h$x, digits=4))
		mz.max <- max(round(h$x, digits=4))
		ms.intensity.min <- min(round(h$y, digits=4))
		ms.intensity.max <- max(round(h$y, digits=4))
		elt <- h$action$findParent("iViewGroup")
		elt$update(mz.min=mz.min, mz.max=mz.max,
			ms.intensity.min=ms.intensity.min,
			ms.intensity.max=ms.intensity.max)
	}
}


