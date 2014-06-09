
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
			handlers$ANY <<- addHandlerChanged(
				obj=interface,
				handler=.changed.MassSpectrum,
				action=.self)
		},
		refresh = function() {
			callSuper()
			.plot.MassSpectrum(plist)
		}))

.changed.MassSpectrum <- function(h, ...) {
	object <- try(get(h$action$plist$dataset, envir=globalenv()), silent=TRUE)
	if ( !is(object, "MSImageSet") ) return()
	if ( abs(diff(h$x)) < 1e-6 || abs(diff(h$y)) < 1e-6 ) {
		mz <- round(h$x[[1]], digits=4)
		feature <- features(object, mz=mz)
		img.intensity.min <- min(spectra(object)[feature,])
		img.intensity.max <- max(spectra(object)[feature,])
		elt <- h$action$findParent("iViewGroup")
		if ( elt$plist$feature.linked ) {
			tmp <- elt$findParent("iViewTab")
			tmp$update(mz=mz, feature=feature,
				with.properties=c(feature.linked=TRUE),
				blocking=TRUE)
		} else {
			elt$update(mz=mz, feature=feature,
				blocking=TRUE)
		}
		if ( elt$plist$img.intensity.zoom.linked ) {
			elttmp <- elt$findParent("iViewTab")
			elttmp$update(img.intensity.min=img.intensity.min,
				img.intensity.max=img.intensity.max,
				with.properties=c(img.intensity.zoom.linked=TRUE))
		} else {
			elt$update(img.intensity.min=img.intensity.min,
				img.intensity.max=img.intensity.max)
		}
	} else {
		mz.min <- min(round(h$x, digits=4))
		mz.max <- max(round(h$x, digits=4))
		ms.intensity.min <- min(round(h$y, digits=4))
		ms.intensity.max <- max(round(h$y, digits=4))
		elt <- h$action$findParent("iViewGroup")
		if ( elt$plist$ms.zoom.linked ) {
			tmp <- elt$findParent("iViewTab")
			tmp$update(mz.min=mz.min, mz.max=mz.max,
				with.properties=c(ms.zoom.linked=TRUE),
				blocking=TRUE)
		} else {
			elt$update(mz.min=mz.min, mz.max=mz.max,
				blocking=TRUE)
		}
		if ( elt$plist$ms.intensity.zoom.linked ) {
			elttmp <- elt$findParent("iViewTab")
			elttmp$update(ms.intensity.min=ms.intensity.min,
				ms.intensity.max=ms.intensity.max,
				with.properties=c(ms.intensity.zoom.linked=TRUE))
		} else {
			elt$update(ms.intensity.min=ms.intensity.min,
				ms.intensity.max=ms.intensity.max)
		}
	}
}
