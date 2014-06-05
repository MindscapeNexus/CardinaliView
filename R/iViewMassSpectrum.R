
#### Class for a mass spectrum plot ####
## -------------------------------------
.iViewMassSpectrum <- setRefClass("iViewMassSpectrum",
	contains = "iViewGraphics",
	methods = list(
		initialize = function(...) {
			interface <<- ggraphics(...)
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
		refresh = function(...) {
			callSuper(...)
			.plot.MassSpectrum(plist)
		}))

.changed.MassSpectrum <- function(h, ...) {
	# NEED TO FIX TO CHANGE FEATURE AT THE SAME TIME!
	if ( abs(diff(h$x)) < 1e-6 || abs(diff(h$y)) < 1e-6 ) {
		mz <- round(h$x[[1]], digits=4)
		elt <- h$action$findParent("iViewGroup")
		if ( elt$plist$feature.linked ) {
			elt <- elt$findParent("iViewTab")
			elt$update(mz=mz,
				with.properties=c(feature.linked=TRUE))
		} else {
			elt$update(mz=mz)
		}
	} else {
		mz.min <- min(round(h$x, digits=4))
		mz.max <- max(round(h$x, digits=4))
		ms.intensity.min <- min(round(h$y, digits=4))
		ms.intensity.max <- max(round(h$y, digits=4))
		elt <- h$action$findParent("iViewGroup")
		if ( elt$plist$ms.zoom.linked ) {
			elt2 <- elt$findParent("iViewTab")
			elt2$update(mz.min=mz.min, mz.max=mz.max,
				with.properties=c(ms.zoom.linked=TRUE))
		} else {
			elt$update(mz.min=mz.min, mz.max=mz.max)
		}
		if ( elt$plist$ms.intensity.zoom.linked ) {
			elt2 <- elt$findParent("iViewTab")
			elt2$update(ms.intensity.min=ms.intensity.min,
				ms.intensity.max=ms.intensity.max,
				with.properties=c(ms.intensity.zoom.linked=TRUE))
		} else {
			elt$update(ms.intensity.min=ms.intensity.min,
				ms.intensity.max=ms.intensity.max)
		}
	}
}
