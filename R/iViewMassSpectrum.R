
#### Class for a mass spectrum plot ####
## -------------------------------------
.iViewMassSpectrum <- setRefClass("iViewMassSpectrum",
	contains = "iViewGraphics",
	methods = list(
		initialize = function(...) {
			callSuper(...)
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
	# need to fix to change feature at the same time!
	mz <- round(h$x[[1]], digits=4)
	elt <- h$action$findParent("CardinaliView")
	if ( elt$plist$feature.linked ) {
		elt <- elt$findParent("CardinaliViewGroup")
		elt$update(mz=mz,
			with.properties=c(feature.linked=TRUE))
	} else {
		elt$update(mz=mz)
	}
}
