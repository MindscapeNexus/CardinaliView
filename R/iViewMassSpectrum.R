
#### Class for a mass spectrum plot ####
## -------------------------------------
.iViewMassSpectrum <- setRefClass("iViewMassSpectrum",
	contains = "iViewGraphics",
	methods = list(
		initialize = function(...) {
			callSuper(...)
			plist$mz <<- numeric(1)
			handlers$changed <<- addHandlerChanged(interface,
				handler=.changedMassSpectrum,
				action=.self)
		},
		refresh = function(...) {
			callSuper(...)
			.plotMassSpectrum(plist)
		},
		update = function(...) {
			dots <- list(...)
			if ( "mz" %in% names(dots) )
				plist$mz <<- dots$mz
			callSuper(...)
		}))

.changedMassSpectrum <- function(h, ...) {
	self <- h$action
	self$update(mz=h$x[[1]])
}
