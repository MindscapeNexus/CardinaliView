
#### Class for an ion image plot ####
## ----------------------------------
.iViewIonImage <- setRefClass("iViewIonImage",
	contains = "iViewGraphics",
	methods = list(
		initialize = function(...) {
			callSuper(...)
			plist$x <<- numeric(1)
			plist$y <<- numeric(1)
			handlers$changed <<- addHandlerChanged(interface,
				handler=.changedIonImage,
				action=.self)
		},
		refresh = function(...) {
			callSuper(...)
			.plotIonImage(plist)
		},
		update = function(...) {
			dots <- list(...)
			if ( "x" %in% names(dots) )
				plist$x <<- dots$x
			if ( "y" %in% names(dots) )
				plist$y <<- dots$y
			callSuper(...)
		}))

.changedIonImage <- function(h, ...) {
	self <- h$action
	self$update(x=h$x[[1]], y=h$y[[1]])
}
