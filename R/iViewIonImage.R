
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
	# need to fix to change pixel at the same time!
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
}
