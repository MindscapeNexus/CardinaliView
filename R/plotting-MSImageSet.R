
#### plotting methods ####
## -----------------------

.plot.Error <- function(e) {
	plot(0, 0, type="n")
	text(0, 0, labels="Plot not available.")
}

.plot.MassSpectrum <- function(plist) {
	tryCatch({
		plot(get(plist$dataset, envir=globalenv()),
			pixel=plist$pixel,
			xlim=c(plist$mz.min, plist$mz.max),
			ylim=c(plist$ms.intensity.min, plist$ms.intensity.max))
		abline(v=plist$mz, lty=2, lwd=0.75, col="blue")
	}, error=.plot.Error)
}

.plot.IonImage <- function(plist) {
	tryCatch({
		image(get(plist$dataset, envir=globalenv()),
			feature=plist$feature,
			xlim=c(plist$x.min, plist$x.max),
			ylim=c(plist$y.min, plist$y.max),
			zlim=c(plist$img.intensity.min, plist$img.intensity.max),
			useRaster=TRUE)
		points(plist$x, plist$y, pch=4, lwd=2, col="black")
		points(plist$x, plist$y, pch=4, lwd=1, col="white")
	}, error=.plot.Error)
}
