
#### plotting methods ####
## -----------------------

.plot.Error <- function(e) {
	plot(0, 0, type="n")
	text(0, 0, labels="Plot not available.")
}

.plot.MassSpectrum <- function(elt) {
	visible(elt$interface) <- TRUE
	tryCatch({
		with(elt$plist, {
			plot(get(dataset, envir=globalenv()),
				pixel=pixel,
				xlim=c(mz.min, mz.max),
				ylim=c(ms.intensity.min, ms.intensity.max))
			abline(v=mz, lty=2, lwd=0.75, col="blue")
		})
	}, error=.plot.Error)
	elt$dirty <- FALSE
}

.plot.IonImage <- function(elt) {
	visible(elt$interface) <- TRUE
	tryCatch({
		with(elt$plist, {
			image(get(dataset, envir=globalenv()),
				feature=feature,
				xlim=c(x.min, x.max),
				ylim=c(y.min, y.max),
				zlim=c(img.intensity.min, img.intensity.max),
				contrast.enhance=Cardinal:::contrast.enhance.histogram,
				useRaster=TRUE)
			points(x, y, pch=4, lwd=2, col="black")
			points(x, y, pch=4, lwd=1, col="white")
		})
	}, error=.plot.Error)
	elt$dirty <- FALSE
}
