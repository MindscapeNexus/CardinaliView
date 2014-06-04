
#### plotting methods ####
## -----------------------

.plot.Error <- function(e) {
	plot(0, 0, type="n")
	text(0, 0, labels="Plot not available.")
}

.plot.MassSpectrum <- function(plist) {
	attach(plist)
	tryCatch({
		plot(1:100, rep(50, 100),
			xlim=c(mz.min, mz.max),
			ylim=c(ms.intensity.min, ms.intensity.max))
		abline(v=mz, lty=2, lwd=0.6, col="blue")
	}, error=.plot.Error)
	detach("plist")
}

.plot.IonImage <- function(plist) {
	attach(plist)
	tryCatch({
		image(1:100, 1:100, matrix(seq(from=1, to=100, length.out=100^2), nrow=100),
			xlim=c(x.min, x.max),
			ylim=c(x.min, x.max),
			zlim=c(img.intensity.min, img.intensity.max),
			useRaster=TRUE)
		points(x, y, pch=4, lwd=2, col="black")
		points(x, y, pch=4, lwd=1, col="white")
	}, error=.plot.Error)
	detach("plist")
}
