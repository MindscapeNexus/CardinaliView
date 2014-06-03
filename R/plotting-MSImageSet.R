
#### plotting methods ####
## -----------------------

.plotError <- function(e) {
	plot(0, 0, type="n")
	text(0, 0, labels="Plot not available.")
}

.plotMassSpectrum <- function(plist) {
	attach(plist)
	tryCatch({
		plot(1:100, rep(50, 100), xlim=c(1,100), ylim=c(1,100))
		abline(v=mz, lty=2, lwd=0.6, col="blue")
	}, error=.plotError)
	detach("plist")
}

.plotIonImage <- function(plist) {
	attach(plist)
	tryCatch({
		image(1:2, 1:2, matrix(1:4, nrow=2), xlim=c(0.5, 2.5), ylim=c(0.5, 2.5))
		points(x, y, pch=4, lwd=2, col="black")
		points(x, y, pch=4, lwd=1, col="white")
	}, error=.plotError)
	detach("plist")
}
