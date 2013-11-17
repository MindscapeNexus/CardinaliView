
#### functions for plots ####

updateAll <- function(instance) {
	prev <- dev.cur()
	if ( !isExtant(instance$windows$main$topWidgets$plotControlWindow) ) {
		makePlotControls(instance)
	}
	if ( !isExtant(instance$windows$main$topWidgets$imageControlWindow) ) {
		makeImageControls(instance)
	}
	updatePlots(instance)
	updateImages(instance)
	if ( prev != 1 ) dev.set(prev)
}

updatePlots <- function(instance) {
	prev <- dev.cur()
	object <- try(get(instance$objectname, envir=instance$objectenv), silent=TRUE)
	if ( inherits(object, "try-error") ) {
		setDevice(instance, "main", "plot")
		plot(0, 0, type="n")
		text(0, 0, labels="No dataset loaded")
		return()
	}
	if ( instance$windows$main$open ) {
		setDevice(instance, "main", "plot")
		plot(object, pixel=instance$parameters$pixel, xlim=instance$parameters$mzRange,
			ylim=instance$parameters$plot.intensityRange,
			filter=instance$windows$main$filter,
			window=instance$windows$main$filter.window)
		if ( instance$interactive) abline(v=instance$parameters$mz, lty=2, lwd=0.2)
	}
	updateProcessingPlots(instance)
	updateAnalysisPlots(instance)
	if ( prev != 1 ) dev.set(prev)
}

updateImages <- function(instance) {
	prev <- dev.cur()
	object <- try(get(instance$objectname, envir=instance$objectenv), silent=TRUE)
	if ( inherits(object, "try-error") ) {
		setDevice(instance, "main", "image")
		plot(0, 0, type="n")
		text(0, 0, labels="No dataset loaded")
		return()
	}
	if ( instance$windows$main$open ) {
		setDevice(instance, "main", "image")
		image(object, feature=instance$parameters$feature,
			sliceDimNames=instance$parameters$sliceDimNames,
			fixCoord=instance$parameters$fixCoord,
			xlim=instance$parameters$coordRange[c(1,2)],
			ylim=instance$parameters$coordRange[c(3,4)],
			zlim=instance$parameters$image.intensityRange,
			contrast=instance$windows$main$contrast,
			smoothing=instance$windows$main$smoothing,
			window=instance$windows$main$smoothing.window,
			interpolate=instance$windows$main$interpolate,
			xres=instance$windows$main$interpolate.xres,
			auto.scale="max",
			useRaster=instance$parameters$useRaster)
		if ( instance$interactive) points(instance$parameters$coord[[instance$parameters$sliceDimNames[1]]],
			instance$parameters$coord[[instance$parameters$sliceDimNames[2]]],
			pch=4, lwd=2, col="black")
		if ( instance$interactive) points(instance$parameters$coord[[instance$parameters$sliceDimNames[1]]],
			instance$parameters$coord[[instance$parameters$sliceDimNames[2]]],
			pch=4, lwd=1, col="white")
	}
	updateAnalysisImages(instance)
	if ( prev != 1 ) dev.set(prev)
}

updateProcessingPlots <- function(instance) {
	prev <- dev.cur()
	object <- try(get(instance$objectname, envir=instance$objectenv), silent=TRUE)
	if ( inherits(object, "try-error") ) return()
	if ( instance$windows$removeNoise$open ) {
		setDevice(instance, "removeNoise", "plot")
		removeNoise(object, pixel=instance$parameters$pixel,
			method=instance$windows$removeNoise$method,
			window=instance$windows$removeNoise$window,
			xlim=instance$parameters$mzRange,
			ylim=instance$parameters$plot.intensityRange,
			plot=TRUE)
		if ( instance$interactive) abline(v=instance$parameters$mz, lty=2, lwd=0.2)
	}
	if ( instance$windows$removeBaseline$open ) {
		setDevice(instance, "removeBaseline", "plot")
		removeBaseline(object, pixel=instance$parameters$pixel,
			method=instance$windows$removeBaseline$method,
			interp1=instance$windows$removeBaseline$dots.method,
			blocks=instance$windows$removeBaseline$dots.blocks,
			choose=instance$windows$removeBaseline$dots.choose,
			preserve.level=instance$windows$removeBaseline$dots.preserve.level,
			xlim=instance$parameters$mzRange,
			ylim=instance$parameters$plot.intensityRange,
			plot=TRUE)
		if ( instance$interactive) abline(v=instance$parameters$mz, lty=2, lwd=0.2)
	}
	if ( instance$windows$detectPeaks$open ) {
		setDevice(instance, "detectPeaks", "plot")
		if ( instance$windows$detectPeaks$spectrum == "pixel" ) {
			instance$windows$detectPeaks$object@spectra$spectra[,1] <- intensities(object, pixel=instance$parameters$pixel)
		}
		pixel <- switch(instance$windows$detectPeaks$spectrum,
			"pixel"=1,
			"mean"=2,
			"max"=3)
		detectPeaks(instance$windows$detectPeaks$object, pixel=pixel,
			method=instance$windows$detectPeaks$method,
			noise=instance$windows$detectPeaks$dots.method,
			blocks=instance$windows$detectPeaks$dots.blocks,
			span=instance$windows$detectPeaks$dots.span,
			snr=instance$windows$detectPeaks$dots.snr,
			xlim=instance$parameters$mzRange,
			ylim=instance$parameters$plot.intensityRange,
			plot=TRUE,
			sub=paste("spectrum type:", instance$windows$detectPeaks$spectrum))
		noise <- estimateNoise(instance$windows$detectPeaks$object, pixel=pixel,
			method=instance$windows$detectPeaks$dots.method,
			blocks=instance$windows$detectPeaks$dots.blocks)
		lines(mz(object), noise, col="blue")
		if ( instance$interactive) abline(v=instance$parameters$mz, lty=2, lwd=0.2)
	}
	if ( instance$windows$selectPeaks$open ) {
		setDevice(instance, "selectPeaks", "plot")
		if ( instance$windows$selectPeaks$spectrum == "pixel" ) {
			instance$windows$selectPeaks$object@spectra$spectra[,1] <- intensities(object, pixel=instance$parameters$pixel)
		}
		pixel <- switch(instance$windows$selectPeaks$spectrum,
			"pixel"=1,
			"mean"=2,
			"max"=3)
		plot(instance$windows$selectPeaks$object, pixel=pixel,
			xlim=instance$parameters$mzRange,
			ylim=range(instance$windows$selectPeaks$object@spectra$spectra[,pixel], na.rm=TRUE),
			xlab="m/z", ylab="intensity", col.sub="gray",
			sub=paste("spectrum type:", instance$windows$selectPeaks$spectrum))
		if ( nrow(instance$windows$selectPeaks$peaks@peaks) > 0 ) {
			peaks <- resampleSpectra(instance$windows$selectPeaks$object,
				peaks=instance$windows$selectPeaks$peaks, pixel=pixel)
			plot(peaks, pixel=1, col="red", add=TRUE)
		}
		if ( instance$interactive) abline(v=instance$parameters$mz, lty=2, lwd=0.2)
	}
	if ( instance$windows$bin$open ) {
		setDevice(instance, "bin", "plot")
		if ( Cardinal:::isMSPeakFrame(instance$windows$bin$peaks) ) {
			binSpectra(object, peaks=instance$windows$bin$peaks,
				pixel=instance$parameters$pixel,
				xlim=instance$parameters$mzRange,
				ylim=instance$parameters$plot.intensityRange,
				plot=TRUE)
		} else {
			binSpectra(object, pixel=instance$parameters$pixel,
				width=instance$windows$bin$width,
				offset=instance$windows$bin$offset,
				xlim=instance$parameters$mzRange,
				ylim=instance$parameters$plot.intensityRange,
				plot=TRUE)
		}
		if ( instance$interactive) abline(v=instance$parameters$mz, lty=2, lwd=0.2)
	}
	if ( instance$windows$resample$open ) {
		setDevice(instance, "resample", "plot")
		if ( Cardinal:::isMSPeakFrame(instance$windows$resample$peaks) ) {
			resampleSpectra(object, peaks=instance$windows$resample$peaks,
				pixel=instance$parameters$pixel,
				xlim=instance$parameters$mzRange,
				ylim=instance$parameters$plot.intensityRange,
				plot=TRUE)
		} else {
			resampleSpectra(object, pixel=instance$parameters$pixel,
				step=instance$windows$resample$step,
				offset=instance$windows$resample$offset,
				xlim=instance$parameters$mzRange,
				ylim=instance$parameters$plot.intensityRange,
				plot=TRUE)
		}
		if ( instance$interactive) abline(v=instance$parameters$mz, lty=2, lwd=0.2)
	}
	if ( prev != 1 ) dev.set(prev)
}

updateAnalysisPlots <- function(instance) {
	prev <- dev.cur()
	object <- try(get(instance$objectname, envir=instance$objectenv), silent=TRUE)
	if ( inherits(object, "try-error") ) return()
	if ( instance$windows$PCA$open ) {
		setDevice(instance, "PCA", "plot")
		ncomp <- instance$windows$PCA$ncomp
		plot(mz(object), instance$windows$PCA$object$loadings[,ncomp],
			xlim=instance$parameters$mzRange,
			main=object@metaData[["name"]],
			sub=paste("PC", ncomp, sep=""), type="h",
			xlab="m/z", ylab="loadings")
		if ( instance$interactive) abline(v=instance$parameters$mz, lty=2, lwd=0.2)
	}
	if ( instance$windows$MSImageSegmentation$open ) {
		setDevice(instance, "MSImageSegmentation", "plot")
		plot(instance$windows$MSImageSegmentation$object,
			which=instance$windows$MSImageSegmentation$which,
			climits=instance$windows$MSImageSegmentation$plot.options$climits,
			mode=instance$windows$MSImageSegmentation$plot.mode,
			nlabels=if(isPeaks(object)) 6 else 0,
			xlim=instance$parameters$mzRange)
		if ( instance$windows$MSImageSegmentation$plot.options$overlay ) {
			plot(object, pixel=instance$parameters$pixel,
				filter=instance$windows$main$filter,
				window=instance$windows$main$filter.window,
				add=TRUE, nlabels=0,
				col=rgb(red=0, green=0, blue=0,
					alpha=instance$windows$MSImageSegmentation$transparency))
		}
		if ( instance$interactive) abline(v=instance$parameters$mz, lty=2, lwd=0.2)
	}
	if ( prev != 1 ) dev.set(prev)
}

updateAnalysisImages <- function(instance) {
	prev <- dev.cur()
	object <- try(get(instance$objectname, envir=instance$objectenv), silent=TRUE)
	if ( inherits(object, "try-error") ) return()
	if ( instance$windows$PCA$open ) {
		setDevice(instance, "PCA", "image")
		ncomp <- instance$windows$PCA$ncomp
		image(object, values=instance$windows$PCA$object$scores[,ncomp],
			sliceDimNames=instance$parameters$sliceDimNames,
			fixCoord=instance$parameters$fixCoord,
			xlim=instance$parameters$coordRange[c(1,2)],
			ylim=instance$parameters$coordRange[c(3,4)],
			main=object@metaData[["name"]],
			sub=paste("PC", ncomp, sep=""),
			xlab="m/z", ylab="loadings",
			col=risk.colors(100),
			useRaster=instance$parameters$useRaster)
		if ( instance$interactive) points(instance$parameters$coord[[instance$parameters$sliceDimNames[1]]],
			instance$parameters$coord[[instance$parameters$sliceDimNames[2]]],
			pch=4, lwd=2, col="black")
		if ( instance$interactive) points(instance$parameters$coord[[instance$parameters$sliceDimNames[1]]],
			instance$parameters$coord[[instance$parameters$sliceDimNames[2]]],
			pch=4, lwd=1, col="white")
	}
	if ( instance$windows$MSImageSegmentation$open ) {
		setDevice(instance, "MSImageSegmentation", "image")
		image(instance$windows$MSImageSegmentation$object,
			which=instance$windows$MSImageSegmentation$which,
			sliceDimNames=instance$parameters$sliceDimNames,
			fixCoord=instance$parameters$fixCoord,
			mode=instance$windows$MSImageSegmentation$image.mode,
			true.labels=instance$windows$MSImageSegmentation$image.options$true.labels,
			mask.missing=instance$windows$MSImageSegmentation$image.options$mask.missing,
			xlim=instance$parameters$coordRange[c(1,2)],
			ylim=instance$parameters$coordRange[c(3,4)],
			useRaster=instance$parameters$useRaster)
		if ( instance$windows$MSImageSegmentation$image.options$overlay ) {
			image(object, feature=instance$parameters$feature,
				sliceDimNames=instance$parameters$sliceDimNames,
				fixCoord=instance$parameters$fixCoord,
				contrast=instance$windows$main$contrast,
				smoothing=instance$windows$main$smoothing,
				window=instance$windows$main$smoothing.window,
				interpolate=instance$windows$main$interpolate,
				xres=instance$windows$main$interpolate.xres,
				useRaster=instance$parameters$useRaster,
				auto.scale="max",
				colorkey=FALSE,
				add=TRUE,
				col=intensity.colors(100,
					alpha=instance$windows$MSImageSegmentation$transparency))
		}
		if ( instance$interactive) points(instance$parameters$coord[[instance$parameters$sliceDimNames[1]]],
			instance$parameters$coord[[instance$parameters$sliceDimNames[2]]],
			pch=4, lwd=2, col="black")
		if ( instance$interactive) points(instance$parameters$coord[[instance$parameters$sliceDimNames[1]]],
			instance$parameters$coord[[instance$parameters$sliceDimNames[2]]],
			pch=4, lwd=1, col="white")
	}
	if ( prev != 1 ) dev.set(prev)
}


