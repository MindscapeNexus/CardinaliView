
#### functions for instance of the GUI ####

initializeInstance <- function(instance) {
	.guiState$busy <- TRUE
	instance$windows$main$open <- TRUE
	makePlotControls(instance)
	makeImageControls(instance)
	updatePlots(instance)
	updateImages(instance)
	.guiState$busy <- FALSE
	instance$interactive <- TRUE
	instance
}

newInstance <- function(objectname, objectenv=parent.frame()) {
	instancename <- nameInstance(objectname)
	object <- try(get(objectname, envir=objectenv), silent=TRUE)
	if ( inherits(object, "try-error") ) {
		instance <- defaultInstance(emptyenv(), objectname="", instancename="null")
	} else {
		instance <- defaultInstance(objectname, objectenv, instancename=instancename)
		init.sliceDimNames <- object@metaData[["coordDimNames"]][c(1,2)]
		if ( length(object@metaData[["coordDimNames"]]) > 2 ) {
			init.fixCoord <- as.list(coord(object)[1,-c(1,2),drop=FALSE])
		} else {
			init.fixCoord <- list()
		}
		init.intensityRange <- c(min(object@spectra$spectra, na.rm=TRUE),
			max(object@spectra$spectra, na.rm=TRUE))
		instance$parameters$feature <- 1
		instance$parameters$mz <- mz(object)[[1]]
		instance$parameters$mzRange <- range(mz(object))
		instance$parameters$pixel <- 1
		instance$parameters$coord <- coord(object)[1,]
		instance$parameters$coordRange <- c(range(coord(object)[init.sliceDimNames[[1]]]),
			range(coord(object)[init.sliceDimNames[[2]]]))
		instance$parameters$sliceDimNames <- init.sliceDimNames
		instance$parameters$fixCoord <- init.fixCoord
		instance$parameters$minIntensity <- init.intensityRange[[1]]
		instance$parameters$maxIntensity <- init.intensityRange[[2]]
		instance$parameters$plot.intensityRange <- range(intensities(object, pixel=1), na.rm=TRUE)
		instance$parameters$image.intensityRange <- range(intensities(object, feature=1), na.rm=TRUE)
	}
	.guiState$instances[[instancename]] <- instance
	instance
}

nameInstance <- function(name) {
	preexisting <- grep(pattern=name, x=names(.guiState$instances))
	if ( length(preexisting) == 0 ) {
		instancename <- name
	} else {
		instancename <- paste(name, length(preexisting), sep="-")
	}
	instancename
}

defaultInstance <- function(objectname, objectenv, instancename) {
	instance <- new.env(parent=.guiState)
	instance$name <- instancename
	instance$objectname <- objectname
	instance$objectenv <- objectenv
	instance$interactive <- FALSE
	instance$parameters <- list(feature=1,
		mz=0,
		mzRange=c(0,1),
		pixel=1,
		coord=list(x=1, y=1),
		coordRange=c(1, 2, 1, 2),
		sliceDimNames=c("x","y"),
		fixCoord=list(),
		minIntensity=0,
		maxIntensity=1,
		plot.intensityRange=c(0,1),
		image.intensityRange=c(0,1),
		useRaster=TRUE)
	instance$windows <- list(
		main=list(open=FALSE,
			devices=list(plot=NA, image=NA),
			devicetitles=list(plot="Mass spectrum", image="Ion image"),
			widgets=list(),
			topWidgets=list(),
			gtkWidgets=list(),
			plot.options=list("common intensity scale"=FALSE),
			image.options=list("common intensity scale"=FALSE),
			filter="none",
			filter.window=5,
			contrast="none",
			smoothing="none",
			smoothing.window=5,
			interpolate="none",
			interpolate.xres=2),
		peaks=list(open=FALSE,
			widgets=list(),
			topWidgets=list()),
		slices=list(open=FALSE,
			widgets=list(),
			topWidgets=list()),
		roi=list(open=FALSE,
			widgets=list(),
			topWidgets=list()),
		crop=list(open=FALSE,
			widgets=list(),
			topWidgets=list()),
		removeNoise=list(open=FALSE,
			devices=list(plot=NA),
			devicetitles=list(plot="Remove noise"),
			widgets=list(),
			topWidgets=list(),
			gtkWidgets=list(),
			method="gaussian",
			window=25),
		removeBaseline=list(open=FALSE,
			devices=list(plot=NA),
			devicetitles=list(plot="Remove baseline"),
			widgets=list(),
			topWidgets=list(),
			gtkWidgets=list(),
			method="interp",
			dots.method="linear",
			dots.blocks=500,
			dots.preserve.level=1,
			dots.choose="min"),
		detectPeaks=list(open=FALSE,
			devices=list(plot=NA),
			devicetitles=list(plot="Detect peaks"),
			widgets=list(),
			topWidgets=list(),
			gtkWidgets=list(),
			object=NA,
			method="snr",
			dots.method="sd",
			dots.blocks=100,
			dots.span=5,
			dots.snr=6,
			spectrum="pixel"),
		selectPeaks=list(open=FALSE,
			devices=list(plot=NA),
			devicetitles=list(plot="Select peaks"),
			widgets=list(),
			topWidgets=list(),
			gtkWidgets=list(),
			object=NA,
			peaks=Cardinal:::emptyMSPeakFrame(),
			spectrum="pixel"),
		batchPeaks=list(open=FALSE,
			widgets=list(),
			topWidgets=list(),
			search.percent=10,
			minfreq.percent=1,
			align.diff.max=200,
			align.units="ppm"),
		bin=list(open=FALSE,
			devices=list(plot=NA),
			devicetitles=list(plot="Bin"),
			widgets=list(),
			topWidgets=list(),
			gtkWidgets=list(),
			peaks=NA,
			width=1,
			offset=0),
		resample=list(open=FALSE,
			devices=list(plot=NA),
			devicetitles=list(plot="Resample"),
			widgets=list(),
			topWidgets=list(),
			gtkWidgets=list(),
			peaks=NA,
			step=1,
			offset=0),
		PCA=list(open=FALSE,
			devices=list(plot=NA, image=NA),
			devicetitles=list(plot="PCA plot", image="PCA image"),
			widgets=list(),
			topWidgets=list(),
			gtkWidgets=list(),
			object=NA,
			objectname="",
			ncomp=1),
		PLS=list(open=FALSE,
			devices=list(plot=NA, image=NA),
			devicetitles=list(plot="PLS loadings", image="PLS predictions"),
			widgets=list(),
			topWidgets=list(),
			gtkWidgets=list(),
			object=NA,
			ncomp=1),
		OPLS=list(open=FALSE,
			devices=list(plot=NA, image=NA),
			devicetitles=list(plot="PLS loadings", image="PLS predictions"),
			widgets=list(),
			topWidgets=list(),
			gtkWidgets=list(),
			object=NA,
			objectname="",
			ncomp=1,
			acomp=1),
		MSImageSegmentation=list(open=FALSE,
			devices=list(plot=NA, image=NA),
			devicetitles=list(plot="Segmentation plot", image="Segmentation image"),
			widgets=list(),
			topWidgets=list(),
			gtkWidgets=list(),
			object=NA,
			objectname="",
			plot.options=list(overlay=FALSE, climits=FALSE),
			image.options=list(overlay=FALSE, true.labels=FALSE, mask.missing=FALSE),
			plot.mode="centroids",
			image.mode="classes",
			which=1,
			transparency=0.5))
	instance
}

cloneInstance <- function(instance) {
	clone <- list2env(as.list(instance, all.names=TRUE), parent=.guiState, hash=TRUE)
	clone <- scrubInstance(clone)
	clone$name <- nameInstance(clone$objectname)
	.guiState$instances[[clone$name]] <- clone
	clone
}

pushInstance <- function(instance, objectname, objectenv) {
	old <- list2env(as.list(instance, all.names=TRUE), parent=.guiState, hash=TRUE)
	if ( objectname == old$objectname ) {
		old <- popInstance(old)
	} else {
		old <- scrubInstance(old)
		.guiState$instances[[old$name]] <- old
	}
	instance$objectname <- objectname
	instance$objectenv <- objectenv
	instance$name <- nameInstance(instance$objectname)
	.guiState$instances[[instance$name]] <- instance
	instance <- resetInstance(instance)
	instance$interactive <- TRUE
	instance
}

popInstance <- function(instance, andKill=FALSE) {
	.guiState$instances[[instance$name]] <- NULL
	if ( andKill ) {
		instance <- killInstance(instance)
	} else {
		instance <- scrubInstance(instance)
	}
	instance
}

resetInstance <- function(instance) {
	instance <- killInstance(instance, exceptMain=TRUE)
	if ( !isExtant(instance$windows$main$topWidgets$plotControlWindow) ) {
		makePlotControls(instance)
	}
	if ( !isExtant(instance$windows$main$topWidgets$imageControlWindow) ) {
		makeImageControls(instance)
	}
	instance$windows$main$plot.options[["common intensity scale"]] <- FALSE
	svalue(instance$windows$main$widgets$plotCommonIntensity) <- FALSE
	instance$windows$main$image.options[["common intensity scale"]] <- FALSE
	svalue(instance$windows$main$widgets$imageCommonIntensity) <- FALSE
	instance$windows$main$filter <- "none"
	svalue(instance$windows$main$widgets$filter) <- "none"
	instance$windows$main$smoothing <- "none"
	svalue(instance$windows$main$widgets$smoothing) <- "none"
	instance$windows$main$contrast <- "none"
	svalue(instance$windows$main$widgets$contrast) <- "none"
	for ( i in seq_along(instance$windows$main$topWidgets) ) {
		if ( names(instance$windows$main$topWidgets[i]) %in% c("plotControlWindow", "imageControlWindow") ) {
			title <- svalue(instance$windows$main$topWidgets[[i]])
			title <- strsplit(title, split=" - ")[[1]]
			title <- paste(instance$name, title[[2]], sep=" - ")
			svalue(instance$windows$main$topWidgets[[i]]) <- title
		} else if ( isExtant(instance$windows$main$topWidgets[[i]]) ) {
			dispose(instance$windows$main$topWidgets[[i]])
		}
	}
	for ( i in instance$windows$main$devices ) {
		if ( getOption("guiToolkit") == "RGtk2" ) next
		if ( i %in% dev.list() ) {
			dev.off(i)
		}
	}
	for ( i in seq_along(instance$windows$main$gtkWidgets) ) {
		if ( !inherits(instance$windows$main$gtkWidgets[[i]], "<invalid>") ) {
			title <- instance$windows$main$gtkWidgets[[i]]$getTitle()
			title <- strsplit(title, split=" - ")[[1]]
			title <- paste(instance$name, title[[2]], sep=" - ")
			instance$windows$main$gtkWidgets[[i]]$setTitle(title)
		}
	}
	instance
}

scrubInstance <- function(instance, exceptMain=FALSE) {
	instance$interactive <- FALSE
	for ( i in seq_along(instance$windows) ) {
		if ( exceptMain && names(instance$windows[i]) == "main" ) next
		instance$windows[[i]]$open <- FALSE
		instance$windows[[i]]$widgets <- list()
		instance$windows[[i]]$devices[] <- NA
		instance$windows[[i]]$gtkWidgets <- list()
	}
	instance
}

killInstance <- function(instance, exceptMain=FALSE) {
	instance$interactive <- FALSE
	for ( i in seq_along(instance$windows) ) {
		if ( exceptMain && names(instance$windows[i]) == "main" ) next
		instance$windows[[i]]$open <- FALSE
		for ( j in seq_along(instance$windows[[i]]$topWidgets) ) {
			if ( isExtant(instance$windows[[i]]$topWidgets[[j]]) ) {
				dispose(instance$windows[[i]]$topWidgets[[j]])
			}
		}
		instance$windows[[i]]$widgets <- list()
		for ( k in instance$windows[[i]]$devices ) {
			if ( k %in% dev.list() ) {
				dev.off(k)
			}
		}
		instance$windows[[i]]$devices[] <- NA
		for ( l in seq_along(instance$windows[[i]]$gtkWidgets) ) {
			if ( !inherits(instance$windows[[i]]$gtkWidgets[[l]], "<invalid>") ) {
				instance$windows[[i]]$gtkWidgets[[l]]$destroy()
			}
		}
		instance$windows[[i]]$gtkWidgets <- list()
	}
	instance
}

killWindow <- function(instance, windowname) {
	instance$windows[[windowname]]$open <- FALSE
	for ( j in seq_along(instance$windows[[windowname]]$topWidgets) ) {
		if ( isExtant(instance$windows[[windowname]]$topWidgets[[j]]) ) {
			dispose(instance$windows[[windowname]]$topWidgets[[j]])
		}
	}
	instance$windows[[windowname]]$widgets <- list()
	for ( k in instance$windows[[windowname]]$devices ) {
		if ( k %in% dev.list() ) {
			dev.off(k)
		}
	}
	instance$windows[[windowname]]$devices[] <- NA
	for ( l in seq_along(instance$windows[[windowname]]$gtkWidgets) ) {
		if ( !inherits(instance$windows[[windowname]]$gtkWidgets[[l]], "<invalid>") ) {
			instance$windows[[windowname]]$gtkWidgets[[l]]$destroy()
		}
	}
	instance$windows[[windowname]]$gtkWidgets <- list()
	if ( getOption("guiToolkit") == "tcltk" ) {
		tcl("update")
	} else if ( getOption("guiToolkit") == "RGtk2" ) {
		gtkMainIterationDo(blocking=FALSE)
	}
}

activeInstances <- function() {
	sapply(.guiState$instances, function(inst) {
		any(sapply(inst$windows, function(w) w$open))
	}, USE.NAMES=TRUE)
}

activeDatasets <- function() {
	active <- activeInstances()
	active <- names(active)[active]
	unique(sapply(.guiState$instances[active], function(e) e$objectname))
}

freezeInstance <- function(instance, freeze=TRUE) {
	for ( i in seq_along(instance$windows) ) {
		for ( j in seq_along(instance$windows[[i]]$topWidgets) ) {
			if ( isExtant(instance$windows[[i]]$topWidgets[[j]]) ) {
				enabled(instance$windows[[i]]$topWidgets[[j]]) <- !freeze
			}
		}
		for ( k in seq_along(instance$windows[[i]]$gtkWidgets) ) {
			if ( !inherits(instance$windows[[i]]$gtkWidgets[[k]], "<invalid>") ) {
				instance$windows[[i]]$gtkWidgets[[k]]$setSensitive(!freeze)
			}
		}
	}
	instance$interactive <- !freeze
}

freezeGUI <- function(freeze=TRUE) {
	for ( instance in .guiState$instances ) {
		freezeInstance(instance, freeze=freeze)
	}
	if ( !is.null(.guiState$messageWindow) && isExtant(.guiState$messageWindow) ) {
		dispose(.guiState$messageWindow)
	}
	.guiState$isFrozen <- freeze
	Sys.sleep(0.1)
}



