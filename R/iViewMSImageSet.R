
#### Class for viewing a single MSImageSet ####
## --------------------------------------------
.iViewMSImageSet <- setRefClass("iViewMSImageSet",
	contains = "iViewGroup",
	methods = list(
		initialize = function(...) {
			uuid <<- Cardinal:::uuid()
			interface <<- gexpandgroup(...)
			plist$dataset <<- character(1)
			plist$feature.linked <<- logical(1)
			plist$pixel.linked <<- logical(1)
			plist$ms.zoom.linked <<- logical(1)
			plist$ms.intensity.zoom.linked <<- logical(1)
			plist$img.zoom.linked <<- logical(1)
			plist$img.intensity.zoom.linked <<- logical(1)
			addElement(.iViewMSImageSetControls())
			addElement(.iViewMSImageSetPlots(), expand=TRUE)
		},
		load = function() {
			object <- try(get(plist$dataset, envir=globalenv()))
			if ( !is(object, "MSImageSet") ) return()
			setup(dataset=plist$dataset,
				ms.zoom=100,
				feature=1,
				feature.linked=FALSE,
				mz=mz(object)[[1]],
				mz.plusminus=0,
				mz.min=min(mz(object)),
				mz.max=max(mz(object)),
				ms.intensity.zoom=100,
				ms.intensity.zoom.linked=FALSE,
				ms.intensity.min=min(spectra(object)[,1]),
				ms.intensity.max=max(spectra(object)[,1]),
				img.zoom=100,
				img.zoom.linked=FALSE,
				pixel=1,
				pixel.linked=FALSE,
				x=coord(object)[1,1],
				y=coord(object)[1,2],
				x.min=min(coord(object)[[1]]),
				x.max=max(coord(object)[[1]]),
				y.min=min(coord(object)[[2]]),
				y.max=max(coord(object)[[2]]),
				img.intensity.zoom=100,
				img.intensity.zoom.linked=FALSE,
				img.intensity.min=min(spectra(object)[1,]),
				img.intensity.max=max(spectra(object)[1,]))
			while (gtkEventsPending())
				gtkMainIterationDo(blocking=FALSE)
			Sys.sleep(0.1)
			refresh()
		},
		update=function(...) {
			callSuper(...)
			dots <- list(...)
			if ( "dataset" %in% names(dots) ) {
				names(interface) <<- plist$dataset
				load()
			}
		}))

#### Class for MSImageSet control panels ####
## ------------------------------------------
.iViewMSImageSetControls <- setRefClass("iViewMSImageSetControls",
	contains = "iViewElement",
	methods = list(
		initialize = function(..., horizontal=FALSE) {
			uuid <<- Cardinal:::uuid()
			interface <<- ggroup(..., horizontal=horizontal)
			addElement(.iViewDatasetControls())
			addElement(.iViewMassSpectrumControls())
			addElement(.iViewIonImageControls())
			addElement(.iViewGroupControls())
		}))

#### Class for displaying MSImageSet plots ####
## --------------------------------------------
.iViewMSImageSetPlots <- setRefClass("iViewMSImageSetPlots",
	contains = "iViewElement",
	methods = list(
		initialize = function(...) {
			uuid <<- Cardinal:::uuid()
			interface <<- gpanedgroup(...)
			addElement(.iViewMassSpectrum(width=400, height=400))
			addElement(.iViewIonImage(width=400, height=400))
		}))

.iView <- function(data) {
	w <- .iViewWindow()
	.CardinaliView[[Cardinal:::uuid()]] <- w
	visible(w$interface) <- TRUE
	while (gtkEventsPending())
		gtkMainIterationDo(blocking=FALSE)
	Sys.sleep(0.1)
	if ( missing(data) ) {
		w$refresh()
	} else if ( is(data, "MSImageSet") ) {
		w$update(dataset=deparse(substitute(data)))
	} else {
		stop(deparse(substitute(data)), " is not an MSImageSet")
	}
}



