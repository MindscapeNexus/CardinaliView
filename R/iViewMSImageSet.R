
#### Class for a top-level iView MSImageSet window ####
## ----------------------------------------------------
.iViewMSImageWindow <- setRefClass("iViewMSImageWindow",
	contains = "iViewWindow",
	methods = list(
		initialize = function(...) {
			callSuper(...)
			toolbar <- list()
			toolbar$Datasets <- gaction(
				label="Datasets",
				icon="harddisk",
				tooltip="Manage datasets")
			toolbar$DataGroup = list(separator = TRUE)
			toolbar$Import <- gaction(
				label="Import",
				icon="goto-bottom",
				tooltip="Import dataset from disk")
			toolbar$Export <- gaction(label="Export",
				icon="goto-top",
				tooltip="Write current dataset to disk")
			toolbar$ActionGroup = list(separator = TRUE)
			toolbar$Preprocess <- gaction(
				label="Pre-Process",
				icon="execute",
				tooltip="Perform pre-processing on current dataset")
			toolbar$Analysis <- gaction(
				label="Analysis",
				icon="plot",
				tooltip="Perform statistical analysis on current dataset")
			toolbar$Results <- gaction(
				label="Results",
				icon="contour",
				tooltip="View results of previous analyses")
			toolbar$WindowGroup = list(separator = TRUE)
			toolbar$Window <- gaction(
				label="New Window",
				icon="add",
				tooltip="Open a new window")
			toolbar$Tab <- gaction(
				label="New Tab",
				icon="add",
				tooltip="Open a new tab")
			widgets$toolbar <<- gtoolbar(toolbarlist=toolbar, container=interface)
			addElement(.iViewMSImageNotebook(), expand=TRUE)
		}))

#### Class for viewing a group of MSImageSet ####
## ----------------------------------------------
.iViewMSImageNotebook <- setRefClass("iViewMSImageNotebook",
	contains = "iViewNotebook",
	methods = list(
		initialize = function(..., horizontal=FALSE,
			use.scrollwindow=TRUE)
		{
			callSuper(..., closebuttons=TRUE, dontCloseTHese=1)
			if ( length(children) == 0 ) {
				addElement(.iViewMSImageGroup(), expand=TRUE,
					label="(no dataset)")
			}
		}))

#### Class for viewing a group of MSImageSet ####
## ----------------------------------------------
.iViewMSImageGroup <- setRefClass("iViewMSImageGroup",
	contains = "iViewGroup",
	methods = list(
		initialize = function(..., horizontal=FALSE,
			use.scrollwindow=TRUE)
		{
			callSuper(..., horizontal=horizontal,
				use.scrollwindow=use.scrollwindow)
			if ( length(children) == 0 )
				addElement(.iViewMSImageSet(), expand=TRUE)
			for ( child in children )
				visible(child$interface) <- TRUE
		},
		update = function(..., with.properties=NULL) {
			if ( !is.null(with.properties) ) {
				for ( child in children ) {
					properties <- child$plist[names(with.properties)]
					if ( isTRUE(all(properties == with.properties)) ) {
						if ( child$isDirty(...) )
							child$update(...)
					}
				}
			} else {
				callSuper(...)
			}
		}))

#### Class for viewing a single MSImageSet ####
## --------------------------------------------
.iViewMSImageSet <- setRefClass("iViewMSImageSet",
	contains = "iViewExpandGroup",
	methods = list(
		initialize = function(...) {
			callSuper(...)
			plist$feature.linked <<- logical(1)
			plist$pixel.linked <<- logical(1)
			plist$ms.zoom.linked <<- logical(1)
			plist$ms.intensity.zoom.linked <<- logical(1)
			plist$img.zoom.linked <<- logical(1)
			plist$img.intensity.zoom.linked <<- logical(1)
			addElement(.iViewMSImageSetControls())
			addElement(.iViewMSImageSetPlots(), expand=TRUE)
		},
		update = function(...) {
			dots <- list(...)
			for ( par in names(plist) ) {
				if ( par %in% names(dots) )
					plist[[par]] <<- dots[[par]]
			}
			callSuper(...)
		}))

#### Class for MSImageSet control panels ####
## ------------------------------------------
.iViewMSImageSetControls <- setRefClass("iViewMSImageSetControls",
	contains = "iViewGroup",
	methods = list(
		initialize = function(..., horizontal=FALSE) {
			callSuper(..., horizontal=horizontal)
			addElement(.iViewDatasetControls())
			addElement(.iViewMassSpectrumControls())
			addElement(.iViewIonImageControls())
			addElement(.iViewAddControls())
		}))

#### Class for displaying MSImageSet plots ####
## --------------------------------------------
.iViewMSImageSetPlots <- setRefClass("iViewMSImageSetPlots",
	contains = "iViewPanedGroup",
	methods = list(
		initialize = function(...) {
			callSuper(...)
			addElement(.iViewMassSpectrum())
			addElement(.iViewIonImage())
		}))

