
#### Class for viewing a single MSImageSet ####
## --------------------------------------------
.iViewMSImageSet <- setRefClass("iViewMSImageSet",
	contains = "CardinaliView",
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
			addElement(.iViewMassSpectrum(width=400, height=400))
			addElement(.iViewIonImage(width=400, height=400))
		}))
