
#### Class for viewing a single MSImageSet ####
## --------------------------------------------
.iViewMSImageSet <- setRefClass("iViewMSImageSet",
	contains = "iViewGroup",
	methods = list(
		initialize = function(...) {
			interface <<- gexpandgroup(...)
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
	contains = "iViewElement",
	methods = list(
		initialize = function(..., horizontal=FALSE) {
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
			interface <<- gpanedgroup(...)
			addElement(.iViewMassSpectrum(width=400, height=400))
			addElement(.iViewIonImage(width=400, height=400))
		}))
