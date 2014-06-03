
#### Class selecting a dataset ####
## --------------------------------
.iViewDatasetControls <- setRefClass("iViewDatasetControls",
	contains = "iViewGroup",
	methods = list(
		initialize = function(..., horizontal=FALSE, expand=TRUE) {
			callSuper(..., horizontal=horizontal, expand=expand)
			plist$dataset <<- character(1)
			widgets$dataset.frame  <<- gframe(
				container=interface,
				horizontal=FALSE,
				expand=FALSE,
				text="Dataset")
			widgets$dataset <<- gcombobox(
				container=widgets$dataset.frame,
				expand=TRUE,
				items=c("(no dataset)"))
		}))
