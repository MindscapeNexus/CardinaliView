
#### Class selecting a dataset ####
## --------------------------------
.iViewDatasetControls <- setRefClass("iViewDatasetControls",
	contains = "iViewControls",
	methods = list(
		initialize = function(...) {
			interface <<- ggroup(...)
			plist$dataset <<- character(1)
			widgets$dataset.frame  <<- gframe(
				container=interface,
				horizontal=FALSE,
				expand=TRUE,
				text="Dataset")
			widgets$dataset <<- gcombobox(
				container=widgets$dataset.frame,
				expand=TRUE,
				items=c("(no dataset)"))
			handlers$dataset <<- addHandlerChanged(
				obj=widgets$dataset,
				handler=.changed.dataset,
				action=.self)
		}))

.changed.dataset <- function(h, ...) {
	print("stub")
}
