
#### Class selecting a dataset ####
## --------------------------------
.iViewDatasetControls <- setRefClass("iViewDatasetControls",
	contains = "iViewControls",
	methods = list(
		initialize = function(...) {
			uuid <<- Cardinal:::uuid()
			interface <<- ggroup(...)
			plist$dataset <<- "(no dataset)"
			widgets$dataset.frame  <<- gframe(
				container=interface,
				horizontal=FALSE,
				expand=TRUE,
				text="Dataset")
			widgets$dataset <<- gcombobox(
				container=widgets$dataset.frame,
				expand=TRUE,
				items=c("(no dataset)", .ls.MSImageSet()))
			handlers$dataset <<- addHandlerChanged(
				obj=widgets$dataset,
				handler=.changed.dataset,
				action=.self)
		}))

.changed.dataset <- function(h, ...) {
	dataset <- svalue(h$obj)
	blockHandler(h$action$widgets$dataset, h$action$handlers$dataset)
	h$obj[] <- .ls.MSImageSet()
	unblockHandler(h$action$widgets$dataset, h$action$handlers$dataset)
	elt <- h$action$findParent("iViewGroup")
	elt$update(dataset=dataset)
	elt <- h$action$findParent("iViewNotebook")
	tab <- svalue(elt$interface)
	names(elt$interface)[tab] <- dataset
}

.ls.MSImageSet <- function() {
	objects <- eapply(globalenv(), is, "MSImageSet")
	names(objects)[unlist(objects)]
}
