
#### Class adding or closing a dataset view ####
## -------------------------------------
.iViewGroupControls <- setRefClass("iViewGroupControls",
	contains = "iViewControls",
	methods = list(
		initialize = function(...) {
			uuid <<- Cardinal:::uuid()
			interface <<- ggroup(...)
			widgets$close.view <<- gbutton(
				container=interface,
				expand=TRUE,
				border=FALSE,
				text="x")
			widgets$hide.view <<- gbutton(
				container=interface,
				expand=TRUE,
				border=FALSE,
				text="-")
			widgets$new.view <<- gbutton(
				container=interface,
				expand=TRUE,
				border=FALSE,
				text="+")
			handlers$close.view <<- addHandlerClicked(
				obj=widgets$close.view,
				handler=.clicked.close.view,
				action=.self)
			handlers$hide.view <<- addHandlerClicked(
				obj=widgets$hide.view,
				handler=.clicked.hide.view,
				action=.self)
			handlers$new.view <<- addHandlerClicked(
				obj=widgets$new.view,
				handler=.clicked.new.view,
				action=.self)
		}))

.clicked.close.view <- function(h, ...) {
	elt <- h$action$findParent("iViewGroup")
	if ( length(elt$parent$children) > 1 ) {
		delete(elt$parent$interface, elt$interface)
		for ( i in seq_along(elt$parent$children) ) {
			if ( !isExtant(elt$parent$children[[i]]$interface) )
				elt$parent$children <- elt$parent$children[-i]
		}
	}
	closed <- which(names(elt$parent$children) == elt$uuid)
	elt$parent$children <- elt$parent$children[-closed]
}

.clicked.hide.view <- function(h, ...) {
	elt <- h$action$findParent("iViewGroup")
	visible(elt$interface) <- FALSE
}

.clicked.new.view <- function(h, ...) {
	elt <- h$action$findParent("iViewTab")
	elt$addElement(.iViewMSImageSet(), expand=TRUE)
	visible(elt$children[[length(elt$children)]]$interface) <- TRUE
}

