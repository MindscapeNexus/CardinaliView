
#### Virtual class for iView GUI nodes ####
## defined as an node containing iView nodes and a parent
## ------------------------------------------------------
setRefClass("iView",
	fields = c(
		uuid = "character",			# unique identifier
		parent = "ANY",				# enclosing element
		children = "list",			# child elements
		interface = "ANY",			# interfacing widget
		widgets = "list",			# additional widgets
		handlers = "list",			# handler functions
		plist = "list"),			# properties
	contains = "VIRTUAL",
	methods = list(
		setup = function(...) {
			dots <- list(...)
			for ( par in names(plist) ) {
				if ( par %in% names(dots) ) {
					plist[[par]] <<- dots[[par]]
					if ( par %in% names(widgets) ) {
						blockHandler(widgets[[par]], handlers[[par]])
						svalue(widgets[[par]]) <<- dots[[par]]
						unblockHandler(widgets[[par]], handlers[[par]])
					}
				}
			}
			for ( child in children )
				child$setup(...)
		},
		addElement = function(child, ...) {
			child$parent <- .self
			children[[child$uuid]] <<- child
			add(interface, child$interface, ...)
		},
		refresh = function(force=FALSE) {
			for ( child in children ) {
				child$refresh(force=force)
			}
		},
		update = function(...) {
			for ( child in children )
				child$update(...)
		},
		findParent = function(class) {
			current <- parent
			while ( !is.null(current) && !is(current, class) )
				current <- current$parent
			current
		},
		root = function() {
			current <- .self
			while ( !is.null(current$parent) )
				current <- current$parent
			current
		},
		show = function() {
			print(class(.self))
			cat("interface:", class(interface), "\n")
			cat("children:", length(children), "\n")
		}))

#### Virtual class for iView GUI child elements ####
## a widget or family of widgets in an iView GUI interface
## -------------------------------------------------------
setRefClass("iViewElement",
	fields = c(
		parent = "iView",
		interface = "guiWidget"),
	contains = c("iView", "VIRTUAL"))

#### Class for a gGraphics widget ####
## holds an updateable graphical plotting device
## ---------------------------------------------
setRefClass("iViewGraphics",
	fields = c(
		interface = "gGraphics",
		dirty = "logical"),
	contains = c("iViewElement", "VIRTUAL"),
	methods = list(
		update = function(..., blocking=FALSE) {
			dots <- list(...)
			if ( any(names(dots) %in% names(plist)) )
				dirty <<- TRUE
			for ( par in names(plist) ) {
				if ( par %in% names(dots) )
					plist[[par]] <<- dots[[par]]
			}
			if ( !blocking )
				refresh()
			callSuper(...)
		}))

#### Class for a control panel ####
## a group with updateable controls
## --------------------------------
setRefClass("iViewControls",
	fields = c(interface = "guiContainer"),
	contains = c("iViewElement", "VIRTUAL"),
	methods = list(
		update = function(...) {
			dots <- list(...)
			for ( par in names(plist) ) {
				if ( par %in% names(dots) ) {
					plist[[par]] <<- dots[[par]]
					if ( par %in% names(widgets) ) {
						blockHandler(widgets[[par]], handlers[[par]])
						svalue(widgets[[par]]) <<- dots[[par]]
						unblockHandler(widgets[[par]], handlers[[par]])
					}
				}
			}
			callSuper(...)
		}))

#### Class for a top-level a CardinaliView window ####
## ----------------------------------------------------
.iViewWindow <- setRefClass("CardinaliViewWindow",
	fields = c(interface = "gWindow"),
	contains = "iView",
	methods = list(
		initialize = function(..., visible=FALSE, title="CardinaliView") {
			uuid <<- Cardinal:::uuid()
			interface <<- gwindow(..., visible=visible, title=title)
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
			handlers$Datasets <<- addHandlerChanged(
				obj=toolbar$Datasets,
				handler=.clicked.Datasets,
				action=.self)
			handlers$Import <<- addHandlerChanged(
				obj=toolbar$Import,
				handler=.clicked.Import,
				action=.self)
			handlers$Export <<- addHandlerChanged(
				obj=toolbar$Export,
				handler=.clicked.Export,
				action=.self)
			handlers$Preprocess <<- addHandlerChanged(
				obj=toolbar$Preprocess,
				handler=.clicked.Preprocess,
				action=.self)
			handlers$Analysis <<- addHandlerChanged(
				obj=toolbar$Analysis,
				handler=.clicked.Analysis,
				action=.self)
			handlers$Results <<- addHandlerChanged(
				obj=toolbar$Results,
				handler=.clicked.Results,
				action=.self)
			handlers$Window <<- addHandlerChanged(
				obj=toolbar$Window,
				handler=.clicked.Window,
				action=.self)
			handlers$Tab <<- addHandlerChanged(
				obj=toolbar$Tab,
				handler=.clicked.Tab,
				action=.self)
			widgets$toolbar <<- gtoolbar(toolbarlist=toolbar, container=interface)
			addElement(.iViewNotebook(), expand=TRUE)
		}))

#### Class for a tabbed interface of CardinaliView groups ####
## -----------------------------------------------------------
.iViewNotebook <- setRefClass("iViewNotebook",
	fields = c(interface = "gNotebook"),
	contains = "iViewElement",
	methods = list(
		initialize = function(..., closebuttons=TRUE, dontCloseThese=1) {
			uuid <<- Cardinal:::uuid()
			interface <<- gnotebook(...,
				closebuttons=closebuttons,
				dontCloseThese=dontCloseThese)
			if ( length(children) == 0 ) {
				addElement(.iViewTab(), expand=TRUE,
					label="(no dataset)")
			}
		}))

#### Class for holding expandable CardinaliView instances ####
## -----------------------------------------------------------
.iViewTab <- setRefClass("iViewTab",
	fields = c(interface = "gGroup"),
	contains = "iViewElement",
	methods = list(
		initialize = function(..., horizontal=FALSE, use.scrollwindow=TRUE) {
			uuid <<- Cardinal:::uuid()
			interface <<- ggroup(...,
				horizontal=horizontal,
				use.scrollwindow=use.scrollwindow)
			if ( length(children) == 0 )
				addElement(.iViewMSImageSet(), expand=TRUE)
			for ( child in children )
				visible(child$interface) <- TRUE
		},
		update = function(..., with.properties=NULL) {
			if ( is.null(with.properties) ) {
				callSuper(...)
			} else {
				for ( child in children ) {
					properties <- child$plist[names(with.properties)]
					if ( isTRUE(all(properties == with.properties)) )
						child$update(...)
				}
			}
		}))

#### Virtual class for a single CardinaliView instance ####
## --------------------------------------------------------
.iViewGroup <- setRefClass("iViewGroup",
	fields = c(interface = "gExpandGroup"),
	contains = c("iViewElement", "VIRTUAL"),
	methods = list(
		update = function(...) {
			dots <- list(...)
			for ( par in names(plist) ) {
				if ( par %in% names(dots) )
					plist[[par]] <<- dots[[par]]
			}
			callSuper(...)
		}))

.clicked.Datasets <- function(h, ...) {
	print("stub")
}

.clicked.Import <- function(h, ...) {
	print("stub")
}

.clicked.Export <- function(h, ...) {
	print("stub")
}

.clicked.Preprocess <- function(h, ...) {
	print("stub")
}

.clicked.Analysis <- function(h, ...) {
	print("stub")
}

.clicked.Results <- function(h, ...) {
	print("stub")
}

.clicked.Window <- function(h, ...) {
	.iView()
}

.clicked.Tab <- function(h, ...) {
	elt <- h$action$children[[1]]
	elt$addElement(.iViewTab(),
		expand=TRUE, label="(no dataset)")
}

