
#### Virtual class for iView GUI nodes ####
## defined as an node containing iView nodes and a parent
## ------------------------------------------------------
setRefClass("iView",
	fields = c(
		parent = "ANY",				# enclosing element
		children = "list",			# child elements
		interface = "ANY",			# interfacing widget
		widgets = "list",			# additional widgets
		handlers = "list",			# handler functions
		plist = "list"),			# properties
	contains = "VIRTUAL",
	methods = list(
		addElement = function(child, ...) {
			child$parent <- .self
			children[[length(children)+1]] <<- child
			add(interface, child$interface, ...)
		},
		isDirty = function(...) {
			dots <- match.call(expand.dots=FALSE)$...
			any(names(plist) %in% names(dots))
		},
		refresh = function(..., all = FALSE) {
			if ( all )
				for ( child in children ) {
					child$refresh(all)
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

#### Class for a top-level gWindow ####
## a top-level window for GUI elements
## -----------------------------------
.iViewWindow <- setRefClass("iViewWindow",
	fields = c(
		parent = "NULL",
		interface = "gWindow"),
	contains = "iView",
	methods = list(
		initialize = function(..., visible=FALSE) {
			interface <<- gwindow(..., visible=visible)
		}))

#### Virtual class for iView GUI child elements ####
## a widget or family of widgets in an iView GUI interface
## -------------------------------------------------------
setRefClass("iViewElement",
	fields = c(
		parent = "iView",
		interface = "guiWidget"),
	contains = c("iView", "VIRTUAL"))

#### Class for a gNotebook widget ####
## a tabbed interface for displaying multiple pages
## ------------------------------------------------
.iViewNotebook <- setRefClass("iViewNotebook",
	fields = c(interface = "gNotebook"),
	contains = "iViewElement",
	methods = list(
		initialize = function(...) {
			interface <<- gnotebook(...)
		}))

#### Class for a gGroup widget ####
## holds a group into which other iView widgets can be packed
## ----------------------------------------------------------
.iViewGroup <- setRefClass("iViewGroup",
	fields = c(interface = "gGroup"),
	contains = "iViewElement",
	methods = list(
		initialize = function(...) {
			interface <<- ggroup(...)
		}))

#### Class for a gExpandGroup widget ####
## holds a paned group with two dynamically-adjustable panes
## ---------------------------------------------------------
.iViewExpandGroup <- setRefClass("iViewExpandGroup",
	fields = c(interface = "gExpandGroup"),
	contains = "iViewElement",
	methods = list(
		initialize = function(...) {
			interface <<- gexpandgroup(...)
		}))

#### Class for a gPanedGroup widget ####
## holds a paned group with two dynamically-adjustable panes
## ---------------------------------------------------------
.iViewPanedGroup <- setRefClass("iViewPanedGroup",
	fields = c(interface = "gPanedGroup"),
	contains = "iViewElement",
	methods = list(
		initialize = function(...) {
			interface <<- gpanedgroup(...)
		}))

#### Class for a gGraphics widget ####
## holds an updateable graphical plotting device
## ---------------------------------------------
.iViewGraphics <- setRefClass("iViewGraphics",
	fields = c(interface = "gGraphics"),
	contains = "iViewElement",
	methods = list(
		initialize = function(...) {
			interface <<- ggraphics(...)
		},
		refresh = function(...) {
			visible(interface) <<- TRUE
			callSuper(...)
		},
		update = function(..., blocking=FALSE) {
			dots <- list(...)
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
.iViewControls <- setRefClass("iViewControls",
	fields = c(interface = "guiContainer"),
	contains = "iViewElement",
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
.CardinaliViewWindow <- setRefClass("CardinaliViewWindow",
	contains = "iViewWindow",
	methods = list(
		initialize = function(..., title="CardinaliView") {
			callSuper(..., title=title)
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
			addElement(.CardinaliViewNotebook(), expand=TRUE)
		}))

#### Class for a tabbed interface of CardinaliView groups ####
## -----------------------------------------------------------
.CardinaliViewNotebook <- setRefClass("CardinaliViewNotebook",
	contains = "iViewNotebook",
	methods = list(
		initialize = function(..., horizontal=FALSE,
			use.scrollwindow=TRUE)
		{
			callSuper(..., closebuttons=TRUE, dontCloseTHese=1)
			if ( length(children) == 0 ) {
				addElement(.CardinaliViewGroup(), expand=TRUE,
					label="(no dataset)")
			}
		}))

#### Class for holding expandable CardinaliView instances ####
## -----------------------------------------------------------
.CardinaliViewGroup <- setRefClass("CardinaliViewGroup",
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
.CardinaliView <- setRefClass("CardinaliView",
	contains = c("iViewExpandGroup", "VIRTUAL"),
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
	print("stub")	
}

.clicked.Tab <- function(h, ...) {
	elt <- h$action$children[[1]]
	elt$addElement(.CardinaliViewGroup(),
		expand=TRUE, label="(no dataset)")
}

