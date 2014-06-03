
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
		refresh = function(all = FALSE) {
			if ( all )
				for ( child in children ) {
					child$refresh(all)
				}
		},
		update = function(...) {
			for ( child in children ) {
				if ( child$isDirty(...) )
					child$update(...)
			}
		},
		findParent = function(class) {
			current <- parent
			while ( !is.null(current) && class(current) != class )
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
		update = function(...) {
			refresh()
			callSuper(...)
		}))

