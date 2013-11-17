
#### functions controlling window placement ####

tryWindowPlacement <- function(instance, window, type) {
	if ( getOption("guiToolkit") != "RGtk2" ) return()
	offset <- 100 * (sum(activeInstances()) - 1)
	if ( type == "plot" ) {
		if ( instance$windows$main$devices$plot %in% dev.list() && !inherits(instance$windows$main$gtkWidgets$plot, "<invalid>") ) {
			pos <- instance$windows$main$gtkWidgets$plot$getPosition()
			getToolkitWidget(window)$move(pos$root.x - window$getSize()$width, pos$root.y)
		} else {
			getToolkitWidget(window)$move(50, 30 + offset)
		}
	} else if ( type == "image" ) {
		if ( instance$windows$main$devices$plot %in% dev.list() && !inherits(instance$windows$main$gtkWidgets$image, "<invalid>") ) {
			pos <- instance$windows$main$gtkWidgets$image$getPosition()
			getToolkitWidget(window)$move(pos$root.x - window$getSize()$width, pos$root.y)
		} else {
			getToolkitWidget(window)$move(gtkScreenResolution() / 2, 30 + offset)
		}
	}
}

tryDevicePlacement <- function(instance, widget, devicename) {
	if ( getOption("guiToolkit") != "RGtk2" ) return()
	if ( grepl(pattern="plot", x=devicename) ) {
		type <- "plot"
	} else if ( grepl(pattern="image", x=devicename) ) {
		type <- "image"
	}
	if ( type == "plot" ) {
		if ( !isExtant(instance$windows$main$topWidgets$plotControlWindow) ) {
			makePlotControls(instance)
		}
		pos <- getToolkitWidget(instance$windows$main$topWidgets$plotControlWindow)$getPosition()
		size <- getToolkitWidget(instance$windows$main$topWidgets$plotControlWindow)$getSize()
		offset <- 30 * (length(activeDevices(instance, type="plot")) - 1)
		widget$move(pos$root.x + size$width + offset, pos$root.y + offset)
	} else if ( type == "image" ) {
		if ( !isExtant(instance$windows$main$topWidgets$imageControlWindow) ) {
			makeImageControls(instance)
		}
		pos <- getToolkitWidget(instance$windows$main$topWidgets$imageControlWindow)$getPosition()
		size <- getToolkitWidget(instance$windows$main$topWidgets$imageControlWindow)$getSize()
		offset <- 30 * (length(activeDevices(instance, type="image")) - 1)
		widget$move(pos$root.x + size$width + offset, pos$root.y + offset)
	}
}

gtkScreenResolution <- function() {
	win <- gtkWindow(show=FALSE)
	width <- win$getScreen()$getWidth()
	height <- win$getScreen()$getHeight()
	win$destroy()
	c(width, height)
}

