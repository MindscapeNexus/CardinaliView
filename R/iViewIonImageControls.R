
#### Class for mass spectrum controls ####
## ---------------------------------------
.iViewIonImageControls <- setRefClass("iViewIonImageControls",
	contains = "iViewControls",
	methods = list(
		initialize = function(..., horizontal=FALSE, expand=TRUE) {
			interface <<- gexpandgroup(...,
				horizontal=horizontal,
				expand=expand,
				text="Ion Image")
			# properties list
			plist$img.zoom <<- numeric(1)
			plist$img.zoom.linked <<- logical(1)
			plist$pixel <<- numeric(1)
			plist$pixel.linked <<- logical(1)
			plist$x <<- numeric(1)
			plist$y <<- numeric(1)
			plist$img.intensity.zoom <<- numeric(1)
			plist$img.intensity.zoom.linked <<- logical(1)
			plist$img.intensity.min <<- numeric(1)
			plist$img.intensity.max <<- numeric(1)
			# coord - zoom
			widgets$img.zoom.group <<- ggroup(
				container=interface,
				expand=FALSE)
			widgets$img.zoom.label <<- glabel(
				container=widgets$img.zoom.group,
				text="zoom")
			widgets$img.zoom <<- gspinbutton(
				container=widgets$img.zoom.group,
				expand=TRUE,
				from=100,
				to=1000,
				by=25)
			widgets$img.zoomprcnt.label <<- glabel(
				container=widgets$img.zoom.group,
				text="%")
			widgets$img.zoom.linked <<- gcheckbox(
				container=widgets$img.zoom.group,,
				text="")
			# coord - pixel
			widgets$pixel.group <<- ggroup(
				container=interface,
				expand=FALSE)
			widgets$pixel.label <<- glabel(
				container=widgets$pixel.group,
				text="pixel")
			widgets$pixel <<- gspinbutton(
				container=widgets$pixel.group,
				expand=TRUE,
				from=1,
				to=2^31-1,
				by=1)
			widgets$pixel.linked <<- gcheckbox(
				container=widgets$pixel.group,,
				text="")
			widgets$x.group <<- ggroup(
				container=interface,
				expand=FALSE)
			widgets$x.label <<- glabel(
				container=widgets$x.group,
				text="x")
			widgets$x <<- gspinbutton(
				container=widgets$x.group,
				expand=TRUE,
				from=1,
				to=2^31-1,
				by=1)
			widgets$y.group <<- ggroup(
				container=interface,
				expand=FALSE)
			widgets$y.label <<- glabel(
				container=widgets$y.group,
				text="y")
			widgets$y <<- gspinbutton(
				container=widgets$y.group,
				expand=TRUE,
				from=1,
				to=2^31-1,
				by=1)
			## ion image intensity controls
			widgets$img.intensity.frame <<- gframe(
				container=interface,
				horizontal=FALSE,
				expand=FALSE,
				text="Intensity Range")
			# intensity - zoom
			widgets$img.intensity.zoom.group <<- ggroup(
				container=widgets$img.intensity.frame,
				expand=FALSE)
			widgets$img.intensity.zoom.label <<- glabel(
				container=widgets$img.intensity.zoom.group,
				text="zoom")
			widgets$img.intensity.zoom <<- gspinbutton(
				container=widgets$img.intensity.zoom.group,
				expand=TRUE,
				from=100,
				to=1000,
				by=25)
			widgets$img.intensity.zoomprcnt.label <<- glabel(
				container=widgets$img.intensity.zoom.group,
				text="%")
			widgets$img.intensity.zoom.linked <<- gcheckbox(
				container=widgets$img.intensity.zoom.group,,
				text="")
			# intensity - range
			widgets$img.intensity.group <<- ggroup(
				container=widgets$img.intensity.frame,
				expand=FALSE)
			widgets$img.intensity.min.label <<- glabel(
				container=widgets$img.intensity.group,
				text="min")
			widgets$img.intensity.min <<- gedit(
				container=widgets$img.intensity.group,
				expand=TRUE,
				width=5)
			widgets$img.intensity.max.label <<- glabel(
				container=widgets$img.intensity.group,
				text="max")
			widgets$img.intensity.max <<- gedit(
				container=widgets$img.intensity.group,
				expand=TRUE,
				width=5)
			# handlers
			handlers$img.zoom <<- addHandlerChanged(
				obj=widgets$img.zoom,
				handler=.changed.img.zoom,
				action=.self)
			handlers$img.zoom.linked <<- addHandlerChanged(
				obj=widgets$img.zoom.linked,
				handler=.changed.img.zoom.linked,
				action=.self)
			handlers$pixel <<- addHandlerChanged(
				obj=widgets$pixel,
				handler=.changed.pixel,
				action=.self)
			handlers$pixel.linked <<- addHandlerChanged(
				obj=widgets$pixel.linked,
				handler=.changed.pixel.linked,
				action=.self)
			handlers$x <<- addHandlerChanged(
				obj=widgets$x,
				handler=.changed.x,
				action=.self)
			handlers$y <<- addHandlerChanged(
				obj=widgets$y,
				handler=.changed.y,
				action=.self)
			handlers$img.intensity.zoom <<- addHandlerChanged(
				obj=widgets$img.intensity.zoom,
				handler=.changed.img.intensity.zoom,
				action=.self)
			handlers$img.intensity.zoom.linked <<- addHandlerChanged(
				obj=widgets$img.intensity.zoom.linked,
				handler=.changed.img.intensity.zoom.linked,
				action=.self)
			handlers$img.intensity.min <<- addHandlerChanged(
				obj=widgets$img.intensity.min,
				handler=.changed.img.intensity.min,
				action=.self)
			handlers$img.intensity.max <<- addHandlerChanged(
				obj=widgets$img.intensity.max,
				handler=.changed.img.intensity.max,
				action=.self)
		}))

.changed.img.zoom <- function(h, ...) {
	print("stub")
}

.changed.img.zoom.linked <- function(h, ...) {
	img.zoom.linked <- as.logical(svalue(h$obj))
	elt <- h$action$findParent("CardinaliView")
	elt$update(img.zoom.linked=img.zoom.linked)
}

.changed.pixel <- function(h, ...) {
	# need to fix to change coord at the same time!
	pixel <- as.integer(svalue(h$obj))
	elt <- h$action$findParent("CardinaliView")
	if ( elt$plist$pixel.linked ) {
		elt <- elt$findParent("CardinaliViewGroup")
		elt$update(pixel=pixel,
			with.properties=c(pixel.linked=TRUE))
	} else {
		elt$update(pixel=pixel)
	}
}

.changed.pixel.linked <- function(h, ...) {
	value <- as.logical(svalue(h$obj))
	elt <- h$action$findParent("CardinaliView")
	elt$update(pixel.linked=value)
}

.changed.x <- function(h, ...) {
	# need to fix to change pixel at the same time!
	x <- as.numeric(svalue(h$obj))
	elt <- h$action$findParent("CardinaliView")
	if ( elt$plist$pixel.linked ) {
		elt <- elt$findParent("CardinaliViewGroup")
		elt$update(x=x,
			with.properties=c(pixel.linked=TRUE))
	} else {
		elt$update(x=x)
	}
}

.changed.y <- function(h, ...) {
	# need to fix to change pixel at the same time!
	y <- as.numeric(svalue(h$obj))
	elt <- h$action$findParent("CardinaliView")
	if ( elt$plist$pixel.linked ) {
		elt <- elt$findParent("CardinaliViewGroup")
		elt$update(y=y,
			with.properties=c(pixel.linked=TRUE))
	} else {
		elt$update(y=y)
	}
}

.changed.img.intensity.zoom <- function(h, ...) {
	print("stub")
}

.changed.img.intensity.zoom.linked <- function(h, ...) {
	img.intensity.zoom.linked <- as.logical(svalue(h$obj))
	elt <- h$action$findParent("CardinaliView")
	elt$update(img.intensity.zoom.linked=img.intensity.zoom.linked)
}

.changed.img.intensity.zoom.min <- function(h, ...) {
	img.intensity.zoom.min <- round(as.numeric(svalue(h$obj)), digits=4)
	elt <- h$action$findParent("CardinaliView")
	if ( elt$plist$img.zoom.linked ) {
		elt <- elt$findParent("CardinaliViewGroup")
		elt$update(img.intensity.zoom.min=img.intensity.zoom.min,
			with.properties=c(img.zoom.linked=TRUE))
	} else {
		elt$update(img.intensity.zoom.min=img.intensity.zoom.min)
	}
}

.changed.img.intensity.zoom.max <- function(h, ...) {
	img.intensity.zoom.max <- round(as.numeric(svalue(h$obj)), digits=4)
	elt <- h$action$findParent("CardinaliView")
	if ( elt$plist$img.zoom.linked ) {
		elt <- elt$findParent("CardinaliViewGroup")
		elt$update(img.intensity.zoom.max=img.intensity.zoom.max,
			with.properties=c(img.zoom.linked=TRUE))
	} else {
		elt$update(img.intensity.zoom.max=img.intensity.zoom.max)
	}
}
