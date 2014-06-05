
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
			plist$x.min <<- numeric(1)
			plist$x.max <<- numeric(1)
			plist$y.min <<- numeric(1)
			plist$y.max <<- numeric(1)
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
				to=10^9,
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
				from=0,
				to=10^9,
				by=5)
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
		},
		update = function(...) {
			callSuper(...)
			dots <- list(...)
			if ( any(c("x.min", "x.max", "ymin", "y.max") %in% names(dots)) ) {
				abs.x.min <- 0
				abs.x.max <- 100
				abs.y.min <- 0
				abs.y.max <- 100
				img.zoom <- 100 * sqrt(
					((abs.x.max - abs.x.min) * (abs.y.max - abs.y.min)) / 
					((plist$x.max - plist$x.min) * (plist$y.max - plist$y.min)))
				plist$img.zoom <<- img.zoom
				blockHandler(widgets$img.zoom, handlers$img.zoom)
				svalue(widgets$img.zoom) <<- img.zoom
				unblockHandler(widgets$img.zoom, handlers$img.zoom)
			}
			if ( any(c("img.intensity.min", "img.intensity.max") %in% names(dots)) ) {
				abs.img.intensity.min <- 0
				abs.img.intensity.max <- 100
				img.intensity.zoom <- 100 * abs(
					(abs.img.intensity.max - abs.img.intensity.min) / 
					(plist$img.intensity.max - plist$img.intensity.min))
				plist$img.intensity.zoom <<- img.intensity.zoom
				blockHandler(widgets$img.intensity.zoom, handlers$img.intensity.zoom)
				svalue(widgets$img.intensity.zoom) <<- img.intensity.zoom
				unblockHandler(widgets$img.intensity.zoom, handlers$img.intensity.zoom)
			}
		}))

.changed.img.zoom <- function(h, ...) {
	abs.x.min <- 0
	abs.x.max <- 100
	abs.y.min <- 0
	abs.y.max <- 100
	percent <- as.numeric(svalue(h$obj)) / 100
	if ( percent <= 0 ) percent <- 1
	x <- h$action$plist$x
	y <- h$action$plist$y
	x.min <- x + ((abs.x.min - x) / percent)
	x.max <- x + ((abs.x.max - x) / percent)
	y.min <- y + ((abs.y.min - y) / percent)
	y.max <- y + ((abs.y.max - y) / percent)
	elt <- h$action$findParent("iViewGroup")
	if ( elt$plist$img.zoom.linked ) {
		elt <- elt$findParent("iViewTab")
		elt$update(x.min=x.min, x.max=x.max,
			y.min=y.min, y.max=y.max,
			with.properties=c(img.zoom.linked=TRUE))
	} else {
		elt$update(x.min=x.min, x.max=x.max,
			y.min=y.min, y.max=y.max)
	}
}

.changed.img.zoom.linked <- function(h, ...) {
	img.zoom.linked <- as.logical(svalue(h$obj))
	elt <- h$action$findParent("iViewGroup")
	elt$update(img.zoom.linked=img.zoom.linked)
}

.changed.pixel <- function(h, ...) {
	# NEED TO FIX TO CHANGE X AND Y AT THE SAME TIME
	pixel <- as.integer(svalue(h$obj))
	elt <- h$action$findParent("iViewGroup")
	if ( elt$plist$pixel.linked ) {
		elt <- elt$findParent("iViewTab")
		elt$update(pixel=pixel,
			with.properties=c(pixel.linked=TRUE))
	} else {
		elt$update(pixel=pixel)
	}
}

.changed.pixel.linked <- function(h, ...) {
	value <- as.logical(svalue(h$obj))
	elt <- h$action$findParent("iViewGroup")
	elt$update(pixel.linked=value)
}

.changed.x <- function(h, ...) {
	# NEED TO FIX TO CHANGE PIXEL AT THE SAME TIME!
	x <- as.numeric(svalue(h$obj))
	elt <- h$action$findParent("iViewGroup")
	if ( elt$plist$pixel.linked ) {
		elt <- elt$findParent("iViewTab")
		elt$update(x=x,
			with.properties=c(pixel.linked=TRUE))
	} else {
		elt$update(x=x)
	}
}

.changed.y <- function(h, ...) {
	# NEED TO FIX TO CHANGE PIXEL AT THE SAME TIME!
	y <- as.numeric(svalue(h$obj))
	elt <- h$action$findParent("iViewGroup")
	if ( elt$plist$pixel.linked ) {
		elt <- elt$findParent("iViewTab")
		elt$update(y=y,
			with.properties=c(pixel.linked=TRUE))
	} else {
		elt$update(y=y)
	}
}

.changed.img.intensity.zoom <- function(h, ...) {
	abs.img.intensity.min <- 0
	abs.img.intensity.max <- 100
	percent <- as.numeric(svalue(h$obj)) / 100
	if ( percent <= 0 ) percent <- 0.05
	img.intensity.min <- abs.img.intensity.min
	img.intensity.max <- abs.img.intensity.min + 
		((abs.img.intensity.max  - abs.img.intensity.min) / percent)
	elt <- h$action$findParent("iViewGroup")
	if ( elt$plist$img.intensity.zoom.linked ) {
		elt <- elt$findParent("iViewTab")
		elt$update(img.intensity.min=img.intensity.min,
			img.intensity.max=img.intensity.max,
			with.properties=c(img.intensity.zoom.linked=TRUE))
	} else {
		elt$update(img.intensity.min=img.intensity.min,
			img.intensity.max=img.intensity.max)
	}
}

.changed.img.intensity.zoom.linked <- function(h, ...) {
	img.intensity.zoom.linked <- as.logical(svalue(h$obj))
	elt <- h$action$findParent("iViewGroup")
	elt$update(img.intensity.zoom.linked=img.intensity.zoom.linked)
}

.changed.img.intensity.min <- function(h, ...) {
	img.intensity.zoom.min <- round(as.numeric(svalue(h$obj)), digits=4)
	elt <- h$action$findParent("iViewGroup")
	if ( elt$plist$img.zoom.linked ) {
		elt <- elt$findParent("iViewTab")
		elt$update(img.intensity.zoom.min=img.intensity.zoom.min,
			with.properties=c(img.zoom.linked=TRUE))
	} else {
		elt$update(img.intensity.zoom.min=img.intensity.zoom.min)
	}
}

.changed.img.intensity.max <- function(h, ...) {
	img.intensity.zoom.max <- round(as.numeric(svalue(h$obj)), digits=4)
	elt <- h$action$findParent("iViewGroup")
	if ( elt$plist$img.zoom.linked ) {
		elt <- elt$findParent("iViewTab")
		elt$update(img.intensity.zoom.max=img.intensity.zoom.max,
			with.properties=c(img.zoom.linked=TRUE))
	} else {
		elt$update(img.intensity.zoom.max=img.intensity.zoom.max)
	}
}
