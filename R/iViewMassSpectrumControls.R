
#### Class for mass spectrum controls ####
## ---------------------------------------
.iViewMassSpectrumControls <- setRefClass("iViewMassSpectrumControls",
	fields = c(interface = "gGroup"),
	contains = "iViewGroup",
	methods = list(
		initialize = function(..., horizontal=FALSE, expand=TRUE) {
			callSuper(..., horizontal=horizontal, expand=expand)
			# properties list
			plist$ms.zoom <<- numeric(1)
			plist$ms.zoom.linked <<- logical(1)
			plist$feature <<- numeric(1)
			plist$feature.linked <<- logical(1)
			plist$mz <<- numeric(1)
			plist$mz.plusminus <<- numeric(1)
			plist$mz.min <<- numeric(1)
			plist$mz.max <<- numeric(1)
			plist$ms.intensity.zoom <<- numeric(1)
			plist$ms.intensity.zoom.linked <<- logical(1)
			plist$ms.intensity.min <<- numeric(1)
			plist$ms.intensity.max <<- numeric(1)
			## mass spectrum m/z controls
			widgets$ms.frame <<- gframe(
				container=interface,
				horizontal=FALSE,
				expand=FALSE,
				text="Mass Spectrum")
			# m/z - zoom
			widgets$ms.zoom.group <<- ggroup(
				container=widgets$ms.frame,
				expand=FALSE)
			widgets$ms.zoom.label <<- glabel(
				container=widgets$ms.zoom.group,
				text="zoom")
			widgets$ms.zoom <<- gspinbutton(
				container=widgets$ms.zoom.group,
				expand=TRUE,
				from=100,
				to=1000,
				by=25)
			widgets$ms.zoomprcnt.label <<- glabel(
				container=widgets$ms.zoom.group,
				text="%")
			widgets$ms.zoom.linked <<- gcheckbox(
				container=widgets$ms.zoom.group,
				text="")
			# m/z - feature
			widgets$feature.group <<- ggroup(
				container=widgets$ms.frame,
				expand=FALSE)
			widgets$feature.label <<- glabel(
				container=widgets$feature.group,
				text="feature")
			widgets$feature <<- gspinbutton(
				container=widgets$feature.group,
				expand=TRUE,
				from=1,
				to=2^31-1,
				by=1)
			widgets$feature.linked <<- gcheckbox(
				container=widgets$feature.group,
				text="")
			widgets$mz.group <<- ggroup(
				container=widgets$ms.frame,
				expand=FALSE)
			widgets$mz.label <<- glabel(
				container=widgets$mz.group,
				text="m/z")
			widgets$mz <<- gedit(
				container=widgets$mz.group,
				expand=TRUE,
				width=10)
			widgets$mz.plusminus.group <<- ggroup(
				container=widgets$ms.frame,
				expand=FALSE)
			widgets$mz.plusminus.label <<- glabel(
				container=widgets$mz.plusminus.group,
				text="+/-")
			widgets$mz.plusminus <<- gedit(
				container=widgets$mz.plusminus.group,
				expand=TRUE,
				width=5)
			widgets$mz.plusminus.kind <<- gcombobox(
				container=widgets$mz.plusminus.group,
				expand=FALSE,
				items=c("Da"))
			# m/z - range
			widgets$mz.range.group <<- ggroup(
				container=widgets$ms.frame,
				expand=FALSE)
			widgets$mz.min.label <<- glabel(
				container=widgets$mz.range.group,
				text="from")
			widgets$mz.min <<- gedit(
				container=widgets$mz.range.group,
				expand=TRUE,
				width=5)
			widgets$mz.max.label <<- glabel(
				container=widgets$mz.range.group,
				text="to")
			widgets$mz.max <<- gedit(
				container=widgets$mz.range.group,
				expand=TRUE,
				width=5)
			## mass spectrum intensity controls
			widgets$ms.intensity.frame <<- gframe(
				container=widgets$ms.frame,
				horizontal=FALSE,
				expand=FALSE,
				text="Intensity Range")
			# intensity - zoom
			widgets$ms.intensity.zoom.group <<- ggroup(
				container=widgets$ms.intensity.frame,
				expand=FALSE)
			widgets$ms.intensity.zoom.label <<- glabel(
				container=widgets$ms.intensity.zoom.group,
				text="zoom")
			widgets$ms.intensity.zoom <<- gspinbutton(
				container=widgets$ms.intensity.zoom.group,
				expand=TRUE,
				from=100,
				to=1000,
				by=25)
			widgets$ms.intensity.zoomprcnt.label <<- glabel(
				container=widgets$ms.intensity.zoom.group,
				text="%")
			widgets$ms.intensity.zoom.linked <<- gcheckbox(
				container=widgets$ms.intensity.zoom.group,,
				text="")
			# intensity - range
			widgets$ms.intensity.group <<- ggroup(
				container=widgets$ms.intensity.frame,
				expand=FALSE)
			widgets$ms.intensity.min.label <<- glabel(
				container=widgets$ms.intensity.group,
				text="min")
			widgets$ms.intensity.min <<- gedit(
				container=widgets$ms.intensity.group,
				expand=TRUE,
				width=5)
			widgets$ms.intensity.max.label <<- glabel(
				container=widgets$ms.intensity.group,
				text="max")
			widgets$ms.intensity.max <<- gedit(
				container=widgets$ms.intensity.group,
				expand=TRUE,
				width=5)
			# handlers
			handlers$ms.zoom <<- addHandlerChanged(
				obj=widgets$ms.zoom,
				handler=function(h, ...) {
					# do something
				}, action=.self)
			handlers$ms.zoom.linked <<- addHandlerChanged(
				obj=widgets$ms.zoom.linked,
				handler=.changed.ms.zoom.linked,
				action=.self)
			handlers$feature <<- addHandlerChanged(
				obj=widgets$feature,
				handler=.changed.feature,
				action=.self)
			handlers$feature.linked <<- addHandlerChanged(
				obj=widgets$feature.linked,
				handler=.changed.feature.linked,
				action=.self)
			handlers$mz <<- addHandlerChanged(
				obj=widgets$mz,
				handler=.changed.mz,
				action=.self)
			handlers$mz.plusminus <<- addHandlerChanged(
				obj=widgets$mz.plusminus,
				handler=.changed.mz.plusminus,
				action=.self)
			handlers$mz.min <<- addHandlerChanged(
				obj=widgets$mz.min,
				handler=.changed.mz.min,
				action=.self)
			handlers$mz.max <<- addHandlerChanged(
				obj=widgets$mz.max,
				handler=.changed.mz.max,
				action=.self)
			handlers$ms.intensity.zoom <<- addHandlerChanged(
				obj=widgets$ms.intensity.zoom,
				handler=.changed.ms.intensity.zoom,
				action=.self)
			handlers$ms.intensity.zoom.linked <<- addHandlerChanged(
				obj=widgets$ms.intensity.zoom.linked,
				handler=.changed.ms.intensity.zoom.linked,
				action=.self)
			handlers$ms.intensity.min <<- addHandlerChanged(
				obj=widgets$ms.intensity.min,
				handler=.changed.ms.intensity.min,
				action=.self)
			handlers$ms.intensity.max <<- addHandlerChanged(
				obj=widgets$ms.intensity.max,
				handler=.changed.ms.intensity.max,
				action=.self)
		},
		update = function(...) {
			dots <- list(...)
			for ( par in names(plist) ) {
				if ( par %in% names(dots) ) {
					plist[[par]] <<- dots[[par]]
					blockHandler(widgets[[par]], handlers[[par]])
					svalue(widgets[[par]]) <<- dots[[par]]
					unblockHandler(widgets[[par]], handlers[[par]])
				}
			}
			callSuper(...)
		}))

.changed.ms.zoom <- function(h, ...) {
	print("stub")
}

.changed.ms.zoom.linked <- function(h, ...) {
	ms.zoom.linked <- as.logical(svalue(h$obj))
	elt <- h$action$findParent("iViewMSImageSet")
	elt$update(ms.zoom.linked=ms.zoom.linked)
}

.changed.feature <- function(h, ...) {
	# need to fix to change mz at the same time!
	feature <- as.integer(svalue(h$obj))
	elt <- h$action$findParent("iViewMSImageSet")
	if ( elt$plist$feature.linked ) {
		elt <- elt$findParent("iViewMSImageGroup")
		elt$update(feature=feature,
			with.properties=c(feature.linked=TRUE))
	} else {
		elt$update(feature=feature)
	}
}

.changed.feature.linked <- function(h, ...) {
	value <- as.logical(svalue(h$obj))
	elt <- h$action$findParent("iViewMSImageSet")
	elt$update(feature.linked=value)
}

.changed.mz <- function(h, ...) {
	# need to fix to change feature at the same time!
	mz <- as.numeric(svalue(h$obj))
	elt <- h$action$findParent("iViewMSImageSet")
	if ( elt$plist$feature.linked ) {
		elt <- elt$findParent("iViewMSImageGroup")
		elt$update(mz=mz,
			with.properties=c(feature.linked=TRUE))
	} else {
		elt$update(mz=mz)
	}
}

.changed.mz.plusminus <- function(h, ...) {
	mz.plusminus <- as.numeric(svalue(h$obj))
	elt <- h$action$findParent("iViewMSImageSet")
	elt$update(mz.plusminus=mz.plusminus)
}

.changed.mz.min <- function(h, ...) {
	mz.min <- as.numeric(svalue(h$obj))
	elt <- h$action$findParent("iViewMSImageSet")
	if ( elt$plist$ms.zoom.linked ) {
		elt <- elt$findParent("iViewMSImageGroup")
		elt$update(mz.min=mz.min,
			with.properties=c(ms.zoom.linked=TRUE))
	} else {
		elt$update(mz.min=mz.min)
	}
}

.changed.mz.max <- function(h, ...) {
	mz.max <- as.numeric(svalue(h$obj))
	elt <- h$action$findParent("iViewMSImageSet")
	if ( elt$plist$ms.zoom.linked ) {
		elt <- elt$findParent("iViewMSImageGroup")
		elt$update(mz.max=mz.max,
			with.properties=c(ms.zoom.linked=TRUE))
	} else {
		elt$update(mz.max=mz.max)
	}
}

.changed.ms.intensity.zoom <- function(h, ...) {
	print("stub")
}

.changed.ms.intensity.zoom.linked <- function(h, ...) {
	ms.intensity.zoom.linked <- as.logical(svalue(h$obj))
	elt <- h$action$findParent("iViewMSImageSet")
	elt$update(ms.intensity.zoom.linked=ms.intensity.zoom.linked)
}

.changed.ms.intensity.zoom.min <- function(h, ...) {
	ms.intensity.zoom.min <- as.numeric(svalue(h$obj))
	elt <- h$action$findParent("iViewMSImageSet")
	if ( elt$plist$ms.zoom.linked ) {
		elt <- elt$findParent("iViewMSImageGroup")
		elt$update(ms.intensity.zoom.min=ms.intensity.zoom.min,
			with.properties=c(ms.zoom.linked=TRUE))
	} else {
		elt$update(ms.intensity.zoom.min=ms.intensity.zoom.min)
	}
}

.changed.ms.intensity.zoom.max <- function(h, ...) {
	ms.intensity.zoom.max <- as.numeric(svalue(h$obj))
	elt <- h$action$findParent("iViewMSImageSet")
	if ( elt$plist$ms.zoom.linked ) {
		elt <- elt$findParent("iViewMSImageGroup")
		elt$update(ms.intensity.zoom.max=ms.intensity.zoom.max,
			with.properties=c(ms.zoom.linked=TRUE))
	} else {
		elt$update(ms.intensity.zoom.max=ms.intensity.zoom.max)
	}
}
