
#### Class for mass spectrum controls ####
## ---------------------------------------
.iViewMassSpectrumControls <- setRefClass("iViewMassSpectrumControls",
	contains = "iViewControls",
	methods = list(
		initialize = function(..., horizontal=FALSE, expand=TRUE) {
			uuid <<- Cardinal:::uuid()
			interface <<- gexpandgroup(...,
				horizontal=horizontal,
				expand=expand,
				text="Mass Spectrum")
			# properties list
			plist$dataset <<- character(1)
			plist$ms.zoom <<- numeric(1)
			# plist$ms.zoom.linked <<- logical(1)
			plist$pixel <<- numeric(1)
			plist$feature <<- numeric(1)
			plist$feature.linked <<- logical(1)
			plist$mz <<- numeric(1)
			plist$mz.plusminus <<- numeric(1)
			plist$mz.min <<- numeric(1)
			plist$mz.max <<- numeric(1)
			plist$ms.intensity.zoom <<- numeric(1)
			# plist$ms.intensity.zoom.linked <<- logical(1)
			plist$ms.intensity.min <<- numeric(1)
			plist$ms.intensity.max <<- numeric(1)
			# m/z - zoom
			widgets$ms.zoom.group <<- ggroup(
				container=interface,
				expand=FALSE)
			widgets$ms.zoom.label <<- glabel(
				container=widgets$ms.zoom.group,
				text="zoom")
			widgets$ms.zoom <<- gspinbutton(
				container=widgets$ms.zoom.group,
				expand=TRUE,
				from=100,
				to=10^9,
				by=25)
			widgets$ms.zoomprcnt.label <<- glabel(
				container=widgets$ms.zoom.group,
				text="%")
			# m/z - feature
			widgets$feature.group <<- ggroup(
				container=interface,
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
				container=interface,
				expand=FALSE)
			widgets$mz.label <<- glabel(
				container=widgets$mz.group,
				text="m/z")
			widgets$mz <<- gedit(
				container=widgets$mz.group,
				expand=TRUE,
				width=10)
			# widgets$mz.plusminus.group <<- ggroup(
			# 	container=interface,
			# 	expand=FALSE)
			# widgets$mz.plusminus.label <<- glabel(
			# 	container=widgets$mz.plusminus.group,
			# 	text="+/-")
			# widgets$mz.plusminus <<- gedit(
			# 	container=widgets$mz.plusminus.group,
			# 	expand=TRUE,
			# 	width=5)
			# widgets$mz.plusminus.kind <<- gcombobox(
			# 	container=widgets$mz.plusminus.group,
			# 	expand=FALSE,
			# 	items=c("Da"))
			# m/z - range
			widgets$mz.range.group <<- ggroup(
				container=interface,
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
				container=interface,
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
				from=0,
				to=10^10,
				by=5)
			widgets$ms.intensity.zoomprcnt.label <<- glabel(
				container=widgets$ms.intensity.zoom.group,
				text="%")
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
				handler=.changed.ms.zoom,
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
			# handlers$mz.plusminus <<- addHandlerChanged(
			# 	obj=widgets$mz.plusminus,
			# 	handler=.changed.mz.plusminus,
			# 	action=.self)
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
			callSuper(...)
			dots <- list(...)
			if ( any(c("mz.min", "mz.max") %in% names(dots)) ) {
				object <- get(plist$dataset, envir=globalenv())
				abs.mz.min <- min(mz(object))
				abs.mz.max <- max(mz(object))
				ms.zoom <- 100 * abs(
					(abs.mz.max - abs.mz.min) / 
					(plist$mz.max - plist$mz.min))
				plist$ms.zoom <<- ms.zoom
				blockHandler(widgets$ms.zoom, handlers$ms.zoom)
				svalue(widgets$ms.zoom) <<- ms.zoom
				unblockHandler(widgets$ms.zoom, handlers$ms.zoom)
			}
			if ( any(c("ms.intensity.min", "ms.intensity.max") %in% names(dots)) ) {
				object <- get(plist$dataset, envir=globalenv())
				abs.ms.intensity.min <- min(spectra(object)[,plist$pixel])
				abs.ms.intensity.max <- max(spectra(object)[,plist$pixel])
				ms.intensity.zoom <- 100 * abs(
					(abs.ms.intensity.max - abs.ms.intensity.min) / 
					(plist$ms.intensity.max - plist$ms.intensity.min))
				plist$ms.intensity.zoom <<- ms.intensity.zoom
				blockHandler(widgets$ms.intensity.zoom, handlers$ms.intensity.zoom)
				svalue(widgets$ms.intensity.zoom) <<- ms.intensity.zoom
				unblockHandler(widgets$ms.intensity.zoom, handlers$ms.intensity.zoom)
			}
		}))

.changed.ms.zoom <- function(h, ...) {
	object <- try(get(h$action$plist$dataset, envir=globalenv()), silent=TRUE)
	if ( !is(object, "MSImageSet") ) return()
	abs.mz.min <- min(mz(object))
	abs.mz.max <- max(mz(object))
	percent <- as.numeric(svalue(h$obj)) / 100
	if ( percent <= 0 ) percent <- 1
	mz <- h$action$plist$mz
	mz.min <- mz + ((abs.mz.min - mz) / percent)
	mz.max <- mz + ((abs.mz.max - mz) / percent)
	elt <- h$action$findParent("iViewGroup")
	elt$update(mz.min=mz.min, mz.max=mz.max)
}

.changed.feature <- function(h, ...) {
	object <- try(get(h$action$plist$dataset, envir=globalenv()), silent=TRUE)
	if ( !is(object, "MSImageSet") ) return()
	feature <- as.integer(svalue(h$obj))
	mz <- mz(object)[[feature]]
	.update.feature(h$action, feature=feature, mz=mz)
}

.changed.feature.linked <- function(h, ...) {
	object <- try(get(h$action$plist$dataset, envir=globalenv()), silent=TRUE)
	if ( !is(object, "MSImageSet") ) return()
	value <- as.logical(svalue(h$obj))
	elt <- h$action$findParent("iViewGroup")
	elt$update(feature.linked=value)
}

.changed.mz <- function(h, ...) {
	object <- try(get(h$action$plist$dataset, envir=globalenv()), silent=TRUE)
	if ( !is(object, "MSImageSet") ) return()
	mz <- round(as.numeric(svalue(h$obj)), digits=4)
	feature <- features(object, mz=mz)
	.update.feature(h$action, feature=feature, mz=mz)
}

.changed.mz.min <- function(h, ...) {
	object <- try(get(h$action$plist$dataset, envir=globalenv()), silent=TRUE)
	if ( !is(object, "MSImageSet") ) return()
	mz.min <- round(as.numeric(svalue(h$obj)), digits=4)
	elt <- h$action$findParent("iViewGroup")
	elt$update(mz.min=mz.min)
}

.changed.mz.max <- function(h, ...) {
	object <- try(get(h$action$plist$dataset, envir=globalenv()), silent=TRUE)
	if ( !is(object, "MSImageSet") ) return()
	mz.max <- round(as.numeric(svalue(h$obj)), digits=4)
	elt <- h$action$findParent("iViewGroup")
	elt$update(mz.max=mz.max)
}

.changed.ms.intensity.zoom <- function(h, ...) {
	object <- try(get(h$action$plist$dataset, envir=globalenv()), silent=TRUE)
	if ( !is(object, "MSImageSet") ) return()
	abs.ms.intensity.min <- min(spectra(object)[,h$action$plist$pixel])
	abs.ms.intensity.max <- max(spectra(object)[,h$action$plist$pixel])
	percent <- as.numeric(svalue(h$obj)) / 100
	if ( percent <= 0 ) percent <- 0.05
	ms.intensity.min <- abs.ms.intensity.min
	ms.intensity.max <- abs.ms.intensity.min + 
		((abs.ms.intensity.max  - abs.ms.intensity.min) / percent)
	elt <- h$action$findParent("iViewGroup")
	elt$update(ms.intensity.min=ms.intensity.min,
		ms.intensity.max=ms.intensity.max)
}

.changed.ms.intensity.min <- function(h, ...) {
	object <- try(get(h$action$plist$dataset, envir=globalenv()), silent=TRUE)
	if ( !is(object, "MSImageSet") ) return()
	ms.intensity.min <- round(as.numeric(svalue(h$obj)), digits=4)
	elt <- h$action$findParent("iViewGroup")
	elt$update(ms.intensity.min=ms.intensity.min)
}

.changed.ms.intensity.max <- function(h, ...) {
	object <- try(get(h$action$plist$dataset, envir=globalenv()), silent=TRUE)
	if ( !is(object, "MSImageSet") ) return()
	ms.intensity.max <- round(as.numeric(svalue(h$obj)), digits=4)
	elt <- h$action$findParent("iViewGroup")
	elt$update(ms.intensity.max=ms.intensity.max)
}

