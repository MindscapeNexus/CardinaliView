
#### functions for setting CardinalView devices ####

setDevice <- function(instance, windowname, devicename) {
	which <- instance$windows[[windowname]]$devices[[devicename]]
	if ( which %in% dev.list() ) {
		dev.set(which)
	} else {
		newDevice(instance, windowname, devicename)	
	}
}

newDevice <- function(instance, windowname, devicename) {
	if ( getOption("guiToolkit") == "RGtk2" ) {
		newGtkDevice(instance, windowname, devicename, width=7, height=7)
	} else {
		dev.new(title=paste(instance$name, instance$windows[[windowname]]$devicetitles[[devicename]],
			sep=" - "), width=5, height=5)
	}
	instance$windows[[windowname]]$devices[[devicename]] <- dev.cur()
	dev.cur()
}

updateDeviceList <- function(instance) {
	if ( isExtant(instance$windows$main$topWidgets$plotControlWindow) ) {
		delete(instance$windows$main$widgets$plotDeviceFrame,
			instance$windows$main$widgets$plotActiveDevice)
		instance$windows$main$widgets$plotActiveDevice <- gcombobox(items=activeDevices(instance, "plot"),
			container=instance$windows$main$widgets$plotDeviceFrame, expand=TRUE)
	} else {
		makePlotControls(instance)
	}
	if ( isExtant(instance$windows$main$topWidgets$imageControlWindow) ) {
		delete(instance$windows$main$widgets$imageDeviceFrame,
			instance$windows$main$widgets$imageActiveDevice)
		instance$windows$main$widgets$imageActiveDevice <- gcombobox(items=activeDevices(instance, "image"),
			container=instance$windows$main$widgets$imageDeviceFrame, expand=TRUE)
	} else {
		makeImageControls(instance)
	}
}

activeDevices <- function(instance, type=c("plot", "image")) {
	type <- match.arg(type)
	activeWindows <- sapply(instance$windows, function(w) w$open, USE.NAMES=TRUE)
	devices <- sapply(instance$windows[activeWindows], function(w) {
		any(grepl(pattern=type, x=names(w$devices)))
	}, USE.NAMES=TRUE)
	names(devices[devices])
}

