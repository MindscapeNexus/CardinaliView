
#### functions for Gtk+ and Cairo devices ####

newGtkDevice <- function(instance, windowname, devicename, width=7, height=7, dpi=72) {
	if ( getOption("guiToolkit") == "RGtk2" ) {
		require(RGtk2)
		require(cairoDevice)
	}
	win <- gtkWindow(show=FALSE)
	win["title"] <- paste(instance$name, instance$windows[[windowname]]$devicetitles[[devicename]],
			sep=" - ")
	win$setDefaultSize(dpi * width, dpi * height)
	draw <- gtkDrawingArea()
	draw$addEvents(GdkEventMask["button-press-mask"]
		+ GdkEventMask["button-release-mask"]
		+ GdkEventMask["key-press-mask"]
		+ GdkEventMask["key-release-mask"])
	clickSignal <- gSignalConnect(draw, "button-press-event", gtkClickOrDrag,
		data=list(instance=instance, windowname=windowname, devicename=devicename))
	asCairoDevice(draw)
	win$add(draw)
	tryDevicePlacement(instance, win, devicename)
	win$show()
	gdkWindowProcessAllUpdates()
	while (gtkEventsPending()) gtkMainIterationDo(blocking=FALSE)
	instance$windows[[windowname]]$gtkWidgets[[devicename]] <- win
	dev.cur()
}
