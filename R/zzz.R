
#### set up the GUI environments ####

.guiState <- new.env()
.userState <- globalenv()

.onLoad <- function(libname, pkgname) {
	options(guiToolkit="RGtk2")
	options(device.ask.default=FALSE)
	options(Cardinal.track.progress=TRUE)
}

#### end setup ####
