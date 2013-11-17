
#### set up the GUI environments ####

.guiState <- new.env()
.userEnvironment <- globalenv()

.onLoad <- function(libname, pkgname) {
	options(device.ask.default=FALSE)
	options(Cardinal.track.progress=TRUE)
}

#### end setup ####
