
#### set up the CardinaliView environment ####

.CardinaliView <- new.env()

.onLoad <- function(libname, pkgname) {
	options(guiToolkit="RGtk2")
}

#### end setup ####
