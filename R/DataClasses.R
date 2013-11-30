
#### Class for iView GUI element ####
## defined as an environment containing iView elements
## ---------------------------------------------------
setClass("iView",
	slots = c(
		parent = "environment", # enclosing element
		plist = "environment"), # user preferences
	contains = c("environment", "VIRTUAL"),
	prototype = prototype(
		parent = emptyenv(),
		plist = new.env(parent=emptyenv())))

#### Virtual class for a GUI interface elements ####
## a widget or family of widgets in an iView GUI interface
## -------------------------------------------------------
setClass("iViewWidget",
	slots = c(
		parent = "iView", # explicitly another iView element
		interface = "list"), # list of GUI toolkit widgets
	contains = c("iView", "VIRTUAL"),
	prototype = prototype(
		interface = list()))

#### Class for a GUI for an MSImageSet ####
## -----------------------------------
setClass("iViewMSImageSet", contains = "iView")

