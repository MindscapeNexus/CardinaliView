
#### Class for iView GUI element ####
## defined as an environment containing iView elements
## ---------------------------------------------------
setClass("iView",
	slots = c(
		plist = "environment"), # user preferences
	contains = c("environment", "VIRTUAL"),
	prototype = prototype(
		plist = new.env(parent=emptyenv())))

#### Virtual class for a GUI interface elements ####
## a widget or family of widgets in an iView GUI interface
## -------------------------------------------------------
setClass("iViewWidget",
	slots = c(
		interface = "list"), # list of GUI toolkit widgets
	contains = c("iView", "VIRTUAL"),
	prototype = prototype(
		interface = list()))

