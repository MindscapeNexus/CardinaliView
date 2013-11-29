
setClassUnion("PList", c("list", "environment"))

#### Class for a GUI interface element
## -----------------------------------
setClass("iWidget",
	slots = c(
		interface = "list", # list of other iWidgets
		members = "list", # list of GUI toolkit widgets
		plist = "PList"), # widget preferences and settings
	contains = "VIRTUAL",
	prototype = prototype(
		interface = list(),
		members = list(),
		plist = list()))


