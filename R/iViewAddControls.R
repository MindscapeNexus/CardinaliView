
#### Class adding or closing a dataset view ####
## -------------------------------------
.iViewAddControls <- setRefClass("iViewAddControls",
	contains = "iViewGroup",
	methods = list(
		initialize = function(...) {
			callSuper(...)
			widgets$close <<- gbutton(
				container=interface,
				expand=TRUE,
				border=FALSE,
				text="x")
			widgets$minimize <<- gbutton(
				container=interface,
				expand=TRUE,
				border=FALSE,
				text="-")
			widgets$close <<- gbutton(
				container=interface,
				expand=TRUE,
				border=FALSE,
				text="+")

		}))


