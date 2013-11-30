
setMethod("initialize", "iView",
	function(.Object,
			plist = new.env(parent=emptyenv()),
			parent = emptyenv(),
			...) {
		.Object@.xData <- new.env(parent=parent)
		.Object@plist <- plist
		.Object
})

setMethod("pData", "iView", function(object) object@plist)

setReplaceMethod("pData",
	signature = c("iView", "list"), 
	function(object, value) {
		names <- ls(object@plist)[ls(object@plist) %in% names(value)]
		for ( nm in names ) object@plist[[nm]] <- value[[nm]]
		object
	})

setMethod("isDirty", "iView", function(object, ...) {
	dots <- match.call(expand.dots=FALSE)[["..."]]
	any(ls(pData(object)) %in% names(dots))
})

setMethod("updateView", "iView", function(object, ...) {
	if ( isDirty(object, ...) ) {
		pData(object) <- list(...)
		names <- ls(object)
		for ( nm in names )
			object[[nm]] <- updateView(object[[nm]], ...)
	}
	object
})
