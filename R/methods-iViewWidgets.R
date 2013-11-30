
setMethod("initialize", "iViewWidget",
	function(.Object,
			interface = list(),
			...) {
		.Object@interface <- interface
		callNextMethod(.Object,
			...)
})

