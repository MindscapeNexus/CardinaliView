
#### functions for updating user on progress ####

iViewMessage <- function(..., precedes.progress.output=TRUE) {
	if ( precedes.progress.output ) {
		.guiState$message <- paste(..., sep="")
	} else {
		if ( is.null(.guiState$isFrozen) || !.guiState$isFrozen ) return()
		if ( !is.null(.guiState$messageWindow) && isExtant(.guiState$messageWindow) ) {
			delete(.guiState$messageWindow, .guiState$messageText)
		} else {
			.guiState$messageWindow <- gwindow(title="Working...",
				show=FALSE, width=300, height=120)
		}
		.guiState$messageText <- glabel(text=paste(..., sep=""),
			container=.guiState$messageWindow, expand=TRUE)
		visible(.guiState$messageWindow) <- TRUE
		if ( getOption("guiToolkit") == "tcltk" ) {
			tcl("update")
			Sys.sleep(0.1)
		} else if ( getOption("guiToolkit") == "RGtk2" ) {
			gdkWindowProcessAllUpdates()
			while (gtkEventsPending()) gtkMainIterationDo(blocking=FALSE)
		}
	}
}

tkProgressStart <- function() {
	if ( is.null(.guiState$isFrozen) || !.guiState$isFrozen ) return()
	if ( is.null(.guiState$progress) && Cardinal:::.verboseState$total > 1 ) {
		if ( !is.null(.guiState$messageWindow) && isExtant(.guiState$messageWindow) ) {
			dispose(.guiState$messageWindow)
		}
		if ( is.null(.guiState$message) ) .guiState$message <- ""
		.guiState$progress <- tkProgressBar(title="Working...", min=0,
			max=Cardinal:::.verboseState$total, label=.guiState$message)
		.guiState$updateAt <- floor(seq(from=1, to=Cardinal:::.verboseState$total,
			length.out=min(Cardinal:::.verboseState$total, 100)))
	}
}

tkProgressStop <- function() {
	if ( is.null(.guiState$isFrozen) || !.guiState$isFrozen ) return()
	if ( !is.null(.guiState$progress) ) {
		close(.guiState$progress)
		.guiState$message <- ""
		.guiState$progress <- NULL
	}
}

tkProgressIncrement <- function() {
	if ( is.null(.guiState$isFrozen) || !.guiState$isFrozen ) return()
	if ( !is.null(.guiState$progress) ) {
		if ( Cardinal:::.verboseState$counter %in% .guiState$updateAt ) {
			setTkProgressBar(.guiState$progress,
				value=Cardinal:::.verboseState$counter,
				label=.guiState$message)
		}
	}
}

gtkProgressStart <- function() {
	if ( is.null(.guiState$isFrozen) || !.guiState$isFrozen ) return()
	if ( is.null(.guiState$progress) && Cardinal:::.verboseState$total > 1 ) {
		if ( !is.null(.guiState$messageWindow) && isExtant(.guiState$messageWindow) ) {
			dispose(.guiState$messageWindow)
		}
		if ( is.null(.guiState$message) ) .guiState$message <- ""
		progress <- list()
		progress$win <- gwindow(title="Working...", width=300, height=80, visible=FALSE)
		progress$group <- ggroup(container=progress$win, horizontal=FALSE, expand=TRUE)
		progress$label$group <- ggroup(container=progress$group, expand=TRUE)
		progress$label$label <- glabel(text=.guiState$message,
			container=progress$label$group, expand=TRUE)
		progress$bar$group <- ggroup(container=progress$group, expand=TRUE)
		progress$bar$bar <- gtkProgressBar()
		getToolkitWidget(progress$bar$group)$add(progress$bar$bar)
		visible(progress$win) <- TRUE
		.guiState$progress <- progress
		.guiState$updateAt <- floor(seq(from=1, to=Cardinal:::.verboseState$total,
			length.out=min(Cardinal:::.verboseState$total, 100)))
		gdkWindowProcessAllUpdates()
		while (gtkEventsPending()) gtkMainIterationDo(blocking=FALSE)
	}
}

gtkProgressStop <- function() {
	if ( is.null(.guiState$isFrozen) || !.guiState$isFrozen ) return()
	if ( !is.null(.guiState$progress) && isExtant(.guiState$progress$win) ) {
		dispose(.guiState$progress$win)
		.guiState$message <- ""
		.guiState$progress <- NULL
	}	
}

gtkProgressIncrement <- function() {
	if ( is.null(.guiState$isFrozen) || !.guiState$isFrozen ) return()
	if ( !is.null(.guiState$progress) && isExtant(.guiState$progress$win) ) {
		if ( Cardinal:::.verboseState$counter %in% .guiState$updateAt ) {
			if ( .guiState$message != svalue(.guiState$progress$label$label) ) {
				delete(.guiState$progress$label$group, .guiState$progress$label$label)
				.guiState$progress$label$label <- glabel(text=.guiState$message,
					container=.guiState$progress$label$group)
			}
			frac <- Cardinal:::.verboseState$counter / Cardinal:::.verboseState$total
			.guiState$progress$bar$bar$setFraction(frac)
			gdkWindowProcessAllUpdates()
			while (gtkEventsPending()) gtkMainIterationDo(blocking=FALSE)
		}
	}
}


