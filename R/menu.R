
#### functions for the main menu ####

makeMenu <- function(instance) {
	menu <- list()
	menu$File$"Load dataset"$handler <- handlerLoadDataset(instance)
	menu$File$"Load peaks"$handler <- handlerLoadPeaks(instance)
	menu$File$"Load segmentation"$handler <- handlerLoadSegmentation(instance)
	menu$File$"Section - Save"$separator <- TRUE
	menu$File$"Save dataset to R Data file"$handler <- handlerSaveDatasetToRDataFile(instance)
	menu$File$"Save segmentation to R Data file"$handler <- handlerSaveSegmentationToRDataFile(instance)
	menu$File$"Section - Import"$separator <- TRUE
	menu$File$"Import from R Data file"$handler <- handlerImportFromRDataFile(instance)
	menu$File$"Import from imzML file"$handler <- handlerImportFromImzML(instance)
	menu$File$"Import from Analyze 7.5 file"$handler <- handlerImportFromAnalyze7.5(instance)
	menu$File$"Section - Export"$separator <- TRUE
	menu$File$"Export plots..."$handler <- handlerExportPlot(instance)
	menu$File$"Export images..."$handler <- handlerExportImage(instance)
	menu$File$"Section - Close"$separator <- TRUE
	menu$File$"Close"$handler <- handlerCloseInstance(instance)
	menu$Edit$"Refresh windows"$handler <- handlerRefreshWindows(instance)
	menu$Edit$"Section - Transform"$separator <- TRUE
	menu$Edit$"Flip horizontal"$handler <- handlerFlipHorizontal(instance)
	menu$Edit$"Flip vertical"$handler <- handlerFlipVertical(instance)
	menu$Edit$"Rotate left"$handler <- handlerRotateLeft(instance)
	menu$Edit$"Rotate right"$handler <- handlerRotateRight(instance)
	menu$Edit$"Section - Crop"$separator <- TRUE
	menu$Edit$"Crop..."$handler <- handlerCrop(instance)
	menu$Edit$"Section - ROI"$separator <- TRUE
	menu$Edit$"Select ROI..."$handler <- handlerSelectROI(instance)
	menu$Process$"Normalize to TIC"$handler <- handlerNormalizeToTIC(instance)
	menu$Process$"Section - Signal"$separator <- TRUE
	menu$Process$"Remove noise"$handler <- handlerRemoveNoise(instance)
	menu$Process$"Remove baseline"$handler <- handlerRemoveBaseline(instance)
	menu$Process$"Section - Peaks"$separator <- TRUE
	menu$Process$"Detect peaks"$handler <- handlerDetectPeaks(instance)
	menu$Process$"Select peaks"$handler <- handlerSelectPeaks(instance)
	menu$Process$"Section - Reduce"$separator <- TRUE
	menu$Process$"Bin..."$handler <- handlerBin(instance)
	menu$Process$"Resample..."$handler <- handlerResample(instance)
	menu$Analyze$"PCA"$handler <- handlerPCA(instance)
	# menu$File$"Section - PLS"$separator <- TRUE
	# menu$Analyze$"PLS-DA"$handler <- function(h, ...) print("stub")
	# menu$Analyze$"OPLS-DA"$handler <- function(h, ...) print("stub")
	menu$Analyze$"Section - Compare"$separator <- TRUE
	menu$Analyze$"Compare ROI"$handler <- handlerCompareROI(instance)
	menu$Analyze$"Section - Segmentation"$separator <- TRUE
	menu$Analyze$"Spatial classify..."$handler <- handlerSpatialClassify(instance)
	menu$Analyze$"Spatial cluster..."$handler <- handlerSpatialCluster(instance)
	menu
}

#### File menu ####

handlerLoadDataset <- function(instance) {
	function(h, ...) {
		killFileDialogs(instance)
		makeLoadDatasetControl(instance)
	}
}

handlerLoadPeaks <- function(instance) {
	function(h, ...) {
		object <- try(get(instance$objectname, envir=instance$objectenv), silent=TRUE)
		if ( inherits(object, "try-error") ) return()
		if ( isPeaks(object) ) {
			if ( instance$windows$peaks$open ) {
				focus(instance$windows$peaks$topWidgets$controlWindow) <- TRUE
			} else {
				makePeaksControl(instance)
			}
		} else {
			killFileDialogs(instance)
			makeLoadPeaksControl(instance)
		}
	}
}

handlerLoadSegmentation <- function(instance) {
	function(h, ...) {
		killFileDialogs(instance)
		makeLoadSegmentationControl(instance)
	}
}

handlerSaveDatasetToRDataFile <- function(instance) {
	function(h, ...) {
		handleSaveDatasetToRDataFile(h=list(action=instance))
	}
}

handlerSaveSegmentationToRDataFile <- function(instance) {
	function(h, ...) {
		handleSaveSegmentationToRDataFile(h=list(action=instance))
	}
}

handlerImportFromRDataFile <- function(instance) {
	function(h, ...) {
		handleImportFromRDataFile(h=list(action=instance))
	}
}

handlerImportFromImzML <- function(instance) {
	function(h, ...) {
		killFileDialogs(instance)
		makeImportFromImzMLControl(instance)
	}
}

handlerImportFromAnalyze7.5 <- function(instance) {
	function(h, ...) {
		killFileDialogs(instance)
		makeImportFromAnalyze7.5Control(instance)
	}
}

handlerExportPlot <- function(instance) {
	function(h, ...) {
		killFileDialogs(instance)
		makeExportPlotControl(instance)
	}
}

handlerExportImage <- function(instance) {
	function(h, ...) {
		killFileDialogs(instance)
		makeExportImageControl(instance)
	}
}

handlerCloseInstance <- function(instance) {
	function(h, ...) {
		killInstance(instance)
	}
}

#### Edit menu ####

handlerRefreshWindows <- function(instance) {
	function(h, ...) {
		updateAll(instance)
	}
}

handlerFlipHorizontal <- function(instance) {
	function(h, ...) {
		handleFlipHorizontal(h=list(action=instance))
	}
}

handlerFlipVertical <- function(instance) {
	function(h, ...) {
		handleFlipVertical(h=list(action=instance))
	}
}

handlerRotateLeft <- function(instance) {
	function(h, ...) {
		handleRotateLeft(h=list(action=instance))
	}
}

handlerRotateRight <- function(instance) {
	function(h, ...) {
		handleRotateRight(h=list(action=instance))
	}
}

handlerCrop <- function(instance) {
	function(h, ...) {
		if ( instance$windows$crop$open ) {
			focus(instance$windows$crop$topWidgets$dialogWindow) <- TRUE
		} else {
			makeCropControl(instance)
			instance$windows$crop$open <- TRUE
		}
	}
}

handlerSelectROI <- function(instance) {
	function(h, ...) {
		if ( instance$windows$roi$open ) {
			focus(instance$windows$roi$topWidgets$dialogWindow) <- TRUE
		} else {
			makeROIControl(instance)
			instance$windows$roi$open <- TRUE
		}
	}
}

#### Process menu ####

handlerNormalizeToTIC <- function(instance) {
	function(h, ...) {
		handleNormalizeToTIC(h=list(action=instance))
	}
}

handlerRemoveNoise <- function(instance) {
	function(h, ...) {
		if ( !instance$windows$removeNoise$open ) {
			window <- makeRemoveNoiseControl(instance)
			instance$windows$removeNoise$open <- TRUE
			updateProcessingPlots(instance)
		}
		focus(instance$windows$removeNoise$topWidgets$dialogWindow) <- TRUE
	}
}

handlerRemoveBaseline <- function(instance) {
	function(h, ...) {
		if ( !instance$windows$removeBaseline$open ) {
			window <- makeRemoveBaselineControl(instance)
			instance$windows$removeBaseline$open <- TRUE
			updateProcessingPlots(instance)
		}
		focus(instance$windows$removeBaseline$topWidgets$dialogWindow) <- TRUE
	}
}

handlerDetectPeaks <- function(instance) {
	function(h, ...) {
		if ( !instance$windows$detectPeaks$open ) {
			object <- try(get(instance$objectname, envir=instance$objectenv), silent=TRUE)
			if ( inherits(object, "try-error") ) {
				gmessage(message=paste("Could not find object '", instance$objectname, "'.", sep=""), title="Error")
				return()
			}
			freezeGUI(TRUE)
			iViewMessage("Generating summary mass spectra...")
			instance$windows$detectPeaks$object <- spectralApply(object, 1, function(x) {
				c(x[[1]], mean(x), max(x))
			} )
			freezeGUI(FALSE)
			window <- makeDetectPeaksControl(instance)
			instance$windows$detectPeaks$open <- TRUE
			updateProcessingPlots(instance)
		}
		focus(instance$windows$detectPeaks$topWidgets$dialogWindow) <- TRUE
	}
}

handlerSelectPeaks <- function(instance) {
	function(h, ...) {
		if ( !instance$windows$selectPeaks$open ) {
			object <- try(get(instance$objectname, envir=instance$objectenv), silent=TRUE)
			if ( inherits(object, "try-error") ) {
				gmessage(message=paste("Could not find object '", instance$objectname, "'.", sep=""), title="Error")
				return()
			}
			freezeGUI(TRUE)
			iViewMessage("Generating summary mass spectra...")
			instance$windows$selectPeaks$object <- spectralApply(object, 1, function(x) {
				c(x[[1]], mean(x), max(x))
			} )
			freezeGUI(FALSE)
			window <- makeSelectPeaksControl(instance)
			instance$windows$selectPeaks$open <- TRUE
			updateProcessingPlots(instance)
		}
		focus(instance$windows$selectPeaks$topWidgets$dialogWindow) <- TRUE
	}
}

handlerBin <- function(instance) {
	function(h, ...) {
		if ( !instance$windows$bin$open ) {
			window <- makeBinControl(instance)
			instance$windows$bin$open <- TRUE
			updateProcessingPlots(instance)
		}
		focus(instance$windows$bin$topWidgets$dialogWindow) <- TRUE
	}
}

handlerResample <- function(instance) {
	function(h, ...) {
		if ( !instance$windows$resample$open ) {
			makeResampleControl(instance)
			instance$windows$resample$open <- TRUE
			updateProcessingPlots(instance)
		}
		focus(instance$windows$resample$topWidgets$dialogWindow) <- TRUE
	}
}

#### Analyze menu ####

handlerPCA <- function(instance) {
	function(h, ...) {
		if ( is.null(instance$windows$PCA$topWidgets$dialogWindow) || !isExtant(instance$windows$PCA$topWidgets$dialogWindow) ) {
			makePCADialog(instance)
		} else {
			focus(instance$windows$PCA$topWidgets$dialogWindow) <- TRUE
		}
	}
}

handlerCompareROI <- function(instance) {
	function(h, ...) {
		if ( !is.null(instance$windows$MSImageSegmentation$topWidgets$dialogWindow) && isExtant(instance$windows$MSImageSegmentation$topWidgets$dialogWindow) ) {
			dispose(instance$windows$MSImageSegmentation$topWidgets$dialogWindow)
		}
		makeCompareROIDialog(instance)
	}
}

handlerSpatialClassify <- function(instance) {
	function(h, ...) {
		object <- try(get(instance$objectname, envir=instance$objectenv), silent=TRUE)
		if ( inherits(object, "try-error") ) {
			gmessage(message=paste("Could not find object '", instance$objectname, "'.", sep=""), title="Error")
			return()
		}
		if ( !is.null(instance$windows$MSImageSegmentation$topWidgets$dialogWindow) && isExtant(instance$windows$MSImageSegmentation$topWidgets$dialogWindow) ) {
			dispose(instance$windows$MSImageSegmentation$topWidgets$dialogWindow)
		}
		makeSpatialClassifyDialog(instance)
		if ( !(isBinned(object) || isResampled(object)) ) {
			galert(message="It is strongly recommended to bin or resample the data before classification.", title="Error")
		}
	}
}

handlerSpatialCluster <- function(instance) {
	function(h, ...) {
		object <- try(get(instance$objectname, envir=instance$objectenv), silent=TRUE)
		if ( inherits(object, "try-error") ) {
			gmessage(message=paste("Could not find object '", instance$objectname, "'.", sep=""), title="Error")
			return()
		}
		if ( !is.null(instance$windows$MSImageSegmentation$topWidgets$dialogWindow) && isExtant(instance$windows$MSImageSegmentation$topWidgets$dialogWindow) ) {
			dispose(instance$windows$MSImageSegmentation$topWidgets$dialogWindow)
		}
		makeSpatialClusterDialog(instance)
		if ( !(isBinned(object) || isResampled(object)) ) {
			galert(message="It is strongly recommended to bin or resample the data before clustering.", title="Error")
		}
	}
}

