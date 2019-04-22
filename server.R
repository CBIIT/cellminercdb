library(shiny)
#library(d3heatmap)
library(rcellminer)
library(rcellminerElasticNet)
library(geneSetPathwayAnalysis)
library(jsonlite)
library(stringr)
library(glmnet)
library(ggplot2)
library(plotly)
##library(xlsx)
library(shinycssloaders)
library(gplots)
library(heatmaply)
library(memoise)
## library(shinyHeatmaply)
#library(dplyr)
#library(svglite)
#library(clusterProfiler)

#library(tooltipsterR)

if (!require(rcellminerUtilsCDB)){
	warning("rcellminerUtilsCDB package must be installed for full cross-database functionality.")
}


#--------------------------------------------------------------------------------------------------
# LOAD CONFIGURATION AND REQUIRED DATA SOURCE PACKAGES.
#--------------------------------------------------------------------------------------------------
config <- jsonlite::fromJSON("config.json")
appConfig <- jsonlite::fromJSON("appConfig.json")
metaConfig <- jsonlite::fromJSON("configMeta.json")

oncolor <- read.delim("oncotree1_colors.txt",row.names = 1,stringsAsFactors = F)
rownames(oncolor)=toupper(rownames(oncolor))

source("modal.R")
source("appUtils.R")
source("dataLoadingFunctions.R")

#if (!is.null(appConfig$appName)){
#	appTitle <- appConfig$appName
#} else{
#	appTitle <- "CellMiner"
#}

# Construct named character vector mapping displayed data source names to
# internally used source identifiers.
dataSourceChoices <- setNames(names(config),
															vapply(config, function(x) { x[["displayName"]] }, 
																		 character(1)))

metaChoices <- setNames(names(metaConfig),
												vapply(metaConfig, function(x) { x[["displayName"]] }, 
															 character(1)))
if(!file.exists("srcContent.rds")) {
	
 for (configSrcId in names(config)){
	srcName <- config[[configSrcId]][["displayName"]]
	srcPackages <- names(config[[configSrcId]][["packages"]])
	for (pkgName in srcPackages){
		 if (!require(pkgName, character.only = TRUE)){
			  dataSourceChoices[srcName] <- NA
		  	break
	  	}
	 }
  }

 if (any(is.na(dataSourceChoices))){
	stop("Check configuration file: one or more required data source packages must be installed.")
 } 
	
} else {
	srcContent <- readRDS("srcContent.rds")
}
#--------------------------------------------------------------------------------------------------

#if("rCharts" %in% installed.packages()) {
#	options(RCHART_LIB='highcharts')	
#	library(rCharts)
#	hasRCharts <- TRUE
#} else {
#	hasRCharts <- FALSE
#}

colorSet <- loadNciColorSet(returnDf=TRUE)

###--------

options("DT.TOJSON_ARGS" = list(na = "string")) ## try dev version of DT

#--------------------------------------------------------------------------------------------------
sysinfo <- Sys.info()
if (sysinfo["nodename"]=="discovery.nci.nih.gov" | sysinfo["nodename"]=="ncias-d2059-v.nci.nih.gov") {
db <- cache_filesystem("/srv/shiny-server/cellminercdb_internal/.rcache")
 } else {
  if (sysinfo["nodename"]=="discover.nci.nih.gov" | sysinfo["nodename"]=="ncias-p2122-v.nci.nih.gov")  {
    db <- cache_filesystem("/srv/shiny-server/cellminercdb/.rcache") }
   else {
     db <- cache_filesystem("/Users/elloumif/.rcache")
   }
}
patternComparison <- memoise(rcellminer::patternComparison, cache = db)
# patternComparison <- memoise(rcellminer::patternComparison) # cache = cache_memory()
getMolDataType <- memoise(rcellminer::getMolDataType, cache = db)
removeMolDataType <- memoise(rcellminer::removeMolDataType, cache = db)

# sink("sessioninfo.txt")
# print(sessionInfo())
# sink()

shinyServer(function(input, output, session) {
	#----[Reactive Variables]---------------------------------------------------------------
	# Record current input validity status, data type prefix values.
	globalReactiveValues <- reactiveValues(xPrefix = NULL, yPrefix = NULL)
	
	# Provides a srcContent (list-based) data structure containing all molecular profiling
	# drug response, and feature/sample annotation data required for the application 
	# (for data sources specified in the config.json file).
	srcContentReactive <- reactive({
		if(!exists("srcContent")) {
			srcContent <- lapply(config, loadSourceContent)
			isLoadedSrc <- vapply(srcContent, function(x) { !is.null(x) }, logical(1))
			if (any(!isLoadedSrc)){
				srcContent <- srcContent[isLoadedSrc]
			}

			# For NCI-60, replace default color map to use CellMiner tissue type colors.
			nci60ColorTab <- loadNciColorSet(returnDf=TRUE)
			nci60ColorTab$OncoTree1 <- srcContent$nci60$sampleData$OncoTree1
			srcContent$nci60$tissueColorMap <- c(by(nci60ColorTab, nci60ColorTab$OncoTree1,
																							FUN = function(x) unique(x$colors)))
		}
		
		return(srcContent)
	})
	
	isPackageLoadingComplete <- reactive({
		srcContentReactive()
		
		return(TRUE)
	})

	# Provides the set of all possible feature identifiers available for plotting along
	# the x-axis of the 2-D scatter plot (and for use in pattern comparisons).
	# xIdChoices <- reactive({
	# 	srcContent <- srcContentReactive()
	# 
	# 	t1 <- lapply(srcContent[[input$xDataset]][["molPharmData"]], function(x) {
	# 		return(unname(removeMolDataType(rownames(x))))
	# 	})
	# 	l1 <- unique(unname(unlist(t1)))
	# 
	# 	t2 <- srcContent[[input$xDataset]][["drugInfo"]]
	# 	l2 <- unname(removeMolDataType(rownames(t2)))
	# 
	# 	l3 <- c(l1, l2)
	# 
	# 	return(l3)
	# })

	# Provides the set of all possible feature identifiers available for plotting along
	# the y-axis of the 2-D scatter plot.
	# yIdChoices <- reactive({
	# 	srcContent <- srcContentReactive()
	# 
	# 	t1 <- lapply(srcContent[[input$yDataset]][["molPharmData"]], function(x) {
	# 		return(unname(removeMolDataType(rownames(x))))
	# 	})
	# 	l1 <- unique(unname(unlist(t1)))
	# 
	# 	t2 <- srcContent[[input$yDataset]][["drugInfo"]]
	# 	l2 <- unname(removeMolDataType(rownames(t2)))
	# 
	# 	l3 <- c(l1, l2)
	# 
	# 	return(l3)
	# })
	
	
	# Returns all valid OncoTree types with respect to
	# the xDataset cell line OncoTree types AND user-selected tissue
	# types for inclusion or exclusion. These types need not be
	# mutually exclusive, given the nested OncoTree structure.
	# NOTE: with added data packages, always verify that matched cell
	# line OncoTree tissue type annotations are consistent across packages.
	analysisTissueTypes <- reactive({
		# Note: We want this code to re-run whenever either 
		# input$tissueSelectionMode OR  input$selectedTissues change.
		# BUT, *reactivity can be based on the selectedTissues alone*,
		# because switching the tissueSelectionMode will always trigger
		# a change to a distinct (default) selectedTissues value (e.g.,
		# "all" in the case of "Include", or "none" in the case of 
		# "Exclude"). 
		# Without the isolate() around input$tisssueSelectionMode (below),
		# this code actually runs twice upon a change to the
		# tissueSelectionMode, with the first run having an invalid
		# selectedTissues value (because the reativity relative
		# to the tissueSelectionMode appears to trigger the code below
		# before the selectedValues changes, for the first run). Then
		# the update of the selectedValues to the default value triggers
		# a second run. This behavior causes a bug-like re-drawing of
		# the 2D plot, etc.)
		tissueSelectionMode <- isolate(input$tissueSelectionMode)
		selectedTissues <- input$selectedTissues
		
		# cat("--- Entering analysisTissueTypes()", sep = "\n")
		# cat(paste0("Selection Mode: ", tissueSelectionMode), sep = "\n")
		# cat(paste0("Selected Tissues: ", selectedTissues), sep = "\n")
		
		srcContent <- srcContentReactive()
		tissueToSamplesMap <- srcContent[[input$xDataset]][["tissueToSamplesMap"]]
		tissueTypes <- names(tissueToSamplesMap)
		
		if (tissueSelectionMode == "To include"){
			if (!("all" %in% selectedTissues)){
				selectedLines <- unique(c(tissueToSamplesMap[selectedTissues], recursive = TRUE))
				# For which tissue types are ALL lines in selectedLines?
				allInSelectedLines <- vapply(tissueToSamplesMap, function(x){
					all(x %in% selectedLines)
				}, logical(1))
				tissueTypes <- names(tissueToSamplesMap[allInSelectedLines])
			}
		} else{ # tissueSelectionMode == "Exclude"
			if (!("none" %in% selectedTissues)){
				selectedLines <- unique(c(tissueToSamplesMap[selectedTissues], recursive = TRUE))
				# For which tissue types are NO lines in selectedLines?
				notInSelectedLines <- vapply(tissueToSamplesMap, function(x){
					length(intersect(x, selectedLines)) == 0
				}, logical(1))
				tissueTypes <- names(tissueToSamplesMap[notInSelectedLines])
			}
		}
		
		#cat("--- LEAVING analysisTissueTypes()", sep = "\n")
		
		return(sort(unique(tissueTypes)))
	})
	
	# Provides a data frame with columns indicating the matched cell lines between
	# the input$xDataset (column 1) and the input$yDataset (column 2).
	# matchedCellLinesTab <- reactive({
	# 	shiny::validate(need(require(rcellminerUtils),
	# 											 "ERROR: x and y axis data sets must be the same."))
	# 	matchedCellLinesTab <- getMatchedCellLines(c(input$xDataset, input$yDataset))
	# 	shiny::validate(need(nrow(matchedCellLinesTab) > 0, 
	# 											 "There are no shared cell lines between the selected datasets."))
	# 	colnames(matchedCellLinesTab) <- c("xDataset", "yDataset")
	# 	return(matchedCellLinesTab)
	# })
	
	# Provides a data frame with columns indicating the matched cell lines between
	# the input$xDataset (column 1) and the input$yDataset (column 2).
	# The cell line pairing will be updated to reflect restrictions based on:
	# --- matched cell lines across databases (if input$xDataset != input$yDataset)
	# --- user tissue type selections.
	matchedCellLinesTab <- reactive({
		srcContent <- srcContentReactive()
		analysisTissueTypes <- analysisTissueTypes()
		
		if (input$xDataset == input$yDataset){
			matchedCellLinesTab <- data.frame(
				xDataset = srcContent[[input$xDataset]]$sampleData[, "Name"],
				stringsAsFactors = FALSE
			)
			matchedCellLinesTab$yDataset <- matchedCellLinesTab$xDataset
		} else{
			shiny::validate(need(require(rcellminerUtilsCDB),
													 "ERROR: x and y axis data sets must be the same."))
			matchedCellLinesTab <- getMatchedCellLines(c(input$xDataset, input$yDataset))
			shiny::validate(need(nrow(matchedCellLinesTab) > 0, 
													 "There are no shared cell lines between the selected datasets."))
			colnames(matchedCellLinesTab) <- c("xDataset", "yDataset")
		}
		stopifnot(all(!duplicated(matchedCellLinesTab$xDataset)))
		rownames(matchedCellLinesTab) <- matchedCellLinesTab$xDataset
		
		tissueMatchedLines <- getTissueTypeSamples(analysisTissueTypes, input$xDataset, srcContent)
		tissueMatchedLines <- intersect(tissueMatchedLines, rownames(matchedCellLinesTab))
		
		shiny::validate(need(length(tissueMatchedLines) > 0, 
												 "There are no cell lines of the selected tissue type(s)."))
		
		matchedCellLinesTab <- matchedCellLinesTab[tissueMatchedLines, ]
		
		return(matchedCellLinesTab)
	})
	
	##------------
	PatternCompTable <- reactive({
	  srcContent <- srcContentReactive()
	  
	  if (input$patternComparisonSeed == "xPattern"){
	    dat <- xData()
	    pcDataset <- input$xDataset
	  } else{
	    dat <- yData()
	    pcDataset <- input$yDataset
	  }
	  selectedLines <- names(dat$data)
	  
	  shiny::validate(need(length(selectedLines)>0, paste("ERROR:", " No common complete data found.")))
	  shiny::validate(need(length(selectedLines)>2, paste("ERROR:", " No display for less than 3 observations.")))
	  
	  if(input$patternComparisonType == "drug") {
	    shiny::validate(need(srcContent[[pcDataset]][["molPharmData"]][["act"]], "No drug available for this cell line set"))
	    #if (is.null(srcContent[[pcDataset]][["molPharmData"]][["act"]])) stop("No drug available for this cell line set")
	    results <- patternComparison(dat$data,
	                                 srcContent[[pcDataset]][["molPharmData"]][["act"]][, selectedLines])
	    results$ids <- rownames(results)
	    results$NAME <- srcContent[[pcDataset]][["drugInfo"]][rownames(results), "NAME"]
	    
	    if ("MOA" %in% colnames(srcContent[[pcDataset]][["drugInfo"]])){
	      results$MOA <- srcContent[[pcDataset]][["drugInfo"]][rownames(results), "MOA"]
	      results <- results[, c("ids", "NAME", "MOA", "COR", "PVAL")]
	      colnames(results) <- c("ID", "Name", "MOA", "Correlation", "P-Value")
	    } else{
	      results <- results[, c("ids", "NAME", "COR", "PVAL")]
	      colnames(results) <- c("ID", "Name", "Correlation", "P-Value")
	    }
	    DataType <- getMolDataType(results$ID)
	    DrugID <- removeMolDataType(results$ID)
	    results$ID <- NULL
	    results$FDR=p.adjust(results[,"P-Value"],method="BH",nrow(results))
	    results=cbind(DataType,DrugID,results)
	    colnames(results)[1:2]=c("Data Type", "Drug ID")
	    
	  } else {
	    molPharmData <- srcContent[[pcDataset]][["molPharmData"]]
	    molData <- molPharmData[setdiff(names(molPharmData), c("act","copA","mutA","metA","expA","xaiA","proA","mirA","mdaA","swaA","xsqA","mthA"))]
	    shiny::validate(need(length(molData)>0, "No molecular data available for this cell line set"))
	    ##if (length(molData)==0) stop("No molecular data available for this cell line set")
	    ## old: molData <- lapply(molData, function(X) X[, selectedLines])
	    ## new selection in case a dataset has only one row
	    ## one solution: molData <- lapply(molData, function(X) subset(X, select=selectedLines))
	    molData <- lapply(molData, function(X) X[, selectedLines,drop=FALSE])
	    results <- patternComparison(dat$data, molData)
	    results$ids <- rownames(results)
	    
	    results$molDataType <- getMolDataType(results$ids)
	    results$gene <- removeMolDataType(results$ids)
	    
	    # Reorder columns
	    results <- results[, c("ids", "molDataType", "gene", "COR", "PVAL")]
	    colnames(results) <- c("ID", "Data Type", "Gene", "Correlation", "P-Value")
	    
	    if (require(rcellminerUtilsCDB)){
	      chromLocs <- character(nrow(results))
	      haveLoc <- results$Gene %in% names(geneToChromBand)
	      chromLocs[haveLoc] <- geneToChromBand[results$Gene[haveLoc]]
	      
	      results$Location <- chromLocs
	      results <- results[, c("ID", "Data Type", "Gene", "Location", "Correlation", "P-Value")]
	    }
	    results$FDR=p.adjust(results[,"P-Value"],method="BH",nrow(results))
	    
	    if (require(geneSetPathwayAnalysis)){
	      # old :results$Annotation <- geneSetPathwayAnalysis::geneAnnotTab[results$Gene, "SHORT_ANNOT"]
	      # issue related to prefix subsetting ex: "age" >> gives "ager"
	      #st=Sys.time()
	      results$Annotation <- geneSetPathwayAnalysis::geneAnnotTab[match(results$Gene,rownames(geneSetPathwayAnalysis::geneAnnotTab)), "SHORT_ANNOT"]
	      #et=Sys.time()
	      #cat(et-st,"\n")
	      results$Annotation[is.na(results$Annotation)] <- ""
	    }
	    
	    results$ID <- NULL
	    colnames(results)[2]="ID"
	  }
	  
	  results[, "Correlation"] <- round(results[, "Correlation"], 3)
	  results[, "P-Value"] <- signif(results[, "P-Value"], 3)
	  results[, "FDR"] <- signif(results[, "FDR"], 3)
	  ## sort by p-value
	  results <- results[order(results[, "P-Value"]),]
	  
	  return(results)
	})
	##--------------
	
	# Explanation of xData, yData reactive variables -------------------------------------------------
	# The xData and yData reactive variables provide list objects (accessed via xData() and yData()) 
	# that store the essential information about a data source feature that the application code
	# requires (e.g., to make plots, for pattern comparisons, etc.).
	# For example, if the x-axis feature is SLFN11 NCI-60 mRNA expression, the xData() 
	# list object would contain:
	# xData()$dataSource: "nci60"
	# xData()$name: "expSLFN11" (the feature identifier prepended with a data type specifier)
	# xData()$uniqName: "expSLFN11_nci60" (the above identifier appended with the data source)
	# xData()$plotLabel: "SLFN11 (exp, nci60)"
	# xData()$data: A vector of numeric feature data.
	#
	# Note: the reactive variable construction code ensures that:
	# (1) User-requested feature data is available for the specified data source,
	# (2) if the x-axis and y-axis features are derived from different data sources,
	#     the xData()$data and yData()$data vectors *have feature data from matched cell lines*.
	#
	# Through the reactivity, these variables are always 'current', reflecting the latest
	# user selections, with regard to feature identifiers, data source(s), etc.
	# ------------------------------------------------------------------------------------------------
	
	# Provides an a list object with x-axis feature-related data, including numeric data,
	# data type prefix, data source, and plot label.
	# Note that use of reactive matchedLinesTab() ensures that xData() and yData() are
	# current and coupled, reflecting matched cell lines (even when different x any y data 
	# sources are selected) of whatever tissue types are selected.
	xData <- reactive({
		shiny::validate(need(length(input$selectedTissues) > 0, "Please select tissue types."))

		xPrefix <- input$xPrefix
		if (!is.character(xPrefix)){
			xPrefix <- srcContentReactive()[[input$xDataset]][["defaultFeatureX"]]
		}
		#
		originalId <- trimws(input$xId)
		# 
		xId <- getMatchedIds(xPrefix, trimws(input$xId), input$xDataset, srcContent = srcContentReactive())
		
		if (length(xId) == 0){
			shiny::validate(need(FALSE, paste("ERROR:", paste0(xPrefix, input$xId), "not found. Please use the Search IDs tab to find available IDs for each dataset.")))
		} else{
			globalReactiveValues$xPrefix <- xPrefix
			if (length(xId) > 1){
				warningMsg <- paste0("Other identifiers matching x-axis ID: ",
														 paste0(xId[-1], collapse = ", "), ".")
				showNotification(warningMsg, duration = 10, type = "message")
				xId <- xId[1]
			}
			# xData <- getFeatureData(xPrefix, xId, input$xDataset, srcContent = srcContentReactive())
			xData <- getFeatureData(xPrefix, xId, input$xDataset, srcContent = srcContentReactive(), originalId)
			
			matchedLinesTab <- matchedCellLinesTab()
			# xData$data <- xData$data[matchedLinesTab[, "xDataset"]]
			xData$data <- xData$data[as.character(matchedLinesTab[, "xDataset"])]
			
		}
		
		return(xData)
	})
	
	# Provides an a list object with y-axis feature-related data, including numeric data,
	# data type prefix, data source, and plot label.
	# Note that use of reactive matchedLinesTab() ensures that xData() and yData() are
	# current and coupled, reflecting matched cell lines (even when different x any y data 
	# sources are selected) of whatever tissue types are selected.
	yData <- reactive({
		shiny::validate(need(length(input$selectedTissues) > 0, "Please select tissue types."))
		
		yPrefix <- input$yPrefix
		if (!is.character(yPrefix)){
			yPrefix <- srcContentReactive()[[input$yDataset]][["defaultFeatureY"]]
		}
		#
		originalId <- trimws(input$yId)
		# 
		
		yId <- getMatchedIds(yPrefix, trimws(input$yId), input$yDataset, srcContent = srcContentReactive())
		
		if (length(yId) == 0){
			shiny::validate(need(FALSE, paste("ERROR:", paste0(yPrefix, input$yId), "not found. Please use the Search IDs tab to find available IDs for each dataset.")))
		} else{
			globalReactiveValues$yPrefix <- yPrefix
			if (length(yId) > 1){
				warningMsg <- paste0("Other identifiers matching y-axis ID: ",
														 paste0(yId[-1], collapse = ", "), ".")
				showNotification(warningMsg, duration = 10, type = "message")
				yId <- yId[1]
			}
			# yData <- getFeatureData(yPrefix, yId, input$yDataset, srcContent = srcContentReactive())
			yData <- getFeatureData(yPrefix, yId, input$yDataset, srcContent = srcContentReactive(), originalId)
			
			matchedLinesTab <- matchedCellLinesTab()
			# yData$data <- yData$data[matchedLinesTab[, "yDataset"]]
			yData$data <- yData$data[as.character(matchedLinesTab[, "yDataset"])]
		}
		
		return(yData)
	})
	
	#----[outputs]--------------------------------------------------------------------------

  #----[Render Application Title]------------------------------------------------------
  output$public <- renderText("CellMiner")
  #------------------------------------------------------------------------------------

  #----[Render 2D Plot in 'Plot Data' Tab]---------------------------------------------
#   if(require(rCharts)) {
# 		output$rCharts <- renderChart({
# 			h1 <- makePlot(xData = xData(), yData = yData(), showColor = input$showColor,
# 										 showColorTissues = input$showColorTissues, dataSource = input$xDataset,
# 										 srcContent = srcContentReactive(), dom="rCharts")
# 		})
#   }

	# Alternative plotting
	output$rChartsAlternative <- renderPlotly({
		#-----[range check]----------------------------------------------------------
		# Note: Until a better solution can be found, these checks are needed.
		# The issue is that upon a data source change, there appears to be a moment 
		# when the xData() or yData() are updated, but the input$xAxisRange
		# or input$yAxisRange (from the sliderInput UI element) are not yet updated.
		# As such, the data value range can be out of synch with the invalidated
		# axis ranges. In the most extreme case, there are no points in the
		# specified range. The reactive code is quickly re-run with the proper
		# inputs, correcting the plot, but the error flashes briefly in a buggy 
		# looking way. 
		# Below we do a range check and quietly exit if something is amiss (knowing
		# that the reactivity will ensure that the code is re-run with a proper
		# set of inputs once thing settle down).
		#****************************************************************************
		xData <- xData()
		yData <- yData()
		shiny::validate(need(xData$uniqName != yData$uniqName, 
			"Please select distinct x and y axis variables."))
		
		xValRange <- range(xData$data, na.rm = TRUE)
		xLimits <- input$xAxisRange
		
		yValRange <- range(yData$data, na.rm = TRUE)
		yLimits <- input$yAxisRange
		
		# req(FALSE) causes immediate but quiet exit.
		req(!any(is.null(xValRange)), !any(is.null(xLimits)))
		req(!any(is.null(yValRange)), !any(is.null(yLimits)))
		req(!any(is.na(xValRange)), !any(is.na(xLimits)))
		req(!any(is.na(yValRange)), !any(is.na(yLimits)))
		# commented below
		# req((xLimits[1] <= xValRange[1]) && (xValRange[2] <= xLimits[2]))
		# req((yLimits[1] <= yValRange[1]) && (yValRange[2] <= yLimits[2]))
		
		cat("xAxis Limits: ", paste0(xLimits, collapse = " "), sep = "\n")
		cat("X_VAL_RANGE: ",  paste0(xValRange, collapse = " "), sep = "\n")
		
		cat("yAxis Limits: ", paste0(yLimits, collapse = " "), sep = "\n")
		cat("Y_VAL_RANGE: ",  paste0(yValRange, collapse = " "), sep = "\n")
		cat("-------------------------------------------", sep = "\n")
		#----------------------------------------------------------------------------
		
		p1 <- makePlotStatic(xData = xData, yData = yData, showColor = input$showColor, 
												 showColorTissues = input$showColorTissues, dataSource = input$xDataset, 
												 xLimVals = xLimits, yLimVals = yLimits,
												 srcContent = srcContentReactive(),oncolor=oncolor)
		p1 <- p1 + theme(axis.text = element_text(size=16), plot.title = element_text(size = 16), 
		                 axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 16))
		g1 <- ggplotly(p1, width=plotWidth, height=plotHeight, tooltip=tooltipCol)
		#g1 <- layout(g1, margin=list(t = 75))
		g1 <- layout(g1, margin=list(t = 75), legend = list(font = list(size = 14)))
		g2 <- config(p = g1, collaborate=FALSE, cloud=FALSE, displaylogo=FALSE, displayModeBar=TRUE,
								 modeBarButtonsToRemove=c("select2d", "sendDataToCloud", "pan2d", "resetScale2d",
								 												 "hoverClosestCartesian", "hoverCompareCartesian",
								 												 "lasso2d", "zoomIn2d", "zoomOut2d"))
		g2
	})
	#--------------------------------------------------------------------------------------

  #----[Render Data Table in 'Download Data' Tab]----------------------------------------
  # Generate an HTML table view of the data
  output$table <- DT::renderDataTable({
  	# Column selection below is to restrict to cell line, x, y features,
  	# and tissue type information (source-provided + OncoTree).
  	dlDataTab <- getPlotData(xData = xData(), yData = yData(), showColor = input$showColor, 
  		showColorTissues = input$showColorTissues, dataSource = input$xDataset, 
  		srcContent = srcContentReactive())
  
  	# cat(colnames(dlDataTab),"\n")
  	
  		shiny::validate(need(nrow(dlDataTab)>0, paste("ERROR:", " No common complete data found.")))
  
  	dlDataTabCols <- c(colnames(dlDataTab)[1:4], paste0("OncoTree", 1:4))
  	if ("EMT" %in% colnames(dlDataTab)) {
  		dlDataTabCols <- c(dlDataTabCols, "EMT")
  	}
  	
  	if ("NeuroEndocrineScore" %in% colnames(dlDataTab)) {
  	  dlDataTabCols <- c(dlDataTabCols, "NeuroEndocrineScore")
  	}
  	
  	dlDataTab <- dlDataTab[, dlDataTabCols]
  	dlDataTab[, 2] <- round(dlDataTab[, 2], 3)
  	dlDataTab[, 3] <- round(dlDataTab[, 3], 3)

  	DT::datatable(dlDataTab, rownames=FALSE, colnames=colnames(dlDataTab),
  								filter='top', style='bootstrap', selection="none",
  								options=list(pageLength = nrow(dlDataTab), language=list(paginate = list(previous = 'Previous page', `next`= 'Next page'))))
  })
	#--------------------------------------------------------------------------------------
	output$cortable <- DT::renderDataTable({
	  # Column selection below is to restrict to cell line, x, y features,
	  # and tissue type information (source-provided + OncoTree).
	  rescor= CorrelationTable(xData(),yData(),srcContentReactive())
	  
	  DT::datatable(rescor, rownames=FALSE, colnames=colnames(rescor),extensions='Buttons',
	                filter='top', style='bootstrap', selection="none",
	                options=list(pageLength = nrow(rescor), language=list(paginate = list(previous = 'Previous page', `next`= 'Next page')) ,dom='lipBt', buttons = list('copy', 'print', list(extend = 'collection',buttons = list(list(extend='csv',filename='tissue_correlation',title='Exported data from CellMinerCDB'), list(extend='excel',filename='tissue_correlation',title='Exported data from CellMinerCDB'), list(extend='pdf',filename='tissue_correlation',title='Exported data from CellMinerCDB')),text = 'Download'))))
	 
	  })
  #--------------------------------------------------------------------------------------
  #----[Render Data Table in 'Search IDs' Tab]-------------------------------------------
  # Generate an HTML table view of the data
  # Note: Searchable data is derived from the x-axis data source.
	output$ids <- DT::renderDataTable({
	  srcContent <- srcContentReactive()
	  drugIds   <- srcContent[[input$xDataset]][["drugInfo"]][, "ID"]
	  drugNames <- srcContent[[input$xDataset]][["drugInfo"]][, "NAME"]
	  moaNames  <- srcContent[[input$xDataset]][["drugInfo"]][, "MOA"]
	  exptype <- rep("act", length(drugIds))
	  
	  results <- data.frame(availableTypes=exptype, availableIds = drugIds, idNames=drugNames, properties=moaNames,
	                        stringsAsFactors=FALSE)
	  
	  # Make molecular data data.frame
	  molPharmData <- srcContent[[input$xDataset]][["molPharmData"]]
	  molData <- molPharmData[setdiff(names(molPharmData), "act")]
	  molDataIds <- as.vector(unlist(lapply(molData, function(x) { rownames(x) })))
	  
	  exptype2 <- substr(molDataIds,1,3)
	  molDataIds <- substr(molDataIds,4,nchar(molDataIds))
	  
	  molDataNames <- rep("", length(molDataIds))
	  moaNames <- rep("", length(molDataIds))
	  
	  tmp <- data.frame(availableTypes=exptype2,availableIds=molDataIds, idNames=molDataNames, properties=moaNames,
	                    stringsAsFactors=FALSE)
	  
	  # Join data.frames
	  results <- rbind(results, tmp)
	  
	  # Reverse Order/ no need for reverse for me
	  #results <- results[rev(rownames(results)),]
	  
	  colnames(results) <- c("Data type","ID ", "Drug Name", "Drug MOA")
	  selsource=metaConfig[[input$xDataset]][["fullName"]]
	  DT::datatable(results, rownames=FALSE, colnames=colnames(results),
	                filter='top', style='bootstrap', selection = "none",
	                options=list(pageLength = 10, language=list(paginate = list(previous = 'Previous page', `next`= 'Next page'))), caption=htmltools::tags$caption(paste0("Ids table for ",selsource),style="color:dodgerblue; font-size: 18px"))
	})
		#--------------------------------------------------------------------------------------

	#----[Render Data Table in 'SearchIDs' Tab]-------------------------------------------
	# Generate an HTML table view of the data
	# Note: Searchable data is derived from the x-axis data source.
	output$ids_s <- DT::renderDataTable({
	  srcContent <- srcContentReactive()
	  if (input$dataTyp=="act") {
	    myframe=srcContent[[input$dataSrc]][["drugInfo"]]
	    
	  }
	  else
	  {
	    mytype=paste0(input$dataTyp,"A")
	    if (is.null(srcContent[[input$dataSrc]][["molPharmData"]][[mytype]]) | ncol(srcContent[[input$dataSrc]][["molPharmData"]][[mytype]])==0) {
	      ID=unlist(lapply(rownames(srcContent[[input$dataSrc]][["molPharmData"]][[input$dataTyp]]),function(x) {return(substr(x,4,nchar(x)))}))
	      myframe=data.frame(ID,stringsAsFactors = F)
	    }
	    else
	    { myframe=srcContent[[input$dataSrc]][["molPharmData"]][[mytype]]
	      if (input$dataTyp=="swa") {
	        myframe=myframe[,c(2,1)]
	      } else 
	        if (input$dataTyp=="exp" & input$dataSrc=="nciSclc")
	        { myframe=myframe[,c(4,3,5:13,1:2)] 
	        }
	      
	      colnames(myframe)[1]="ID" 
	    }
	  }
	  
	  
	  selsource=metaConfig[[input$dataSrc]][["fullName"]]
	  DT::datatable(myframe, rownames=FALSE,extensions='Buttons',
	                filter='top', style='bootstrap', selection = "none",
	                options=list(pageLength = 10,language=list(paginate = list(previous = 'Previous page', `next`= 'Next page')) ,dom='lipBt',buttons = list('copy', 'print', list(extend = 'collection',buttons = list(list(extend='csv',filename='search_id',title='Exported data from CellMinerCDB'), list(extend='excel',filename='search_id',title='Exported data from CellMinerCDB'), list(extend='pdf',filename='search_id',title='Exported data from CellMinerCDB')),text = 'Download')))
	                , caption=htmltools::tags$caption(paste0("Identifier search for ",selsource),style="color:dodgerblue; font-size: 18px")
	)})

	## new version
	output$ids2 <- DT::renderDataTable({
	  srcContent <- srcContentReactive()
	  drugIds   <- srcContent[[input$dataSrc]][["drugInfo"]][, "ID"]
	  drugNames <- srcContent[[input$dataSrc]][["drugInfo"]][, "NAME"]
	  moaNames  <- srcContent[[input$dataSrc]][["drugInfo"]][, "MOA"]
	  exptype <- rep("act", length(drugIds))
	  
	  results <- data.frame(availableTypes=exptype, availableIds = drugIds, idNames=drugNames, properties=moaNames,
	                        stringsAsFactors=FALSE)
	  
	  # Make molecular data data.frame
	  molPharmData <- srcContent[[input$dataSrc]][["molPharmData"]]
	  molData <- molPharmData[setdiff(names(molPharmData), "act")]
	  molDataIds <- as.vector(unlist(lapply(molData, function(x) { rownames(x) })))
	  
	  exptype2 <- substr(molDataIds,1,3)
	  molDataIds <- substr(molDataIds,4,nchar(molDataIds))
	  
	  molDataNames <- rep("", length(molDataIds))
	  moaNames <- rep("", length(molDataIds))
	  
	  tmp <- data.frame(availableTypes=exptype2,availableIds=molDataIds, idNames=molDataNames, properties=moaNames,
	                    stringsAsFactors=FALSE)
	  
	  # Join data.frames
	  results <- rbind(results, tmp)
	  
	  # Reverse Order/ no need for reverse for me
	  #results <- results[rev(rownames(results)),]
	  
	  colnames(results) <- c("Data type","ID ", "Drug Name", "Drug MOA")
	  selsource=metaConfig[[input$dataSrc]][["fullName"]]
	  DT::datatable(results, rownames=FALSE, colnames=colnames(results),extensions='Buttons',
	                filter='top', style='bootstrap', selection = "none",
	                options=list(pageLength = 10,language=list(paginate = list(previous = 'Previous page', `next`= 'Next page')) ,dom='lipBt',buttons = list('copy', 'print', list(extend = 'collection',buttons = list(list(extend='csv',filename='search_id'), list(extend='excel',filename='search_id'), list(extend='pdf',filename='search_id')),text = 'Download')))
	                , caption=htmltools::tags$caption(paste0("Identifier search for ",selsource),style="color:dodgerblue; font-size: 18px")
	  )})
	
		#--------------------------------------------------------------------------------------
	
	
	#----[Render Data Table in 'Compare Patterns' Tab]-------------------------------------
	output$patternComparison <- DT::renderDataTable({
	# 	srcContent <- srcContentReactive()
	# 	
	# 	if (input$patternComparisonSeed == "xPattern"){
	# 		dat <- xData()
	# 		pcDataset <- input$xDataset
	# 	} else{
	# 		dat <- yData()
	# 		pcDataset <- input$yDataset
	# 	}
	# 	selectedLines <- names(dat$data)
	# 
	#   if(input$patternComparisonType == "drug") {
	#     results <- patternComparison(dat$data,
	#     														 srcContent[[pcDataset]][["molPharmData"]][["act"]][, selectedLines])
	#     results$ids <- rownames(results)
	#     results$NAME <- srcContent[[pcDataset]][["drugInfo"]][rownames(results), "NAME"]
	# 
	#     if ("MOA" %in% colnames(srcContent[[pcDataset]][["drugInfo"]])){
	#     	results$MOA <- srcContent[[pcDataset]][["drugInfo"]][rownames(results), "MOA"]
	#     	results <- results[, c("ids", "NAME", "MOA", "COR", "PVAL")]
	#     	colnames(results) <- c("ID", "Name", "MOA", "Correlation", "P-Value")
	#     } else{
	#     	results <- results[, c("ids", "NAME", "COR", "PVAL")]
	#     	colnames(results) <- c("ID", "Name", "Correlation", "P-Value")
	#     }
	#     results$FDR=p.adjust(results[,"P-Value"],method="BH",nrow(results))
	#     
	#   } else {
	#     molPharmData <- srcContent[[pcDataset]][["molPharmData"]]
	#     molData <- molPharmData[setdiff(names(molPharmData), c("act","copA","mutA","metA","expA","xaiA","proA","mirA","mdaA","swaA","xsqA"))]
	#     molData <- lapply(molData, function(X) X[, selectedLines])
	#     results <- patternComparison(dat$data, molData)
	#     results$ids <- rownames(results)
	# 
	#     results$molDataType <- getMolDataType(results$ids)
	#     results$gene <- removeMolDataType(results$ids)
	# 
	#     # Reorder columns
	#     results <- results[, c("ids", "molDataType", "gene", "COR", "PVAL")]
	#     colnames(results) <- c("ID", "Data Type", "Gene", "Correlation", "P-Value")
	# 
	#     if (require(rcellminerUtilsCDB)){
	#     	chromLocs <- character(nrow(results))
	#     	haveLoc <- results$Gene %in% names(geneToChromBand)
	#     	chromLocs[haveLoc] <- geneToChromBand[results$Gene[haveLoc]]
	# 
	#     	results$Location <- chromLocs
	#     	results <- results[, c("ID", "Data Type", "Gene", "Location", "Correlation", "P-Value")]
	#     }
	#     results$FDR=p.adjust(results[,"P-Value"],method="BH",nrow(results))
	#     
	#     if (require(geneSetPathwayAnalysis)){
	#     	results$Annotation <- geneSetPathwayAnalysis::geneAnnotTab[results$Gene, "SHORT_ANNOT"]
	# 			results$Annotation[is.na(results$Annotation)] <- ""
	#     }
	#     
	#     results$ID <- NULL
	#   }
	#   
	#   results[, "Correlation"] <- round(results[, "Correlation"], 3)
	#   results[, "P-Value"] <- signif(results[, "P-Value"], 3)
	#   results[, "FDR"] <- signif(results[, "FDR"], 3)
	# 	## sort by p-value
	#   results <- results[order(results[, "P-Value"]),]
    results= PatternCompTable()	 
	  # DT::datatable(results, rownames=FALSE, colnames=colnames(results),extensions='Buttons',
	  # 							filter='top', style='bootstrap', selection = "none",
	  # 							options=list(lengthMenu = c(10, 50, 100,500), pageLength = 100,language=list(paginate = list(previous = 'Previous page', `next`= 'Next page')) ,dom='lipBt', buttons = list('copy', 'print', list(extend = 'collection',buttons = list(list(extend='csv',filename='pattern_comp',title='Exported data from CellMinerCDB'), list(extend='excel',filename='pattern_comp',title='Exported data from CellMinerCDB'), list(extend='pdf',filename='pattern_comp',title='Exported data from CellMinerCDB')),text = 'Download'))))
	  DT::datatable(results, rownames=FALSE, colnames=colnames(results),
	                filter='top', style='bootstrap', selection = "none",
	                options=list(lengthMenu = c(10, 50, 100,500), pageLength = 100,language=list(paginate = list(previous = 'Previous page', `next`= 'Next page')) ,dom='lipt'))
	  
	})
	

	#----[Render Data Table in 'Metadata' Tab]-------------------------------------------
	output$cellLineTable <- DT::renderDataTable({
		
		configSelect <- metaConfig[[input$mdataSource]][["packages"]][[1]][["MetaData"]]
		jsonFrame <- as.data.frame(configSelect)
		
		colnames(jsonFrame) <- c("Data Type", "Description", "Units", 
														 "Platform/Assay", "PubMed Ref. ID")
		
		DT::datatable(jsonFrame, rownames=FALSE, colnames=colnames(jsonFrame),
									filter='top', style='bootstrap', selection = "none",
									options=list(pageLength = 10,language=list(paginate = list(previous = 'Previous page', `next`= 'Next page'))),escape=F)
	})
	#---------------------------------------------------------------------------------------
	output$log <- renderText({
			paste(names(input), collapse=" ")
			query <- parseQueryString(session$clientData$url_search)
			sapply(names(input), function(x) { query[[x]] <- input[[x]] })
		})

# 	output$genUrl <- renderText({
# 		#query <- parseQueryString(session$clientData$url_search)
# 		query <- list()
#
# 		# Get current input values and combine with
# 		tmp <- sapply(names(input), function(x) { query[[x]] <- input[[x]] })
# 		query <- c(query, tmp)
#
# 		paramStr <- paste(sapply(names(query), function(x) { paste0(x, "=", query[[x]]) }), collapse="&")
#
# 		urlStr <- paste(session$clientData$url_protocol, "//",
# 										session$clientData$url_hostname, ":",
# 										session$clientData$url_port,
# 										session$clientData$url_pathname,
# 										"?", paramStr,
# 										sep="")
#
# 		paste0(a(paste("Shareable Link (Right-click then 'Copy' to share)"), href=urlStr), hr())
#
# 		#paste("Shareable Link:", urlStr)
# 	})

  #**********************************************************************************************
	output$tabsetPanel = renderUI({
		#verbatimTextOutput("log") can be used for debugging
		#tabPanel("Plot", verbatimTextOutput("genUrl"), showOutput("rCharts", "highcharts")),
	  tab4 <- tabPanel("Tissue Correlation",
	                   #downloadLink("downloadData", "Download selected x and y axis data as a Tab-Delimited File"),
	                   DT::dataTableOutput("cortable"))
		tab1 <- tabPanel("View Data",
                     downloadLink("downloadData", "Download selected x and y axis data as a Tab-Delimited File"),
                     DT::dataTableOutput("table"))
		tab2 <- tabPanel("Search IDs",
                     includeMarkdown("www/files/help.md"),
                     DT::dataTableOutput("ids"))
		tab3 <- tabPanel("Compare Patterns",
										 includeMarkdown("www/files/help.md"),
										 #br(),
										 HTML("<b>Pattern comparison results are computed with respect to that data defined and shared by both the x and y-axis inputs.</b>"),
										 br(),br(),
										 fluidRow(
                     	#column(3, selectInput("patternComparisonType", "Pattern Comparison",
                      #           						choices=c("Molecular Data"="molData", "Drug Data"="drug"), 
                     	#											selected="molData")),
                     	
                     	column(3, HTML(
                     	  paste("<label class='control-label' for='patternComparisonType'>Pattern Comparison</label>","<select id='patternComparisonType'><option value='moldata' selected>Molecular Data</option><option value='drug'>Drug Data</option></select>")
                     	)),
										 	#column(3, selectInput("patternComparisonSeed", "With Respect to",
										 	#											choices=c("x-Axis Entry"="xPattern", 
										 	#																"y-Axis Entry"="yPattern"), 
										 	#											selected="xPattern"))
										 	column(3, HTML(
										 	  paste("<label class='control-label' for='patternComparisonSeed'>With Respect to</label>","<select id='patternComparisonSeed'><option value='xPattern' selected>x-Axis Entry</option><option value='yPattern'>y-Axis Entry</option></select>")
										 	))
										 ),
										 br(),br(),
										 renderUI({
										   req(PatternCompTable())
										   downloadLink("downloadDataComp", "Download All as a Tab-Delimited File")
										 }),
										 ##downloadLink("downloadDataComp", "Download All as a Tab-Delimited File"),
                     withSpinner(DT::dataTableOutput("patternComparison")))
		
		                 # withSpinner(DT::dataTableOutput("patternComparison")),
		                 # downloadLink("downloadDataComp", "Download All as a Tab-Delimited File"))	

#if(input$hasRCharts == "TRUE") {
#		if (FALSE) {
#			tsPanel <- tabsetPanel(type="tabs",
#									#tabPanel("Plot Data", htmlOutput("genUrl"), showOutput("rCharts", "highcharts")),
#									tabPanel("Plot Data", showOutput("rCharts", "highcharts")),
#									tab1, tab2, tab3
#			)
#		} else {
			plotPanel <- tabPanel("Plot Data", plotlyOutput("rChartsAlternative", width = plotWidth, height = plotHeight),
														br(), br(), p("Plot point tooltips provide additional information."))
			#tsPanel <- tabsetPanel(plotPanel, tab1, tab2, tab3)
			tsPanel <- tabsetPanel(plotPanel, tab1, tab3,tab4)
#		}

		return(tsPanel)
	})
	
	#**********************************************************************************************
	output$metadataPanel = renderUI({
		#verbatimTextOutput("log") can be used for debugging
		#tabPanel("Plot", verbatimTextOutput("genUrl"), showOutput("rCharts", "highcharts")),
		
		#mtab1 <- tabPanel("Features",
											#DT::dataTableOutput("featTable"))
		mtab2 <- tabPanel("Cell Line Information",
											DT::dataTableOutput("cellLineTable"))
		#mtab3 <- tabPanel("Drug Information",
											#includeMarkdown("www/files/help.md"),
											#DT::dataTableOutput("drugTable"))
		
		dataFullname <- metaConfig[[input$mdataSource]][["fullName"]]
		
		tabsetPanel(type="pills",
								tabPanel(dataFullname, tags$hr(),
													mtab2
								)
		)
	})
	##*********************************************************
	# output$searchPanel = renderUI({
	#   #verbatimTextOutput("log") can be used for debugging
	#   #tabPanel("Plot", verbatimTextOutput("genUrl"), showOutput("rCharts", "highcharts")),
	#   
	#   includeMarkdown("www/files/help.md")
	#   DT::dataTableOutput("ids2")
	#  
	# })
	
	output$searchPanel = renderUI({
	  #verbatimTextOutput("log") can be used for debugging
	  #tabPanel("Plot", verbatimTextOutput("genUrl"), showOutput("rCharts", "highcharts")),
	  
	  #includeMarkdown("www/files/help.md")
	  DT::dataTableOutput("ids_s")
	  
	})
	##*********************************************************
	#**************************************************************************************
	output$sourceLink <- renderUI({
		
		urlString <- metaConfig[[input$mdataSource]][["url"]]
		sourceName <- metaConfig[[input$mdataSource]][["displayName"]]
		visibleText <- paste("Select here to learn more about ", sourceName, sep="")
		if (input$mdataSource=="nci60")
		tags$div(
		tags$a(visibleText, href=paste(urlString), target = "_blank"),
		tags$a("   and the DTP",href='https://dtp.cancer.gov', target='_blank'))
		else tags$a(visibleText, href=paste(urlString), target = "_blank")
		#  
		# if (input$mdataSource=="nci60") { 
		#    a(visibleText, href=paste(urlString), target = "_blank")
		#    a("DTP",href='https://dtp.cancer.gov', target='_blank')
		# }
	})
	#**********************************************************************************************

  output$downloadData <- downloadHandler(
    filename = function() {
      query <- parseQueryString(session$clientData$url_search)
      
      if("filename" %in% names(query)) {
        filename <- query[["filename"]]
      } else {
        filename <- "dataset"
      }
      
      if("extension" %in% names(query)) {
        extension <- query[["extension"]]
      } else {
        extension <- "txt"
      }
      
      paste(filename, extension, sep=".")
    },
    content = function(file) {
      df <- getPlotData(xData = xData(), yData = yData(), showColor = input$showColor, 
                        showColorTissues = input$showColorTissues, dataSource = input$xDataset, 
                        srcContent = srcContentReactive())
      
      # Column selection below is to restrict to cell line, x, y features,
      # and tissue type information (source-provided + OncoTree).
      dfCols <- c(colnames(df)[1:4], paste0("OncoTree", 1:4))
      if ("EMT" %in% colnames(df)) {
        dfCols <- c(dfCols, "EMT")
      }
      df <- df[, dfCols]
      
      write.table(df, file, quote=FALSE, row.names=FALSE, sep="\t")
    }
  )
  ### data types for search ---------
  output$dataTypeUi <- renderUI({
    srcContent <- srcContentReactive()
    
    # The last selected (data type) prefix is recorded in 
    # globalReactiveValues$xPrefix whenever xData() is updated. When the data set 
    # is changed, we try to use this same data type prefix, if it is available.
    prefixChoices <- srcContent[[input$mdataSource]][["featurePrefixes"]]
    selectedPrefix <- globalReactiveValues$xPrefix
    if ((is.null(selectedPrefix)) || (!(selectedPrefix %in% prefixChoices))){
      selectedPrefix <- srcContent[[input$mdataSource]][["defaultFeatureX"]]
      if (is.na(selectedPrefix)) selectedPrefix <- srcContent[[input$mdataSource]][["defaultFeatureY"]]
    }
    opt = "";
    for(y in 1:length(prefixChoices)){
      if (prefixChoices[y]==selectedPrefix)
      {
        opt =  paste0(opt,"<option value=",prefixChoices[y]," selected>",names(prefixChoices)[y],"</option>;")
      }
      else
      {
        opt =  paste0(opt,"<option value=",prefixChoices[y],">",names(prefixChoices)[y],"</option>;");
      }
    }
    # selectInput("xPrefix", "x-Axis Type", choices = prefixChoices, selected = selectedPrefix)
    HTML(
      paste("<label class='control-label' for='dataType'>Select Data Type to Download</label>","<select id='dataType' style='word-wrap:break-word; width: 100%;'>",opt,"</select>")
    )
  })
 ## new version for search tab
  output$dataTypeUi_s <- renderUI({
    srcContent <- srcContentReactive()
    
    # The last selected (data type) prefix is recorded in 
    # globalReactiveValues$xPrefix whenever xData() is updated. When the data set 
    # is changed, we try to use this same data type prefix, if it is available.
    prefixChoices <- srcContent[[input$dataSrc]][["featurePrefixes"]]
    selectedPrefix <- globalReactiveValues$xPrefix
    if ((is.null(selectedPrefix)) || (!(selectedPrefix %in% prefixChoices))){
      selectedPrefix <- srcContent[[input$dataSrc]][["defaultFeatureX"]]
      if (is.na(selectedPrefix)) selectedPrefix <- srcContent[[input$dataSrc]][["defaultFeatureY"]]
    }
    opt = "";
    for(y in 1:length(prefixChoices)){
      if (prefixChoices[y]==selectedPrefix)
      {
        opt =  paste0(opt,"<option value=",prefixChoices[y]," selected>",names(prefixChoices)[y],"</option>;")
      }
      else
      {
        opt =  paste0(opt,"<option value=",prefixChoices[y],">",names(prefixChoices)[y],"</option>;");
      }
    }
    # selectInput("xPrefix", "x-Axis Type", choices = prefixChoices, selected = selectedPrefix)
    HTML(
      paste("<label class='control-label' for='dataTyp'>Select Data Type</label>","<select id='dataTyp' style='word-wrap:break-word; width: 100%;'>",opt,"</select>")
    )
  })
  
  
  
  ### Download data
  output$downloadExp <- downloadHandler(
    
    # This function returns a string which tells the client
    # browser what name to use when saving the file.
    filename = function() {
      paste0("data_",input$mdataSource,"_",input$dataType,".txt")
    },
    # filename = function() {
    #   paste0(input$mdataSource,"_",input$dataType,".zip")
    # },
    # This function should write data to a file given to it by
    # the argument 'file'.
    content = function(file) {
      
      wdata=srcContent[[input$mdataSource]][["molPharmData"]][[input$dataType]]
      rownames(wdata)=substr(rownames(wdata),4,nchar(rownames(wdata)))
      # Write to a file specified by the 'file' argument
      #write.table(srcContent[[input$mdataSource]][["molPharmData"]][[input$dataType]], file, sep = "\t",col.names = NA)      
      write.table(wdata, file, sep = "\t", col.names = NA)  
      
      # myname=paste0(input$mdataSource,"_",input$dataType,".txt")
      # write.table(srcContent[[input$mdataSource]][["molPharmData"]][[input$dataType]], myname, sep = "\t",
      #             col.names = NA)
      # configSelect <- metaConfig[[input$mdataSource]][["packages"]][[1]][["MetaData"]]
      # jsonFrame <- as.data.frame(configSelect)
      # 
      # colnames(jsonFrame) <- c("DataType", "Description", "Units", 
      #                          "Platform/Assay", "PubMed Ref. ID")
      # write.table(t(jsonFrame[which(jsonFrame$DataType==input$dataType),]),"footnotes.txt",sep="\t",col.names=F)
      # zip(zipfile=file, files=c(myname,"footnotes.txt"))
    }
  )
##
  ### Download footnotes
  output$downloadFoot <- downloadHandler(
    
    # This function returns a string which tells the client
    # browser what name to use when saving the file.
    filename = function() {
      paste0("footnotes_",input$mdataSource,"_",input$dataType,".csv")
    },
    # filename = function() {
    #   paste0(input$mdataSource,"_",input$dataType,".zip")
    # },
    # This function should write data to a file given to it by
    # the argument 'file'.
    content = function(file) {
      
      # Write to a file specified by the 'file' argument
      #write.table(srcContent[[input$mdataSource]][["molPharmData"]][[input$dataType]], file, sep = "\t",
      #            col.names = NA)      
      
      # myname=paste0(input$mdataSource,"_",input$dataType,".txt")
      # write.table(srcContent[[input$mdataSource]][["molPharmData"]][[input$dataType]], myname, sep = "\t",
      #             col.names = NA)
      configSelect <- metaConfig[[input$mdataSource]][["packages"]][[1]][["MetaData"]]
      jsonFrame <- as.data.frame(configSelect)
       
      colnames(jsonFrame) <- c("DataType", "Description", "Units", 
                                "Platform/Assay", "PubMed Ref. ID")
      mydata=t(jsonFrame[which(jsonFrame$DataType==input$dataType),])
      colnames(mydata)=paste("footnotes for data source:",input$mdataSource)
       write.csv(mydata,file)
       #write.table(t(jsonFrame[which(jsonFrame$DataType==input$dataType),]),file,sep="\t",col.names=F)
       # zip(zipfile=file, files=c(myname,"footnotes.txt"))
    }
  )
  
  ### Download Synonym table
  output$downloadSyn <- downloadHandler(
    
    # This function returns a string which tells the client
    # browser what name to use when saving the file.
    filename = function() {
      paste0("Table_","Drugs_Synonyms_cdb",".txt")
    },
    
     content = function(file) {
      
      write.table(findDrugIDs("*"), file, sep = "\t", row.names = F,quote=F)  
      
     }
  )
  ##
  output$downloadDataComp <- downloadHandler(
    
    # This function returns a string which tells the client
    # browser what name to use when saving the file.
    filename = function() {
      if (input$patternComparisonSeed == "xPattern"){
        
        pcDataset <- input$xDataset
        pcType <- input$xPrefix
        pcId <- input$xId
      } 
      else{
        
        pcDataset <- input$yDataset
        pcType <- input$yPrefix
        pcId <- input$yId
      
      }
        
      paste0("Pattern_Comp_all_cdb_",input$patternComparisonSeed,"_",pcDataset,"_",pcType,"_",pcId,"_",input$patternComparisonType,".txt")
    },
    
    content = function(file) {
      
      write.table(PatternCompTable(), file, sep = "\t", row.names = F,quote=F)  
      
    }
  )
  
  
  ###
  
  output$xPrefixUi <- renderUI({
  	srcContent <- srcContentReactive()
  	
  	# The last selected (data type) prefix is recorded in 
  	# globalReactiveValues$xPrefix whenever xData() is updated. When the data set 
  	# is changed, we try to use this same data type prefix, if it is available.
  	prefixChoices <- srcContent[[input$xDataset]][["featurePrefixes"]]
  	selectedPrefix <- globalReactiveValues$xPrefix
  	if ((is.null(selectedPrefix)) || (!(selectedPrefix %in% prefixChoices))){
  		selectedPrefix <- srcContent[[input$xDataset]][["defaultFeatureX"]]
  		if (is.na(selectedPrefix)) selectedPrefix <- srcContent[[input$xDataset]][["defaultFeatureY"]]
  	}
  	opt = "";
  	for(y in 1:length(prefixChoices)){
  	  if (prefixChoices[y]==selectedPrefix)
  	  {
  	    opt =  paste0(opt,"<option value=",prefixChoices[y]," selected>",names(prefixChoices)[y],"</option>;")
  	  }
  	  else
  	  {
  	    opt =  paste0(opt,"<option value=",prefixChoices[y],">",names(prefixChoices)[y],"</option>;");
  	  }
  	}
  	# selectInput("xPrefix", "x-Axis Type", choices = prefixChoices, selected = selectedPrefix)
  	HTML(
  	  paste("<label class='control-label' for='xPrefix'>x-Axis Data Type</label>","<select id='xPrefix' style='word-wrap:break-word; width: 100%;'>",opt,"</select>")
  	)
  })

  output$yPrefixUi <- renderUI({
  	srcContent <- srcContentReactive()
  	prefixChoices <- srcContent[[input$yDataset]][["featurePrefixes"]]
  	selectedPrefix <- globalReactiveValues$yPrefix
  	if ((is.null(selectedPrefix)) || (!(selectedPrefix %in% prefixChoices))){
  		selectedPrefix <- srcContent[[input$yDataset]][["defaultFeatureY"]]
  	}
  	opt = "";
  	for(y in 1:length(prefixChoices)){
  	  if (prefixChoices[y]==selectedPrefix)
  	  {
  	    opt =  paste0(opt,"<option value=",prefixChoices[y]," selected>",names(prefixChoices)[y],"</option>;")
  	  }
  	  else
  	  {
  	    opt =  paste0(opt,"<option value=",prefixChoices[y],">",names(prefixChoices)[y],"</option>;");
  	  }
  	}
  	#selectInput("yPrefix", "y-Axis Type", choices = prefixChoices, selected = selectedPrefix)
  	HTML(
  	  paste("<label class='control-label' for='yPrefix'>y-Axis Data Type</label>","<select id='yPrefix' style='word-wrap:break-word; width: 100%;'>",opt,"</select>")
  	)
  })
  
  output$xAxisRangeUi <- renderUI({
  	srcContent <- srcContentReactive()
  	
  	# Note: req() ensures values are available or 'truthy' (not NULL, "", FALSE, empty, etc.),
  	# returning the value if so; otherwise the operation is stopped with a silent exception.
  	# The idea is to exit quietly if inputs are momentarily in an invalid state, as might
  	# occur when the app is first loading, etc.
  	valRange <- srcContent[[req(input$xDataset)]][["featureValRanges"]][[req(input$xPrefix)]]
  	
  	xData <- NULL
  	try(xData <- xData())
  	if (is.null(xData)){
  		xInitSliderVals <- valRange
  	} else{
  		xDataRange <- range(xData$data, na.rm = TRUE)
  		delta <- max((0.05 * (xDataRange[2] - xDataRange[1])), 0.1)
  		xInitSliderVals <- c((xDataRange[1] - delta), (xDataRange[2] + delta))
  	}
  	
  	sliderInput("xAxisRange", "x-Axis Range", 
  							min = valRange[1], max = valRange[2], value = xInitSliderVals, step = 0.5)
  })
  
  output$yAxisRangeUi <- renderUI({
  	srcContent <- srcContentReactive()
  	
 		# Note: see comment in output#xAxisRangeUi explaining the use of req().
  	valRange <- srcContent[[req(input$yDataset)]][["featureValRanges"]][[req(input$yPrefix)]]
  	
  	yData <- NULL
  	try(yData <- yData())
  	if (is.null(yData)){
  		yInitSliderVals <- valRange
  	} else{
  		yDataRange <- range(yData$data, na.rm = TRUE)
  		delta <- max((0.05 * (yDataRange[2] - yDataRange[1])), 0.1)
  		yInitSliderVals <- c((yDataRange[1] - delta), (yDataRange[2] + delta))
  	}
  	
  	sliderInput("yAxisRange", "y-Axis Range", 
  							min = valRange[1], max = valRange[2], value = yInitSliderVals, step = 0.5)
  })

  # output$xIdUi <- renderUI({
  # 	updateSelectizeInput(session, inputId='xId', choices=xIdChoices(), selected="SLFN11", server=TRUE)
  # 	selectizeInput('xId', label="ID: (e.g. 94600 or SLFN11); Case-Sensitive", choices=NULL, options=list(maxOptions=5))
  # })
  # 
  # output$yIdUi <- renderUI({
  # 	updateSelectizeInput(session, inputId='yId', choices=yIdChoices(), selected="94600", server=TRUE)
  # 	selectizeInput('yId', label="ID: (e.g. 94600 or SLFN11); Case-Sensitive", choices=NULL, options=list(maxOptions=5))
  # })
  
  output$selectTissuesUi <- renderUI({
  	srcContent <- srcContentReactive()
  	tissueToSamplesMap <- srcContent[[input$xDataset]][["tissueToSamplesMap"]]
  	tissueTypes <- sort(unique(names(tissueToSamplesMap)))
  	
  	#if (input$tissueSelectionMode == "Include"){
  	#	selectInput("selectedTissues", label = NULL, choices=c("all", tissueTypes),
  	#							multiple=TRUE, selected="all")
  	#} else{ # input$tissueSelectionMode == "Exclude"
  	#	selectInput("selectedTissues", label = NULL, choices=c("none", tissueTypes),
  	#							multiple=TRUE, selected="none")
  	#}
  	
  	## new code
  	if (input$tissueSelectionMode == "To include"){
  	       choices=c("all", tissueTypes); mysel="all"
  	              
  	} else{ # input$tissueSelectionMode == "To exclude"
  	       choices=c("none", tissueTypes); mysel="none"
  	}
  	opt = "";
  	for(y in 1:length(choices)){
  	  # style works only for browser Chrome
  	  if (choices[y]==mysel)
  	  {
  	    opt =  paste0(opt,"<option style='white-space: pre-wrap' selected>",choices[y],"</option>;");
  	  }
  	  else {
  	  opt =  paste0(opt,"<option style='white-space: pre-wrap'>",choices[y],"</option>;");
  	  }
  	}
  	HTML(
  	  paste("<label class='control-label' for='selectedTissues'>Select Tissue/s of Origin</label>","<select id='selectedTissues' style='word-wrap:break-word; width: 100%;' multiple>",opt,"</select>")
  	)
  	
  	## 
  })

  output$showColorTissuesUi <- renderUI({
  	#tissueChoices <- analysisTissueTypes()
  	tissueChoices <- getSampleSetTissueTypes(
  		sampleSet = rownames(req(matchedCellLinesTab())), 
  		dataSource = input$xDataset, 
  		srcContent = srcContentReactive()
  		) 
  	opt = "";
  	for(y in 1:length(tissueChoices)){
  	    # style works only for browser Chrome
  	    opt =  paste0(opt,"<option style='white-space: pre-wrap'>",tissueChoices[y],"</option>;");
  	}
  	HTML(
  	  paste("<label class='control-label' for='showColorTissues'>Select Tissues to Color</label>","<select id='showColorTissues' style='word-wrap:break-word; width: 100%;' multiple>",opt,"</select>")
  	)
  	# selectInput("showColorTissues", "Tissues to Color",choices = tissueChoices, multiple = TRUE)
	})
  
  output$ipAddress <- renderText({
  	# debug
  	text <- readLines("http://api.ipify.org")
  })

  #----[observers]-----------------------------------------------------------------------

  # Observe reactive variable and send message to Javascript code
  observe({
  	if(isPackageLoadingComplete()) {
  if (is.null(appConfig$modal))
  	  session$sendCustomMessage(type='showLoading', list(show=FALSE))
  	 else 
  	  session$sendCustomMessage(type='showSkip', list(show=TRUE))
     }
  })
	
  #-----[NavBar Tab Server Code]---------------------------------------------------------
  rm <- callModule(regressionModels, "rm", srcContentReactive = srcContentReactive,
  								 appConfig = appConfig, oncolor=oncolor)

})
#-----[end of shinyServer()]-----------------------------------------------------------------------
