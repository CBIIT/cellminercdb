#-----[NavBar Tab: Regression Models (UI code)]----------------------------------------------------

regressionModelsInput <- function(id, dataSourceChoices) {
	# Create a namespace function using the provided id
	ns <- NS(id)
	voptions = "";
	for(y in 1:length(dataSourceChoices)){
	  if (dataSourceChoices[y]=="nci60")
	  {
	    voptions =  paste0(voptions,"<option value=",dataSourceChoices[y]," selected>",names(dataSourceChoices)[y],"</option>;")
	  }
	  else
	  {
	    voptions =  paste0(voptions,"<option value=",dataSourceChoices[y],">",names(dataSourceChoices)[y],"</option>;");
	  }
	}
	tabPanel("Regression Models",
					 fluidPage(
					 	sidebarLayout(
					 		sidebarPanel(
					 			width=3, 
					 			tags$div(
					 				id="rm_input_container",
					 				tags$a(id="skiplink"),
					 				#selectInput(ns("dataset"), "Dataset", choices=dataSourceChoices, selected = "nci60"),
					 				HTML(
					 				  paste("<label class='control-label' for=",ns("dataset"),">Cell Line Set</label>","<select id=",ns("dataset"),">",voptions,"</select>")
					 				),
					 				br(),br(),
					 				uiOutput(ns("responseDataTypeUi")),
					 				textInput(ns("responseId"), "Response Identifier:", "topotecan"),
					 				br(),
					 				uiOutput(ns("predDataTypesUi")),
					 				sliderInput(ns("minPredValueRange"), 
					 										"Minimum Predictor Range (for first listed data type):", 
					 										min=0, max=5, value=0, step = 0.25),
					 				textInput(ns("predIds"), "Predictor Identifiers: (Case-Sensitive, e.g. SLFN11 BPTF)", "SLFN11 BPTF"),
					 				br(),
					 				radioButtons(ns("tissueSelectionMode"), "Select Tissues", c("To include", "To exclude")),
					 				uiOutput(ns("selectTissuesUi")),
					 				#selectInput(ns("algorithm"), "Algorithm", 
					 				#						choices=c("Linear Regression", "Lasso"), 
					 				#					selected = "Linear Regression"),
					 				HTML(
			   		 				  paste("<br><label class='control-label' for=",ns("algorithm"),">Algorithm</label>","<select id=",ns("algorithm"),"><option selected>Linear Regression</option><option>Lasso</option></select>")
					 				),
					 				# Only show these panels if selected algorithm is Lasso.
					 				conditionalPanel(
					 					# condition must be a Javascript expression.
					 					condition = paste0("input['", ns("algorithm"), "'] == 'Lasso'"),
					 					uiOutput(ns("selectInputGeneSetsUi"))),
					 				conditionalPanel(
					 					condition = paste0("input['", ns("algorithm"), "'] == 'Lasso'"),
					 					numericInput(ns("maxNumPredictors"), 
					 											 "Maximum Number of Predictors", value = 4, 
					 											 min = 1, max = 100, step = 1))
					 			)
					 		),
					 		mainPanel(
					 			uiOutput(ns('tabsetPanel'))
					 		)
					 	)
					 )						 
	)
}
#-----[NavBar Tab: Regression Models (Server code)]------------------------------------------------
regressionModels <- function(input, output, session, srcContentReactive, appConfig, oncolor) {
	
	#----[Utility Functions]----------------------------------------------------------------
	# TO DO: Move to rcellminerUtils (?)
	getValueRange <- function(x, lowQtl = 0.05, highQtl = 0.95, naRm = TRUE){
		xRange <- quantile(x, probs = c(lowQtl, highQtl), na.rm = TRUE)
		return(unname(xRange[2] - xRange[1]))
	}
	
	# TO DO: Move to appUtils.R (?)
	getFeatureDataMatrix <- function(dataSetName, dataTypes, srcContent, rmNaCols = TRUE,
																	 responseVec = NULL, geneSetNames = NULL, 
																	 minValueRange = 0, valueRangeLowQtl = 0.05,
																	 valueRangeHighQtl = 0.95) {
		# Get gene set-associated genes, if necessary.
		genes <- character(0)
		if ((!is.null(geneSetNames)) && (!("All Genes" %in% geneSetNames))) {
			geneSetNames <- intersect(geneSetNames, names((geneSetPathwayAnalysis::geneSets)))
			if (length(geneSetNames) > 0) {
				genes <- sort(unique(c(geneSetPathwayAnalysis::geneSets[geneSetNames], 
															 recursive = TRUE)))
			}
		} 
		
		featureDataMat <- NULL
		for (dType in dataTypes) {
			tmpData <- srcContent[[dataSetName]][["molPharmData"]][[dType]]
			if (!is.null(responseVec)){
				# This ensures that any cell line set restrictions associated with the 
				# response vector (e.g. by tissue type) are applied to the feature matrix.
				tmpData <- tmpData[, names(responseVec), drop = FALSE]
			}
			
			# ----[restrict to selected gene set genes if necessary]--------------------
			if ((isGeneProtDataType(dType)) && (length(genes) > 0)) {
				dataTypeGenes <- intersect(paste0(dType, genes), rownames(tmpData))
				if (length(dataTypeGenes) > 0){
					tmpData <- tmpData[dataTypeGenes, , drop = FALSE]
				} else {
					tmpData <- NULL
				}
			}
			# --------------------------------------------------------------------------
			
			featureDataMat <- rbind(featureDataMat, tmpData)
		}
		
		# Remove columns with missing values (if requested).
		if ((nrow(featureDataMat) > 0) && rmNaCols) {
			hasNaInCol <- apply(featureDataMat, MARGIN = 2, FUN = function(x) {
				any(is.na(x))
			})
			featureDataMat <- featureDataMat[, !hasNaInCol, drop = FALSE]
		}
		
		# Filter features with limited range (if requested).
		if (minValueRange > 0) {
			valRange <- apply(featureDataMat, MARGIN = 1, FUN = getValueRange,
												lowQtl = valueRangeLowQtl, highQtl = valueRangeHighQtl)
			i <- which(valRange >= minValueRange)
			featureDataMat <- featureDataMat[i, , drop = FALSE]	
		}
		
		return(featureDataMat)
	}
	
	# TO DO: Move to appropriate general functions file/package.
	scaleDataForHeatmap <- function(dat, scaleByRow = FALSE){
		if (is.vector(dat)){
			vecNames <- names(dat)
			dat <- matrix(dat, nrow = 1, ncol = length(dat))
			rownames(dat) <- "tmp1"
			colnames(dat) <- vecNames
		}
		
		scaledDat <- matrix(NA, nrow = nrow(dat), ncol = ncol(dat))
		rownames(scaledDat) <- rownames(dat)
		colnames(scaledDat) <- colnames(dat)
		rowDataTypes <- unname(rcellminer::getMolDataType(rownames(dat)))
		dataTypes <- unique(rowDataTypes)
		
		getQuantiles <- function(dataType){
			if (dataType == "mut"){
				loQtl <- 0
				hiQtl <- 1
			} else{
				loQtl <- 0.05
				hiQtl <- 0.95
			}
			return(c(loQtl, hiQtl))
		}
		
		if (scaleByRow){
			for (i in seq_len(nrow(scaledDat))){
				valQtls <- quantile(x = as.numeric(dat[i, ]), 
					probs = getQuantiles(rowDataTypes[i]), na.rm = TRUE)
				dTypeMin <- valQtls[1]
				dTypeMax <- valQtls[2]
				dTypeRange <- dTypeMax - dTypeMin
				if (dTypeRange != 0){
					tmp <- (dat[i, ] - dTypeMin) / dTypeRange
					tmp[which(tmp < 0)] <- 0
					tmp[which(tmp > 1)] <- 1
					scaledDat[i, ] <- tmp
				} else{
					scaledDat[i, ] <- 0.5
				}
			}
		} else{
			for (dType in dataTypes){
				indexSet <- which(rowDataTypes == dType)
				valQtls <- quantile(x = as.numeric(dat[indexSet, ]), 
					probs = getQuantiles(dType), na.rm = TRUE)
				dTypeMin <- valQtls[1]
				dTypeMax <- valQtls[2]
				dTypeRange <- dTypeMax - dTypeMin
				for (i in indexSet){
					if (dTypeRange != 0){
						tmp <- (dat[i, ] - dTypeMin) / dTypeRange
						tmp[which(tmp < 0)] <- 0
						tmp[which(tmp > 1)] <- 1
						scaledDat[i, ] <- tmp
					} else{
						scaledDat[i, ] <- 0.5
					}
				}
			}
		}
		
		return(scaledDat)			 
	}
	
	isFeatureSelectionAlgorithm <- function(){
		featureSelectionAlgorithms <- "Lasso" # TO DO: (perhaps) set from configuration.
		if (input$algorithm %in% featureSelectionAlgorithms){
			return(TRUE)
		} else{
			return(FALSE)
		}
	}
	
	summary.LassoResults <- function(x) {
		stopifnot(inherits(x, "LassoResults"))
		cat("\n\n")
		cat("Predictors Selected by Lasso (Ordered by Regression Coefficient Magnitude)")
		cat("\n")
		cat("--------------------------------------------------------------------------")
		cat("\n")
		predSummaryTab <- data.frame(PREDICTOR = names(x$predictorWts),
																 COEFFICIENT = x$predictorWts,
																 stringsAsFactors = FALSE)
		rownames(predSummaryTab) <- NULL
		print(predSummaryTab)
		cat("--------------------------------------------------------------------------")
		cat("\n")
		cat(paste0("Multiple R-squared: ", round(x$rSquared, 4)))
	}

	#----[Reactive Variables]---------------------------------------------------------------

	# Returns a data frame with data suitable for predictive modeling (updated according
	# to current user selections). The first column indicates the cell line, with
	# subsequent columns containing response and predictor variables.
	# Note: Observations with missing values (in predictor or reponse variables) are removed.
	inputData <- reactive({
		shiny::validate(need(input$responseId != "", "Please enter a response variable."))
		shiny::validate(need(length(input$selectedTissues) > 0, "Please select tissue types."))
		shiny::validate(need(length(input$predDataTypes) > 0,
												 "Please select one or more predictor data types."))
##
		originalId <- trimws(input$responseId)
##
		responseId <- getMatchedIds(input$responseDataType, trimws(input$responseId), 
																input$dataset, srcContent = srcContentReactive())
		if (length(responseId) == 0) {
			shiny::validate(need(FALSE, 
				paste("ERROR:", paste0("(", input$responseDataType, ") ", input$responseId), "not found. Please use the Univariate Analyses Search IDs tab to find available IDs for each dataset.")))
		} else {
			if (length(responseId) > 1){
				warningMsg <- paste0("Other identifiers matching response variable ID: ",
														 paste0(responseId[-1], collapse = ", "), ".")
				showNotification(warningMsg, duration = 10, type = "message")
				responseId <- responseId[1]
			}
## added original Id
			yData <- getFeatureData(input$responseDataType, responseId, input$dataset, 
															srcContent = srcContentReactive(),originalId)
			yData$data <- na.exclude(yData$data)
		}
		
		dataTab <- data.frame(CellLine = names(yData$data), stringsAsFactors = FALSE)
		rownames(dataTab) <- dataTab$CellLine
		dataTab[, yData$uniqName] <- yData$data
		
		featurePrefixes <- unname(srcContentReactive()[[input$dataset]][["featurePrefixes"]])
		predIds <- stringr::str_split(stringr::str_trim(input$predIds), pattern = "\\s+")[[1]] 
		for (id in predIds) {
			idPrefix <- rcellminer::getMolDataType(id)
			if (idPrefix %in% featurePrefixes) {
				#------------------------------------------------------------------------------------
				# This is for predictors ids of the form "expSLFN11". The explicit data type
				# prefix indicates that one (and only one) specified data type is to be 
				# retrieved.
				id <- rcellminer::removeMolDataType(id) # e.g. "expSLFN11" --> "SLFN11"
				if (validateEntry(idPrefix, id, input$dataset, srcContentReactive())){
## add new parameter original id
					xData <- getFeatureData(idPrefix, id, input$dataset, srcContentReactive(),id)
					xData$data <- xData$data[names(yData$data)] # Match lines w/non-NA response data.
					dataTab[, xData$uniqName] <- xData$data
				} else {
					warning(paste0(idPrefix, id), " not found.")
				}
				#------------------------------------------------------------------------------------
			} else {
				#------------------------------------------------------------------------------------
				# This is for predictor ids of the form "SLFN11". An attempt will be made to
				# retrieve data for this predictor from all data types specified in
				# input$predDataTypes.
				for (dataType in input$predDataTypes) {
					if (validateEntry(dataType, id, input$dataset, srcContentReactive())) {
## added new parameter
						xData <- getFeatureData(dataType, id, input$dataset, srcContentReactive(),id)
						xData$data <- xData$data[names(yData$data)] # Match lines w/non-NA response data.
						dataTab[, xData$uniqName] <- xData$data
					} else {
						warning(paste0(dataType, id), " not found.")
					}
				}
				#------------------------------------------------------------------------------------
			}
		}
		
		dataTab <- na.exclude(dataTab)
		
		if (!isFeatureSelectionAlgorithm()) {
			shiny::validate(need(ncol(dataTab) > 2,
													 paste("ERROR: No data for specified predictors.")))
		}
		
		shiny::validate(need(nrow(dataTab) > 0,
												 paste("ERROR: All cell lines have missing response or predictor data.")))
		
		# Filter cell lines (rows) by tissue type (if requested) ----------------------------------------
		# Accepting all available tissue types corrsponds to either,
		# selecting 'all' if the Include radio button is selected, OR
		# selecting 'none' if the Exclude radio button is selected.
		allTissuesSelected <- ("all" %in% input$selectedTissues) || ("none" %in% input$selectedTissues)
		if (!allTissuesSelected) {
			selectedTissueSamples <- getTissueTypeSamples(tissueTypes = input$selectedTissues, 
																										dataSource = input$dataset,
																										srcContent = srcContentReactive())
			if (input$tissueSelectionMode == "To include") {
				matchedLines <- intersect(rownames(dataTab), selectedTissueSamples)
			} else { # input$tissueSelectionMode == "Exclude"
				matchedLines <- setdiff(rownames(dataTab), selectedTissueSamples)
			}
			shiny::validate(need(matchedLines > 0,
				paste("ERROR: no data available with specified tissue type criteria.")))
			
			dataTab <- dataTab[matchedLines, , drop = FALSE]
		}
		# -----------------------------------------------------------------------------------------------
		
		# Filter predictors by value range (if requested) -----------------------------------------------
		# Note: this must follow the tissue type-based filtering done above, 
		# or the range checks will not apply to the ultimately retained cell lines.
		# Iteration detail: the predictor data begins in column 3, 
		#  after the cell line name and response value columns.
		# Iteration detail: (As noted in the interface), the predictor range-based filtering
		#   is only done for predictors of the first specified predictor data type.
		minValueRange <- input$minPredValueRange
		rangeFilterDataType <- input$predDataTypes[1]
		
		if ((minValueRange > 0) && (ncol(dataTab) >= 3)){
			colsToDrop <- NULL
			for (j in (3:ncol(dataTab))) {
				predictorDataType <- rcellminer::getMolDataType(colnames(dataTab)[j])
				if (predictorDataType != rangeFilterDataType) {
					next
				}
				
				if (getValueRange(dataTab[, j, drop = TRUE]) < minValueRange) {
					colsToDrop <- c(colsToDrop, j)
				}
			}
			
			if (length(colsToDrop) > 0) {
				dataTab[, colsToDrop] <- NULL
			}
		}
		
		if (!isFeatureSelectionAlgorithm()) {
			shiny::validate(need(ncol(dataTab) > 2,
				paste("ERROR: None of the selected predictors have the specified minimum value range.")))
		}
		# -----------------------------------------------------------------------------------------------
		
		return(dataTab)
	})
	
	
	# Provides an a list object with response variable-related data, including numeric data,
	# data type prefix, data source, and plot label.
	# Note: Observations with missing values (in predictor or reponse variables) are removed.
	# Note: The numeric data always matches what is provided within the inputData() data frame.
	rmResponseData <- reactive({
		dataTab <- inputData() # Validation already done.
		
		yData <- list()
		yData$name <- colnames(dataTab)[2] # 1st column: cell line name, 2nd column: response variable.
		yData$data <- setNames(dataTab[, yData$name], rownames(dataTab))
		yData$plotLabel <- yData$name
		yData$uniqName  <- yData$name
		yData$dataSource <- strsplit(yData$name, split = "_")[[1]][2]
		
		return(yData)
	})
	
	# Returns a list with (at least) the following elements:
	# predictorWts: a named vector indicating the intercept term and predictor coefficients
	#  in the regression model.
	# algorithm: the name of the algorithm.
	# predictedResponse: a vector of predicted reponse values.
	# cvPredictedResponse: a vector of cross-validation predicted response values (can be
	#   empty if cross validation is not supported).
	# techDetails: an object that can be passed to a suitable summary() function to provide
	#   a printed summary of technical results.
	algoResults <- reactive({
		srcContent <- srcContentReactive()
		dataTab <- inputData()
		rmAlgoResults <- list()
		
		shiny::validate(need(nrow(dataTab) >= 10, "Insufficient number of cell lines."))
		
		# Note: refactor this function to gather and validate parameters, which are then passed
		# to a specialized implementation function that returns a standard format algoResults
		# list object.
		if (input$algorithm == "Linear Regression"){
			lmData <- dataTab[, -1, drop = FALSE] # First column has cell line names
			
			# Note: Handling issues caused by variable names with spaces 
			# or other characters that cannot be used within a formula.
			colnames(lmData) <- stringr::str_replace_all(colnames(lmData), 
				pattern = "[-|/| ]", replacement = "")
			
			lmFormula <- as.formula(paste0(colnames(lmData)[1], " ~ ."))
			lmFit <- lm(lmFormula, lmData)
			lmCvFit <- rcellminerElasticNet::getLmCvFit(X = as.matrix(lmData[, -1, drop = FALSE]), 
																									y = lmData[, 1, drop = TRUE], nFolds = 10, nRepeats = 1)
			# -----[assemble results]---------------------------------------------------
			rmAlgoResults$algorithm <- "Linear Regression"
			rmAlgoResults$predictorWts <- coef(lmFit)
			rmAlgoResults$predictedResponse <- predict(lmFit)
			rmAlgoResults$cvPredictedResponse <- lmCvFit$cvPred
			rmAlgoResults$techDetails <- lmFit
			rmAlgoResults$eqnStr <- getLmEquationString(rmAlgoResults$predictorWts)
			
			stopifnot(identical(names(rmAlgoResults$predictedResponse), rownames(lmData)))
			stopifnot(identical(names(rmAlgoResults$cvPredictedResponse), rownames(lmData)))
			# --------------------------------------------------------------------------
		} else if (input$algorithm == "Lasso"){
			# TO DO:
			# --- Consider filtering candidate predictors by data range.
			# ----[enable progress bar]--------------------------------------------------
			progress <- shiny::Progress$new()
			progress$set(message = "Computing Lasso Results ... ", value = 0)
			# Close the progress when this reactive exits (even if there's an error).
			on.exit(progress$close())
			updateProgress <- function(detail = NULL) {
				progress$inc(amount = 1, detail = detail)
			}
			# ---------------------------------------------------------------------------
			shiny::validate(need(length(input$inputGeneSets) > 0,
													 "Please select one or more gene sets."))
			shiny::validate(need(input$maxNumPredictors > 0,
													 "Please set maximum number of predictors."))
			
			# First column has cell line names, second column has response data.
			lassoResponseVec <- setNames(dataTab[, 2, drop = TRUE], rownames(dataTab))
			lassoPredData <- t(getFeatureDataMatrix(dataSetName = input$dataset,
				dataTypes = input$predDataTypes, srcContent = srcContent,
				responseVec = lassoResponseVec, geneSetNames = input$inputGeneSets,
				minValueRange = input$minPredValueRange))
			
			#shiny::validate(need((!is.null(lassoPredData)) && (ncol(lassoPredData) > 0),
			# "Insufficient admissible predictor data to run lasso algorithm."))
			
			# Check if user has supplied starting predictors, and add their data if
			# it isn't already included.
			if (ncol(dataTab) > 2){
				userPredData <- as.matrix(dataTab[, -(1:2), drop = FALSE])
				# remove datasource suffix, e.g., "expSLFN11_nci60" ---> "expSLFN11"
				colnames(userPredData) <- unname(vapply(colnames(userPredData), function(x) {
					stringr::str_split(x, "_")[[1]][1]}, character(1)))
				featuresToAdd <- setdiff(colnames(userPredData), colnames(lassoPredData))
				
				if (length(featuresToAdd) > 0){
					userPredData <- userPredData[, featuresToAdd, drop = FALSE]
					
					# (1) Match cell lines between userPredData and lassoPredData (some lines
					# may have been dropped from the latter due to missing values for some
					# predictors).
					userPredData  <- userPredData[rownames(lassoPredData), , drop = FALSE]
					
					lassoPredData <- cbind(userPredData, lassoPredData)
					
					# (2) Remove any additional lines with missing predictor data 
					# (introduced by userPredData).
					hasNaValues <- apply(lassoPredData, MARGIN = 1, FUN = function(x){
						any(is.na(x))
					})
					lassoPredData <- lassoPredData[!hasNaValues, , drop = FALSE]
				}
			}
			
			# ---- handle edge case: response variable is in candidate predictor set ----
			# i.e., exclude response variable as a candidate predictor (of itself).
			responseVarName <- paste0(input$responseDataType, input$responseId)
			lassoPredData <- lassoPredData[, setdiff(colnames(lassoPredData), responseVarName), drop = FALSE]
			# ---------------------------------------------------------------------------
			
			shiny::validate(need((!is.null(lassoPredData)) && (ncol(lassoPredData) > 2),
			                     "Insufficient admissible predictor data to run lasso algorithm."))
			
			# Check and update: lines with missing predictor data may have been removed.
			shiny::validate(need(nrow(lassoPredData) > 10, 
													 "10 or more cell lines (with NA-free data for candidate predictor set) are required to run LASSO"))
			lassoResponseVec <- lassoResponseVec[rownames(lassoPredData)]
			
			#-----[glmnet]--------------------------------------------------------------
			set.seed (1)
			lassoCvOutput <- cv.glmnet(x = lassoPredData, y = lassoResponseVec, alpha=1)
			lassoIntercept <- coef(lassoCvOutput, s = "lambda.min")[1,1]
			lassoPredictorWts <- coef(lassoCvOutput, s = "lambda.min")[-1,1]
			lassoPredictorWts <- lassoPredictorWts[lassoPredictorWts != 0]
			
			shiny::validate(need(length(lassoPredictorWts) > 0, 
													 "No predictor variables selected by LASSO algorithm."))
			
			lassoPredictorWts <- lassoPredictorWts[order(abs(lassoPredictorWts), decreasing = TRUE)]
			
			# TO DO: Investigate implementation and behavior of glmnet pmax argument.
			lassoPredictorWts <- lassoPredictorWts[1:min(input$maxNumPredictors, length(lassoPredictorWts))]
			
			lassoPredictedResponse <- rcellminerElasticNet::predictWithLinRegModel(
				coeffVec = lassoPredictorWts, yIntercept = lassoIntercept, 
				newData = lassoPredData[, names(lassoPredictorWts), drop = FALSE])
			lassoPredResponseCor <- cor.test(lassoPredictedResponse, lassoResponseVec)$estimate
			#---------------------------------------------------------------------------

			# Make 'updated input' data frame: starting response and feature data
			# ultimately selected by Lasso.
			lassoModelData <- dataTab[names(lassoResponseVec), 1:2] # CellLine, Response.
			for (predId in names(lassoPredictorWts)){
				uniqPredId <- paste0(predId, "_", input$dataset)
				lassoModelData[, uniqPredId] <- lassoPredData[, predId]
			}
			
			lassoLmModelData <- lassoModelData[, -1] # dropping CellLine column
			lassoLmCvFit <- rcellminerElasticNet::getLmCvFit(
				X = as.matrix(lassoLmModelData[, -1, drop = FALSE]), 
				y = lassoLmModelData[, 1, drop = TRUE], nFolds = 10, nRepeats = 1)
			
			# TO DO : Augment results object and summary function for better
			# presentation of appropriate results.
			lassoResultsObj <- list()
			lassoResultsObj$predictorWts <- signif(lassoPredictorWts, 3)
			lassoResultsObj$rSquared <- lassoPredResponseCor^2
			class(lassoResultsObj) <- "LassoResults"
			
			# -----[assemble results]---------------------------------------------------
			rmAlgoResults$algorithm <- "Lasso"
			rmAlgoResults$predictorWts <- lassoPredictorWts
			rmAlgoResults$predictedResponse <- lassoPredictedResponse
			rmAlgoResults$cvPredictedResponse <- lassoLmCvFit$cvPred
			rmAlgoResults$techDetails <- lassoResultsObj
			rmAlgoResults$eqnStr <- getLmEquationString(rmAlgoResults$predictorWts)
			
			# Feature selection algorithms are expected to find additional 
			# predictors. The entry updates the starting inputData(), adding
			# data for selected predictors.
			rmAlgoResults$updatedInputData <- lassoModelData
			updateProgress()
			# --------------------------------------------------------------------------
		} else{
			shiny::validate(FALSE, paste("ERROR: Algorithm not available."))
		}
		
		return(rmAlgoResults)
	})
	
	# Returns a data frame with partial correlation-based pattern comparison results.
	parCorPatternCompResults <- eventReactive(input$computeParCors, {
		shiny::validate(need(length(input$pcGeneSets) > 0,
												 "Please select one or more gene sets."))
		shiny::validate(need(length(input$pcDataTypes) > 0,
												 "Please select one or more data types for computing partial correlations."))
		
		srcContent <- srcContentReactive()
		
		# TO DO: Refactor to more generic call to isFeatureSelectionAlgorithm().
		if (input$algorithm == "Lasso"){
			rmAlgoResults <- algoResults()
			if (!is.null(rmAlgoResults$updatedInputData)){
				# Feature selection algorithms will add additional predictors, and
				# may drop cell lines with missing values for candidate predictors,
				# requiring update of response data below.
				dataTab <- rmAlgoResults$updatedInputData
				
				# First column has cell line names, second column has response data.
				responseVec <- setNames(dataTab[, 2, drop = TRUE], rownames(dataTab))
			}
		} else{
			dataTab <- inputData()
			responseData <- rmResponseData()
			responseVec <- responseData$data
		}
	
		# TO DO: Right now, in the context of a feature selection algorithm, the user
		# may specify features, but these are not forced into the model - they are only
		# included in the starting candidate set.  This is a reasonable choice, but
		# the partial correlation will be computed with respect to whatever features
		# were selected by the algorithm.  This behavior may be non-intuitive because
		# the use-specified features will be visibly entered, but if the algorithm
		# does not select them, the partial correlation will not be excluding their
		# effect in terms of response prediction.
		currentPredictorData <- t(as.matrix(dataTab[, c(-1, -2), drop = FALSE]))
		shiny::validate(need(nrow(currentPredictorData) > 0,
												 "Initial predictors must be available for partial correlation computation."))
		
		# TO DO: investigate NA handling details in partial correlation computation.
		comparisonData <- getFeatureDataMatrix(dataSetName = input$dataset, 
			dataTypes = input$pcDataTypes, srcContent = srcContent,
			responseVec = responseVec, geneSetNames = input$pcGeneSets,
			minValueRange = input$minParCorDataValueRange,
			rmNaCols = FALSE)
		shiny::validate(need(nrow(comparisonData) > 0,
												 "No data (satisfying range criteria) for partial correlation computation."))

		N <- nrow(comparisonData)
		
		if ((appConfig$runParCorsInParallel) && 
				(N > appConfig$runParCorsInParallelThreshold)){
			library(foreach)
			library(doSNOW)
			library(parallel) # Needed for detectCores
			
			numCores <- detectCores()
			cl <- makeSOCKcluster(numCores)
			registerDoSNOW(cl)
			
			# Split comparison data matrix into a list of approximately equal-sized
			# matrices, based on the number of available cores.
			parSplitNum <- seq_len(nrow(comparisonData)) %% numCores
			compDataBatches <- split(as.data.frame(comparisonData), f = parSplitNum)
			compDataBatches <- lapply(compDataBatches, FUN = as.matrix)
			
			# ----[enable progress bar]--------------------------------------------------
			progress <- shiny::Progress$new()
			progress$set(message = "Computing Pattern Comparison Results ... ", value = 0)
			# Close the progress when this reactive exits (even if there's an error).
			on.exit(progress$close())
			updateProgress <- function(detail = NULL) {
				progress$inc(amount = 1/numCores, detail = detail)
			}
			opts <- list(progress=updateProgress)
			# ---------------------------------------------------------------------------
			pcResults <- foreach(i=1:length(compDataBatches), .combine = rbind,
													 .packages = "shiny", .options.snow=opts) %dopar% {
				tmp <- rcellminer::parCorPatternComparison(x = responseVec,
																									 Y = compDataBatches[[i]],
																									 Z = currentPredictorData)
				return(tmp)
			}
			stopCluster(cl)
			
			pcResults <- pcResults[order(pcResults$PARCOR, decreasing = TRUE), ]
			pcResults$PARCOR <- round(pcResults$PARCOR, 3)
			pcResults$PVAL   <- signif(pcResults$PVAL, 3)
			# pcResults$FDR <- p.adjust(pcResults[,"PVAL"],method="BH",nrow(pcResults))
			# pcResults$FDR   <- signif(pcResults$FDR, 3)
		} else{
			# ----[enable progress bar]--------------------------------------------------
			progress <- shiny::Progress$new()
			progress$set(message = "Computing Partial Correlation Results: ", value = 0)
			# Close the progress when this reactive exits (even if there's an error).
			on.exit(progress$close())
			updateProgress <- function(detail = NULL) {
				progress$inc(amount = 1/N, detail = detail)
			}
			# ---------------------------------------------------------------------------
			pcResults <- rcellminer::parCorPatternComparison(x = responseVec,
																											 Y = comparisonData,
																											 Z = currentPredictorData,
																											 updateProgress = updateProgress)
#			pcResults$FDR <- p.adjust(pcResults[,"PVAL"],method="BH",nrow(pcResults))
			pcResults$ANNOT <- ""
			
			for (i in seq_len(nrow(pcResults))){
			  name <- rcellminer::removeMolDataType(pcResults[i, "NAME"])
			  if (name %in% rownames(geneSetPathwayAnalysis::geneAnnotTab)){
			    pcResults[i, "ANNOT"] <-  geneSetPathwayAnalysis::geneAnnotTab[name, "SHORT_ANNOT"]
			  } 
			}
			pcResults$PARCOR <- round(pcResults$PARCOR, 3)
			pcResults$PVAL   <- signif(pcResults$PVAL, 3)
#			pcResults$FDR   <- signif(pcResults$FDR, 3)
			
			# DT::datatable(pcResults, rownames=FALSE, colnames=colnames(pcResults), filter='top', 
			#               style='bootstrap', selection = "none",
			#               options=list(lengthMenu = c(10, 25, 50, 100), pageLength = 10,language=list(paginate = list(previous = 'Previous page', `next`= 'Next page'))))
			
		}
		## new 
		dtype=substr(pcResults$NAME,1,3)
		nname=substr(pcResults$NAME,4,nchar(pcResults$NAME))
		pcResults=cbind(DATATYPE=dtype,NAME=nname,pcResults[,-1])
		
		##
		return(pcResults)
	})
	
	# Returns a data frame with information on genes that are differentially expressed
	# between high and low response cell lines.
	diffExpResultsTab <- eventReactive(input$computeDiffExp, {
		shiny::validate(need(length(input$deGeneSets) > 0,
												 "Please select one or more gene sets."))
		shiny::validate(need(length(input$deDataTypes) > 0,
												 "Please select one or more data types for differential expression analysis."))
		
		srcContent <- srcContentReactive()
		
		if (input$algorithm == "Lasso") {
			rmAlgoResults <- algoResults()
			if (!is.null(rmAlgoResults$updatedInputData)) {
				# Feature selection algorithms will add additional predictors, and
				# may drop cell lines with missing values for candidate predictors,
				# requiring update of response data below.
				dataTab <- rmAlgoResults$updatedInputData
				
				# First column has cell line names, second column has response data.
				responseVec <- setNames(dataTab[, 2, drop = TRUE], rownames(dataTab))
			}
		} else {
			responseData <- rmResponseData()
			responseVec <- responseData$data
		}
		
		dat <- getFeatureDataMatrix(dataSetName = input$dataset, 
																dataTypes = input$deDataTypes[1], srcContent = srcContent,
																responseVec = responseVec, geneSetNames = input$deGeneSets,
																minValueRange = input$minDiffExpDataValueRange,
																rmNaCols = FALSE)
		if (length(input$deDataTypes) > 1) {
			# Range restriction specified in the interface only applies to the first listed data type.
			dat <- rbind(dat,
									 getFeatureDataMatrix(dataSetName = input$dataset, 
									 										  dataTypes = input$deDataTypes[-1], srcContent = srcContent,
									 										  responseVec = responseVec, geneSetNames = input$deGeneSets,
									 										  minValueRange = 0, rmNaCols = FALSE))
		}
		shiny::validate(need(nrow(dat) > 0,
												 "No data (satisfying range criteria) for differential expression analysis."))
		stopifnot(identical(names(responseVec), colnames(dat)))
		
		# Order data matrix columns (cell lines) by decreasing response values.
		dat <- dat[, order(responseVec, decreasing = TRUE), drop =  FALSE]
		N <- nrow(dat)
		numHiLoCols <- min(input$numHiLoResponseLines, floor(ncol(dat)/2))
		hiResCols <- 1:numHiLoCols
		loResCols <- (ncol(dat) - numHiLoCols + 1):ncol(dat)
		
		deResults <- data.frame(NAME = rownames(dat), PVAL = NA, QVAL = NA,
														stringsAsFactors = FALSE)
		
		progress <- shiny::Progress$new()
		progress$set(message = "Computing Differential Expression Results: ", value = 0)
		# Close the progress when this reactive exits (even if there's an error).
		on.exit(progress$close())
	
		for (i in seq_len(N)) {
			tmp <- wilcox.test(x = dat[i, hiResCols, drop = TRUE], y = dat[i, loResCols, drop = TRUE])
			deResults[i, "PVAL"] <- tmp$p.value
			progress$inc(amount = 1/N)
		}
		deResults$QVAL <- p.adjust(deResults$PVAL, method = "fdr")
		deResults <- deResults[order(deResults$QVAL, decreasing = FALSE), , drop = FALSE]
		
		return(deResults)
	})
	
	
	# Returns a data frame with gene set enrichment results for genes differentially expressed
	# between high and low response cell lines.
	enrichmentResultsTab <- eventReactive(input$computeEnrichment, {
		diffExpResults <- NULL
		tryCatch(expr = {
			# Note: this try/catch construct is needed to allow the error checking
			# and reporting below. Otherwise, a silent error results when diffExpResultTab()
			# is called before the differential expression analysis is run.
			diffExpResults <- diffExpResultsTab()
		}, error = function(e) { 
			# Ignore: error will be detected by retained NULL value for diffExpResults.
		})
		
		shiny::validate(need((!is.null(diffExpResults)) && (nrow(diffExpResults) > 0),
			"Please run the differential expression analysis to derive input for the gene set enrichment analysis."))
		diffExpFdr <- suppressWarnings(as.numeric(input$enDiffExpFdr))
		shiny::validate(need((!is.na(diffExpFdr)) && (diffExpFdr > 0) && (diffExpFdr <= 1),
			"Please enter a number between 0 and 1 for the differential expression analysis FDR threshold."))
		
		diffExpResults <- diffExpResults[which(diffExpResults$QVAL < diffExpFdr), , drop = FALSE]
		
		enResults <- clusterProfiler::enricher(
			gene = unique(rcellminer::removeMolDataType(diffExpResults$NAME)),
			qvalueCutoff = 0.2,
			TERM2GENE = geneSetPathwayAnalysis::emGmt, 
			minGSSize = 3, 
			maxGSSize = 500
		)@result

		return(enResults)
	})
	
	#--------------------------------------------------------------------------------------
	
	#----[Show 2D Actual vs. Predicted Response Scatter Plot in 'Plot' Tab]----------------
	output$plot <- renderPlotly({
		rmAlgoResults <- algoResults()
		
		responseData <- rmResponseData()
		if (!is.null(rmAlgoResults$updatedInputData)){
			# Feature selection algorithms will add additional predictors, and
			# may drop cell lines with missing values for candidate predictors,
			# requiring update of response data below.
			updatedInputData <- rmAlgoResults$updatedInputData
			
			# First column has cell line names, second column has response data.
			responseData$data <- setNames(updatedInputData[, 2, drop = TRUE], 
																		rownames(updatedInputData))
		}
		
		## new labeling --------------------------------
		idd=unlist(strsplit(responseData$name,"_"))[1]
		idd=substr(idd,4,nchar(idd))
		srd=unlist(strsplit(responseData$name,"_"))[2]
		pprefix=substr(responseData$name,1,3)
		labs=metaConfig[[srd]][["displayName"]]
		responseData$plotLabel <- paste0("Observed ",idd, " (", pprefix, ", ", labs, ")")
		## ---------------------------------------------
		
		predResponseData <- list()
		predResponseData$name <- paste0("predicted_", responseData$name)
		predResponseData$data <- rmAlgoResults$predictedResponse
		# predResponseData$plotLabel <- predResponseData$name
		predResponseData$plotLabel <- paste0("Predicted ",idd, " (", pprefix, ", ", labs, ")")
		predResponseData$uniqName  <- predResponseData$name
		predResponseData$dataSource <- responseData$dataSource
		
		p1 <- makePlotStatic(xData = predResponseData, yData = responseData, showColor = T, 
												 showColorTissues = character(0), dataSource = input$dataset, 
												 srcContent = srcContentReactive(),oncolor=oncolor)
		g1 <- ggplotly(p1, width=plotWidth, height=plotHeight, tooltip=tooltipCol)
		g1 <- layout(g1, margin=list(t = 75))
		g2 <- config(p = g1, collaborate=FALSE, cloud=FALSE, displaylogo=FALSE,displayModeBar=TRUE,
								 modeBarButtonsToRemove=c("select2d", "sendDataToCloud", "pan2d", "resetScale2d",
								 												 "hoverClosestCartesian", "hoverCompareCartesian",
								 												 "lasso2d", "zoomIn2d", "zoomOut2d"))
		g2
	})
	
	#----[Show 2D Actual vs. CV-Predicted Response Scatter Plot in 'Cross-Validation' Tab]-
	output$cvPlot <- renderPlotly({
		rmAlgoResults <- algoResults()
		
		responseData <- rmResponseData()
		if (!is.null(rmAlgoResults$updatedInputData)){
			# Feature selection algorithms will add additional predictors, and
			# may drop cell lines with missing values for candidate predictors,
			# requiring update of response data below.
			updatedInputData <- rmAlgoResults$updatedInputData
			
			# First column has cell line names, second column has response data.
			responseData$data <- setNames(updatedInputData[, 2, drop = TRUE], 
																		rownames(updatedInputData))
		}
		
		## new labeling --------------------------------
		idd=unlist(strsplit(responseData$name,"_"))[1]
		idd=substr(idd,4,nchar(idd))
		srd=unlist(strsplit(responseData$name,"_"))[2]
		pprefix=substr(responseData$name,1,3)
		labs=metaConfig[[srd]][["displayName"]]
		responseData$plotLabel <- paste0("Observed ",idd, " (", pprefix, ", ", labs, ")")
		## ---------------------------------------------
		
		cvPredResponseData <- list()
		cvPredResponseData$name <- paste0("cv_predicted_", responseData$name)
		cvPredResponseData$data <- rmAlgoResults$cvPredictedResponse
		#cvPredResponseData$plotLabel <- cvPredResponseData$name
		cvPredResponseData$plotLabel <- paste0("10-Fold Cross-Validation ",idd, " (", pprefix, ", ", labs, ")")
		cvPredResponseData$uniqName  <- cvPredResponseData$name
		cvPredResponseData$dataSource <- responseData$dataSource
		
		p1 <- makePlotStatic(xData = cvPredResponseData, yData = responseData, showColor = T, 
												 showColorTissues = character(0), dataSource = input$dataset, 
												 srcContent = srcContentReactive(),oncolor=oncolor)
		g1 <- ggplotly(p1, width=plotWidth, height=plotHeight, tooltip=tooltipCol)
		g1 <- layout(g1, margin=list(t = 75))
		g2 <- config(p = g1, collaborate=FALSE, cloud=FALSE, displaylogo=FALSE,displayModeBar=TRUE,
								 modeBarButtonsToRemove=c("select2d", "sendDataToCloud", "pan2d", "resetScale2d",
								 												 "hoverClosestCartesian", "hoverCompareCartesian",
								 												 "lasso2d", "zoomIn2d", "zoomOut2d"))
		g2
	})
	
	#----[Show Input + Predicted Response Data in 'Data' Tab]-------------------------------
	output$data <- DT::renderDataTable({
		dat <- inputData()
		
		if (nrow(dat) >= 10){
			rmAlgoResults <- algoResults()
			if (!is.null(rmAlgoResults$updatedInputData)){
				# Algorithm selected additional features.
				dat <- rmAlgoResults$updatedInputData
			}
			dat <- cbind(predicted_response = signif(rmAlgoResults$predictedResponse, 3), dat[, -1])
			if (length(rmAlgoResults$cvPredictedResponse) > 0){
				dat <- cbind(cv_predicted_response = signif(rmAlgoResults$cvPredictedResponse, 3), dat)
			}
			dat <- cbind(CellLine = rownames(dat), dat)	
		}

# 		DT::datatable(dat, callback=JS('$("a.buttons-copy").css("background","lightblue");
#                     $("a.buttons-print").css("background","lightblue");
#                     $("a.buttons-excel").css("background","lightblue");
# 		                               return table;'),
# 		              rownames=FALSE, colnames=colnames(dat), filter='top', selection = "none",extensions='Buttons',
# 									style='bootstrap', options=list(pageLength = nrow(dat),language=list(paginate = list(previous = 'Previous page', `next`= 'Next page')) ,dom='lipBt',buttons = list('copy', 'print',list(extend='excel',filename='regression_data',text='Download'))))


		DT::datatable(dat, rownames=FALSE, colnames=colnames(dat), filter='top', selection = "none",extensions='Buttons',
		               style='bootstrap', options=list(pageLength = nrow(dat),language=list(paginate = list(previous = 'Previous page', `next`= 'Next page')) ,dom='lipBt',buttons = list('copy', 'print', list(extend = 'collection',buttons = list(list(extend='csv',filename='regression_data',title='Exported data from CellMinerCDB'), list(extend='excel',filename='regression_data',title='Exported data from CellMinerCDB'), list(extend='pdf',filename='regression_data',title='Exported data from CellMinerCDB')),text = 'download'))))
		
			})
	
	# DT::datatable(myframe, rownames=FALSE,extensions='Buttons',
	#               filter='top', style='bootstrap', selection = "none",
	#               options=list(pageLength = 10, dom='lipBt',buttons = list('copy', 'print', list(extend = 'collection',buttons = c('csv', 'excel', 'pdf'),text = 'Download')))
	#               , caption=htmltools::tags$caption(paste0("Identifier search for ",selsource),style="color:dodgerblue; font-size: 18px")
	              
	
	#----[Show Predictors and Response in 'Heatmap' Tab]------------------------------------
	output$heatmap <- renderD3heatmap({
		rmAlgoResults <- algoResults()
		if (is.null(rmAlgoResults$updatedInputData)){
			dataTab <- inputData()
		} else{
			dataTab <- rmAlgoResults$updatedInputData
		}
		
		dataMatrix <- as.matrix(t(dataTab[, -1, drop = FALSE]))
		numHiLoCols <- min(input$numHiLoResponseLines, floor(ncol(dataMatrix)/2))
		# Order columns by decreasing response values.
		dataMatrix <- dataMatrix[, order(dataMatrix[1, , drop = TRUE], decreasing = TRUE), drop = FALSE]
		# Extract highest/lowest responder columns.
		colIndexSet <- c(1:numHiLoCols, (ncol(dataMatrix) - numHiLoCols + 1):ncol(dataMatrix))
		dataMatrix <- dataMatrix[, colIndexSet, drop = FALSE]
		# Remove data source identifier in predictor names.
		rownames(dataMatrix) <- vapply(rownames(dataMatrix), function(x) { 
			stringr::str_split(x, "_")[[1]][1] }, character(1))
		
		xAxisFontSize <- "6pt"

		scaledDataMatrix <- scaleDataForHeatmap(dataMatrix, input$useHeatmapRowColorScale)
		#save(scaledDataMatrix, file = "~/Downloads/scaledDataMatrix.RData")
		
		# d3heatmap::d3heatmap(x = scaledDataMatrix,  # Used for color scaling.
		# 										 cellnote = dataMatrix, # Used for tooltip values.
		# 										 dendrogram = "none", 
		# 										 colors = colorRamp(colors = c("green", "black", "red")),
		# 										 xaxis_font_size = xAxisFontSize,
		# 										 xaxis_height = 200, yaxis_width = 200)
		d3heatmap::d3heatmap(x = scaledDataMatrix,  # Used for color scaling.
		                     cellnote = dataMatrix, # Used for tooltip values.
		                     dendrogram = "none", 
		                     colors = colorRamp(colors = c("blue", "white", "red")),
		                     xaxis_font_size = xAxisFontSize,
		                     xaxis_height = 200, yaxis_width = 200)
		## heatmaply(mtcars, scale_fill_gradient_fun = ggplot2::scale_fill_gradient2(low = "blue", high = "red",midpoint = 200),dendogram="none")
		
	})
	##--- download heatmap data
	output$downloadHeat <- downloadHandler(
	  
	  # This function returns a string which tells the client
	  # browser what name to use when saving the file.
	  filename = function() {
	    paste0("Heatmap_dataset_",input$dataset,"_responseID_",input$responseDataType,"_",input$responseId,".txt")
	  },
	 
	  content = function(file) {
	    rmAlgoResults <- algoResults()
	    if (is.null(rmAlgoResults$updatedInputData)){
	      dataTab <- inputData()
	    } else{
	      dataTab <- rmAlgoResults$updatedInputData
	    }
	    
	    dataMatrix <- as.matrix(t(dataTab[, -1, drop = FALSE]))
	    numHiLoCols <- min(input$numHiLoResponseLines, floor(ncol(dataMatrix)/2))
	    # Order columns by decreasing response values.
	    dataMatrix <- dataMatrix[, order(dataMatrix[1, , drop = TRUE], decreasing = TRUE), drop = FALSE]
	    # Extract highest/lowest responder columns.
	    colIndexSet <- c(1:numHiLoCols, (ncol(dataMatrix) - numHiLoCols + 1):ncol(dataMatrix))
	    dataMatrix <- dataMatrix[, colIndexSet, drop = FALSE]
	    # Remove data source identifier in predictor names.
	    rownames(dataMatrix) <- vapply(rownames(dataMatrix), function(x) { 
	      stringr::str_split(x, "_")[[1]][1] }, character(1))
	    
	    
	    
	    # scaledDataMatrix <- scaleDataForHeatmap(dataMatrix, input$useHeatmapRowColorScale)
	    # write.table(scaledDataMatrix, file, sep = "\t", col.names = NA,quote=F)  
	    
	    write.table(dataMatrix, file, sep = "\t", col.names = NA,quote=F)  
	    
	 	  }
	)
	#----[Show Technical Details in 'Technical Details' Tab]--------------------------------
	output$techDetails <- renderPrint({
		rmAlgoResults <- algoResults()
		
		if ("eqnStr" %in% names(rmAlgoResults)){
			cat("PREDICTED RESPONSE AS A FUNCTION OF INPUT VARIABLES:")
			cat("\n\n")
			
			eqnStr <- rmAlgoResults$eqnStr
			if (nchar(eqnStr) < 60){
				cat(eqnStr)
			} else{
				# Print equation over multiple lines.
				eqnElements <- str_split(eqnStr, "\\+")[[1]]
				cat(eqnElements[1])
				
				numTerms <- length(eqnElements)
				if (numTerms > 1){
					cat("+")
					charCount <- 0
					#-------------------------------------------
					for (i in (2:numTerms)){
						eqnTerm <- eqnElements[i]
						charCount <- charCount + nchar(eqnTerm)
						cat(eqnTerm)
						if (i != numTerms){
							cat("+")
							if (charCount > 60){
								charCount <- 0
								cat("\n     ")
							}
						}
					}
					#-------------------------------------------
				}
			}
			
			cat("\n")
			#cat(paste0(rep("_", 100), collapse = ""))
			cat("\n")
		}
		
		summary(rmAlgoResults$techDetails)
	})
	
	#----[Show Partial Correlation Results in 'Partial Correlations' Tab]-------------------
	output$patternCompResults <- DT::renderDataTable({
		pcResults <- parCorPatternCompResults()
		#pcResults$ANNOT <- ""
		
		#for (i in seq_len(nrow(pcResults))){
		#	name <- rcellminer::removeMolDataType(pcResults[i, "NAME"])
		#	if (name %in% rownames(geneSetPathwayAnalysis::geneAnnotTab)){
		#		pcResults[i, "ANNOT"] <-  geneSetPathwayAnalysis::geneAnnotTab[name, "SHORT_ANNOT"]
		#	} 
		#}
	#	pcResults$PARCOR <- round(pcResults$PARCOR, 3)
	#	pcResults$PVAL   <- signif(pcResults$PVAL, 3)
	## new removed
		 # dtype=substr(pcResults$NAME,1,3)
		 # nname=substr(pcResults$NAME,4,nchar(pcResults$NAME))
		 # pcResults=cbind(DATATYPE=dtype,NAME=nname,pcResults[,-1])
  ## end new 
		#  DT::datatable(pcResults, rownames=FALSE, colnames=colnames(pcResults), filter='top', 
		# 						style='bootstrap', selection = "none",
		# 							options=list(lengthMenu = c(10, 25, 50, 100), pageLength = 10,language=list(paginate = list(previous = 'Previous page', `next`= 'Next page'))))
		 
		 # pcResults$FDR=p.adjust(pcResults[,"PVAL"],method="BH",nrow(results))
		
		 # DT::datatable(pcResults, rownames=FALSE, colnames=colnames(pcResults),extensions='Buttons',
		 #               filter='top', style='bootstrap', selection = "none",
		 #               options=list(lengthMenu = c(10, 50, 100, nrow(pcResults)),pageLength = 100,language=list(paginate = list(previous = 'Previous page', `next`= 'Next page')) ,dom='lipBt', buttons = list('copy', 'print', list(extend = 'collection',buttons = list(list(extend='csv',filename='partial_corr',title='Exported data from CellMinerCDB'), list(extend='excel',filename='partial_corr',title='Exported data from CellMinerCDB'), list(extend='pdf',filename='partial_corr',title='Exported data from CellMinerCDB')),text = 'Download'))))
		 DT::datatable(pcResults, rownames=FALSE, colnames=colnames(pcResults),
		               filter='top', style='bootstrap', selection = "none",
		               options=list(lengthMenu = c(10, 50, 100, 500),pageLength = 100,language=list(paginate = list(previous = 'Previous page', `next`= 'Next page')) ,dom='lipt'))
		  
		 
		 
		 })
	
	#----[Show Differential Expression Results in 'Differential Expression' Tab]-------------------
	output$diffExpResults <- DT::renderDataTable({
		deResults <- diffExpResultsTab()
		deResults$ANNOT <- ""
		
		for (i in seq_len(nrow(deResults))){
			name <- rcellminer::removeMolDataType(deResults[i, "NAME"])
			if (name %in% rownames(geneSetPathwayAnalysis::geneAnnotTab)){
				deResults[i, "ANNOT"] <-  geneSetPathwayAnalysis::geneAnnotTab[name, "SHORT_ANNOT"]
			} 
		}
		deResults$PVAL <- signif(deResults$PVAL, 3)
		deResults$QVAL <- signif(deResults$QVAL, 3)
		
		DT::datatable(deResults, rownames=FALSE, colnames=colnames(deResults), filter='top', 
									style='bootstrap', selection = "none",
									options=list(lengthMenu = c(10, 25, 50, 100), pageLength = 10,language=list(paginate = list(previous = 'Previous page', `next`= 'Next page'))))
	})
	
	output$enrichmentResults <- DT::renderDataTable({
		enResults <- enrichmentResultsTab()
		
		DT::datatable(enResults, rownames=FALSE, colnames=colnames(enResults), 
									filter='top', style='bootstrap', selection = "none",
									options=list(lengthMenu = c(10, 25, 50, 100), pageLength = 10,language=list(paginate = list(previous = 'Previous page', `next`= 'Next page'))))
	})
	
	#----[Organize Above Tabs for Display]--------------------------------------------------
	output$tabsetPanel = renderUI({
		maxNumHiLoResponseLines <- 100
		ns <- session$ns
		##
		choices = c(names(geneSetPathwayAnalysis::geneSets), "All Genes")
		# print(choices)
		mysel = "All Gene Sets"
		opt0 = ""
		for(y in 1:length(choices)){
		  # style works only for browser Chrome
		  
		  if (choices[y] == mysel)
		  {
		    opt0 =  paste0(opt0,"<option style='white-space: pre-wrap' selected>",choices[y],"</option>;");
		  }
		   else 
		    {
		    opt0 =  paste0(opt0,"<option style='white-space: pre-wrap'>",choices[y],"</option>;");
		  }
		}
		## xx=paste0(opt0,"end")
		## print(xx)
		## print("*")
		choices1 = srcContentReactive()[[input$dataset]][["featurePrefixes"]]
		mysel1 = input$predDataTypes # could cause issue if NULL
		##--------
	  if (is.null(mysel1))
		{
		  mysel1= srcContentReactive()[[input$dataset]][["defaultFeatureX"]]
		  if (is.na(mysel1)) mysel1= srcContentReactive()[[input$dataset]][["defaultFeatureY"]]
		}
		##--------
		opt1 = ""
		for(k in 1:length(choices1)){
		  # style works only for browser Chrome
		  
		  if (choices1[k] == mysel1)
		  {
		    opt1 =  paste0(opt1,"<option value=",choices1[k]," style='white-space: pre-wrap' selected>",names(choices1)[k],"</option>;");
		  }
		  else 
		    {
		    opt1 =  paste0(opt1,"<option value=",choices1[k]," style='white-space: pre-wrap'>",names(choices1)[k],"</option>;");
		   }
		}
		## yy=paste0(opt1,"end")
		## print(yy)
		## print("**")
		##
		# TABS: Plot, Cross-Validation, Data, Heatmap, Technical Details
		#ns <- session$ns
		dataTabPanel <- tabPanel("Data", DT::dataTableOutput(ns("data")))
		heatmapTabPanel <- tabPanel("Heatmap", 
																sliderInput(ns("numHiLoResponseLines"), 
																						"Number of High/Low Response Lines to Display:", 
																						min=1, max=maxNumHiLoResponseLines, 
																						value=20, width = "50%"),
																checkboxInput(ns("useHeatmapRowColorScale"), "Use Row Color Scale", FALSE),
																d3heatmapOutput(ns("heatmap")),
															  p("Select cell line or feature name to highlight heatmap columns or rows, respectively."),br(),
																downloadButton(ns("downloadHeat"), "Download Heatmap Data")
																)
		techDetailsTabPanel <- tabPanel("Technical Details", verbatimTextOutput(ns("techDetails")))
		diffExpTabPanel <- tabPanel("Differential Expression", 
																 selectInput(ns("deGeneSets"), "Select Gene Sets",
																					 choices  = c(names(geneSetPathwayAnalysis::geneSets), "All Genes"),
																						#choices = names(geneSetPathwayAnalysis::geneSets),
																				   selected = "All Gene Sets",
																			     multiple=TRUE),
																selectInput(ns("deDataTypes"), "Select Data Types",
																					choices  = srcContentReactive()[[input$dataset]][["featurePrefixes"]],
																					selected = input$predDataTypes,
																					multiple=TRUE),
																
																sliderInput(ns("minDiffExpDataValueRange"), 
																						"Minimum Range (First Listed Data Type):", 
																						min=0, max=5, value=0, step = 0.25),
																actionButton(ns("computeDiffExp"), "Run"),
																tags$hr(),
																DT::dataTableOutput(ns("diffExpResults")))
		enrichmentTabPanel <- tabPanel("Gene Set Enrichment", 
																textInput(ns("enDiffExpFdr"), "FDR Level for Differential Expression", value = "0.05"),
																actionButton(ns("computeEnrichment"), "Run"),
																tags$hr(),
																DT::dataTableOutput(ns("enrichmentResults")))
		patternCompTabPanel <- tabPanel("Partial Correlation", 
																		## selectInput(ns("pcGeneSets"), "Select Gene Sets",
																		##						choices  = c(names(geneSetPathwayAnalysis::geneSets), "All Genes"),
																								#choices = names(geneSetPathwayAnalysis::geneSets),
																		##						selected = "All Gene Sets",
																		##						multiple=TRUE),
																	 ##	selectInput(ns("pcDataTypes"), "Select Data Types",
																		##						choices  = srcContentReactive()[[input$dataset]][["featurePrefixes"]],
																		##						selected = input$predDataTypes,
																		##						multiple=TRUE),
																		##
																		br(),
																		HTML(
																		  paste("<label class='control-label' for=",ns("pcGeneSets"),">Select Gene Sets</label>","<select id=",ns("pcGeneSets")," style='word-wrap:break-word;' multiple>",opt0,"</select>")
																		),
																		#
																		br(),br(),
																		HTML(
																		  paste("<label class='control-label' for=",ns("pcDataTypes"),">Select Data Types</label>","<select id=",ns("pcDataTypes")," style='word-wrap:break-word;' multiple>",opt1,"</select>")
																		),
																		br(),br(),
																		##
																		sliderInput(ns("minParCorDataValueRange"), 
																								"Minimum Range (First Listed Data Type):", 
																								min=0, max=5, value=0, step = 0.25),
																		br(),
																		HTML("<b>Click Run to determine additional predictors, after the influence of the input predictors are removed.</b>"),
																		br(),
																		actionButton(ns("computeParCors"), "Run"),
																		tags$hr(),
																		renderUI({
																		  req(parCorPatternCompResults())
																		  downloadLink(ns("downloadPartialCorr"), "Download All as a Tab-Delimited File")
																		}),
																		DT::dataTableOutput(ns("patternCompResults")))
		
		
		if (require(plotly)){
			plotTabPanel   <- tabPanel("Plot", 
																 plotlyOutput(ns("plot"),   width = plotWidth, height = plotHeight))
			cvPlotTabPanel <- tabPanel("Cross-Validation", 
																 plotlyOutput(ns("cvPlot"), width = plotWidth, height = plotHeight))
			tabsetPanel(type = "tabs", 
									heatmapTabPanel, dataTabPanel, plotTabPanel, cvPlotTabPanel, techDetailsTabPanel, 
									#diffExpTabPanel, enrichmentTabPanel, 
									patternCompTabPanel)
		} else{
			tabsetPanel(type = "tabs", 
									heatmapTabPanel, dataTabPanel, techDetailsTabPanel, 
									#diffExpTabPanel, enrichmentTabPanel, 
									patternCompTabPanel)	
		}
	})
	
	#-----[Support for Reactive UI Elements]-----------------------------------------------
	output$responseDataTypeUi <- renderUI({
		ns <- session$ns
		srcContent <- srcContentReactive()
		#selectInput(ns("responseDataType"), "Response Data Type",
		#						choices  = srcContent[[input$dataset]][["featurePrefixes"]],
		#						selected = srcContent[[input$dataset]][["defaultFeatureY"]])
		vopt = "";
		choices  = srcContent[[input$dataset]][["featurePrefixes"]]
		mych= srcContent[[input$dataset]][["defaultFeatureY"]]
		for(y in 1:length(choices)){
		  if (choices[y]==mych)
		  {
		    vopt =  paste0(vopt,"<option value=",choices[y]," selected>",names(choices)[y],"</option>;")
		  }
		  else
		  {
		    vopt =  paste0(vopt,"<option value=",choices[y],">",names(choices)[y],"</option>;");
		  }
		}
		HTML(
		  paste("<label class='control-label' for=",ns("responseDataType"),">Response Data Type</label>","<select id=",ns("responseDataType"),"style='word-wrap:break-word; width: 100%;' >",vopt,"</select>")
		)
	})
	
	output$predDataTypesUi <- renderUI({
		ns <- session$ns
		srcContent <- srcContentReactive()
		#selectInput(ns("predDataTypes"), "Predictor Data Types",
		#						choices  = srcContent[[input$dataset]][["featurePrefixes"]],
		#						selected = srcContent[[input$dataset]][["defaultFeatureX"]],
		#						multiple=TRUE)
		vopt = ""
		choices  = srcContent[[input$dataset]][["featurePrefixes"]]
		mych= srcContent[[input$dataset]][["defaultFeatureX"]]
		if (is.na(mych)) mych= srcContent[[input$dataset]][["defaultFeatureY"]]
		for(y in 1:length(choices)){
		  if (choices[y]==mych)
		  {
		    vopt =  paste0(vopt,"<option value=",choices[y]," selected>",names(choices)[y],"</option>;")
		  }
		  else
		  {
		    vopt =  paste0(vopt,"<option value=",choices[y],">",names(choices)[y],"</option>;");
		  }
		}
		HTML(
		  paste("<label class='control-label' for=",ns("predDataTypes"),">Predictor Data Type/s</label>","<select id=",ns("predDataTypes"),"style='word-wrap:break-word; width: 100%;' multiple>",vopt,"</select>")
		)
	})
	
	output$selectTissuesUi <- renderUI({
		ns <- session$ns
		srcContent <- srcContentReactive()
		tissueTypes <- sort(unique(names(srcContent[[input$dataset]][["tissueToSamplesMap"]])))

		#if (input$tissueSelectionMode == "Include"){
		#	selectInput(ns("selectedTissues"), label = NULL, choices=c("all", tissueTypes),
		#							multiple=TRUE, selected="all")
		#} else{ # input$tissueSelectionMode == "Exclude"
		#	selectInput(ns("selectedTissues"), label = NULL, choices=c("none", tissueTypes),
		#							multiple=TRUE, selected="none")
		#}
		## new code
		if (input$tissueSelectionMode == "To include"){
		  choices=c("all", tissueTypes); mysel="all"
		  
		} else{ # input$tissueSelectionMode == "Exclude"
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
		  paste("<label class='control-label' for=",ns("selectedTissues"),">Select Tissue/s of Origin</label>","<select id=",ns("selectedTissues")," style='word-wrap:break-word; width: 100%;' multiple>",opt,"</select>")
		)
		
		## 
	})

	output$downloadPartialCorr <- downloadHandler(
	  
	  # This function returns a string which tells the client
	  # browser what name to use when saving the file.
	  filename = function() {
	    
	    paste0("Partial_Correlation_all_cdb_",input$dataset,"_",input$responseDataType,"_",input$responseId,".txt")
	  },
	  
	  content = function(file) {
	    
	    write.table(parCorPatternCompResults(), file, sep = "\t", row.names = F,quote=F)  
	    
	  }
	)
	
	output$selectInputGeneSetsUi <- renderUI({
		ns <- session$ns
		## selectInput(ns("inputGeneSets"), "Select Gene Sets",
								#choices  = c(names(geneSetPathwayAnalysis::geneSets), "All Genes"),
			##					choices = names(geneSetPathwayAnalysis::geneSets),
			##					selected = "All Gene Sets",
			##					multiple=TRUE)
		choices = names(geneSetPathwayAnalysis::geneSets)
		mysel="All Gene Sets"
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
		  paste("<label class='control-label' for=",ns("inputGeneSets"),">Select Gene Sets</label>","<select id=",ns("inputGeneSets")," style='word-wrap:break-word; width: 100%;' multiple>",opt,"</select>")
		)
		
	})
	#--------------------------------------------------------------------------------------
	
	#************************************************************************************************
}