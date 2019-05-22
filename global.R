# NOTE: Size is not automatically set for rChartsAlternative output
plotHeight <- 1000
plotWidth <- 1200

tooltipCol <- "tooltip"

isDrugActivityDataType <- function(prefix){
	# TO DO: Make configurable.
	drugActTypePrefixes <- "act"
	if (prefix %in% drugActTypePrefixes){
		return(TRUE)
	} else{
		return(FALSE)
	}
}

isGeneProtDataType <- function(prefix){
	# TO DO: Make configurable.
	geneProtDataTypePrefixes <- c("cop", "mut", "met", "exp", "xai", "swa", "pro","xsq","mth")
	if (prefix %in% geneProtDataTypePrefixes){
		return(TRUE)
	} else{
		return(FALSE)
	}
}

isGeneID <- function(prefix){
  # TO DO: Make configurable.
  geneProtDataTypePrefixes <- c("cop", "mut", "met", "exp", "xai", "xsq","mth")
  if (prefix %in% geneProtDataTypePrefixes){
    return(TRUE)
  } else{
    return(FALSE)
  }
}