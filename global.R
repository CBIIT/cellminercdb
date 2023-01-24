# NOTE: Size is not automatically set for rChartsAlternative output
plotHeight <- 1100
plotWidth <- 1400

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
	# TO DO: Make configurable. this is for regression models
	geneProtDataTypePrefixes <- c("cop", "mut", "met", "exp", "xai","pro","swa", "xsq","mth","his","cri","rrb","bmt","hs4")
	if (prefix %in% geneProtDataTypePrefixes){
		return(TRUE)
	} else{
		return(FALSE)
	}
}

isGeneID <- function(prefix){
  # TO DO: Make configurable.
  geneProtDataTypePrefixes <- c("cop", "mut", "met", "exp", "xai", "xsq","mth","his","cri","rrb","bmt","hs4")
  if (prefix %in% geneProtDataTypePrefixes){
    return(TRUE)
  } else{
    return(FALSE)
  }
}