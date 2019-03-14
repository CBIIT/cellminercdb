library(rcellminer)

config <- jsonlite::fromJSON("config_filtered.json") # could be original config file

source("appUtils.R")
source("dataLoadingFunctions.R")
# here an example for SCLC
# srcContent <- lapply(config, loadSourceContentFiltered,onco1="Lung",onco2="Small Cell Lung Cancer (SCLC)")
srcContent <- lapply(config, loadSourceContentFiltered,onco1=NA,onco2="sarcoma")

isLoadedSrc <- vapply(srcContent, function(x) { !is.null(x) }, logical(1))
if (any(!isLoadedSrc)){
	srcContent <- srcContent[isLoadedSrc]
}

# For NCI-60, replace default color map to use CellMiner tissue type colors.
# nci60ColorTab <- loadNciColorSet(returnDf=TRUE)
# nci60ColorTab$OncoTree1 <- srcContent$nci60$sampleData$OncoTree1
# srcContent$nci60$tissueColorMap <- c(by(nci60ColorTab, nci60ColorTab$OncoTree1, 
# 																				FUN = function(x) unique(x$colors)))

saveRDS(srcContent, "srcContent_filtered.rds", compress = FALSE)
