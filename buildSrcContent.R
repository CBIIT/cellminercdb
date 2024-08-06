library(rcellminer)

config <- jsonlite::fromJSON("config.json")

source("appUtils.R")
source("dataLoadingFunctions.R")

srcContent <- lapply(config, loadSourceContent)
isLoadedSrc <- vapply(srcContent, function(x) { !is.null(x) }, logical(1))
if (any(!isLoadedSrc)){
	srcContent <- srcContent[isLoadedSrc]
}

# Replace default color map to use CellMiner tissue type colors.
dataPackage <- "ncisarcoma"
colorMapping <- c(
  "Soft_Tissue" = "red",
  "Bone" = "blue",
  "Lung" = "green",
  "Rhabdomyosarcoma" = "purple",
  "Ewing Sarcoma" = "orange",
  "Alveolar Soft Part Sarcoma" = "pink",
  "Non-Small Cell Lung Cancer" = "cyan"
)
srcContent[[dataPackage]]$tissueColorMap <- colorMapping

saveRDS(srcContent, "srcContent.rds", compress = FALSE)
