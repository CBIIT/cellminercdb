library(rcellminer)

config <- jsonlite::fromJSON("config.json")

source("appUtils.R")
source("dataLoadingFunctions.R")

srcContent <- lapply(config, loadSourceContent)
isLoadedSrc <- vapply(srcContent, function(x) { !is.null(x) }, logical(1))
if (any(!isLoadedSrc)){
	srcContent <- srcContent[isLoadedSrc]
}

saveRDS(srcContent, "srcContent.rds", compress = FALSE)