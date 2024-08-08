library(rcellminer)

config <- jsonlite::fromJSON("config.json") # could be original config file

source("appUtils.R")
source("dataLoadingFunctions.R")

# An example removing samples that are not sarcoma
srcContent <- lapply(config, loadSourceContentFiltered, onco1=NA, onco2="sarcoma")

isLoadedSrc <- vapply(srcContent, function(x) { !is.null(x) }, logical(1))
if (any(!isLoadedSrc)){
	srcContent <- srcContent[isLoadedSrc]
}

saveRDS(srcContent, "srcContent.rds", compress = FALSE)
