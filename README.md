# CellminerCDB
CellMinerCDB is a Shiny/R app that simplifies access and exploration of cancer cell line pharmacogenomic data across different sources.

# Quickstart 
To run the app, run the following commands in R inside the project folder:
```
shiny::runApp()
```

# Dependencies and Source Repositories
## Dependencies Created by the NCI DTB Team 
* rcellminer: https://github.com/CBIIT/rcellminer
* rcellminerData: https://github.com/CBIIT/rcellminerData
* rcellminerUtilsCDB: https://github.com/CBIIT/rcellminerUtilsCDB
* rcellminerElasticNet: https://github.com/CBIIT/rcellminerElasticNet
* geneSetPathwayAnalysis: https://github.com/CBIIT/geneSetPathwayAnalysis

## Other Dependencies 
* shiny
* d3heatmap
* jsonlite
* stringr
* glmnet
* ggplot2
* plotly
* foreach
* doSNOW
* parallel

## Data packages 
### Bioconductor
Molecular profiles and drug response for the NCI-60 Cell Lines: https://bioconductor.org/packages/release/data/experiment/html/rcellminerData.html

### Zenodo Hosted
The following pre-built pharmacogenomics data packages are available on Zenodo (https://zenodo.org/records/14846168) for use with rcellminer and CellMinerCDB R code. 

* ccleData: Cancer Cell Line Encycopedia (CCLE) 
* gdscDataDec15: Genomics of Drug Sensitivity in Cancer (GDSC)
* ctrpData: Cancer Therapeutics Response Portal (CTRP)
* mdaMillsData: RPPA proteomics data collected for the NCI60 cell line collection 
* nciSarcomaData: Sarcoma cancer cell line collection
