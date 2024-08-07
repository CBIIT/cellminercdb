# CellminerCDB
CellMinerCDB is a Shiny/R app that simplifies access and  exploration of cancer cell line pharmacogenomic data across different sources.

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
* sarcoma: FIXME
* nciSclcData: https://github.com/CBIIT/nciSclcData
* ccleData: TBA
* gdsc: TBA
* ctrpData: TBA
* nciAlmanac: TBA 