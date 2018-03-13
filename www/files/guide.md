<!-- TOC -->
# Table of Contents
-	[Introduction](#introduction)
-	[Univariate Analyses](#univariate)
  - [Plot Data](#plot)
  - [Download Data](#download)
  - [Compare Patterns](#compare)
-	[Regression Models](#regression)
  - [Heatmap](#heatmap)
  - [Data](#data)
  - [Plot](#plotpred)
  - [Cross-Validation](#cross)
  - [Technical Details](#details)
  - [Partial correlations](#partialcorr)
-	[Metadata](#metadata)
- [Search IDs](#search)
      - [Drug IDs](#drugid)
      - [Gene IDs](#geneid)
-	[About](#about)
-	[Navigation guide](#navigation)
  - [Multiple selection](#multiple)
  - [X-axis or Y-axis range](#range)
  - [Show color](#color)
-	[Video tutorial](#video)
-	[Methods](#methods)
  - [Linear regression](#linear)
  - [Lasso model](#lasso)
- [Main reference](#refer)

<!-- /TOC -->

<h2 id="introduction">Introduction</h2>
CellMinerCDB is an interactive web application that simplifies access and exploration of cancer cell line pharmacogenomic data across different sources (see Metadata section for more details). Navigation in the application is done using main menu tabs (see figure below). It includes 5 tabs: Univariate Analyses, Regression Models, Metadata, About and Help. Univariate Analyses is selected by default when entering the site. Each option includes a side bar menu (to choose input) and a user interface output to display results. Analysis options are available on the top for both the Univariant Analysis and Regression model tabs (see sub-menu on figure). The sub-menu first option result is displayed by default (Figure 1).

#### Figure 1
![Screenshot of CellMinerCDB Application](files/Slide01.jpg)
<h2 id="univariate">Univariate Analyses</h2>
Molecular and/or drug response patterns across sets of cell lines can be compared to look for possible association.  The univariate analysis panel includes 4 options: Plot data, Download Data, Search ID and Compare Patterns. Almost all options have the same input data in the left side panel.

<h3 id="plot">Plot Data</h3>
Any pair of features from different sources across common cell lines can be plotted (as a scatterplot) including the resultant Pearson correlation and p value. To generate a scatterplot, enter on the side bar panel:

1.	The x-axis data choice includes 3 fields to be filled by the user. Field one is the x-Axis Dataset (the data source). The user can choose: NCI60, CCLE, GDSC, CTRP or NCI/DTP SCLC (see about section for more details). In field two the user selects a specific data type to query. The options for this vary between sources, and appear in the x-Axis Type dropdown. See Metadata tab for data type descriptions and abbreviations. In field three, the user should enter the desired feature to query. For instance, if drug activity for the NCI-60 is selected, the user can enter a single drug name or drug ID (NSC number) or a paired drug ID (NSC1_NSC2). The “Search IDs” tab will help the user access potential identifiers for each dataset. The user has also the option to change the x-axis range for better visualization.
2.	The y-axis choice is as explained above.

3.	To include or exclude cell lines from specific tissues, use the Select Tissues functionality at the bottom of the panel. All cell lines were mapped to the four-level OncoTree cancer tissue type hierarchy developed at [Memorial Sloan-Kettering Cancer Center](http://www.cbioportal.org/oncotree/). In the CellminerCDB application, a tissue value is coded as an OncoTree node that can include elements from level 1 to level 4 separated by “:” character. For instance, the cell line MCF7 is a “breast” cell line but also more specifically it is a breast carcinoma. So MCF7 belong to different cancer tissue types (or hierarchical nodes) “breast” (level 1) and “breast: breast carcinoma (BRCA)” (level 2). There is no further sub-categorization for MCF7 however we can identify BT-549 cell line as “breast: breast carcinoma (BRCA): Breast Invasive Ductal Carcinoma (IDC)” (level 3).  

4.	In order to locate cell lines related to desired tissues within the scatter plot, the user has the option to color in red the selected cell lines and have remaining cell lines colored in blue.

Figure 2 shows a scatterplot between SLFN11 gene expression (x-axis) and Topotecan drug activity both from the NCI60. Since Topotecan has 2 different drug ids in the NCI-60, the one with the lowest number of missing data is selected (here 609699). However, the user can type in their specific drug ID of interest. The Pearson correlation value and p value appear at the top of the plot. A linear fitting curve is included. This is an interactive plot and whenever the user changes any input value, the plot will be updated. Any point in the plot can be hovered over to provide additional information about cell line, tissue, Onco tree designation,  and x and y coordinate values.

#### Figure 2
![Screenshot of CellMinerCDB Application](files/Slide02.jpg)

<h3 id="download">Download Data</h3>
This option displays the Plot Data data selected (from the previous tab) in tabular form, and provides a Download Data as Tab-Delimited File function. The user can also change the input data in the left panel as described for Plot Data. The displayed table include the cell line, the x-axis feature, the y-axis feature, the tissue of origin and the 4 onco-tree levels. The selected features are prefixed in the headers by the data type abbreviation and post-fixed by the data source.
Figure 3 below shows the selected values for SLFN11 gene expression (x-axis) and Topotecan (id 609699) drug activity (y-axis) from the NCI-60 across all common lines. The features are coded as expSLFN11_nci60 and act609699_nci60 where “exp” and “act” represent respectively prefixes for gene expression based on z-score and drug activity.

#### Figure 3
![Screenshot of CellMinerCDB Application](files/Slide03.jpg)


<h3 id="compare">Compare Patterns</h3>
This option allows one to compute the correlation between a feature from a data source with either all drug data or all molecular data from the same source. Pearson’s correlations are provided, with reported p-values not adjusted for multiple comparisons. This could be used to identify highly co-correlated features with useful information such as target pathway for genes and mode of action for drugs. The first step is to fill the x-axis, y-axis or tissue information (or keep current information if no change – see plot data section). The next step is to choose the data source from the x-axis or y-axis dataset and finally you select either drug or molecular features to compare with. Once done, a table will display all Pearson correlation value between the selected feature (in the left panel) and all features selected (in main panel). 

The following figure shows correlation results for SLFN11 gene with all other molecular features for all NCI60 datasets sorted by correlation value with gene location and target pathways (annotation field).

#### Figure 4
![Screenshot of CellMinerCDB Application](files/Slide04.jpg)

<h2 id="regression">Regression Models</h2>
The ‘Regression Models’ option (or module) has multiple features (described below), and allows construction and assessment of multivariate linear response prediction models. For instance, we can assess prediction of a drug activity based on some genes expression. To construct a regression model, you need to specify:

1. The response variable by selecting 
  - the Dataset (data source)
  - the Response Data Type (example: a drug or a molecular dataset)
  - the Response ID (e.g., a specific drug or gene identifier)

2. The predictor variables from the same data source by selecting
  - the Predictor Data Type. Use command button on Macs to select more than one dataset
  - Minimal Value Range for the first listed data type (optional – default zero)
  - one or more features (optional for Lasso model –see point4)

3. The tissues: by default, all cell lines are included however you can selected some based-on tissue

4. The algorithm: by default, the basic linear regression model is selected however you can select the Lasso model (penalized linear regression model). If Lasso algorithm is selected, you have to specify:
  - The gene sets: The gene selection is based on curated gene sets such as DNA Damage Repair DDR or Apoptosis. The user can select one or more gene sets.
  - The maximum of predictors (default 4)

Once all the above information is entered, a regression model is built and the results are shown in different ways such as the technical details of the model, observed vs. predictive responses plots or variables heatmap. Find below an explanation of different output for the regression model module.

<h3 id="heatmap">Heatmap</h3>
This option provides the observed response and predictor variables across all source cell lines in an interactive heatmap. The user can restrict the number of cell lines to those that have the highest or lowest response values. The following figure shows a heatmap where we selected topotecan as a response variable and SLFLN11 and BPTF gene expression as predictor variables. We chose to display only 40 cell lines that have the most 20 highest and 20 lowest values for topotecan activity.

#### Figure 5
![Screenshot of CellMinerCDB Application](files/Slide05.jpg)

In case, the Lasso algorithm is selected	more predicted variables are shown based on model result as shown below (STK17B and ABCD3 new genes added)

#### Figure 6
![Screenshot of CellMinerCDB Application](files/Slide06.jpg)

<h3 id="data">Data</h3>
This option shows the detailed data for the model variables for each cell line as well as the predicted and cross-validation (10-fold cross validation) predicted responses. The data is displayed as a table with filtering options for each column. You can see below, the data related to the simple linear regression model presented in the previous section.

#### Figure 7
![Screenshot of CellMinerCDB Application](files/Slide07.jpg)

<h3 id="plotpred">Plot</h3>
This option enables one to plot and compare the observed response values versus the predicted response values. The figure below shows a plot comparing Topotecan observed vs. predicted activity with high correlation value of 0.84

#### Figure 8
![Screenshot of CellMinerCDB Application](files/Slide08.jpg)

<h3 id="cross">Cross-Validation</h3>
This option enables to plot the observed response values versus the 10-fold cross-validation predicted response values and assess model generalization beyond the training data. The figure below shows a plot comparing Topotecan observed vs. cross-validation predicted activity with still high correlation value of 0.82

#### Figure 9
![Screenshot of CellMinerCDB Application](files/Slide09.jpg)

<h3 id="details">Technical Details</h3>
This option enables to view the R statistical and other technical details related to the constructed response model. Below is an example of regular regression model fitting results.

#### Figure 10
![Screenshot of CellMinerCDB Application](files/Slide10.jpg)

<h3 id="partialcorr">Partial correlations</h3>
Additional predictive variables for a multivariate linear model can be selected using the partials correlations results. Conceptually, the aim is to identify variables that are independently correlated with the response variable, after accounting for the influence of the existing predictor set. Computationally, a linear model is fit, with respect to the existing predictor set, for both the response variable and each candidate predictor variable. The partial correlation is then computed as the Pearson’s correlation between the resulting pairs of model residual vectors (which capture the variation not explained by the existing predictor set). The p-values reported for the correlation and linear modeling analyses assume multivariate normal data. The two-variable plot feature of CellMinerCDB allows informal assessment of this assumption, with clear indication of outlying observations. The reported p-values are less reliable as the data deviate from multivariate normality.
In order to run a partial correlation analysis, the user should first construct a linear model (providing response and predictor variables as explained earlier - steps 1 to 4 in figure below-) and then select the:
-	Gene sets: The gene selection is based on curated gene sets. Here the user can select one or more gene sets and even all genes (step 5 in figure)
-	Data types: the user can select one or more data type such as gene expression, methylation or copy number variation (step 6 in figure)
-	And optionally, minimum range for the first listed data type (step 7 in figure)
And finally click on button run (step 8 in figure). A table with partial correlation results for all candidate variables with gene sets annotation

#### Figure 11
![Screenshot of CellMinerCDB Application](files/Slide11.jpg)

<h2 id="metadata">Metadata</h2>
This option enumerates for each data source, the available data types that could be queried within the app providing the data type abbreviation or prefix, description, feature value unit (z-score, intensity, probability …), platform or experiment and related publication reference (pubmed). The following figure shows all data types for NCI60.

#### Figure 12
![Screenshot of CellMinerCDB Application](files/Slide12.jpg)
<h3 id="search">Search IDs</h3>
This page lists the identifiers (ID) that are available in the selected data source to use in the univariate analysis or regression models. Each data source has different types and amounts of data available. 

For the molecular data, the Data type and gene names (ID) are provided. For the drugs and compounds, the Data type (activity), identifiers (ID),  Drug name (when available), and Drug MOA (when available) are displayed. 

The results  are displayed as a table with 4 columns: **Data type, ID, Drug Name, and Drug MOA** The user can scroll down the whole  list of IDs, or search specific ID(s) by entering a value in the header of any column.

<h3 id="drugid">Drug IDs</h3>
For the NCI-60 and NCI/DTP SCLC, the drug identifiers (ID) are NSC's or names. For the CCLE, GTRP, and CTRP, the drug identifiers are the Drug names.

<h3 id="geneid">Gene IDs</h3>
For all data sources, the gene ID is the gene name (Hugo name)

<br>
#### Figure 13
Example of a search: if looking in the NCI-60 for the drug Topotecan, select "NCI-60" as the cell line source and type "topotecan" in search box of column "Drug name". The search can be limited to drugs by entering "act" in the "Data type" search box. See figure 13 below.

![Screenshot of CellMinerCDB Application](files/Slide13.jpg)

<h2 id="about">About</h2>
The about option gives references to the data sources, main publications, collaborators and presents the group team as well as the contact information. (need to update the page) 

#### Figure 14
![Screenshot of CellMinerCDB Application](files/Slide14.jpg)

<h2 id="navigation">Navigation guide</h2>
<h3 id="multiple">Multiple selection</h3>
In order to select multiple choice from a list, use “command” button for Mac or “alt” button for PC and then click

<h3 id="range">X-axis or Y-axis range</h3>
You can change the x-axis or y-axis lower or higher value to have different views of the displayed plot.

<h3 id="color">Show color</h3>
It is a checkbox that enable and disable colors in the scatter plots

<h2 id="video">Video tutorial</h2>
**For an introduction please refer to our <a href="https://www.youtube.com/watch?v=2HicAgcyJHI" target="_blank">video tutorial</a>**

<h2 id="methods">Methods</h2>
<h3 id="linear">Linear regression</h3>
Basic linear regression models are implemented using the R stats package lm() function

<h3 id="lasso">Lasso Model</h3>
Lasso (penalized linear regression models) are implemented using the glmnet R package. The lasso performs both variable selection and linear model coefficient fitting. The lasso lambda parameter controls the tradeoff between model fit and variable set size. Lambda is set to the value giving the minimum error with 10-fold cross-validation. For either standard linear regression or LASSO models, 10-fold cross validation is applied to fit model coefficients and predict response, while withholding portions of the data to better estimate robustness. 

<h2 id="refer">Main reference</h2>

Manuscript coming soon




