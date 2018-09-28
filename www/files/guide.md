<!-- TOC -->
# Table of Contents
-	[Introduction](#introduction)
-	[Univariate Analyses](#univariate)
  - [Plot Data](#plot)
  - [View Data](#download)
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
-	[Navigation guide](#navigation)
  - [Multiple selection](#multiple)
  - [X-axis or Y-axis range](#range)
  - [Show color](#color)
- [Exploratory workflow](#workflow)
-	[Video tutorial](#video)
-	[Methods](#methods)
  - [Linear regression](#linear)
  - [Lasso model](#lasso)
- [Contact/Feedback](#contactfeedback)
- [Data Sources](#data-sources)
- [About the Data](#about-the-data)
- [About CellMinerCDB](#about-cellminercdb)
	- [NCI-DTB Genomics and Bioinformatics Group](#nci-dtb-genomics-and-bioinformatics-group)
	- [Biostatistics and Computational Biology, Dana-Farber Cancer Institute, Harvard Medical School](#biostatistics-and-computational-biology-dana-farber-cancer-institute-harvard-medical-school)
	- [MSKCC Computational Biology](#mskcc-computational-biology)
- [References](#references)

<!-- /TOC -->

<h2 id="introduction">Introduction</h2>
CellMinerCDB is an interactive web application that simplifies access and exploration of cancer cell line pharmacogenomic data across different sources (see Metadata section for more details). Navigation in the application is done using main menu tabs (see figure below). It includes 5 tabs: Univariate Analyses, Regression Models, Metadata, Search and Help. Univariate Analyses is selected by default when entering the site. Each option includes a side bar menu (to choose input) and a user interface output to display results. Analysis options are available on the top for both the Univariant Analysis and Regression model tabs (see sub-menu on figure). The sub-menu first option result is displayed by default (Figure 1).

![Screenshot of CellMinerCDB Application](files/Slide01.jpg)

**Figure 1**: main application interface

<h2 id="univariate">Univariate Analyses</h2>
Molecular and/or drug response patterns across sets of cell lines can be compared to look for possible association.  The univariate analysis panel includes 3 options: Plot data, Download Data and Compare Patterns. Almost all options have the same input data in the left side panel.

<h4 id="inputs">Input data</h4>

1.	The x-axis data choices includes 4 fields to be filled by the user:
  - **x-Axis Cell Line Set** selects the data source. The user can choose: NCI60, CCLE, GDSC, CTRP or NCI/DTP SCLC (see Data Sources for more details). 
  - **x-Axis Data Type** selects the data type to query. The options for this vary dependent on the source selected above, and appear in the x-Axis Data Type dropdown. See the Metadata tab for descriptions and abbreviations. 
  - **Identifier** selects the identifier of interest for the above selected data type. For instance, if drug activity for the NCI-60 is selected, the user can enter a single drug name or drug ID (NSC number) or a paired drug ID (NSC1_NSC2). The Search IDs tab explores potential identifiers interactively, or to download datasets of interest. 
  - **x-Axis Range** allows the user to control the x-axis range for better visualization.
<br><br>
2.	The y-axis data choices are as explained above for the x-axis.
<br><br>
3.  Selected tissues: by default, all tissues are selected and included in the scatter plot. To include or exclude cell lines from specific tissues, the user should specify:
  - **Select Tissues** to include or exclude specific tissues
  - **Select Tissues of Origin Subset/s** functionality at the bottom of the left-hand panel. On Macs, more than one tissue of origin may be selected using the "command" button. On PC's use the "control" key. All cell lines were mapped to the four-level OncoTree cancer tissue type hierarchy developed at [Memorial Sloan-Kettering Cancer Center](http://www.cbioportal.org/oncotree/). In the CellminerCDB application, a tissue value is coded as an OncoTree node that can include elements from level 1 to level 4 separated by “:” character. For instance, the cell line MCF7 is a “breast” cell line but also more specifically it is a breast carcinoma. So MCF7 belong to different cancer tissue types (or hierarchical nodes) “breast” (level 1) and “breast: breast carcinoma (BRCA)” (level 2). There is no further sub-categorization for MCF7 however we can identify BT-549 cell line as “breast: breast carcinoma (BRCA): Breast Invasive Ductal Carcinoma (IDC)” (level 3).
<br><br>
4. Color selection
  - **Tissues to Color** to locate cell lines related to desired tissues within the scatter plot. By default, the cell lines are colored by their OncoTree cancer tissue level 1 pre-assigned color. Selecting a tissue makes related cell lines appear in red while remaining cell lines are colored in blue. The **Show Color** checkbox should be active.
<br><br>

<h3 id="plot">Plot Data</h3>
Any pair of features from different sources across common cell lines can be plotted (as a scatterplot) including the resultant Pearson correlation and p value. Some options are available to play with the plot image using icons on the top from left to right:
<br>
<table>
<tr> <td><img src="files/icon1.png" alt="icon"></td> <td> Downloads the plot as a png.</td> </tr>
<tr> <td><img src="files/icon2.png" alt="icon"></td> <td> Allows the user to zoom in on an area of interest by clicking and dragging with the pointer.</td> </tr>
<tr> <td><img src="files/icon3.png" alt="icon"></td> <td> Autoscales the image.</td></tr>
<tr> <td><img src="files/icon4.png" alt="icon"></td> <td> Allows the user to create horizontal and vertical line from either a cell line dot or the regression line, by hovering over them.</td></tr>
</table>

![Screenshot of CellMinerCDB Application](files/Slide02.jpg)

**Figure 2**: An example scatterplot of SLFN11 gene expression (x-axis)  versus Topotecan drug activity (y-axis)/ both from the NCI60. Since Topotecan has 2 different drug ids in the NCI-60, the one with the lowest number of missing data is selected (here 609699). However, the user can type in their specific drug ID of interest. The Pearson correlation value and p value appear at the top of the plot. A linear fitting curve is included. This is an interactive plot and whenever the user changes any input value, the plot will be updated. Any point in the plot can be hovered over to provide additional information about cell line, tissue, Onco tree designation,  and x and y coordinate values.


<h3 id="download">View Data</h3>
This option both displays the data selected from the **Plot Data** tab in tabular form, and provides a **Download selected x and y axis data as Tab-Delimited File** option. The user can change the input data in the left selection panel as described for Plot Data. The displayed table include the cell line, the x-axis value, the y-axis value, the tissue of origin and the 4 onco-tree levels. Within the header the selected features are prefixed by the data type abbreviation and post-fixed by the data source.

![Screenshot of CellMinerCDB Application](files/Slide03.jpg)

**Figure 3**: shows the selected values for SLFN11 gene expression (x-axis) and Topotecan (id 609699) drug activity (y-axis) from the NCI-60 across all common lines. The features are coded as expSLFN11_nci60 and act609699_nci60 where “exp” and “act” represent respectively prefixes for gene expression based on z-score and drug activity.

<h3 id="compare">Compare Patterns</h3>
This option allows one to compute the correlation between the selected feature as defined from the specified **Cell Line Set, Data Type**, and **Identifier** from either the x or y-axis selections, and either all drug or all molecular data from the same source. 

Pearson’s correlations are provided, with reported p-values (not adjusted for multiple comparisons) in tabular form. This displays features are organized by level of correlation, and includes target pathway for genes and mechanism of action (MOA) for drugs (if available). 

![Screenshot of CellMinerCDB Application](files/Slide04.jpg)

**Figure 4**: shows correlation results for SLFN11 gene with all other molecular features for all NCI60 datasets sorted by correlation value with gene location and target pathways (annotation field).

<h2 id="regression">Regression Models</h2>
The ‘Regression Models’ option (or module) has multiple tabs including Heatmap, Data, Plot, Cross-Validation, Tehnical Details and Partial Correlation (described below), and allows construction and assessment of multivariate linear response prediction models. For instance, we can assess prediction of a drug activity based on some genes expression. To construct a regression model, you need to specify the input data in the left side panel.

<h4 id="inputs2">Input data</h4>

1. **Cell Line Set** selects the data source. The user can choose: NCI60, CCLE, GDSC, CTRP or NCI/DTP SCLC (see Data Sources for more details). 
<br><br>
2. The response variable by selecting 
  - **Response Data Type** (example: a drug or a molecular dataset). The options for this vary dependent on the source selected above, and appear in the Response Data Type dropdown. See the Metadata tab for data types description.
  - **Response Identifier** (e.g., a specific drug or gene identifier)
<br><br>
3. The predictor variables from the same data source by selecting
  - **Predictor Data Type/s** (as explained in response data type). Use command button on Macs or control key on PCs to select more than one dataset.
  - **Minimal Range Value** provides a required minimum for the identifier to be included for the first listed data type. The default is 0. One may increase this value to eliminate predictors that are considered to have insufficient range to be biologically meaningful.
  - **predictor identifiers** from the selected data types are required for the **Linear Regression** algorithm. In figure 5, we explore linear model prediction of Topotecan drug activity in the NCI-60 choosing SLFN11 and BTPF gene expression. Identifiers from different sources may be combined using 2 methods. In the first, select multiple Data Types as desired, and enter your identifiers. The model will be built automatically using those Data Types and Identifiers. For example, if expression and mutation are selected as Data Types and SLFN11 and BPTF are entered as identifiers, the model will be built using 4 identifiers: expSLFN11, expBTPF, mutSLFN11 and mutBTPF. In the second, more specific approach, you enter the identifier with the data type prefix. For example, if your predictor variables are specifically the expression value for SLFN11 and mutation value for BTPF then you can enter as identifiers: expSLFN11 and mutBTPF. Predictors are optional for the Lasso algorithm (see point 5) since it identifies automatically the ones that best fit the Lasso model.
<br><br>
4. Selected tissues: by default, all cell lines are included however you can selected some based-on tissue
  - **Select Tissues** to include or exclude specific tissues
  - **Select Tissues of Origin Subset/s** : by default, all tissues are selected and included. The user may also select specific tissues (to include or exclude). On Macs, more than one tissue of origin may be selected using the “command” key. On PC's use the "control" key.
<br><br>
5. **Algorithm**: by default, the basic linear regression model is selected however you can select the Lasso model (penalized linear regression model). If Lasso algorithm is selected, you have to specify:
  - **Select Gene Sets**: The gene selection is based on curated gene sets such as DNA Damage Repair DDR or Apoptosis. The user can select one or more gene sets.
  - **Maximum Number of Predictors** (default 4)

Once all the above information is entered, a regression model is built and the results are shown in different ways such as the technical details of the model, observed vs. predictive responses plots or variables heatmap. Find below an explanation of different output for the regression model module.

<h3 id="heatmap">Heatmap</h3>
This option provides the observed response and predictor variables across all source cell lines as an interactive heatmap. The user can restrict the number of cell lines to those that have the highest or lowest response values by selecting **Number of High/Low Response Lines to Display** 

![Screenshot of CellMinerCDB Application](files/Slide05.jpg)

**Figure 5**: An example heatmap where we selected topotecan as a response variable and SLFLN11 and BPTF gene expression as predictor variables. In this example, we chose to display only 40 cell lines that have the most 20 highest and 20 lowest values for topotecan activity.

In case, the Lasso algorithm is selected	more predicted variables are shown based on model result as shown below (STK17B and ABCD3 new genes added)


![Screenshot of CellMinerCDB Application](files/Slide06.jpg)

**Figure 6**: same example as previous figure with the lasso algorithm

<h3 id="data">Data</h3>
This option shows the detailed data for the model variables for each cell line. Both the 10-fold cross validation (CV) as well as the predicted responses are given. The data is displayed as a table with filtering options for each column. 


![Screenshot of CellMinerCDB Application](files/Slide07.jpg)

**Figure 7**: data related to the simple linear regression model presented in the previous section.

<h3 id="plotpred">Plot</h3>
This option enables one to plot and compare the observed response values (y-axis) versus the predicted response values (x-axis). The predicted response values are derived from a linear regression model fit to the full data set.

![Screenshot of CellMinerCDB Application](files/Slide08.jpg)

**Figure 8**: plot comparing Topotecan observed vs. predicted activity with high correlation value of 0.84

<h3 id="cross">Cross-Validation</h3>
This option enables to plot the observed response values (y-axis) versus the 10-fold cross-validation predicted response values (x-axis). With this approach, the predicted response values are obtained (over 10 iterations) by successively holding out 10% of the cell lines and predicting their response using a linear regression model fit to the remaining 90% of the data. Cross-validation is widely used in statistics to assess model generalization to independent data – with the caveat that the independent data must still share the same essential structure (i.e., probability distribution) as the training data. It can also indicate possible overfitting of the training data, such as when the observed versus full data set model-predicted correlation (shown in ‘Plot’) is substantially better than the observed versus cross-validation predicted correlation (shown in ‘Cross-Validation’).


![Screenshot of CellMinerCDB Application](files/Slide09.jpg)

**Figure 9**: plot comparing Topotecan observed vs. cross-validation predicted activity with still high correlation value of 0.82

<h3 id="details">Technical Details</h3>
This option enables the user to view the R statistical and other technical details related to the predicted response model. To save, these results may be copied and pasted into the document or spreadsheet of your choice. 

![Screenshot of CellMinerCDB Application](files/Slide10.jpg)

**Figure 10**: example of regular regression model fitting results

<h3 id="partialcorr">Partial correlations</h3>

This function is used to identify additional predictive variables for a multivariate linear model. Conceptually, the aim is to identify additional predictive variables that are independently correlated with the response variable, after accounting for the influence of the existing predictor set. Computationally, a linear model is fit, with respect to the existing predictor set, for both the response variable and each candidate predictor variable. The partial correlation is then computed as the Pearson’s correlation between the resulting pairs of model residual vectors (which capture the variation not explained by the existing predictor set). The p-values reported for the correlation and linear modeling analyses assume multivariate normal data. The two-variable plot feature of CellMinerCDB allows informal assessment of this assumption, with clear indication of outlying observations. The reported p-values are less reliable as the data deviate from multivariate normality.

In order to run a partial correlation analysis, the user should first construct a linear model (providing response and predictor variables as explained earlier - steps 1 to 5 in figure below-) and then:
  -	**Select Gene Sets**: The gene selection is based on curated gene sets. Here the user can select one or more gene sets and even all genes (step 6 in figure below)
  -	**Select Data types**: the user can select one or more data type such as gene expression, methylation or copy number variation (step 7 in figure below)
  -	optionally, specify the **Minimum Range** for the first listed data type (step 8 in figure below)
  - And finally click on button **run** (step 9 in figure below).
  
![Screenshot of CellMinerCDB Application](files/Slide11.jpg)

**Figure 11**: An example of  partial correlation results for selected gene expression data using all gene sets.

<h2 id="metadata">Metadata</h2>
This option enumerates for each cell line set, the available data types that could be queried within the app providing the data type abbreviation or prefix, description, feature value unit (z-score, intensity, probability …), platform or experiment and related publication reference (pubmed). The user should:
  - specify the **Cell Line Set** or data source 
  - **Select Data Type to Download** (optional) and then click on **Download Data type** and/or **Download Data Footnotes** to download any data or footnotes for the selected cell line set.

![Screenshot of CellMinerCDB Application](files/Slide12.jpg)

**Figure 12**: shows all data types for NCI60

<h2 id="search">Search IDs</h2>
This page lists the identifiers (ID) available in the selected data source for use in the univariate analysis or regression models. The user chooses:
  - **Cell Line Set** selects the data source. The user can choose: NCI60, CCLE, GDSC, CTRP or NCI/DTP SCLC (see Data Sources for more details).
  - **Select Data Type** selects the data type to query. The options for this vary dependent on the source selected above, and appear in the x-Axis Data Type dropdown. See the Metadata tab for descriptions and abbreviations.

This enables to search all related ID for each combination. For the molecular data, the **gene names (ID) and specific data type information** are provided. For the drugs and compounds, **the identifiers (ID),  Drug name (when available), and Drug MOA (when available)** are displayed. The user can scroll down the whole  list of IDs, or search specific ID(s) by entering a value in the header of any column.

<h3 id="drugid">Drug IDs</h3>
For the NCI-60 and NCI/DTP SCLC, the drug identifiers (ID) are NSC's or names. For the CCLE, GTRP, and CTRP, the drug identifiers are the Drug names.

![Screenshot of CellMinerCDB Application](files/Slide13.jpg)

**Figure 13**: Example of a search: if looking for a drug ID in the NCI-60 select "NCI-60" as the cell line source and select "Drug Activity" as the data type. You can type in search box of column "Drug name" or "MOA".
<br>
<h3 id="geneid">Gene IDs</h3>
For all data sources, the gene ID is the gene name (Hugo name)


![Screenshot of CellMinerCDB Application](files/Slide14.jpg)

**Figure 14**: Example of a search: if looking for a gene ID in the NCI-60 select "NCI-60" as the cell line source and select "gene expression" as the data type. You can type in search box of column "gene name" or "entrez gene id" or "Chromosme"...

<h2 id="navigation">Navigation guide</h2>
<h3 id="multiple">Multiple selection</h3>
In order to select multiple choice from a list, use “command” button for Mac or “alt” button for PC and then click

<h3 id="range">X-axis or Y-axis range</h3>
You can change the x-axis or y-axis lower or higher value to have different views of the displayed plot.

<h3 id="color">Show color</h3>
It is a checkbox that enable and disable colors in the scatter plots

<h2 id="workflow">Exploratory workflow</h2>
Mutilple data analysis workflows may be used dependent of the question being asked. A typical workflow:

1.  Check the relationship between two variables [2D plot]. Example: SLFN11 transcript expression and topotecan drug activity.
<br>
2.  Examine what else might be associated with either the x-axis or y-axis variable [Pattern Comparison]. Example: considering potential biological affects, TGFBR3 (an apoptosis factor) and BPTF (a chromatin factor) transcript expression might be considered candidates for affecting topotecan activity.
<br>
3.  Upon finding two or more associations with single 'response' variable through [Pattern Comparison/2D Plot], check if they complement one another in a multivariate model [Regression Models]. Example: Starting with the dominant SLFN11, adding TGFBR3 does not add to the regression model, but BPTF does.
<br>
4.  Repeat the above steps as needed.

<h2 id="video">Video tutorial</h2>
**For an introduction please refer to our <a href="https://www.youtube.com/watch?v=2HicAgcyJHI" target="_blank">video tutorial</a>**

<h2 id="methods">Methods</h2>
  - <h3 id="linear">Linear regression</h3>
  Basic linear regression models are implemented using the R stats package lm() function

  - <h3 id="lasso">Lasso Model</h3>
  Lasso (penalized linear regression models) are implemented using the glmnet R package. The lasso performs both variable selection and linear model coefficient fitting. The lasso lambda parameter controls the tradeoff between model fit and variable set size. Lambda is set to the value giving the minimum error with 10-fold cross-validation. For either standard linear regression or LASSO models, 10-fold cross validation is applied to fit model coefficients and predict response, while withholding portions of the data to better estimate robustness. 

<h2 id="contactfeedback">Contact/Feedback</h2>
Please send comments and feedback to 
* fathi.elloumi AT nih.gov 
* aluna AT jimmy.harvard.edu 
* vinodh.rajapakse AT nih.gov

<h2 id="data-sources">Data Sources</h2>
CellMinerCDB integrates data from the following sources, which provide additional data and specialized analyses.
* [CellMiner NCI-60](https://discover.nci.nih.gov/cellminer/)
* [Sanger/Massachusetts General Hospital Genomics of Drug Sensitivity in Cancer (GDSC)](http://www.cancerrxgene.org/)
* [Broad/Novartis Cancer Cell Line Encyclopedia (CCLE)](https://portals.broadinstitute.org/ccle)
* [Broad Cancer Therapeutics Response Portal (CTRP)](https://portals.broadinstitute.org/ctrp/)
* [NCI/DTP Small Cell Lung Cancer Project (SCLC)](https://sclccelllines.cancer.gov/sclc/)


<h2 id="about-the-data">About the Data</h2>
For specific information about the data made available for particular sources, please refer to the 'Metadata' navbar tab.

Drug mechanism of action details:
* [NCI60](https://raw.githubusercontent.com/cannin/rcellminer/devel/inst/extdata/Drug_MOA_Key.txt)
* [GDSC](http://www.cancerrxgene.org/translation/Drug)
* [CTRP](https://portals.broadinstitute.org/ctrp/?page=#ctd2Compounds)

Gene sets used for annotation of analysis results or algorithm input filtering were curated by the
NCI/DTB CellMiner team, based on surveys of the applicable research literature.

<h2 id="about-cellminercdb">About CellMinerCDB</h2>
The CellMinerCDB application is developed and maintained using R and Shiny by:

* Augustin Luna; Research Fellow, Biostatistics and Computational Biology, Dana-Farber Cancer Institute, Harvard Medical School
* Vinodh N. Rajapakse; Postdoctoral Fellow, Developmental Therapeutics Branch, National Cancer Institute
* Fathi Elloumi; Bioinformatics Software Engineer, Developmental Therapeutics Branch, National Cancer Institute

<h3 id="nci-dtb-genomics-and-bioinformatics-group">NCI-DTB Genomics and Bioinformatics Group</h3>
* William C. Reinhold
* Sudhir Varma
* Margot Sunshine
* Fathi Elloumi
* Lisa Loman (Special Volunteer)
* Fabricio G. Sousa
* Kurt W. Kohn
* Yves Pommier

<h3 id="biostatistics-and-computational-biology-dana-farber-cancer-institute-harvard-medical-school">Biostatistics and Computational Biology, Dana-Farber Cancer Institute, Harvard Medical School</h3>
* Chris Sander

<h3 id="mskcc-computational-biology">MSKCC Computational Biology</h3>
* Jianjiong Gao
* Nikolaus Schultz

<h2 id="references">References</h2>
Shankavaram UT, Varma S, Kane D, Sunshine M, Chary KK, Reinhold WC, Pommier Y, Weinstein JN. [CellMiner: a relational database and query tool for the NCI-60 cancer cell lines. ](https://www.ncbi.nlm.nih.gov/pubmed/19549304) BMC Genomics. 2009 Jun 23;10:277. doi: 10.1186/1471-2164-10-277.

Reinhold WC, Sunshine M, Liu H, Varma S, Kohn KW, Morris J, Doroshow J, Pommier Y.
[CellMiner: a web-based suite of genomic and pharmacologic tools to explore transcript and drug patterns in the NCI-60 cell line set. ](https://www.ncbi.nlm.nih.gov/pubmed/22802077) Cancer Res. 2012 Jul 15;72(14):3499-511. doi: 10.1158/0008-5472.CAN-12-1370.

Reinhold WC, Sunshine M, Varma S, Doroshow JH, Pommier Y. [Using CellMiner 1.6 for Systems Pharmacology and Genomic Analysis of the NCI-60. ](https://www.ncbi.nlm.nih.gov/pubmed/26048278) Clin Cancer Res. 2015 Sep 1;21(17):3841-52. doi: 10.1158/1078-0432.CCR-15-0335. Epub 2015 Jun 5.

Luna A, Rajapakse VN, Sousa FG, Gao J, Schultz N, Varma S, Reinhold W, Sander C, Pommier Y. [rcellminer: exploring molecular profiles and drug response of the NCI-60 cell lines in R.](https://www.ncbi.nlm.nih.gov/pubmed/26635141) Bioinformatics. 2015 Dec 3. pii: btv701.





