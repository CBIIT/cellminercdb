library(shiny)
library(rcellminer)
library(shinycssloaders)
library(plotly)
library(markdown)

# getOption("repos")
#
#--------------------------------------------------------------------------------------------------
# LOAD CONFIGURATION AND REQUIRED DATA SOURCE PACKAGES.
#--------------------------------------------------------------------------------------------------
config <- jsonlite::fromJSON("config.json")
appConfig <- jsonlite::fromJSON("appConfig.json")
metaConfig <- jsonlite::fromJSON("configMeta.json")

toplinks <- appConfig$TopLinks
category <- appConfig$category
banner <- appConfig$banner

source("modal_bs5.R")
source("appUtils.R")

if (!is.null(appConfig$appName)){
	appTitle <- appConfig$appName
} else{
	appTitle <- "CellMiner"
}

dataSourceChoices <- setNames(names(config),
															vapply(config, function(x) { x[["displayName"]] }, 
																		 character(1)))
options = ""
for(y in 1:length(dataSourceChoices)){
  if (dataSourceChoices[y]=="nci60")
  {
    options =  paste0(options,"<option value=",dataSourceChoices[y]," selected>",names(dataSourceChoices)[y],"</option>;")
  }
  else
   {
   options =  paste0(options,"<option value=",dataSourceChoices[y],">",names(dataSourceChoices)[y],"</option>;");
   }
  }

#print(options)
metaChoices <- setNames(names(metaConfig),
												vapply(metaConfig, function(x) { x[["displayName"]] }, 
															 character(1)))

metaoptions = ""
for(y in 1:length(metaChoices)){
  if (metaChoices[y]=="nci60")
  {
    metaoptions =  paste0(metaoptions,"<option value=",metaChoices[y]," selected>",names(metaChoices)[y],"</option>;")
  }
  else
  {
    metaoptions =  paste0(metaoptions,"<option value=",metaChoices[y],">",names(metaChoices)[y],"</option>;");
  }
}

listlinks = ''
for (k in 1:nrow(toplinks)) {
  listlinks=paste0(listlinks,tags$a(href=toplinks$url[k],toplinks$label[k],style="font-size: 18px;float: right;background-color: steelblue;color: white;display: inline-block;margin: 5px 5px;padding: 10px 10px;",target="_blank"),"\n")
}
# cat(listlinks)
# if (category == "internal") mytitle="<p style='text-align: center; font-size: 20px; color:blue;' >~ Internal version ~</p>" else  
#      if (category == "private") mytitle="<p style='text-align: center; font-size: 20px; color:red;' >~ Private version ~</p>" else 
#           mytitle=""
# JMR1
if (category == "internal") mytitle="Internal" else  
  if (category == "private") mytitle="Private" else 
    mytitle="Public"  ## remove public by JW***

#if("rCharts" %in% installed.packages()) {
#	options(RCHART_LIB='highcharts')	
#	library(rCharts)
#	hasRCharts <- TRUE
#} else {
#	hasRCharts <- FALSE
#}
## ---

shinyUI(
  fluidPage(
  theme = bslib::bs_theme(version=5),
  tags$html(lang="en"), 
  # #tags$head(tags$style(type="text/css", ".body {color: blue;}",".clear {clear:both}")),
  # tags$a(href="#skiplink","Skip over navigation",style="font-size: 10px; float: left"),
  # 
  # # HTML("<p style='text-align: center; font-size: 20px; color:blue;' >~ Internal version ~</p>"),
  # HTML(mytitle),
  # #tags$h4("~Internal version~",style="color: blue"),
  # # br(),
  # # tags$html("~Internal version~",style="text-align: center; font-size: 20px"),
  # 
  # # tags$a(href="https://discover.nci.nih.gov/cellminer/"," CellMiner NCI-60 ",style="font-size: 14px;float: right;background-color: steelblue;color: white;display: inline-block;margin: 5px 5px;padding: 10px 10px;",target="_blank"),
  # # tags$a(href="https://dtp.cancer.gov"," NCI/DCTD/DTP ",style="font-size: 14px;float: right;background-color: steelblue;color: white;display: inline-block;margin: 5px 5px;padding: 10px 10px;",target="_blank"),

  #JMR2 START - new USWDS header, how you know and skip to main
  ### includeHTML("www/uswds/ui/header2.html"), ## works without search Box !!!!!!!!!!!!!!!!!!!!!****************************
  includeHTML("www/uswds/ui/header.html"), ## new update JW***
  ## tags$div(class="ribbon ribbon-top-left",tags$span(HTML(mytitle))), ## RIBBON not needed

  # tags$div(tags$span(HTML(listlinks))), OK
  # tags$span(HTML(listlinks)), Ok
  ## HTML(listlinks), # Ok ------------------ NOT NEEDED ----------------------------------------
  ## big div
  tags$div(class="usa-section",
           tags$div(class="grid-container margin-bottom-10",
                    tags$div(class="gpf-content usa-prose site-prose",
                             tags$header(
                             ##  tags$p("Tools", class="site-subheading"),  # JW***
                               tags$h1(class="site-page-title tablet:margin-bottom-0",
                                       HTML(appConfig$appName))),
                             tags$div(class="margin-bottom-5",
                                      tags$span("Version:", class="post-date site-subheading",
                                                HTML(appConfig$appVersion)),
                                      tags$span("- Release:", class="post-date site-subheading",
                                                HTML(appConfig$appRelease))),
                             #JMR END
                             
  # HTML(listlinks), # activated by JW***
  # tags$div(tags$span(HTML(listlinks))),
  
  ###tags$p("CellMinerCDB",style="font-size: 24px;color: white;background-color: dodgerblue;text-align:center;height:50px;"),
  ### tags$img(src = "files/banner.jpg",height="110px",width="1650px"),
  # tags$img(src = "files/banner.png",alt= "banner",height="100%",width="100%", border="0"),
  
  ## JMR3
  # tags$img(src = banner,alt= "banner",height="100%",width="100%", border="0"),
  
  #tags$img(src = "files/banner.png",alt= "banner",height="100%",width="100%", border="0", style="padding: 0px; display: block; line-height: 0; font-size: 0px; border: 0px; clear: both; vertical-align: top; margin: 0px 0px 0px 0px;"),
   #navbarPage(h6(style="vertical-align:top;font-size: 24px;color: dodgerblue;",appTitle), 
   # navbarPage(HTML("<p style='font-size: 24px;color: dodgerblue;'>", appTitle,"</p>"), 
  	navbarPage(title="", 
  	## navbarPage(title="CellMinerCDB | Genomics and Pharmacology Facility", ## JW***
  	         id="nv",
						 inverse=FALSE,
						 header = list(tags$head(includeCSS("www/css/hacks.css")),
						               #JMR4 USWDS Update
						               tags$head(tags$link(rel="icon", type="image/png", href="uswds/img/favicons/favicon-32x32.png")),
						               tags$head(includeCSS("www/uswds/css/styles.css")),
						               tags$head(includeCSS("www/uswds/css/gpf_theme.css")),
						               #
						               #tags$head(includeCSS("www/css/tooltip.css")),
						 							 # Add/run startup Javascript
						 							 tags$head(tags$script(onloadJs)),
						 							 # Use JQuery (built into Shiny) calls to show/hide modal based on message
						 							 tags$head(includeScript("www/js/showLoading.js")),
						 							 tags$head(includeScript("www/js/showSkip.js")),
						 							 tags$head(includeScript("www/js/leaving.js")),
						 							 # load Javascript snippet to parse the query string.
						 							 #tags$script(includeScript("www/js/parse_input.js")),
						 				##			 tags$head(includeScript("www/js/google-analytics.js")), ## old
						 							 ## new GA4
						 							 tags$head(HTML(
						 							 "<script async src='https://www.googletagmanager.com/gtag/js?id=G-2Y2FCKE28Y'></script>
						 							 <script>
						 							     window.dataLayer = window.dataLayer || [];
						 							   function gtag(){dataLayer.push(arguments);}
						 							   gtag('js', new Date());
						 							   
						 							   gtag('config', 'G-2Y2FCKE28Y');
						 							   </script>"
						 							 )),
						 							 ## end GA4
						 							 #JMR USWDS Update JW***
						 							 tags$head(tags$script(src="uswds/js/uswds-init.min.js")),
						 							 tags$head(HTML("<script async type='text/javascript' src='https://dap.digitalgov.gov/Universal-Federated-Analytics-Min.js?agency=HHS&subagency=NCI' id='_fed_an_ua_tag'> </script>")),
						 							 # tags$head(includeScript("www/js/dispstatic.js")),
						 							 # new static pages ***********************************
						 					###### >> ## tags$head(includeScript("www/js/addstatic.js")),
						 							 # tags$head(HTML('<script>$(document).ready(function() {$(".navbar-nav").append("<li><a href="static/index.html" target="_blank">Cancer Type Summaries</a></li>");});</script>')),
						 							 # tags$head(HTML('<script>$(document).ready(function() {$("#nv").append("<li><a href="www/static/index.html" target="_blank">Cancer Type Summaries</a></li>");});</script>')),
						 							 # ***********************************************
						 							 
						 							 # tags$head(
						 							 #   tags$style(type="text/css", ".irs-grid-text { font-size: 8pt;color: black; }",
						 							 #              ".irs-min { font-size: 8pt; background: white; }", ".irs-max { font-size: 8pt; background: white;}",
						 							 #              ".irs-from { font-size: 8pt; color: black;background: white;}", ".irs-to { font-size: 8pt;  color: black;background: white;}"
						 							 #              , "body {font-size: 14pt;}", "img {display: block;}", ".clear {clear: both}"
						 							 #   )
						 							 # ),
						 							 tags$head(
						 							   tags$style(HTML(
						 							     paste0(".navbar-nav { font-size: 24px; color: black; }"),
						 							     paste0(".navbar-default .navbar-brand { font-size: 24px; color: dodgerblue; }")
						 							   )
						 							   )
						 							   
						 							 ),
						 				      tags$head(
						 				      tags$style(HTML("
                               .navbar-nav .nav-link {
                                font-size: 1.5rem; /* Adjust this value as needed */
                                }
                             "))
						 				      ),
						 							 tags$head(tags$title("CellMinerCDB")),
						 							 tags$head(tags$meta(name="description",content="CellMiner Cross Database (CellMinerCDB) is the first web application to allow translational researchers to conduct analyses across all major cancer cell line pharmacogenomic data sources from NCI-DTP NCI-60, Sanger GDSC, and Broad CCLE/CTRP"),
						 							           tags$meta(name = "viewport", content = "width=1600")),
						 							 tags$head(HTML("<script type=\"application/ld+json\">
  {
  \"@context\":\"https://schema.org/\",
  \"@type\":\"Dataset\",
  \"name\":\"NCI60 and other cancer cell line datasets\",
  \"description\":\" CellMinerCDB is a resource that simplifies access and exploration of cancer cell line pharmacogenomic data across different sources\",
  \"url\":\" https://discover.nci.nih.gov/cellminercdb/\",
  \"keywords\":[
  \"NCI60\",
  \"GDSC\",
  \"CCLE\",
  \"CTRP\"
  ],
  \"creator\":{
  \"@type\":\"Organization\",
  \"url\": \" https://discover.nci.nih.gov/ \",
  \"name\":\"GPF/DTB/CCR/NCI/NIH\",
  \"contactPoint\":{
  \"@type\":\"ContactPoint\",
  \"contactType\": \"customer service\",
  \"email\":\"Webadmin@discover.nih.gov\"
  }
  }
  }
  </script>"))
						              ),
		#background-color: blue; font-color: white;
		#------[NavBar Tab: Univariate Analyses]---------------------------------------------------------
		tabPanel("Univariate Analyses",
			fluidPage(
    		loadingModal(),
	    	sidebarLayout(
	        sidebarPanel(
	        	width=4, 
	        	tags$div(
	        	  id="input_container",
	        	  tags$a(id="skiplink"),
	            #selectInput("xDataset", "x-Axis Cell Line Set", choices=dataSourceChoices, selected = "nci60"),
	        	  HTML(
	        	    paste("<label class='control-label' for='xDataset'>x-Axis Cell Line Set</label>","<select id='xDataset'>",options,"</select>")
	        	  ),
	        	  uiOutput("xPrefixUi"),
	            ## textInput("xId", "Identifier: (e.g. topotecan or SLFN11)", "SLFN11"),
	        	  uiOutput("xIdUi"),
	        	  
	        	  conditionalPanel(
	        	   condition = "input.ts == 1 || input.ts == 2 || input.ts == 4",
	        	    # cat("inside condition \n"),
	        	    uiOutput("xAxisRangeUi")
	        	  ),
	        	
	        	  br(),br(),
	            #selectInput("yDataset", "y-Axis Dataset", choices=dataSourceChoices, selected = "nci60"),
	        	  HTML(
	        	    paste("<label class='control-label' for='yDataset' id='lyd'>y-Axis Cell Line Set</label>","<select id='yDataset'>",options,"</select>")
	        	  ),
	          conditionalPanel(condition="input.ts==1 || input.ts==2 || input.ts==4",
	        	   uiOutput("yPrefixUi"),
	          	## textInput("yId", "Identifier: (e.g. topotecan or SLFN11)", "topotecan"),
	        	    uiOutput("yIdUi"),
	        	  
	          	  uiOutput("yAxisRangeUi"),
	          	
	            # checkboxInput("showColor", "Show Color?", value=TRUE),
                br()
	        	  ) # end conditional panel
	        	  , 
	          	radioButtons("tissueSelectionMode", "Select Tissues", c("To include", "To exclude")),
	          	uiOutput("selectTissuesUi"),
	        	  ## cat("after tree1"),
	        	  conditionalPanel(condition="input.ts==1 || input.ts==2 || input.ts==4",
	        	   checkboxInput("showColor", "Show Color?", value=TRUE),
	        	   ## cat("prep tree2"),
	             uiOutput("showColorTissuesUi"),
	        	  ), # end conditional panel
	        	  ## cat("after tree2")
	        	)
	       
	        ),
        mainPanel(
          #div(style="font-size: 16px", align="center", "CellMinerCDB enables exploration and analysis of cancer cell line pharmacogenomic data across different sources. If publishing results based on this site, please cite: ", a("Rajapakse.VN, Luna.A, Yamade.M et al. iScience, Cell Press. 2018 Dec 12.", href="https://www.cell.com/iscience/fulltext/S2589-0042(18)30219-0", target = "_blank", style="font-size: 16px;", class = "dm")),
          div(style="font-size: 16px", align="center", "CellMinerCDB enables exploration and analysis of cancer cell line pharmacogenomic data across different sources. If publishing results based on this site, please cite: ", a("Luna A, Elloumi F, Varma S et al. Nucleic Acids Res. 2021 Jan 8.", href="https://academic.oup.com/nar/article/49/D1/D1083/5983630", target = "_blank", style="font-size: 16px;", class = "dm")),
          # uiOutput('tabsetPanel') # old , good for Bootstrap 3
         
           
          tabsetPanel(id="ts",
                      tabPanel("Plot Data", value=1, uiOutput("showCellsUi"), plotlyOutput("rChartsAlternative", width = plotWidth, height = plotHeight),
                               br(), br(), p("Plot point tooltips provide additional information.")),
                      tabPanel("View Data", value=2,
                               downloadLink("downloadData", "Download selected x and y axis data as a Tab-Delimited File"),
                               DT::dataTableOutput("table")), 
                      tabPanel("Compare Patterns", value=3,
                               includeMarkdown("www/files/help.md"),
                               #br(),
                               HTML("<b>Pattern comparison results are computed with respect to that data defined and shared by both the x and y-axis inputs.</b>"),
                               br(),br(),
                               fluidRow(
                                 #column(3, selectInput("patternComparisonType", "Pattern Comparison",
                                 #           						choices=c("Molecular Data"="molData", "Drug Data"="drug"), 
                                 #											selected="molData")),
                                 
                                 column(4, HTML(
                                   paste("<label class='control-label' for='patternComparisonType'>Select molecular or activity data</label>","<select id='patternComparisonType'><option value='moldata' selected>Molecular Data</option><option value='drug'>Drug Data</option></select>")
                                 )),
                                 
                                 column(8, radioButtons("crossdb", label = NULL, choices = list("Compare x-Axis input to x-Axis molecular or activity data" = "No", "Compare x-Axis input to y-Axis molecular or activity data" = "Yes"), selected  = "No", inline=F, width="100%")       
                                 )
                                 
                               ),
                               br(),
                               renderUI({
                                 req(PatternCompTable())
                                 downloadLink("downloadDataComp", "Download All as a Tab-Delimited File")
                               }),
                               ##downloadLink("downloadDataComp", "Download All as a Tab-Delimited File"),
                               withSpinner(DT::dataTableOutput("patternComparison"))),
                      tabPanel("Tissue Correlation", value=4,
                               #downloadLink("downloadData", "Download selected x and y axis data as a Tab-Delimited File"),
                               DT::dataTableOutput("cortable"))
                      )
          
        )
    	 )
			)
		),
		#-----[NavBar Tab: Regression Models]------------------------------------------------------------
		regressionModelsInput("rm", dataSourceChoices),
		#-----[NavBar Tab: Metadata]---------------------------------------------------------------------
		tabPanel("Metadata", 
						 fluidPage(	
						 	sidebarLayout(
						 		sidebarPanel(
						 			width=3, 
						 			tags$div(
						 				id="input_container", 
						 				tags$a(id="skiplink"),
						 				#selectInput("mdataSource", "Data Source", choices=metaChoices, selected = "nci60")
						 				HTML(
						 				  paste("<label class='control-label' for='mdataSource'>Cell Line Set</label>","<select id='mdataSource'>",metaoptions,"</select>")
						 				),
						 				br(),br(),br(),br(),br(),br(),
						 				uiOutput("dataTypeUi"),
						 				br(),
						 				downloadButton('downloadExp', 'Download Data'),
						 				br(),br(),
						 				downloadButton('downloadFoot', 'Download Footnotes'),
						 				br(),br(),br(),br(),br(),br(),
						 				HTML("<b>Download current cell line set information</b>"),
						 				downloadButton('downloadCell', 'Download cell lines annotation'),
						 				br(),br(),
						 				HTML("<b>Download drug synonyms table with matching IDs for all cell line sets</b>"),
						 				downloadButton('downloadSyn', 'Download Table'),
						 				br(),br()
						 				#uiOutput(""),
						 			)
						 		), #end sidebarPanel
						 		mainPanel(
						 		  # htmlOutput('sourceLink'),
						 		  uiOutput('sourceLink'),
						 			uiOutput('metadataPanel')
						 			#h4(htmlOutput('sourceLink'))
						 			# htmlOutput('sourceLink')
						 		)
						 	)
						 ) #end fluidPage
		), #end tabPane 
		#-----[NavBar Tab: Metadata]---------------------------------------------------------------------
		tabPanel("Search IDs",
		         fluidPage(
		           sidebarLayout(
		             sidebarPanel(
		               width=3,
		               tags$div(
		                 id="input_container",
		                 tags$a(id="skiplink"),
		                 #selectInput("mdataSource", "Data Source", choices=metaChoices, selected = "nci60")
		                 HTML(
		                   paste("<label class='control-label' for='dataSrc'>Cell Line Set</label>","<select id='dataSrc'>",options,"</select>")
		                 ),
		                 br(),br(),br(),br(),
		                 uiOutput("dataTypeUi_s")
		                 # uiOutput("dataTypeUi"),
		                 # br(),
		                 # downloadButton('downloadExp', 'Download data for selected type')
		                 #uiOutput(""),
		               )
		             ), #end sidebarPanel
		             mainPanel(
		               #includeMarkdown("www/files/help.md"),
		             ##  DT::dataTableOutput("ids2")
		        ###       DT::dataTableOutput("ids_s")
	               uiOutput('searchPanel')
		               #h4(htmlOutput('sourceLink'))
	 #              htmlOutput('sourceLink')
		             )
		           )
		         ) #end fluidPage
		), #end tabPane
		#-----[NavBar Tab: About]------------------------------------------------------------------------
		tabPanel("Help",
		         tags$a(id="skiplink"),
		         includeMarkdown("www/files/guide.md")
		         ## includeHTML("www/files/guide2.html")
		         #h1("For testing"),
		         #textOutput("ipAddress")
		),
    tabPanel("Video tutorial",
         tags$a(id="skiplink"),
         includeMarkdown("www/files/video.md")
         #h1("For testing"),
         #textOutput("ipAddress")
     ),
   tabPanel("Release notes",
         tags$a(id="skiplink"),
         includeMarkdown("www/files/release.md")
      
     ),
		tabPanel("Cell lines",
		         tags$a(id="skiplink"),
		         HTML('<div id="cellminercdb-cancer-cell-line-pharmacogenomics-exploration" class="section level1 gpf-content usa-prose site-prose">
                        <h1 class="gpf-content usa-prose site-prose">CellMinerCDB: Cancer Cell Line Pharmacogenomics Exploration</h1>
                        <p>CellMiner Cross-Database (CellMinerCDB, <a href="https://discover.nci.nih.gov/cellminercdb" target="_blank">https://discover.nci.nih.gov/cellminercdb</a>) allows integration and analysis of molecular and pharmacological data within and across cancer cell line datasets. If you use content from this site, please cite: <a href="https://pubmed.ncbi.nlm.nih.gov/33196823/" target="_blank">Luna A et al. 2021. PMID: 33196823</a>, <a href="https://pubmed.ncbi.nlm.nih.gov/30553813/" target="_blank">Rajapakse VN et al. 2018. PMID: 30553813</a>, and <a href="https://pubmed.ncbi.nlm.nih.gov/26635141/" target="_blank">Luna et al. 2016., PMID: 33196823</a>.</p>
                        </div>
		                     <br>'),
		         ## includeHTML("www/cell_lines/index2.html")
		         uiOutput('searchCells')
		         
		),
		tabPanel("Common Drugs",
		         tags$a(id="skiplink"),
		         HTML('<div id="cellminercdb-cancer-common-drugs" class="section level1 gpf-content usa-prose site-prose">
                        <h1 class="gpf-content usa-prose site-prose">CellMinerCDB: Common Cancer Drugs between data sources</h1>
                        <p>CellMiner Cross-Database (CellMinerCDB, <a href="https://discover.nci.nih.gov/cellminercdb" target="_blank">https://discover.nci.nih.gov/cellminercdb</a>) allows integration and analysis of molecular and pharmacological data within and across cancer cell line datasets. If you use content from this site, please cite: <a href="https://pubmed.ncbi.nlm.nih.gov/33196823/" target="_blank">Luna A et al. 2021. PMID: 33196823</a>, <a href="https://pubmed.ncbi.nlm.nih.gov/30553813/" target="_blank">Rajapakse VN et al. 2018. PMID: 30553813</a>, and <a href="https://pubmed.ncbi.nlm.nih.gov/26635141/" target="_blank">Luna et al. 2016., PMID: 33196823</a>.</p>
                        </div>
		                     <br>'),
		         
		         uiOutput('searchDrugs')

		)

	)
  
  # tags$head(HTML('<script>$("#nv").append("<li><a href="www/static/index.html" target="_blank">Cancer Type Summaries</a></li>");</script>')),
 # tags$a(id="skiplink")
# tags$div(style="font-size: 12px",

#
# tags$p("CellMinerCDB is a development of the ",
# tags$a("Genomics and Pharmacology Facility,", href="https://discover.nci.nih.gov/", target = "_blank",style="font-size: 12px;"),
# tags$a(" Developmental Therapeutics Branch (DTB), ",href='https://ccr.cancer.gov/Developmental-Therapeutics-Branch', target='_blank',style="font-size: 12px;"),
# tags$a("Center for Cancer Research (CCR), ", href="https://ccr.cancer.gov/", target = "_blank",style="font-size: 12px;"),
# tags$a("National Cancer Institute (NCI) ", href="https://www.cancer.gov/", target = "_blank",style="font-size: 12px;"),
# "prepared in collaboration with the ",
# tags$a("cBio Center", href="http://www.sanderlab.org/", target = "_blank",style="font-size: 12px;", class = "dm"),
# " at the Dana-Farber Cancer Institute.",
# br(),br(),
# # tags$html("Please email 'Webadmin@discover.nci.nih.gov' with any problems, questions or feedback on the tool",style="font-size: 12px; float: left"),
# "Please ", 
# tags$a("email us", href="mailto:Webadmin@discover.nci.nih.gov&subject=CellMinerCDB",style="font-size: 12px;"),
# " with any problems, questions or feedback on the tool",
# br(),br(),
# tags$a("Notice and Disclaimer", href="files/disclaimer.html", target = "_blank")
# # tags$a(" HHS Vulnerability Disclosure.",href='https://www.hhs.gov/vulnerability-disclosure-policy/index.html', target='_blank',style="font-size: 12px;")
# 
# ))

 )
 )
  
  #includeMarkdown("www/files/guide.md")
  ## add email + Notice and Disclaimer + check font size to lower?
  ## library(mailR)

  ),# end big div

#JMR5 USWDS Update - add footer and JS
includeHTML("www/uswds/ui/footer.html"),
tags$div(includeScript("www/uswds/js/uswds.min.js")) 
# new
# tags$script(HTML("
#     async function fetchData() {
#       try {
#         let response = await fetch('your_endpoint');
#         let data = await response.json();
#         console.log(data);
#       } catch (error) {
#         console.error('Error:', error);
#       }
#     }
#     fetchData();
#   "))
  ) # fluid page
) # shiny UI
