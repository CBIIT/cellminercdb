library(shiny)
library(rcellminer)
# library(microplot)
#--------------------------------------------------------------------------------------------------
# LOAD CONFIGURATION AND REQUIRED DATA SOURCE PACKAGES.
#--------------------------------------------------------------------------------------------------
config <- jsonlite::fromJSON("config.json")
appConfig <- jsonlite::fromJSON("appConfig.json")
metaConfig <- jsonlite::fromJSON("configMeta.json")

toplinks <- appConfig$TopLinks
category <- appConfig$category
banner <- appConfig$banner

source("modal1.R")
source("appUtils.R")

if (!is.null(appConfig$appName)){
	appTitle <- appConfig$appName
} else{
	appTitle <- "CellMiner"
}

dataSourceChoices <- setNames(names(config),
															vapply(config, function(x) { x[["displayName"]] }, 
																		 character(1)))
options = "";
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

metaoptions = "";
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
  listlinks=paste0(listlinks,tags$a(href=toplinks$url[k],toplinks$label[k],style="font-size: 14px;float: right;background-color: steelblue;color: white;display: inline-block;margin: 5px 5px;padding: 10px 10px;",target="_blank"),"\n")
}
# cat(listlinks)
if (category == "internal") mytitle="<p style='text-align: center; font-size: 20px; color:blue;' >~ Internal version ~</p>" else  
     if (category == "private") mytitle="<p style='text-align: center; font-size: 20px; color:red;' >~ Private version ~</p>" else 
          mytitle=""

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
  tags$html(lang="en"), 
  #tags$head(tags$style(type="text/css", ".body {color: blue;}",".clear {clear:both}")),
  tags$a(href="#skiplink","Skip over navigation",style="font-size: 10px; float: left"),
  
  # HTML("<p style='text-align: center; font-size: 20px; color:blue;' >~ Internal version ~</p>"),
  HTML(mytitle),
  #tags$h4("~Internal version~",style="color: blue"),
  # br(),
  # tags$html("~Internal version~",style="text-align: center; font-size: 20px"),
 
  # tags$a(href="https://discover.nci.nih.gov/cellminer/"," CellMiner NCI-60 ",style="font-size: 14px;float: right;background-color: steelblue;color: white;display: inline-block;margin: 5px 5px;padding: 10px 10px;",target="_blank"),
  # tags$a(href="https://dtp.cancer.gov"," NCI/DCTD/DTP ",style="font-size: 14px;float: right;background-color: steelblue;color: white;display: inline-block;margin: 5px 5px;padding: 10px 10px;",target="_blank"),
  HTML(listlinks),
  
  ###tags$p("CellMinerCDB",style="font-size: 24px;color: white;background-color: dodgerblue;text-align:center;height:50px;"),
  ### tags$img(src = "files/banner.jpg",height="110px",width="1650px"),
  # tags$img(src = "files/banner.png",alt= "banner",height="100%",width="100%", border="0"),
  tags$img(src = banner,alt= "banner",height="100%",width="100%", border="0"),
  
  #tags$img(src = "files/banner.png",alt= "banner",height="100%",width="100%", border="0", style="padding: 0px; display: block; line-height: 0; font-size: 0px; border: 0px; clear: both; vertical-align: top; margin: 0px 0px 0px 0px;"),
   #navbarPage(h6(style="vertical-align:top;font-size: 24px;color: dodgerblue;",appTitle), 
   # navbarPage(HTML("<p style='font-size: 24px;color: dodgerblue;'>", appTitle,"</p>"), 
  	navbarPage(title="",
						 inverse=FALSE,
						 header = list(tags$head(includeCSS("www/css/hacks.css")),
						 							 #tags$head(includeCSS("www/css/tooltip.css")),
						 							 # Add/run startup Javascript
						 							 tags$head(tags$script(onloadJs)),
						 							 # Use JQuery (built into Shiny) calls to show/hide modal based on message
						 							 tags$head(includeScript("www/js/showLoading.js")),
						 							 tags$head(includeScript("www/js/leaving.js")),
						 							 # load Javascript snippet to parse the query string.
						 							 #tags$script(includeScript("www/js/parse_input.js")),
						 							 tags$head(includeScript("www/js/google-analytics.js")),
						 							 tags$head(HTML("<script async type='text/javascript' src='https://dap.digitalgov.gov/Universal-Federated-Analytics-Min.js?agency=HHS&subagency=NCI' id='_fed_an_ua_tag'> </script>")),
						 							 tags$head(
						 							   tags$style(type="text/css", ".irs-grid-text { font-size: 8pt;color: black; }",
						 							              ".irs-min { font-size: 8pt; background: white; }", ".irs-max { font-size: 8pt; background: white;}",
						 							              ".irs-from { font-size: 8pt; color: black;background: white;}", ".irs-to { font-size: 8pt;  color: black;background: white;}"
						 							              , "body {font-size: 12pt;}", "img {display: block;}", ".clear {clear: both}"
						 							   )
						 							 ),
						 							 tags$head(
						 							   tags$style(HTML(
						 							     paste0(".navbar-nav { font-size: 24px; color: black; }"),
						 							     paste0(".navbar-default .navbar-brand { font-size: 24px; color: dodgerblue; }")
						 							   )
						 							   )
						 							 ),
						 							 tags$head(tags$meta(name="description",content="CellMiner Cross Database (CellMinerCDB) is the first web application to allow translational researchers to conduct analyses across all major cancer cell line pharmacogenomic data sources from NCI-DTP NCI-60, Sanger GDSC, and Broad CCLE/CTRP"))
						              ),
		#background-color: blue; font-color: white;
		#------[NavBar Tab: Univariate Analyses]---------------------------------------------------------
		tabPanel("Univariate Analyses",
			fluidPage(
    		loadingModal(),
	    	sidebarLayout(
	        sidebarPanel(
	        	width=3, 
	        	tags$div(
	        	  id="input_container",
	        	  tags$a(id="skiplink"),
	            #selectInput("xDataset", "x-Axis Cell Line Set", choices=dataSourceChoices, selected = "nci60"),
	        	  HTML(
	        	    paste("<label class='control-label' for='xDataset'>x-Axis Cell Line Set</label>","<select id='xDataset'>",options,"</select>")
	        	  ),
	        	  uiOutput("xPrefixUi"),
	            textInput("xId", "Identifier: (e.g. topotecan or SLFN11)", "SLFN11"),
	        	  uiOutput("xAxisRangeUi"),
	        	  br(),
	            #selectInput("yDataset", "y-Axis Dataset", choices=dataSourceChoices, selected = "nci60"),
	        	  HTML(
	        	    paste("<label class='control-label' for='yDataset'>y-Axis Cell Line Set</label>","<select id='yDataset'>",options,"</select>")
	        	  ),
	        	  uiOutput("yPrefixUi"),
	          	textInput("yId", "Identifier: (e.g. topotecan or SLFN11)", "topotecan"),
	          	uiOutput("yAxisRangeUi"),
	          	
	            # checkboxInput("showColor", "Show Color?", value=TRUE),

	          	radioButtons("tissueSelectionMode", "Select Tissues", c("To include", "To exclude")),
	          	uiOutput("selectTissuesUi"),
	        	  
	        	  checkboxInput("showColor", "Show Color?", value=TRUE),
	        	  
	            uiOutput("showColorTissuesUi")
	            
	            # Generate a hidden input with TRUE or FALSE if rCharts is installed
	          	#tags$label("hasRCharts"),
	        		#tags$input(id="hasRCharts", type="text", value=hasRCharts, style="display:none")
	        	)
	        ),
        mainPanel(
          div(style="font-size: 14px", align="center", "CellMinerCDB enables exploration and analysis of cancer cell line pharmacogenomic data across different sources. If publishing results based on this site, please cite: ", a("Rajapakse.VN, Luna.A, Yamade.M et al. iScience, Cell Press. 2018 Dec 12.", href="https://www.cell.com/iscience/fulltext/S2589-0042(18)30219-0", target = "_blank", style="font-size: 14px;")),
        	uiOutput('tabsetPanel')
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
						 				HTML("<b>Download drug synonyms table with matching IDs for all cell line sets</b>"),
						 				downloadButton('downloadSyn', 'Download Table'),
						 				br(),br()
						 				#uiOutput(""),
						 			)
						 		), #end sidebarPanel
						 		mainPanel(
						 		  htmlOutput('sourceLink'),
						 		  #uiOutput('sourceLink'),
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
# 		   tabPanel("About",
#              tags$a(id="skiplink"),
#     	includeMarkdown("www/files/about.md")
#     	#h1("For testing"),
#     	#textOutput("ipAddress")
#     ),
		tabPanel("Help",
		         tags$a(id="skiplink"),
		         includeMarkdown("www/files/guide.md")
		         #h1("For testing"),
		         #textOutput("ipAddress")
		)
	),
br(),br(),hr(),
 # tags$a(id="skiplink")
tags$div(style="font-size: 12px",
  tags$html("CellMinerCDB is a development of the "),
  tags$a("Genomics and Pharmacology Facility,", href="https://discover.nci.nih.gov/", target = "_blank",style="font-size: 12px;"),
  tags$a(" Developmental Therapeutics Branch (DTB), ",href='https://ccr.cancer.gov/Developmental-Therapeutics-Branch', target='_blank',style="font-size: 12px;"),
  tags$a("Center for Cancer Research (CCR), ", href="https://ccr.cancer.gov/", target = "_blank",style="font-size: 12px;"),
  tags$a("National Cancer Institute (NCI) ", href="https://www.cancer.gov/", target = "_blank",style="font-size: 12px;"),
  tags$html("prepared in collaboration with the "),
  tags$a("cBio Center", href="http://www.sanderlab.org/", target = "_blank",style="font-size: 12px;"),
  tags$html(" at the Dana-Farber Cancer Institute."),
  br(),br(),
  # tags$html("Please email 'Webadmin@discover.nci.nih.gov' with any problems, questions or feedback on the tool",style="font-size: 12px; float: left"),
  tags$html("Please "), 
  tags$a("email us", href="mailto:Webadmin@discover.nci.nih.gov&subject=CellMinerCDB",style="font-size: 12px;"),
  tags$html(" with any problems, questions or feedback on the tool"),
  br(),br(),
  tags$a("Notice and Disclaimer", href="files/disclaimer.html", target = "_blank")
  
  
  #includeMarkdown("www/files/guide.md")
  ## add email + Notice and Disclaimer + check font size to lower?
  ## library(mailR)
  )
  )
)
