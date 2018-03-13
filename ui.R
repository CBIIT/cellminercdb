library(shiny)
library(rcellminer)

#--------------------------------------------------------------------------------------------------
# LOAD CONFIGURATION AND REQUIRED DATA SOURCE PACKAGES.
#--------------------------------------------------------------------------------------------------
config <- jsonlite::fromJSON("config.json")
appConfig <- jsonlite::fromJSON("appConfig.json")
metaConfig <- jsonlite::fromJSON("configMeta.json")
source("modal.R")
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
  tags$a(href="#skiplink","Skip over navigation",style="font-size: 10px"),
   #navbarPage(h6(style="vertical-align:top;font-size: 24px;color: dodgerblue;",appTitle), 
   # navbarPage(HTML("<p style='font-size: 24px;color: dodgerblue;'>", appTitle,"</p>"), 
  	navbarPage(appTitle,
						 inverse=FALSE,
						 header = list(tags$head(includeCSS("www/css/hacks.css")),
						 							 #tags$head(includeCSS("www/css/tooltip.css")),
						 							 # Add/run startup Javascript
						 							 tags$head(tags$script(onloadJs)),
						 							 # Use JQuery (built into Shiny) calls to show/hide modal based on message
						 							 tags$head(includeScript("www/js/showLoading.js")),
						 							 # load Javascript snippet to parse the query string.
						 							 #tags$script(includeScript("www/js/parse_input.js")),
						 							 tags$head(includeScript("www/js/google-analytics.js")),
						 							 tags$head(HTML("<script async type='text/javascript' src='https://dap.digitalgov.gov/Universal-Federated-Analytics-Min.js?agency=HHS&subagency=NCI' id='_fed_an_ua_tag'> </script>")),
						 							 tags$head(
						 							   tags$style(type="text/css", ".irs-grid-text { font-size: 8pt;color: black; }",
						 							              ".irs-min { font-size: 8pt; background: white; }", ".irs-max { font-size: 8pt; background: white;}",
						 							              ".irs-from { font-size: 8pt; color: black;background: white;}", ".irs-to { font-size: 8pt;  color: black;background: white;}"
						 							              , "body {font-size: 12pt}"
						 							   )
						 							 ),
						 							 tags$head(
						 							   tags$style(HTML(
						 							     paste0(".navbar-nav { font-size: 24px; color: black; }"),
						 							     paste0(".navbar-default .navbar-brand { font-size: 24px; color: dodgerblue; }")
						 							   )
						 							   )
						 							 )
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
	            #selectInput("xDataset", "x-Axis Cell line set", choices=dataSourceChoices, selected = "nci60"),
	        	  HTML(
	        	    paste("<label class='control-label' for='xDataset'>x-Axis Cell line set</label>","<select id='xDataset'>",options,"</select>")
	        	  ),
	        	  uiOutput("xPrefixUi"),
	            textInput("xId", "ID: (e.g. topotecan or SLFN11)", "SLFN11"),
	          	uiOutput("xAxisRangeUi"),
	          	
	            #selectInput("yDataset", "y-Axis Dataset", choices=dataSourceChoices, selected = "nci60"),
	        	  HTML(
	        	    paste("<label class='control-label' for='yDataset'>y-Axis Cell line set</label>","<select id='yDataset'>",options,"</select>")
	        	  ),
	        	  uiOutput("yPrefixUi"),
	          	textInput("yId", "ID: (e.g. topotecan or SLFN11)", "topotecan"),
	          	uiOutput("yAxisRangeUi"),
	          	
	            checkboxInput("showColor", "Show Color?", value=TRUE),

	          	radioButtons("tissueSelectionMode", "Select Tissues", c("Include", "Exclude")),
	          	uiOutput("selectTissuesUi"),
	            uiOutput("showColorTissuesUi")
	            
	            # Generate a hidden input with TRUE or FALSE if rCharts is installed
	          	#tags$label("hasRCharts"),
	        		#tags$input(id="hasRCharts", type="text", value=hasRCharts, style="display:none")
	        	)
	        ),
        mainPanel(
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
						 				  paste("<label class='control-label' for='mdataSource'>Cell line set</label>","<select id='mdataSource'>",metaoptions,"</select>")
						 				),
						 				br(),br(),br(),br(),br(),br(),
						 				uiOutput("dataTypeUi"),
						 				br(),
						 				downloadButton('downloadExp', 'Download data type')
						 				#uiOutput(""),
						 			)
						 		), #end sidebarPanel
						 		mainPanel(
						 			uiOutput('metadataPanel'),
						 			#h4(htmlOutput('sourceLink'))
						 			htmlOutput('sourceLink')
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
		                   paste("<label class='control-label' for='dataSrc'>Data Source</label>","<select id='dataSrc'>",options,"</select>")
		                 )
		                 # uiOutput("dataTypeUi"),
		                 # br(),
		                 # downloadButton('downloadExp', 'Download data for selected type')
		                 #uiOutput(""),
		               )
		             ), #end sidebarPanel
		             mainPanel(
		               includeMarkdown("www/files/help.md"),
		               DT::dataTableOutput("ids2")
	#	               uiOutput('searchPanel'),
		               #h4(htmlOutput('sourceLink'))
	 #              htmlOutput('sourceLink')
		             )
		           )
		         ) #end fluidPage
		), #end tabPane
		#-----[NavBar Tab: About]------------------------------------------------------------------------
		   tabPanel("About",
             tags$a(id="skiplink"),
    	includeMarkdown("www/files/about.md")
    	#h1("For testing"),
    	#textOutput("ipAddress")
    ),
		tabPanel("Help",
		         tags$a(id="skiplink"),
		         includeMarkdown("www/files/guide.md")
		         #h1("For testing"),
		         #textOutput("ipAddress")
		)
	)
 # tags$a(id="skiplink")
  )
)
