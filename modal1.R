# Show loading modal when the webpage is ready (different from when the app is ready)
# and prevent user from removing it.
onloadJs <- '
$(document).ready(function() {
  $("#loadingModal").modal({backdrop: "static", keyboard: false, show: true});

  // $("#but1").prop("disabled",true) 
  $("#but1").hide()

  $("#video1").on("ended", function() {
   //TO DO: Your code goes here...
     setTimeout( 
         function() {
           // alert("Video Finished");
         $("#loadingModal").modal("hide");
         },
       5000
     ) ;
   });

});
'

appConfig <- jsonlite::fromJSON("appConfig.json")
pmodal <- appConfig$modal

if (is.null(pmodal)){
  vclass = "modal-dialog modal-sm"
  disp_content <- as.character(tags$div(
    class="progress progress-striped active",
    style="margin-bottom: 0;",
    tags$div(
      class="progress-bar progress-bar-info",
      style="width: 100%"
    )
  ))
  disp_foot <- ''
} else {
  disp_foot <- '<button id="but1" type="button" class="btn btn-primary" style="font-size: 25px;" data-dismiss="modal"> Skip presentation</button>'
  vclass = "modal-dialog modal-lg"
  if (length(grep(".png",pmodal)!=0))
  {
    disp_content <- as.character(tags$img(
      class="img-fluid",
      # src="http://www.google.nl/images/branding/googlelogo/2x/googlelogo_color_272x92dp.png",
      src =base64enc::dataURI(file = pmodal, mime = "image/png"),
      alt="Pharmaco-genomics picture",
      style="width: 100%;"
    ))
  } else
  {
    disp_content <- as.character(tags$video(
      src =base64enc::dataURI(file = pmodal, mime = "video/mp4"),
      id="video1",
      # src=pmodal,
      type="video/mp4",
      autoplay = NA,
      muted = NA,
      controls = NA,
      style="width: 100%; height: auto !important;"
    ))
    
  }
  
  }


# Simple load modal using Bootstrap
# NOTE: Shiny uses Bootstrap 3.3.1
loadingModal <- function() {
	modal <- tags$div(
		class="modal",
#		style="height: 80%; margin-bottom: 0 !important",
		id="loadingModal",
		tags$div(
			class=vclass,
			style="width: 80%; max-height: calc(100% -200px); overflow-y: auto;",
			tags$div(
				class="modal-content",
			#	style="height: 80%;",
				tags$div(
					class="modal-header",
					tags$h4(
						class="modal-title",
						"Loading Data ... (~30 seconds)"
					)
				),
				tags$div(
					class="modal-body",
				# 	tags$img(
				#   class="img-fluid",
				# 	  # src="http://www.google.nl/images/branding/googlelogo/2x/googlelogo_color_272x92dp.png",
				#     src =base64enc::dataURI(file = pmodal, mime = "image/png"),
				# 	  alt="Pharmaco-genomics picture"
				# 	  # style="width: 100%, display: flex"
				# 	)
				 HTML(disp_content)
				),
				tags$div(
				  class="modal-footer",
				#  HTML('<button type="button" class="btn btn-primary" data-dismiss="modal">Skip</button>')
           HTML(disp_foot)
				)
			)
		)
	)
	
	modal
}