Shiny.addCustomMessageHandler("showSkip",
	function(message) {
		//console.log(message);
		
		if(message.show) {
			// $("#but1").prop("disabled",false) 
			$("#but1").show();
		} else {
			// $("#but1").prop("disabled",true) 
			$("#but1").hide();
		}
	}
);