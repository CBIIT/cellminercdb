Shiny.addCustomMessageHandler("showSkip",
	function(message) {
		//console.log(message);
		
		if(message.show) {
			$("#but1").prop("disabled",false) 
		} else {
			$("#but1").prop("disabled",true) 
		}
	}
);