// This recieves messages of type "no_gene_cloumn" from the server.
Shiny.addCustomMessageHandler("alert_message",
	function(alertMessage){
		alert(JSON.stringify(message))
	}
)

// code included inside $(document).ready() will only run once the page is ready for JavaScript code to execute
$(document).ready(function() {
  
  // initialize a counter
  var n = 0;
  
  // create a click handler which listens for a click on the element with id equal to RStudio
  $("#RStudio").on("click", function(){
  
    // increment the counter each time we click on the Rstudio logo
    n++;
    
    // send message to Shiny
    Shiny.onInputChange("count", n);

  });

});




Shiny.addCustomMessageHandler("header_message",
	function(headerMessage){
		$("header").find("nav").append('<span  class="myClass"> Text Here</span>', headerMessage)
	}
);


//$(document).ready(function(){
//		$("#run").on("click", function(){
//			$("header").find("nav").append('<span  class="myClass"> Text Here</span>');
//		});
//
//});