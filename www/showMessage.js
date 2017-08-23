// This recieves messages of type "no_gene_cloumn" from the server.
Shiny.addCustomMessageHandler("show_message",
	function(message){
		$(document).ready(
			function(){
				$("header").find("nav").append('<span class="myClass"> 好犀利 </span>', message);
})
	}
)


