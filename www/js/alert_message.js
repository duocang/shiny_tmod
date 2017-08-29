// This recieves messages of type "no_gene_cloumn" from the server.
Shiny.addCustomMessageHandler("alert_message",
	function(message){
		alert(JSON.stringify(message))
	}
)

Shiny.addCustomMessageHandler("header_message",
	function(message){
		$("header").find("nav").text(message).css({"color":"#2780e3"})
	}
)
