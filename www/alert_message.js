// This recieves messages of type "no_gene_cloumn" from the server.
Shiny.addCustomMessageHandler("alert_message",
	function(message){
		alert(JSON.stringify(message))
	}
)