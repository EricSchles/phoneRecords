$(document).ready(function() {
	Shiny.addCustomMessageHandler('showalert', function(message) {
		alert(message);
	});
});
