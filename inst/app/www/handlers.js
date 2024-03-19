$( document ).ready(function() {
  Shiny.addCustomMessageHandler('controlSpinner', function(message) {
    if (message.action === "show") {
      $("#loadingSpinnerCountry").show();
    } else if (message.action === "hide") {
      $("#loadingSpinnerCountry").hide();
    }
  });
});
