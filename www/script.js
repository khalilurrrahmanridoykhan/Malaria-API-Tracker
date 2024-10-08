$(document).ready(function() {
    // Initially hide the header
    $(".main-header").hide();

    // Listen for login events
    Shiny.addCustomMessageHandler("toggleHeader", function(show) {
      if (show) {
        $(".main-header").show();
      } else {
        $(".main-header").hide();
      }
    });

    Shiny.addCustomMessageHandler("openMapTab", function() {
        // Trigger a click event on the Map tab
        $('a[data-toggle="tab"][href="#shiny-tab-time"]').tab('show');
      });
  });