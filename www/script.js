// script.js
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

    // Listen for opening the Map tab after login
    Shiny.addCustomMessageHandler("openMapTab", function() {
        $('a[data-toggle="tab"][href="#shiny-tab-time"]').tab('show');
    });
});
