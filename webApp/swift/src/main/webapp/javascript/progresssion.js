$(function() {
var test = $( "#percentage" ).text();

    $( '#progressbar' ).progressbar({
      value: parseInt(test)
    });
});
