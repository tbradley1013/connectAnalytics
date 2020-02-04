$(document).keyup(function(event) {
  if ($("#01_server_connect_ui_1-api_key").is(":focus") && (event.key == "Enter")) {
    $("#01_server_connect_ui_1-connect").click();
  }
});


$(document).keyup(function(event) {
  if ($("#02_username_ui_1-username_mod").is(":focus") && (event.key == "Enter")) {
    $("#02_username_ui_1-submit").click();
  }
});


$(document).keyup(function(event) {
  if ($("#02_username_ui_1-username").is(":focus") && (event.key == "Enter")) {
    $("#02_username_ui_1-change_username").click();
  }
});