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


Shiny.addCustomMessageHandler("widthHandler", changeWidth);
      
function changeWidth(width){
  var logo = document.getElementsByClassName("logo");
  var nav = document.getElementsByClassName("navbar navbar-static-top");
  var toggle = document.getElementsByClassName("sidebar-toggle");
  
  var width = width + "px";
  var margin = "-" + width;
  
  logo[0].style.width = width;
  nav[0].style.marginLeft = width;
  toggle[0].style.marginLeft = margin;
  
  //for (var i = 0; i < logo.length; i++) {
  //  logo[i].style.width = message.logo_width;
  //}
  
  //for (var i = 0; i < nav.length; i++) {
  //  nav[i].style.marginLeft = message.nav_margin;
  //}
  
  //for (var i = 0; i < toggle.length; i++) {
  //  toggle[i].style.marginLeft = message.toggle_margin;
  //}

}