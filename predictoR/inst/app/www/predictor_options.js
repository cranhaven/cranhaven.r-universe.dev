$(document).ready(function() {
  $("header").find("nav").append('<span class="header-title"> predictoR </span>');
  $(".sidebar").on("click", ".disabled", function (e) {
    e.preventDefault();
    return false;
  });
  $("#carga_datos_ui_1-run_pred").click(function() {
  var results = document.getElementsByClassName('treeview-menu')
  for (let i = 0; i < results.length; i++) {
  results[i].style.display = "none"
  }
  })
});