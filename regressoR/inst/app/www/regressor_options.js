$(document).ready(function() {
 //   document.querySelectorAll("[data-value = \'<span data-id=\"cros\"></span>\']")[0].style.display = "none";

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