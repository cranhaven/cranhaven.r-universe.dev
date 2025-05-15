//---------Flags-----------
var promidat_flat_models = {knn: true, dt: true, rf: true, 
                            boosting: true, svm: true, rl: true, 
                            nn: true, rlr: true, rd: true};
                            
generating_model = false
//-------------------------

function promidat_model_firt(e, model, id){
  if(promidat_flat_models[model]){
    promidat_flat_models[model] = false;
    $("#"+id).click();
  }
}

 window.addEventListener("load", function(event) {
  /* Al cargarse la página  se pone el titulo */
  $("header").find("nav").append('<span class="header-title"> <i>regresso</i>R </span>');

  $("#carga_datos_ui_1-segmentButton").on('click',function(e){
    promidat_flat_models = {knn: true, dt: true, rf: true, boosting: true, svm: true, rl: true, nn: true, rlr: true, rd:true};
  });
});


Shiny.addCustomMessageHandler("updateLabel",
  function(message) {
    for (var i = 0; i < message.ids.length; i++) {
      element = $("[data-id=" + message.ids[i] + "]")
      for (var j = 0; j < element.length; j++) {
        element[j].innerHTML = message.values[i];
      }
      if(message.ids[i] == "categorico" || message.ids[i] == "numerico" || message.ids[i] == "disyuntivo"){
        $("option[value='"+message.ids[i]+"']").text(message.values[i])
      }
    }
  }
);