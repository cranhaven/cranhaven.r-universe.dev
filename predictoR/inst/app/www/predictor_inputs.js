// ShinyWidgets
var exports4but = window.Shiny = window.Shiny || {};
var $escape4but = exports4but.$escape = function(val) {
  return val.replace(/([!"#$%&'()*+,.\/:;<=>?@\[\\\]^`{|}~])/g, '\\$1');
};

// radioGroupButtons input binding
var radioGroupButtonsBinding = new Shiny.InputBinding();
$.extend(radioGroupButtonsBinding, {
  find: function(scope) {
    return $(scope).find('.radioGroupButtons');
  },
  getId: function(el) {
    return el.id;
  },
  getValue: function(el) {
    return $('input:radio[name="' + $escape4but(el.id) + '"]:checked').val();
  },
  setValue: function(el, value) {
    $('input:radio[name="' + $escape4but(el.id) + '"][value="' + $escape4but(value) + '"]').prop('checked', true);
    $('input:radio[name="' + $escape4but(el.id) + '"]').parent().removeClass('active');
    $('input:radio[name="' + $escape4but(el.id) + '"][value="' + $escape4but(value) + '"]').parent().addClass('active');
  },
  subscribe: function(el, callback) {
    $(el).on('change.radioGroupButtonsBinding', function (event) {
        callback();
    });
  },
  unsubscribe: function(el) {
    $(el).off('.radioGroupButtonsBinding');
  },
  getState: function getState(el) {
      var $objs = $('input:radio[name="' + $escape4but(el.id) + '"]');

      // Store options in an array of objects, each with with value and label
      var options = new Array($objs.length);
      for (var i = 0; i < options.length; i++) {
        options[i] = { value: $objs[i].value,
        label: this._getLabel($objs[i]) };
      }

      return {
        label: $(el).parent().find('label[for="' + $escape4but(el.id) + '"]').text(),
        value: this.getValue(el),
        options: options
    };
  },
  receiveMessage: function receiveMessage(el, data) {
      var $el = $(el);

      // This will replace all the options
      if (data.hasOwnProperty('options')) {
        $el.find('div.btn-group-container-sw').empty();
        $el.find('div.btn-group-container-sw').append(data.options);
      }

      if (data.hasOwnProperty('selected'))
        this.setValue(el, data.selected);

      if (data.hasOwnProperty('label'))
        $(el).parent().find('label[for="' + $escape4but(el.id) + '"]').text(data.label);

      $(el).trigger('change');
  }
});

Shiny.inputBindings.register(radioGroupButtonsBinding, 'shiny.radioGroupButtonsInput');


var promidat_flat_models = {knn: true, dt: true, rf: true, boosting: true, svm: true, bayes: true , xgb: true, nn: true, rl : true, rlr: true};

nya_btn = true;
function nya_btn_fun(e) {
  nya_btn = nya_btn?($("#distribucion_numerica").click(),false):true;
}

function promidat_model_firt(e, model, id){
  if(promidat_flat_models[model]){
    promidat_flat_models[model] = false;
    $("#"+id).click();
  }
}

function hide_element(id){
    $("a[data-value='"+id+"']").hide();
}
function show_element(id){
    $("a[data-value='"+id+"']").show();
}

window.addEventListener("load", function(event) {
    //Botones de Wizard
  $("#ind_nuevos_ui_1-nuevosnext").on('click',function(e) {$("#ind_nuevos_ui_1-predecirPromidat").click()});
  $("#ind_nuevos_ui_1-transButton").on('click',function(e) {$("#ind_nuevos_ui_1-PredNuevosBttnModelo").click()});
  $("#carga_datos_ui_1-segmentButton").on('click',function(e){
    promidat_flat_models = {knn: true, dt: true, rf: true, boosting: true, svm: true, bayes: true, xgb: true, nn: true, rl : true, rlr: true};
  });

});

/*
function eliminar_tabs_extras(){
  $("ul#BoxNormal li")[2].remove();
  $("ul#BoxDisp li")[1].remove();
  $("ul#tabDyA li")[2].remove();
  $("ul#tabCor li").last().remove();
  $("ul#BoxKnn li").last().remove();
  $("ul#BoxDt li").last().remove();
  $("ul#BoxRf li").last().remove();
  $("ul#BoxB li").last().remove();
  $("ul#BoxSvm li").last().remove();
  $("ul#BoxCom li").last().remove();
  $("ul#BoxModelo li").last().remove();
  $("ul#BoxModelo li").last().remove();
  $("ul#BoxPodPred li").last().remove();
  $("ul#BoxPodPred li").last().remove();
  $("ul#BoxBayes li").last().remove();
  $("ul#BoxXgb li").last().remove();
  $("ul#BoxNn li").last().remove();
  $("ul#BoxRl li").last().remove();
  $("ul#BoxRlr li").last().remove();
}
*/
// Opciones DataTable

var showMenu = function(i) {
  $(".tablaHead")[i-1].children[0].classList.toggle("show");
};


