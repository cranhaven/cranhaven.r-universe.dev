/**
@author Christian D. Peikert (peikert)
**/

HTMLWidgets.widget({

    name: 'gradientPickerD3',

    type: 'output',

    initialize: function(el, width, height) {
        return {
            lastTheme: null,
            lastValue: null
        };
    },

    renderValue: function(el, x, instance) {
        this.doRenderValue(el, x, instance);
    },

    resize: function(width, height) {
        if (instance.lastValue) {
            this.doRenderValue(el, instance.lastValue, instance);
        }
    },

    doRenderValue: function(el, x, instance) {
        instance.lastValue = x;
        $(el).gradientPicker({
            change: function(points) {
                if (HTMLWidgets.shinyMode) {
                    Shiny.onInputChange(
                        el.id + "_table",
                        points
                    );
                }
            },
            controlPoints: x.colorstring,
            controlColors: x.colors,
            controlTicks: x.ticks,
            controlProcent: x.procent
        });
    }
});