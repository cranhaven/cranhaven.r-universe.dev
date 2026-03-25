HTMLWidgets.widget({

    name: 'render_bpmn',

    type: 'output',

    factory: function(el, width, height) {

        // TODO: define shared variables for this instance

        return {

            renderValue: function(x) {

                var BpmnViewer = window.BpmnJS;
                var bpmnViewer = new BpmnViewer({ container: el, width: '100%', height: '100%' });

                var xml = x.bpmn_model;

                bpmnViewer.importXML(xml, function(err) {

                    if (err) {
                        console.log('error rendering', err);
                    }

                    var canvas = bpmnViewer.get('canvas');

                    canvas.zoom('fit-viewport', 'auto');

                });

            },

            resize: function(width, height) {

                // TODO: code to re-render the widget with a new size

            }

        };
    }
});
