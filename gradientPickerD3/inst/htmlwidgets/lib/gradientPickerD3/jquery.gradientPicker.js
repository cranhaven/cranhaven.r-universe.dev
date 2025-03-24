/**
@author Christian D. Peikert (peikert)
@author Matt Crinklaw-Vogt, original JS author
*/
/*
The original JS script was created by Matt Crinklaw-Vogt (https://github.com/tantaman/jquery-gradient-picker) and was modified by Christian D. Peikert.

Major changes:
- function call requires now two lists, one for color and one for the ticks
- function returns color ticks, the percentage of the total range as well as the color itself
- added Shiny return value. Changes will be pushed after: change ticks by drag and (after) drop, removing a color tick or changing of the color.
- style is not provided any more
- ticks and labels for initialization color positions were visualised
- color menu of boxes is now opened by doubleclick
*/
(function($) {
    if (!$.event.special.destroyed) {
        $.event.special.destroyed = {
            remove: function(o) {
                if (o.handler) {
                    o.handler();
                }
            }
        }
    }

    function ctrlPtComparator(l, r) {
        return l.position - r.position;
    }

    function bind(fn, ctx) {
        if (typeof fn.bind === "function") {
            return fn.bind(ctx);
        } else {
            return function() {
                fn.apply(ctx, arguments);
            }
        }
    }

    var browserPrefix = "";
    var agent = window.navigator.userAgent;
    if (agent.indexOf('WebKit') >= 0)
        browserPrefix = "-webkit-"
    else if (agent.indexOf('Mozilla') >= 0)
        browserPrefix = "-moz-"
    else if (agent.indexOf('Microsoft') >= 0)
        browserPrefix = "-ms-"
    else
        browserPrefix = ""

    function GradientSelection($el, opts) {
        this.$el = $el;
        this.$el.css("position", "relative");
        this.opts = opts;

        var $preview = $("<canvas id='gradientPicker-ctrlPts_preview'; class='gradientPicker-ctrlPts'></canvas>");
        this.$el.append($preview);
        var canvas = $preview[0];
        canvas.width = canvas.clientWidth;
        canvas.height = canvas.clientHeight;
        this.g2d = canvas.getContext("2d");

        var $ctrlPtContainer = $("<div id='gradientPicker-ctrlPts_container'; class='gradientPicker-ctrlPts'></div>");
        this.$el.append($ctrlPtContainer)
        this.$ctrlPtContainer = $ctrlPtContainer;

        var $ctrlPtContainer_scalebar_line = $("<div id='gradientPicker-scalebar_line'; class='gradientPicker-ctrlPts_scalebar_line'></div>");
        this.$el.append($ctrlPtContainer_scalebar_line);
        var toAdd = document.createDocumentFragment();
        for (i = 0; i < this.opts.controlTicks.length; i++) {
            var newDiv = document.createElement('div');
            newDiv.id = 'sline' + i;
            newDiv.style.left = ((this.g2d.canvas.width * this.opts.controlProcent[i]) - 1) + 'px';
            newDiv.className = 'gradientPicker-ctrlPts_scalemarks_line';
            toAdd.appendChild(newDiv);
        }
        document.getElementById('gradientPicker-scalebar_line').appendChild(toAdd);
        var $ctrlPtContainer_scalebar_label = $("<div id='gradientPicker-scalebar_label'; class='gradientPicker-ctrlPts_scalebar_label'></div>");
        this.$el.append($ctrlPtContainer_scalebar_label);
        var toAdd = document.createDocumentFragment();
        for (i = 0; i < this.opts.controlTicks.length; i++) {
            var newDiv = document.createElement('div');
            newDiv.id = 'sline' + i;
            var newContent = document.createTextNode(this.opts.controlTicks[i]);
            newDiv.appendChild(newContent);
            newDiv.style.left = ((this.g2d.canvas.width * this.opts.controlProcent[i]) - 25) + 'px';
            newDiv.className = 'gradientPicker-ctrlPts_scalemarks_label';
            toAdd.appendChild(newDiv);
        }
        document.getElementById('gradientPicker-scalebar_label').appendChild(toAdd);

        this.updatePreview = bind(this.updatePreview, this);

        this.ctrlPts = [];
        this.ctrlPtConfig = new ControlPtConfig(this.$el, opts);
        for (var i = 0; i < this.opts.controlProcent.length; ++i) {
            var ctrlPt = this.createCtrlPt({
                position: this.opts.controlProcent[i],
                color: opts.controlColors[i]
            });
            this.ctrlPts.push(ctrlPt);
        }

        this.docClicked = bind(this.docClicked, this);
        this.destroyed = bind(this.destroyed, this);
        $(document).bind("click", this.docClicked);
        this.$el.bind("destroyed", this.destroyed);
        this.previewClicked = bind(this.previewClicked, this);
        $preview.click(this.previewClicked);

        result = this.updatePreview();
        this.opts.change(result);
    }

    GradientSelection.prototype = {
        docClicked: function() {
            this.ctrlPtConfig.hide();
        },

        createCtrlPt: function(ctrlPtSetup) {
            return new ControlPoint(this.$ctrlPtContainer, ctrlPtSetup, this.opts.orientation, this, this.ctrlPtConfig, this.opts)
        },

        destroyed: function() {
            $(document).unbind("click", this.docClicked);
        },

        updateOptions: function(opts) {
            $.extend(this.opts, opts);
            this.opts = opts;
            this.updatePreview();
        },

        updatePreview: function() {
            var result = [];
            this.ctrlPts = this.ctrlPts.sort(ctrlPtComparator); // sort colorboxes by position
            if (this.opts.orientation == "horizontal") {
                var grad = this.g2d.createLinearGradient(0, 0, this.g2d.canvas.width, 0);
                var delta = this.opts.max_value - this.opts.min_value
                var controlProcent = []
                var controlTicks = []
                var controlColors = []
                for (var i = 0; i < this.ctrlPts.length; ++i) {
                    pt = this.ctrlPts[i];
                    grad.addColorStop(pt.position, pt.color);
                    controlProcent.push(pt.position)
                    controlTicks.push((this.opts.min_value + (delta * pt.position)))
                    controlColors.push(pt.color)
                    result.push({
                        position: controlProcent[i],
                        color: controlColors[i],
                        ticks: controlTicks[i]
                    });
                }
                this.opts.controlProcent = controlProcent
                this.opts.controlTicks = controlTicks
                this.opts.controlColors = controlColors
                if (result[0].position > 0) {
                    var r0 = {
                        position: 0,
                        color: result[0].color,
                        ticks: this.opts.min_value
                    }
                    result.unshift(r0);
                }
                if (result[result.length - 1].position < 1) {
                    var rn = {
                        position: 1,
                        color: result[result.length - 1].color,
                        ticks: this.opts.max_value
                    }
                    result.push(rn);
                }
            } else {

            }

            this.g2d.fillStyle = grad;
            this.g2d.fillRect(0, 0, this.g2d.canvas.width, this.g2d.canvas.height);
            return (result)
        },

        removeControlPoint: function(ctrlPt) {
            var cpidx = this.ctrlPts.indexOf(ctrlPt)
            if (cpidx != -1) {
                this.ctrlPts.splice(cpidx, 1);
                ctrlPt.$el.remove();
            }
        },

        previewClicked: function(e) {
            var offset = $(e.target).offset();
            var x = e.pageX - offset.left;
            var y = e.pageY - offset.top;

            var imgData = this.g2d.getImageData(x, y, 1, 1);
            var colorStr = "rgb(" + imgData.data[0] + "," + imgData.data[1] + "," + imgData.data[2] + ")";

            var cp = this.createCtrlPt({
                position: (x / this.g2d.canvas.width), // -6
                color: colorStr
            });
            this.ctrlPts.push(cp);
            result = this.updatePreview();
            this.opts.change(result)
            this.opts.controlProcent.push(cp);
            this.opts.controlProcent.sort(ctrlPtComparator);
        }
    }

    function allMatches(string, regex, index) {
        var matches = [];
        var match = null;
        while (match = regex.exec(string)) {
            matches.push(match[index]);
        }
        return matches;
    }

    function convertToHex(c) {

        if (typeof stringValue) {
            c = parseInt(c)
        }
        var hex = c.toString(16);
        return hex.length == 1 ? "0" + hex : hex;
    }

    function convertRgbToHex(rgb) {
        return "#" + convertToHex(rgb[0]) + convertToHex(rgb[1]) + convertToHex(rgb[2]);
    }

    function ControlPoint($parentEl, initialState, orientation, listener, ctrlPtConfig, opts) {

        this.opts = opts;
        this.$el = $("<div class='gradientPicker-ctrlPt'></div>");
        $parentEl.append(this.$el);
        this.$parentEl = $parentEl;
        this.configView = ctrlPtConfig;

        this.position = initialState.position;
        this.color = initialState.color;

        var regEx = /(\d+)/g;
        matches = allMatches(this.color, regEx, 1);
        if (matches.length == 3) {
            this.color = convertRgbToHex(matches);
        }

        this.listener = listener;
        this.outerWidth = this.$el.outerWidth();
        this.$el.css("background-color", this.color);
        if (orientation == "horizontal") {
            var pxLeft = ($parentEl.width() * (this.position)) - (this.$el.outerWidth() / 2);
            this.$el.css("left", pxLeft);
        } else {
            var pxTop = ($parentEl.height() * (this.position)) - (this.$el.outerHeight() / 2);
            this.$el.css("top", pxTop);
        }

        this.drag = bind(this.drag, this);
        this.stop = bind(this.stop, this, this.opts);
        this.clicked = bind(this.clicked, this);
        this.dbclicked = bind(this.dbclicked, this);
        this.colorChanged = bind(this.colorChanged, this);
        this.$el.draggable({
            axis: (orientation == "horizontal") ? "x" : "y",
            drag: this.drag,
            stop: this.stop,
            containment: $parentEl
        });
        this.$el.css("position", 'absolute');
        this.$el.click(this.clicked);
        this.$el.dblclick(this.dbclicked);
    }

    ControlPoint.prototype = {
        drag: function(e, ui) {
            var left = ui.position.left;
            this.position = (left / (this.$parentEl.width() - this.outerWidth));
			if(this.position>1){this.position = 1}
            this.listener.updatePreview();

        },
        stop: function(e, ui) {
            var result = this.listener.updatePreview();
            this.opts.change(result);
            e.stopPropagation();
        },

        clicked: function(e) {
            e.stopPropagation();
            return false;
        },
        dbclicked: function(e) {
            this.configView.show(this.$el.position(), this.color, this);
            e.preventDefault();
            e.stopPropagation();
            return false;
        },

        colorChanged: function(c) {
            this.color = c;
            this.$el.css("background-color", this.color);
            var result = this.listener.updatePreview();
            this.opts.change(result);
        },

        removeClicked: function() {
            this.listener.removeControlPoint(this);
            var result = this.listener.updatePreview();
            this.opts.change(result);
        }
    };

    function ControlPtConfig($parent, opts) {
        this.$el = $('<div class="gradientPicker-ptConfig" style="visibility: hidden"></div>');
        $parent.append(this.$el);
        var $cpicker = $('<div class="color-chooser"></div>');
        this.$el.append($cpicker);
        var $rmEl = $("<div class='gradientPicker-close'></div>");
        this.$el.append($rmEl);

        this.colorChanged = bind(this.colorChanged, this);
        this.removeClicked = bind(this.removeClicked, this);
        $cpicker.ColorPicker({
            onChange: this.colorChanged
        });
        this.$cpicker = $cpicker;
        this.opts = opts;
        this.visible = false;

        $rmEl.click(this.removeClicked);
    }

    ControlPtConfig.prototype = {
        show: function(position, color, listener) {
            this.visible = true;
            this.listener = listener;
            this.$el.css("visibility", "visible");
            this.$cpicker.ColorPickerSetColor(color);
            this.$cpicker.css("background-color", color);
            if (this.opts.orientation === "horizontal") {
                this.$el.css("left", position.left);
            } else {
                this.$el.css("top", position.top);
            }
        },

        hide: function() {
            if (this.visible) {
                this.$el.css("visibility", "hidden");
                this.visible = false;
            }
        },

        colorChanged: function(hsb, hex, rgb) {
            hex = "#" + hex;
            this.listener.colorChanged(hex);
            this.$cpicker.css("background-color", hex)
        },

        removeClicked: function() {
            this.listener.removeClicked();
            this.hide();
        }
    };

    $.fn.gradientPicker = function(opts) {
        opts = $.extend({
            controlProcent: ["#FFF 0%", "#000 100%"],
            orientation: "horizontal",
            type: "linear",
            fillDirection: "left",
            generateStyles: true,
            change: function() {}
        }, opts);
        opts = $.extend({
            min_value: opts.controlTicks[0],
            max_value: opts.controlTicks[opts.controlTicks.length - 1]
        }, opts)
        this.each(function() {
            var $this = $(this);
            var gradSel = $this.data("gradientPicker-sel");

            if (gradSel == null) {
                var gradSel = new GradientSelection($this, opts);
                $this.data("gradientPicker-sel", gradSel);
            } else {
                document.getElementById('gradientPicker-ctrlPts_container').remove();
                document.getElementById('gradientPicker-scalebar_line').remove();
                document.getElementById('gradientPicker-scalebar_label').remove();
                document.getElementById('gradientPicker-ctrlPts_preview').remove();
                var gradSel = $this.data("gradientPicker-sel");
                var gradSel = new GradientSelection($this, opts);
                $this.data("gradientPicker-sel", gradSel);
            }
        });
    };
})(jQuery);