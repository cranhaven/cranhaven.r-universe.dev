/** DOT PLOTS AND SCATTER PLOTS
 * Class for initializing interactivity for dot plots and scatter plots
 * Includes scatterplots drawn with iNZightMaps */
class DotScatter extends Inzplot {

  constructor(props) {
    super(props);
    this.panel = document.querySelector(this.Grob);
    // bind event handlers:
    this.reset = this.reset.bind(this);
    this.brushmove = this.brushmove.bind(this);
    this.filterTable = this.filterTable.bind(this);
  }

  // create options for variable selection:
  createForm() {
    d3.select(".menu").insert("li", ".help")
      .attr("class", "var-control")
    //create form
    var form = d3.select(".var-control").append("div")
                    .attr("class", "form-group form-inline form-div")
                    .attr("id", "form");

    form.append("label")
        .html("Variables to display:");

    form.append("select")
        .attr("class", "form-control select-var")
        .attr("id", "selectVar")
        .attr("multiple", "multiple");

    // get column names:
    var ncol = $("#table thead tr th").length;
    var th = document.getElementsByTagName('th');
    for (var i = 0; i <= ncol; i++) {
        var opt = document.createElement('option');
        if (i == 0) {
          opt.value = 0;
          opt.classList.add('select');
          opt.innerHTML = "Display all";
          opt.selected = "selected";
        } else {
        opt.value = i;
        opt.innerHTML = th[i-1].innerHTML;
      }
      selectVar.appendChild(opt);
    };
  }

  //filter table
  filterTable() {
    let table = this.tbl,
        levelNo = this.levelNo,
        ind = [];
    //search for all those that are selected, then extract id num:
    var selected = document.getElementsByClassName('selected');
    for (var j = 0; j < selected.length; j++) {
        var id = selected[j].id;
        if (levelNo > 1) {
          // search for the middle number due to levels:
          var mid = id.substring(0, id.lastIndexOf('.1.'));
          var levNum = Number(mid.substring(mid.lastIndexOf('.') + 1));
          var num = Number(id.substring(id.lastIndexOf('.') + 1));
          // actual index in table:
          var idNum = chart.countsTab[levNum - 1] + num;
        } else {
          var idNum = id.substring(id.lastIndexOf('.') + 1);
        }
        ind.push("^" + idNum + "$");
    }

    table.search('').columns().search('').draw();
    table.columns(0).search(ind.join("|"), true, false).draw();
  }

  // set tooltip and point interactions
  setHovers() {
    var tooltip = this.tooltip,
        Grob = document.querySelectorAll(this.Grob),
        data = this.data,
        levelNo = this.levelNo,
        filterTable = this.filterTable,
        table = this.tbl;

    d3.selectAll(Grob).selectAll("use")
      .attr("class", "point");

      d3.selectAll('.point')
        .data(data)
        .attr('class', 'point')
        .on('mouseover', function (d, i) {
            var g = ' ';
            var names = chart.varNames;
            var selectVar = document.getElementsByTagName('select')[0];
            var sOpt = selectVar.selectedOptions;
            var s = [];
            for (j = 0; j < sOpt.length; j++) {
                s.push(Number(sOpt[j].value));
              }

            for (var j = 0; j < s.length; j++) {
              if (s[j] !== undefined) {
                var w = names[s[j]-1] + ": <span>"
                var t =  data[i][names[s[j]-1]] + "</span> <br>"
             }

             var g = w + t + g;
             var p = s.length;

             if (s.includes(0)) {
               var g = ' ';
               for (var k = 0; k < names.length; k++) {
                 var w = names[k] + ": <span>"
                 var t =  data[i][names[k]] + "</span> <br>"
               var g = w + t + g;
               var p = names.length;
             }
           }
         }

         return tooltip.style('visibility', 'visible')
                       .style("left", d3.event.pageX - 50 + "px")
                       .style("top", d3.event.pageY - 30 - 15*(p-1) + "px")
                       .html(g);
          })
          .on('mouseout', function () { return tooltip.style("visibility", 'hidden'); })
          .on('click', function (d, i) {
            var selected = this;
            // clear selection box:
            d3.selectAll('.selection')
              .style("display", "none");

            d3.selectAll('.point')
              .attr("class", function() {
                  if (!d3.event.shiftKey) {
                    return (this === selected) ? "point selected" : "point none";
                  } else {
                    if (this.getAttribute('class') === "point selected") {
                      return "point selected";
                    } else if (this === selected) {
                        return "point selected";
                    } else {
                      return "point none";
                    }
                }
              });

              filterTable();

          })
          .on('dblclick', function (d, i) { // deselect
            var selected = this;
             selected.setAttribute('class', 'point none');
             filterTable();
          });
  }

  // add brush to the plot
  addBrush() {
    var tag, selector;
    // identify if a raster map is present (from iNZightMaps)
    if (document.querySelectorAll('g[id^="background.map"]').length > 0) {
      tag = "inz-plot-bg.1.1.1";
      selector = 4;
    }  else {
      tag = "inz-x-grid.1.1.1";
      selector = 4;
    }

    let pp = document.getElementById(tag).parentNode;
    this.enableBrush(pp, selector, "xy", false);
  }

  brushmove() {
    var s = d3.event.selection,
        x1 = s[0][0],
        x2 = s[1][0],
        y1 = s[0][1],
        y2 = s[1][1],
        ind = [];

    d3.selectAll('.point')
          .attr('class', function(d, i) {
            var selected = this;
            var x = this.x.baseVal.value;
            var y = this.y.baseVal.value;
            if ((x1 <= x && x <= x2) && (y1 <= y && y <= y2)) {
              return('point selected');
            } else {
              return('point none');
            }
          });

    this.filterTable();
  }

  reset() {
    d3.selectAll('.point')
      .classed("none selected", false);

    // restore table to original state
    this.tbl.search('').columns().search('').draw();
    this.tbl.rows().nodes().to$().removeClass('active');

    d3.selectAll('.label')
    .classed("hidden", true);

    // clear selection box:
    d3.selectAll('.selection')
      .style("display", "none");

    d3.selectAll('.trend')
      .classed("hidden", false);

    d3.select('.trend-info')
      .classed("hidden", true);
  }

  init() {
    //initialize table:
    var tbl = new Table(this.chartType);
    tbl.init();
    tbl.tableToPlot(tbl.DT, this.panel, "point");
    this.tbl = tbl.DT;

    // set tooltip:
    this.createForm();
    this.setTooltip(100);
    this.setHovers();
    this.addBrush();

    // init box plot if it's a dot plot and boxplots are enabled:
    if (this.chartType === "dot" && this.chart.boxData.length > 0) {
      var box = new Boxplot(this.chart);
      box.init();
    }
    
    if (this.chartType === 'dot' && this.chart.meanData.length > 0) {
      var meanInds = new MeanIndicator(this.chart);
      meanInds.init();
    }

    //enable legend interactions
    let legend = new Legend(this);
    legend.init();

    //enable event handlers:
    d3.select("#reset")
      .on("click", this.reset);

    d3.select(".overlay")
      .on("dblclick", this.reset);
  }

}
