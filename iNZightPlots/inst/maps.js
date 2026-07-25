/** Classes for iNZightMaps 2.0 (maps generated with ggplot2)
 * @param {Object} chart - chart object from R
 * General class for maps generated with iNZightMaps 2.0,
 * which has 3 subclasses : region maps, centroid maps, sparkline maps
 * Addional class for line chart (sparkPlot) for sparkline maps only */
class Inzmap {

  constructor(chart) {
    this.svg = d3.select("svg")
                 .attr("width", null)
                 .attr("height", null);
    this.chart = chart;
    this.data = chart.data;
    this.names = chart.names;
    this.xvar = chart.names[1];
    this.yvar = chart.names[2];
    this.multi = chart.multi[0];
    this.timeData = chart.timeData;
    this.type = chart.type[0];
    this.seqVar = chart.seqVar[0];
    this.int = chart.int[0];
    this.n_polygons = chart.n_polygons;
    // identify which elements represent regions and split if necessary:
    // selecting 'use' and 'path' as some maps have circles that represent regions (world thematic map)
    // Note that for centroid maps, the circles will get selected too, so will need to split nodelist
    let el = document.querySelectorAll('g[id^="grill.gTree"]~g[id^="GRID"] use, path');
    if (el.length === this.n_polygons.length)  {
      this.pathElements = el;
    } else {
      //split into 2: to separate between regions and circles
      let arrayEL = Array.from(el);
      this.pathElements = arrayEL.slice(0, this.n_polygons.length);
      this.extraElements = arrayEL.slice(this.n_polygons.length);
    }
    // default color palette = ggplot2 blues
    let defaultColors = ["#101f33", "#162e47", "#1b3c5e", "#234c78", "#2b5f94",
                      "#3572b2", "#3c89d3", "#46a0f7"];
    this.palette = chart.palette ? chart.palette : defaultColors;
    this.filteredData = this.filterMissing(this.data, this.names);
    this.cnames = this.getColNames();
    
    // bind event handlers:
    this.reset = this.reset.bind(this);
    this.matchRegionID = this.matchRegionID.bind(this);
    this.timeChange = this.timeChange.bind(this);
    this.setTable = this.setTable.bind(this);
    this.filterTable = this.filterTable.bind(this);
  }

  setRegions() {
    let data = this.data;
    let names = this.names;
    let n_polygons = this.n_polygons;

    d3.selectAll(this.pathElements)
      .attr("class", "region")
      .attr("id", (d, i) => {
        let j = n_polygons[i] - 1;
        return(data[j][names[0]] + "." + j);
      });
  }

  filterMissing(data, names) {
    const last = names.length - 1;
    // ids to take into account of missing values:
    let arr = data.map((d, i) => {
      if (d[names[last]]) return i;
    }).filter(d => d !== undefined);
    return (arr);
  }

  getColNames() {
    let colNames = document.getElementsByTagName("th");
    let cnames = [];
    for (let i = 0; i < colNames.length; i++) {
      cnames.push(colNames[i].innerHTML.trim());
    }
    return cnames;
  }

  filterTable() {
    var dd = [];
    var ind = [];
    let table = this.tbl;
    let names = this.names;

    if (this.multi && this.type !== "region") {
    table.search('').columns().search('').draw();
    }

    // find selected regions:
    var regionsSelected = document.getElementsByClassName('region selected');
    for (var i = 0; i < regionsSelected.length; i++) {
        var id = regionsSelected[i].id;
        var regionName = id.substring(0, id.lastIndexOf('.'));
        dd.push(regionName);
        ind.push("^" + regionName + "$");
    }

    // find column number:
    let colNum = this.cnames.findIndex(cnames => cnames === names[0]);
    table.columns(colNum).search(ind.join("|"), true, false).draw();
  }

  /** filter data to specific region by a time period
  * @param data full data set exported from R (timeData)
  * @param group variable that stores the region names
  * @param region the region that is selected
  * @return returns filtered data set for a specific time period
  */
  filterData(data, group, region) {
    let ava = data.filter(d => d[group] == region || d[group] == undefined);
    let names = this.names;

    //sort data relative to how regions are plotted:
    ava = ava.sort(function(a, b) {
      if(a[names[0]] < b[names[0]]) return - 1;
      if(a[names[0]] > b[names[0]]) return 1;
      return 0;
    });

    return ava;
  }

  /** Link table to plot
  * @return links table to plot (when you click, it highlights specific region */
  tableToPlot() {
    let table = this.tbl;
    let names = this.names;
    let cnames = this.cnames;
    let type = this.type;
    let arr = this.filteredData;

    $('#table tbody').on('click', 'tr', function() {
      $(this).toggleClass('active');
      var r = table.rows('.active').data();
      var ind = [];
      // go by region rather than by index:
      var colNum = cnames.findIndex(cnames => cnames === names[0]);
      var regions = [], p;
      for (let i = 0; i < r.length; i++) {
        regions.push(r[i][colNum]);
      }

      d3.selectAll(".region")
        .attr("class", function() {
          var id = this.id;
          var rr = id.substring(0, id.lastIndexOf("."))
          if (regions.includes(rr)) {
            ind.push(+id.substring(id.lastIndexOf(".") + 1));
          }
          return (regions.includes(rr) ? "region selected" : "region none");
        });

      // for centroids and spark lines:
      if (type == "region") return;
      var p = (type === "point") ? "centroid" : "sparkRegion";

      for (let i = 0; i < arr.length; i++) {
        var el = document.getElementById(p + "." + arr[i]);
        if (ind.includes(arr[i])) {
          el.setAttribute('class', p + " selected");
        } else {
          el.setAttribute('class', p + " none");
        }
      }
    });
  }

  setTooltip() {
    let tooltip = d3.select("body").append("div")
                    .attr("class", "tooltip")
                    .style("width", "100");
    this.tooltip = tooltip;
    return tooltip;
  }

  makeTooltips() {
    let names = this.names;
    let data = this.data;
    let chartType = this.type;
    let matchRegionID = this.matchRegionID;
    let filterTable = this.filterTable;
    let tooltip = this.tooltip;
    let table = this.tbl;
    let lineChart = this.lineChart;

    d3.selectAll(".region")
      .on("mouseover", function(d) {
         var selected = d3.select(this);
         var ind = matchRegionID(selected);
        // make things go light and dark:
        selected.classed("selectRegion", true);

        var g = " ";
        var p = names.length;

        for (var j = 0; j < p; j++) {
            var w = names[j] + ": <span>";
            var t = data[ind][names[j]] + "</span> <br />";
            var g = w + t + g;
        }

        // region + specific variables:
        return tooltip.style('visibility', 'visible')
                      .style('left', d3.event.pageX - 50 + "px")
                      .style('top', d3.event.pageY - 50 - 15 * (p - 1) + "px")
                      .html(g);
    })
    .on('mouseout', function() {
        var selected = d3.select(this);
        selected.classed('selectRegion', false);
        tooltip.style('visibility', 'hidden');
    })
    .on('click', function(d, i) {
      var selected = this;
      var ind = [];
      var dd = [];

      d3.selectAll('.region')
        .attr("class", function(d, i) {
          //if the shift key is down, accumulate:
          if(d3.event.shiftKey) {
                if(this.getAttribute('class') === "region selected") {
                  if (this === selected)  { // for deselecting
                    if (chart.type == "sparklines") this.style.fill = null;
                    return "region none";
                  }
                  return "region selected";
                } else if (this === selected) {
                  if (chart.type == "sparklines") this.style.fill = lineChart.colors[i];
                    return "region selected";
                } else {
                  if (chart.type == "sparklines") this.style.fill = null;
                    return "region none";
                }
        } else {
            if(this === selected) {
              if (chart.type == "sparklines") this.style.fill = lineChart.colors[i];
                return "region selected";
            } else {
              if (chart.type == "sparklines") this.style.fill = null;
                return "region none";
            }
          }
        });

        var j = i;

        // for point maps:
        if (chartType === "point") {
          d3.selectAll('.centroid')
            .attr("class", function(d, i) {
              if (!d3.event.shiftKey) {
                return (this.getAttribute('id') === ("centroid." + j)) ?
                        "centroid selected" : "centroid none";
              } else {
                if (this.getAttribute("class") === "centroid selected") {
                  if (this.getAttribute("id") === ("centroid." + j)) return "centroid none";
                  return "centroid selected";
                } else if (this.getAttribute('id') == ("centroid." + j)) {
                  return "centroid selected";
                } else {
                  return "centroid none";
                }
              }
            });
        }

        if (chartType === "sparklines") {
          // fade sparkregions:
          if (lineChart.yvar === names[2]) {
            d3.selectAll(".sparkRegion").attr('class', function(d, i) {
              if (!d3.event.shiftKey) {
                if (this.getAttribute('id') == ("sparkRegion." + j)) {
                    return "sparkRegion selected";
                } else {
                    return "sparkRegion none";
                }
              } else {
                if (this.getAttribute("class") === "sparkRegion selected") {
                  if (this.getAttribute("id") === ("sparkRegion." + j)) return "sparkRegion none";
                  return "sparkRegion selected";
                } else if (this.getAttribute('id') == ("sparkRegion." + j)) {
                  return "sparkRegion selected";
                } else {
                  return "sparkRegion none";
                }
              }
            });
          }
        }

        if (chart.multi[0] == false && chart.type !== "region") {
          table.search('').columns().search('').draw();
        }

        filterTable();

        if (chart.type == "sparklines") {
            lineChart.highlightLines();
          }
      });
  }

  matchRegionID(selected) {
    var regionID = selected.attr("id").substr(0, selected.attr("id").lastIndexOf("."));
    // this is required to match data with regions correctly:
    // in cases like messy gapminder: some countries do not have data recorded in certain years,
    // while some countries do. This is to make sure the right country data is matched to the right region.
    // It is possible that a country may not have a data for a specific year, then that var becomes 'undefined'.
    let ind = undefined;
    let data = this.data;
    let names = this.names;
    for (var j = 0; j < data.length; j++) {
        if (data[j][names[0]] == regionID) ind = j;
    }
    return ind;
  }

  //time change:
  timeChange() {
    var yvar = d3.select('.control-var').property('value'),
        cur = d3.select(".slider").property("value"),
        seqVar = this.seqVar,
        // filter data across time variable:
        data = this.filterData(this.timeData, seqVar, cur),
        names = this.names;

    this.data = data;
    this.changeTitle(yvar, cur);
    d3.select(".slider-val").html(cur);

    // update regions
    if (this.type === "region") {
      this.updateFills();
    } else {
      this.circleControl();
    }

    // update tooltips and table
    this.makeTooltips();
    var colNum = this.cnames.findIndex(cnames => cnames === seqVar);
    this.tbl.search('').columns().search('').draw();
    this.tbl.columns(colNum).search(cur + "|NA", true, false).draw();

  }

  // add time slider (only for multiple observations over time)
  addSlider() {
    var seqVar = this.seqVar,
        current = this.data[0][seqVar],
        range = d3.extent(this.timeData, d => d[seqVar]),
        int = this.int,
        timeChange = this.timeChange;

    // input a slider:
    d3.select(".menu").insert("li", ".help")
      .attr("class", "li-slider");

    var sliderDiv = d3.select(".li-slider").append("div")
                   .attr("class", "slider-div");

    var sliderVal = sliderDiv.append("div")
                             .attr("class", "slider-val")
                             .html(current);

    var slider = sliderDiv.append("input")
                    .attr("class", "slider")
                   .attr("type", "range")
                   .attr("min", range[0])
                   .attr("max", range[1])
                   .attr("step", int)
                   .attr("value", current)
                   .on("input", timeChange);

    // add play/pause button:
    // adapted from: https://stackoverflow.com/questions/34934577/html-range-slider-with-play-pause-loop
    var timer;
    var play = sliderDiv.append("div").append("i")
                        .attr("class", "glyphicon glyphicon-play play-pause")
                        .on("click", function() {
                            clearInterval(timer);
                            var el = d3.select(this);
                            el.classed("glyphicon-pause", !el.classed("glyphicon-pause"));
                            if (el.classed("glyphicon-pause")) {
                                timer = setInterval(function() {
                                  var val = d3.select(".slider").property("value");
                                  var num = (val == range[1]) ? 0 : (val - range[0])/int + 1;
                                  //update value on slider:
                                  d3.select(".slider").property("value", range[0] + num * int);
                                  timeChange();
                                  //stop playing when it gets to the last value... (or second last)
                                  if (num == ((range[1] - range[0])/int + 1) - 1) {
                                    clearInterval(timer);
                                    el.classed("glyphicon-pause", false);
                                  }
                                }, 750);
                            } else {
                              //stop timer
                              clearInterval(timer);
                            }
                        });
  }

  setTable() {
    // this is only run when there are multiple observations across time periods
    let table = this.tbl;
    var cur = $(".slider-val").html();
    let seqVar = this.seqVar;
    var colNum = this.cnames.findIndex(cnames => cnames == seqVar);
    table.columns(colNum).search(cur + "|NA", true, false).draw();
  }

  // add zoom
  enableZoom() {
    // adapted from: https://bl.ocks.org/iamkevinv/0a24e9126cd2fa6b283c6f2d774b69a2
    function zoomed() {
      bg.setAttribute("transform", d3.event.transform);
      // make path strokes smaller as you zoom in
      d3.selectAll(".region")
        .style("stroke-width", (7 - d3.event.transform.k)/10 + "px");
    }
    // attempt zooming within the box
    this.zoom = d3.zoom()
                 .scaleExtent([1, 8])
                 .on("zoom", zoomed);

    var bg = document.querySelectorAll("g[id^='panel']")[0];
    d3.select(bg)
      .attr('class', 'background')
      .call(this.zoom);
  }

  reset() {
    //clear map
    d3.selectAll('.region')
    .classed('selected none', false);

    d3.selectAll('.sparkRegion')
    .classed('.sparkRegion none', false);

    d3.selectAll('.centroid')
    .classed('.centroid none', false);

    // clear table
    let table = this.tbl;
    table.search('').columns().search('').draw();
    table.rows().nodes().to$().removeClass('active');

    if (this.type == "region" && this.multi) {
      // screen for certain year:
      this.setTable();
    }

    if (this.type == "sparklines") {
      d3.selectAll(".region")
        .style("fill", null);
      // deselect lines:
      d3.selectAll(".spark-line")
        .style("stroke", "rgb(0, 0, 0)")
        .style("stroke-width", 1)
        .style("opacity", 0.2);

      d3.select(".region-labels").html(null);
    }

    //reset zooming:
    d3.select('.background')
    .attr('transform', null)
    .call(this.zoom.transform, d3.zoomIdentity);

  }

  // change title of map
  changeTitle(varName, year = null) {
    // change the title as things change
    var title = document.querySelectorAll('g[id^="title"] tspan')[0];
    var text = (year) ? varName + " (" + year + ")" : varName;
    if (title)  {
    // if there's a title present
    title.innerHTML = text;
    return;
    } else {
    title = document.getElementsByClassName("main-title")[0];
    if (title) {
        d3.select(title)
          .text(text);
      } else {
        // attach a title:
        title = document.querySelectorAll('g[id^="layout::title"]')[0];
        // where is this background rectangle:
        var rect = document.querySelectorAll('g[id^="panel.background..rect"] rect')[0];
        let w = +rect.getAttribute("width");
        let h = +rect.getAttribute("height");
        let x = +rect.getAttribute("x");
        let y = +rect.getAttribute("y");
        // append text:
        d3.select(title).append("text")
          .attr("class", "main-title")
          .attr("transform", "scale(1, -1)") // flip text right way up
          .attr("x", w/2 + x)
          .attr("y", - h - y - 1)
          .text(text);
      }
    }
    return;
  }

  selectForm(ff) {
    // numeric variables in data set:
    var numVar = chart.numVar;
    var yvar = chart.names[chart.names.length - 1];

    // create selection:
    d3.select(".menu").insert("li", ".help")
      .attr("class", "var-control");

    var controlVar = d3.select('.var-control').append('div')
                       .attr('class', 'form-group form-inline form-div');

    // add label:
    var label = d3.select('.form-div').append('label')
                  .html('Variable to display:');

    var selection = d3.select('.form-div').append('select')
                      .attr("class", "form-control control-var")
                      .on('change', ff);

    var options = d3.select('.control-var').selectAll('option')
                    .data(numVar).enter()
                    .append('option')
                    .attr('id', function(d) { return ('option.' + d); } )
                    .attr('value', function(d) { return d; })
                    .html(function(d) { return d; });

    // set default:
    document.getElementById('option.' + yvar).setAttribute('selected', "selected");

    //add title:
    var formTitle = selection.append('p')
                           .attr("class", "form-title")
                           .html("Variable to display");
  }

  gen() {
    this.setRegions();
    this.setTooltip();
    this.makeTooltips();
    this.tableToPlot();
    this.enableZoom();

    d3.select("#reset")
      .on("click", this.reset);
  }

}

// CENTROID MAP CLASS
class CentroidMap extends Inzmap {

  constructor(props) {
    super(props);
    // bind event handlers:
    this.circleControl = this.circleControl.bind(this);
  }

  setCentroids() {
    d3.selectAll(this.extraElements)
      .attr("class", "centroid");

    let arr = this.filteredData;

    d3.selectAll(".centroid")
      .attr("id", function(d, i) {
        return "centroid." + arr[i];
      });
  }

  //store circle widths and heights - called before circleControl!
  getCircleSize() {
    var cc = document.getElementsByClassName('centroid');
    var w = d3.range(0, cc.length).map(d => +cc[d].getAttribute("width"));
    return(d3.extent(w));
  }

  // circle control
  circleControl() {
    var newY = d3.select('.control-var').property('value');
    var data = this.data;
    var csize = this.getCircleSize();

    this.changeTitle(newY);

    // filter for missing values...
    var arr = [];

    /*TODO: test on missing data
    var xx = data.filter(d => d[newY]);
    console.log(xx);*/

    for (let i = 0; i < data.length; i++) {
      if (data[i][newY]) {
        arr.push(data[i]);
      } else {
        continue;
      }
    }

    // find the maximum circle
    var max = d3.max(arr, function(d) { return d[newY]; });

    // set a squareroot scale to make circle area proportional to variable:
    var sqrtScale = d3.scaleSqrt()
                  .domain(d3.extent(arr, function(d) { return d[newY]; }))
                  .range(csize);

    // because circles are defined using 'use' elements (with 'shadow DOM' circles)
    // controlling width and height of these 'use' elements control the circle (radius)
    // transform is by half of the width and height
    var circles = d3.selectAll('.centroid')
                  .data(arr)
                  .transition()
                  .duration(300)
                  .attr('width', function(d) {
                      return sqrtScale(d[newY]);
                        })
                  .attr('height', function(d) {
                    return sqrtScale(d[newY]);
                        })
                  .attr("transform", function(d) {
                    var c = - sqrtScale(d[newY])/2;
                        return ("translate(" + c + "," + c + ")");
                  });

    this.names[2] = newY;
  }

  init() {
    //initialized table
    var tbl = new Table(chart);
    tbl.init();
    this.tbl = tbl.DT;

    // set centroids:
    this.setCentroids();
    this.gen();

    // set up circle handlers
    this.selectForm(this.circleControl);

    //add time slider if there are multiple observations
    if (!this.multi) return;
      this.addSlider();
      this.setTable();
  }

}

// REGION MAPS
class RegionMap extends Inzmap {

  constructor(props) {
    super(props);
    // bind relevant event handlers:
    this.updateFills = this.updateFills.bind(this);
  }

  // set up a new legend for switching between variables
  // for now refers to a continuous colour scale
  setLegend() {
    var layout = document.querySelectorAll("g[id='layout.2']");

    var newScale = d3.selectAll(layout)
                     .append('g')
                     .attr('id', 'new-scale')
                     .classed('hidden', true);

    var defs = newScale.append("defs");

    var linearGradient = defs.append("linearGradient")
                             .attr("id", "linear-gradient");

        linearGradient.attr("x1", "0%")
                      .attr("x2", "0")
                      .attr("y1", "0%")
                      .attr("y2", "100%");

    // get tranformation coordinates: for numerics
    var sRect = document.querySelectorAll('g[id^="bar.4-2-4-2.1.1"]')[0].getAttribute('transform');
    // get coordinates:
    var coords = sRect.substring(sRect.lastIndexOf("(") + 1, sRect.lastIndexOf(")")).split(",");
    var w = 15;
    var h = 100;

    // append rect scale:
    newScale.append('rect')
      .attr('width', w)
      .attr('height', h)
      .attr('transform', 'translate(' + coords[0] + "," + (coords[1] - h) + ")")
      .attr('stroke', 'none')
      .attr("fill", "url(#linear-gradient)")
      .attr('fill-opacity', 1);

    var leg = newScale.append('g')
          .attr("class", "leg-axis")
          .attr("fill", "black")
          .attr("fill-opacity", 1)
          .attr("stroke", "none")
          .attr("transform", "translate(" + (+coords[0] + w) + "," + coords[1] + ") scale(1, -1)");
           //flip, cause gridSVG flips

    //add an axis title:
    var legTitle = newScale.append("text")
                         .attr("class", "leg-title")
                         .attr("fill", "black")
                         .attr("fill-opacity", 1)
                         .attr("transform", "translate(" + coords[0] + "," +  (+coords[1] + 10) + ") scale(1, -1)");
  }

  //update fills on regional maps:
  updateFills() {
    var yvar = d3.select('.control-var').property('value'),
      data = this.data,
      pal = (this.palette) ? this.palette : this.defaultColours;

    if (this.multi) {
      var dd = this.timeData;
      var cur = d3.select(".slider").property("value");
      this.changeTitle(yvar, cur);
    } else {
      var dd = data;
      this.changeTitle(yvar);
    }

    //hide original scale:
    var ss = document.querySelectorAll('g[id^="guide-box"]');
      d3.selectAll(ss)
        .classed('hidden', true);

    // divide domain according to number of colours supplied.
    var min = d3.min(dd, function(d) { return d[yvar]; });
    var max = d3.max(dd, function(d) { return d[yvar]; });

    let dval = d3.range(0, pal.length).map(d => min + ((max - min)/(pal.length - 1) * d));

    //colour scale goes by whatever pal is.
    let colorScale = d3.scaleLinear()
                     .range(pal)
                     .domain(dval);
    this.colorScale = colorScale;

    //update linear gradient:
    // code adapted from: https://www.visualcinnamon.com/2016/05/smooth-color-legend-d3-svg-gradient.html
    var linearGradient = d3.select('#linear-gradient');

    //remove and reset all colours on the scale:
    linearGradient.selectAll("stop").remove();

    var stops = linearGradient.selectAll("stop")
                            .data(dval)
                            .enter().append("stop")
                            .attr("offset", function(d, i) { return(i/dval.length * 100 + "%"); })
                            .attr("stop-color", function(d) { return colorScale(d); });

    //update axes and update legend:
    var legAxis = d3.scaleLinear()
                  .range([100, 0])
                  .domain([min, max]);

    var leg = d3.select('.leg-axis')
              .call(d3.axisLeft(legAxis)
              .ticks(4)
              .tickFormat(d3.format("d")));

    // attempt to mimic 'ggplot2' legend:
    leg.selectAll('path')
     .attr('stroke', 'none');

    leg.selectAll('line')
      .attr('stroke', 'white');

    leg.selectAll("text")
     .attr('x', 1)
     .attr('font-size', '8px')
     .attr("text-anchor", "start");

    d3.select('#new-scale')
    .classed('hidden', false);

    // update legend title:
    d3.select('.leg-title')
    .text(yvar);

    let matchRegionID = this.matchRegionID;

    // update regions:
    d3.selectAll('.region')
    .transition()
    .duration(500)
    .attr("fill", function(d) {
      // set default missing to #7f7f7f (ggplot2)
      var selected = d3.select(this);
      var ind = matchRegionID(selected);
      if (ind == undefined) return "#7f7f7f";
      return ((data[ind][yvar] !== undefined) ? colorScale(data[ind][yvar]) : "#7f7f7f");
    });

    // reassign yvar assessed to update tooltip:
    this.names[this.names.length - 1] = yvar;
  }

  init() {
    //initialized table
    var tbl = new Table(this.type);
    tbl.init();
    this.tbl = tbl.DT;

    this.gen();

    // set up legend and form selection handlers
    this.setLegend();
    this.selectForm(this.updateFills);

    //add time slider:
    if (this.multi) {
      this.addSlider();
      this.setTable();
    }
  }

}

// SPARKLINE MAP CLASS
class SparkMap extends Inzmap {
  constructor(props) {
    super(props);
    this.sparkElements = document.querySelectorAll('g[id^="GRID.gTree"] path');
  }

  setSparkRegions() {
    d3.selectAll(this.sparkElements)
      .attr("class", "sparkRegion")
      .attr("id", function(d, i) {
        return ("sparkRegion." + i);
      });

    let arr = this.filteredData;

    d3.selectAll(".sparkRegion")
      .attr("id", function(d, i) {
        return("sparkRegion." + arr[i]);
      });
  }

  init() {
    //initialized table
    var tbl = new Table();
    tbl.init();
    this.tbl = tbl.DT;

    // initialize line chart
    var sp = new SparkPlot(this);
    sp.init();
    this.lineChart = sp;
    // bind form selection
    this.selectForm(sp.changeVar);

    // set spark regions and interactions on map
    this.setSparkRegions();
    this.gen();
  }

}

/** SPARKLINE PLOT CLASS
 * This is used to draw the line chart that shows all spark lines
 * for all regions (for SparkMaps only!)
 * @param {Object} props - SparkMap object
 * @return an 'enlarged' time line chart across all regions
 */
class SparkPlot {

  constructor(props) {
    this.xvar = props.chart.names[1];
    this.yvar = props.chart.names[2];
    this.group = props.chart.names[0];
    this.lineType = props.chart.sparkline_type[0];
    this.timeData = props.timeData;
    this.tbl = props.tbl;
    this.names = props.names;
    this.multi = props.multi;
    this.type = props.type;
    this.cnames = props.cnames;
    this.filterTable = props.filterTable;
    this.changeTitle = props.changeTitle;
    this.margin = {top : 60, right: 80, bottom: 40, left: 70};
    // bind this to event handlers:
    this.changeVar = this.changeVar.bind(this);
  }

  setUp() {
    let xvar = this.xvar,
        yvar = this.yvar,
        group = this.group,
        margin = this.margin;

    let sparkContainer = d3.select('.tbl-div').insert('div', ":first-child")
                       .attr('class', 'spark-div');
    let chartWidth = Number($(".tbl-div").width());
    let chartHeight = chartWidth * 0.7;
    let width = chartWidth - margin.right - margin.left;
    let height = chartHeight - margin.bottom - margin.top;

    this.width = width;
    this.height = height;

    let spark = d3.select(".spark-div").append("svg")
                  .attr("class", "sparkPlot")
                  .attr("viewBox", "0 0 " + chartWidth + " " + chartHeight)
                  .attr("width", "100%")
                  .attr("preserveAspectRatio", "xMidYMid meet");

    // define a clip path:
    spark.append("defs").append("clipPath")
         .attr("id", "spark-clip")
         .append("rect")
         .attr("width", width)
         .attr("height", height);

    let g = spark.append("g")
                 .attr("class", "spark-group")
                 .attr("transform", "translate(" + margin.left + ", " + margin.top + ")");

    // x and y scales:
    let xScale = d3.scaleLinear()
                   .range([0, width])
                   .domain(d3.extent(this.timeData, function(d) { return d[xvar]; }));

    let yScale = d3.scaleLinear()
                   .range([height, 0]);

    // Mimic ggplot2 theme: gray background, major and minor axes
    g.append("rect")
     .attr("class", "grid-background")
     .attr("width", width)
     .attr("height", height);

    //xaxis:
    g.append("g")
     .attr("class", "spark-xaxis")
     .attr("transform", "translate(0," + height + ")")
     .call(d3.axisBottom(xScale).ticks(10).tickFormat(d3.format("d")));

    //yaxis:
    g.append("g")
     .attr("class", "spark-yaxis")
     .call(d3.axisLeft(yScale).ticks(10));

   // major/minor grid lines:
   let gridX = g.append("g")
                .attr("class", "grid grid-X")
                .selectAll(".grid-lines")
                .data(xScale.ticks(10))
                .enter().append("line")
                .attr("class", "grid-lines")
                .attr("x1", function(d) { return xScale(d); })
                .attr("x2", function(d) { return xScale(d); })
                .attr("y1", 0)
                .attr("y2", height)
                .classed("minor", function(d, i) {
                  return (i % 2 === 0) ? true : false;
                });

   let gridY = g.append("g")
                .attr("class", "grid grid-Y");

   // for plotting lines:
   g.append("g")
    .attr("class", "line-group")
    .attr("clip-path", "url(#spark-clip)");

   // add tooltip:
   d3.select("body").append("div")
     .attr("class", " tooltip sparkline-tooltip");

  // add labels:
  spark.append("g")
       .attr("class", "region-labels");

   //add axis labels:
   var xlab = spark.append("text")
                   .attr("class", "x-lab")
                   .attr("transform", "translate(" + chartWidth/2 + "," + (chartHeight - 10) + ")")
                   .text(xvar);

   var ylab = spark.append("text")
                   .attr("class", "y-lab")
                   .attr("transform", "translate(" + (margin.left/2) + "," + (margin.top - 10) +")")
                   .style("text-anchor", "left");

   // add a title for the region:
   var title = spark.append("text")
                    .attr("transform", "translate(" + chartWidth/2 + "," + (margin.top/2) + ")")
                    .attr("class", "spark-title");

    // store scales:
    this.yScale = yScale;
    this.xScale = xScale;
  }

  // for calculating percent changes, absolute/relative:
  calculate(data, xvar, yvar, reduce = false) {
    let groupedNest = data;
    if (this.lineType == "Absolute") return groupedNest;

    let transformedData = [];
    // divide and group by each region:
    let nest = d3.nest().key(function(d) { return d[chart.names[0]]; }).entries(data.reduce((acc, val) => acc.concat(val), []));

    if (this.lineType == "Relative") {
        nest.forEach(function(d) {
            let set = [];
            let vals = d.values;
            // find the most earliest: for relative change
            let minyear = d3.min(vals, function(d) {
                return d[xvar];
            });
            vals.map((value, index, values) => {
                if (value[xvar] == minyear) ind = index;
                set.push({ [chart.names[0]]: value[chart.names[0]],[xvar]: value[xvar], [yvar]: +(value[yvar] - vals[ind][yvar]).toFixed(2) });
            });
            //store all values into relative:
            transformedData.push(set);
        });

    } else {
      nest.forEach(function(d) {
        let set = [];
        let vals = d.values;
        // take the previous value and find the difference
        vals.map((value, index, values) => {
            let ind = (index == 0) ? 0 : index - 1;
            let change = +((value[yvar] - vals[ind][yvar])/vals[ind][yvar] * 100).toFixed(2) ;
            set.push({ [chart.names[0]]: value[chart.names[0]],
                       [xvar]: value[xvar],
                       [yvar]: change
                    });
        });
        transformedData.push(set);
     })
    }

    if (reduce) return (transformedData.reduce((acc, val) => acc.concat(val), []));
    return (transformedData);
  }

  setGridLines(xScale, yScale) {
    // update grid lines:
    d3.select(".grid-Y").selectAll(".grid-lines").remove();

    d3.select(".grid-Y").selectAll(".grid-lines")
      .data(yScale.ticks())
      .enter().append("line")
      .attr("class", "grid-lines")
      .attr("y1", function(d) { return (yScale(d)); })
      .attr("y2", function(d) { return (yScale(d)); })
      .attr("x1", 0)
      .attr("x2", xScale.range()[1])
      .classed("minor", function(d, i) {
        return (i % 2 == 0) ? true : false;
      });

    // if zero is within yScale, emphasize with a line at 0:
    if (yScale.domain()[0] <= 0 && yScale.domain()[1] > 0) {

      d3.select(".grid-Y").select(".zero-line").remove();

      d3.select(".grid-Y")
        .append("line")
        .attr("class", "zero-line")
        .attr("y1", yScale(0))
        .attr("y2", yScale(0))
        .attr("x1", 0)
        .attr("x2", xScale.range()[1])
        .style("stroke", "black");
    }
  }

  setTitle(xvar, yvar, lineType) {
    // set title:
   d3.select('.spark-title')
     .text(yvar + " by " + xvar);

   // set y-variable:
   var ylabText;

   switch(lineType) {
      case "Absolute":
        ylabText = yvar;
        break;

      case "Relative":
        ylabText = "Relative change in " + yvar;
        break;

      case "Percent":
        ylabText = "% change in " + yvar;
        break;
   }

   d3.select('.y-lab')
     .text(ylabText);
  }

  drawLines(xvar, yvar, data, colors) {

    //functions needed:
    let filterTable = this.filterTable;
    let addLabels = this.addLabels;

    let width = this.width;
    let margin = this.margin;
    let xScale = this.xScale;
    let yScale = this.yScale;

    // line function
    let addLines = d3.line()
                 .x(function(d) { return (d[xvar] ? xScale(d[xvar]) : xScale(0)); })
                 .y(function(d) { return (d[yvar] ? yScale(d[yvar]) : yScale(0)); });

    // rejoin data:
    let lines = d3.select(".line-group").selectAll("path")
                  .data(data);

    //exit, enter, update
    lines.exit().remove();
    lines.enter().append("path")
         .attr("class", "spark-line")
         .attr("id", function(d, i) { return ("sparkline." + d.key + "." + i); })
         .merge(lines)
         .on("mouseover", function(d) {
           d3.select(this).style("opacity", 1);

           let coord = d3.mouse(this);
           let x = Math.round(xScale.invert(coord[0]));
           let html;
           // find the closest value:
           let t = (d.values.filter(t => t[xvar] === x));
           if (t.length >= 1) {
             html = x + ", " + t[0][yvar];
           } else {
             html = x + ", " + (yScale.invert(coord[1])).toFixed(2);
           }
           d3.select(".sparkline-tooltip")
               .style("visibility", "visible")
               .style('left', d3.event.pageX - 50 + "px")
               .style('top', d3.event.pageY - 50 - 15 + "px")
               .style("width", "100px")
               .html("<span>" + d.key  + "</span> <br />"
                     + html);

         })
         .on("mouseout", function() {
           d3.select(this).style("opacity", function() {
             return (d3.select(this).style("stroke") !== "rgb(0, 0, 0)") ? 1 : 0.2;
           });

           d3.select(".sparkline-tooltip")
             .style("visibility", "hidden");
         })
         .on("click", function(d, i) {
           let el = this;

           // if clicked, then just pinpoint 1:
           d3.selectAll(".spark-line")
             .classed("on", function(d, i) {
               let region = document.getElementById(d.key + "." + i);
               let sparkRegion = document.getElementById("sparkRegion." + i);
               if (!d3.event.shiftKey) {
                 if (this === el) {
                   d3.select(el).style("opacity", 1)
                                .style("stroke", colors[i])
                                .style("stroke-width", 3);
                  // select regions:
                  region.setAttribute("class", "region selected");
                  region.style.fill = colors[i];
                  sparkRegion.setAttribute("class", "sparkRegion selected");
                  return true;
                 } else {
                   d3.select(this).style("opacity", 0.2)
                                  .style("stroke", null)
                                  .style("stroke-width", 1.5);
                   if (region) {
                     region.setAttribute("class", "region none");
                     region.style.fill = null;
                   }
                   if (sparkRegion) sparkRegion.setAttribute("class", "sparkRegion none");
                  return false;
                 }
               } else {
                 //accumulate:
                 if (this === el) {
                   // deselect:
                   if (d3.select(el).classed("on")) {
                     d3.select(el).style("opacity", 0.2)
                                  .style("stroke", null)
                                  .style("stroke-width", 1.5);
                    if (region) {
                      region.setAttribute("class", "region none");
                      region.style.fill = null;
                    }
                    if (sparkRegion) sparkRegion.setAttribute("class", "sparkRegion none");
                    return false;
                  } else {
                     d3.select(el).style("opacity", 1)
                                  .style("stroke", colors[i])
                                  .style("stroke-width", 3);
                    // select regions:
                    if (region) {
                      region.setAttribute("class", "region selected");
                      region.style.fill = colors[i];
                    }
                    if (sparkRegion) sparkRegion.setAttribute("class", "sparkRegion selected");
                    return true;
                   }
                 } else {
                   return false;
                 }
               }
             });

           // add labels:
           let dd = [];
           let selected = document.getElementsByClassName('region selected');
           for (var i = 0; i < selected.length; i++) {
             let id = selected[i].id;
             dd.push(id);
           }

           addLabels(dd, data, yvar, colors, width, margin);
           filterTable();

         })
         .transition().duration(500)
         .attr("d", function(d) { return addLines(d.values); });
  }

  getColors(n) {
    //colours:
    var start = 0;
    var end = 360;
    var h = d3.range(start, end, end/(n + 1));
    let colors = h.map(function(d) { return d3.hcl(d, 50, 52); });
    return colors;
  }

  addLabels(regions, data, yvar, colors, width, margin) {
    // sort by last value:
    let lastValues = regions.map((d, i) => {
      let index = +d.substr(d.lastIndexOf(".") + 1);
      let values = data[index].values;
      return  {
        ind: index,
        region: d.substr(0, d.lastIndexOf(".")),
        value: values[values.length - 1][yvar]
      }
    });

    let sorted = lastValues.sort((x, y) => {
      return d3.descending(x.value, y.value);
    });

    // add new labels:
    let circles = d3.select(".region-labels").selectAll("circle")
                    .data(sorted);

    circles.exit().remove();
    circles.enter().append("circle")
           .merge(circles)
           .attr("cx", width + margin.left + 10)
           .attr("cy", (d, i) => margin.top + i * 20)
           .attr("r", "3px")
           .attr("fill", d => colors[d.ind]);

    let labels = d3.select(".region-labels").selectAll("text")
                   .data(sorted);

    labels.exit().remove();
    labels.enter().append("text")
          .merge(labels)
          .attr("x", width + margin.left + 15)
          .attr("y", (d, i) => margin.top + i * 20 + 5)
          .attr("fill", d => colors[d.ind])
          .text(d => d.region)
           .style("font-size", "10px");
  }

  update() {
    //if yvar is different, then update yScale titles, and yvar:
    let xvar = this.xvar;
    let yvar = this.yvar;
    let group = this.group;
    let xScale = this.xScale;
    let yScale = this.yScale;

    yScale.domain(d3.extent(this.timeData, function(d) { return d[yvar]; }));

    // calculate y-limits:
    var nest = this.calculate(this.timeData, xvar, yvar, true);

    // set to 0 if it is absolute:
    var min = (this.lineType === "Absolute") ? 0 : d3.min(nest, function(d) { return d[yvar]; });
    var max = d3.max(nest, function(d) { return d[yvar]; });
    yScale.domain([min, max]);

    d3.select(".spark-yaxis")
      .call(d3.axisLeft(yScale)
              .tickFormat(d3.format("d")));

    this.setGridLines(xScale, yScale);
    this.setTitle(xvar, yvar, this.lineType);

    // reduce data to single level and calculate based on sparkline type:
    var dd = this.calculate(this.timeData, xvar, yvar);
    this.dd = dd;

    //regroup by regions:
    var nester = d3.nest().key(function(d) { return d[group]; }).sortKeys(d3.ascending)
                   .entries(dd.reduce((acc, val) => acc.concat(val), []));
    this.nestedData = nester;

    // get colours:
    let colors = this.getColors(nester.length);
    this.colors = colors;

    this.drawLines(xvar, yvar, nester, colors);
  }

  highlightLines() {
    // select a single line:
    let colors = this.colors;
    let margin = this.margin;
    let selected = document.getElementsByClassName('region selected');
    let dd = [];
    for (var i = 0; i < selected.length; i++) {
          let id = selected[i].id;
          dd.push(id);
    }

    // reset to all black:
    d3.selectAll(".spark-line")
      .style("stroke", "rgb(0, 0, 0)")
      .style("stroke-width", 1)
      .style("opacity", 0.2);

    dd.forEach(function(d, i) {
      let el = document.getElementById("sparkline." + d);
      let ind = d.substring(d.lastIndexOf(".") + 1);
      el.style.stroke = colors[ind];
      el.style.opacity = 1;
      el.style.strokeWidth = 3;
    });

    this.addLabels(dd, this.nestedData, this.yvar, colors, this.width, margin);
  }

  changeVar() {
    var newY = d3.select('.control-var').property('value');
    var yy = chart.names[2];

    this.changeTitle(newY);

    if (yy == newY) {
      // show sparklines on map:
      d3.selectAll('.sparkRegion')
        .classed('hidden', false);
    } else {
      d3.selectAll('.sparkRegion')
        .classed('hidden', true);
    }

    // update line chart
    this.yvar = newY;
    this.update();
  }

  // initialize line chart
  init() {
    this.setUp();
    this.update();
  }

}
