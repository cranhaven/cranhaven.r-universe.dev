/** Interactivity for iNZightPlots
 * For each plot there's a sub-class which extends with this class `inzplot`.
 * This class applies for all single panel plots generated with the iNZightPlots
 * package in R.
 * Functions that apply for this class apply for all subclasses.
 * Hierarchy of plots: inzplot -> {hexbin, histogram, dotscatter, bar}
 *                     inzmap -> {region, centroid, sparklines}
 * Additional classes/components: boxplot, legend, table, sparkplot
 * @param {Object} chart - chart.json from R (which includes all data, and
 * other params for identifying the chart originally drawn in R)
 * The relevant plot class is called in the onload function (scroll to the bottom of
 * this script, or search for 'ON LOAD FUNCTION').
 * NOTE: If it so happens that iNZightPlots gets rewritten/gridSVG no longer gets used,
 * it's best to rewrite everything.
 * Naming of grid objects is very important especially when exporting with gridSVG
 * for targeting certain elements and groups of elements. */
class Inzplot {

  constructor(chart) {
    // set svg so that there's variable width and height based on div
    this.svg = d3.select("svg")
                 .attr("width", null)
                 .attr("height", null);
    this.chart = chart;
    this.chartType = chart.type[0];
    this.data = chart.data;
    this.count = this.data.length;
    this.Grob = this.getGrob(this.chartType);
    this.levelNo = (chart.levNames) ? chart.levNames.length : 1;
    this.panel = document.getElementById(this.Grob);
  }

  // create a tooltip for the plot
  setTooltip(width, height = null) {
    var tooltip = d3.select("body").append("div")
                    .attr("class", "tooltip")
                    .attr("id", "tooltip")
                    .style("width", width)
                    .style("height", height);
    this.tooltip = tooltip;
    return tooltip;
  }

  /** Identifying the 'Grob' in the chart
   * (where do all our bars/dots/hexagons lie?)
   * @param {String} chartType - the type of chart identified
   * @return Grob identifier */
  getGrob(chartType) {
    var polygon = document.getElementsByTagName('polygon'),
        p, Grob;
    switch(chartType) {
      case "hist":
      p = polygon[2];
      break;

      case "bar":
      p = polygon[polygon.length - 1];
      break;

      case "dot":
      Grob = 'g[id^="inz-DOTPOINTS."]';
      break;

      case "scatter":
      // test if there's a raster map (originally GRID.rastergrob)
      // OR: you could change the grid name in iNZightMaps package to inz-SCATTERPOINTS...
       Grob = (document.querySelectorAll('g[id^="background.map"]').length > 0) ?
              'g[id^="SCATTERPOINTS."]' : 'g[id^="inz-SCATTERPOINTS."]';
      break;

      default:
      // for hexbins, bp-stacked
      p = polygon[0];
      break;
    }

    if (p) {
      var id = p.getAttribute('id');
      Grob = id.substring(0, id.lastIndexOf('.'));
    }

    return Grob;
  }

  // Functions for enabling brushing over elements (selection box)
  // `brushmove` differs for each plot (depending on what is being
  // accumulated) - see respective subclasses to change this

  /** Enable brushing on plot
   * @param node - an element to target and add brush to
   * @param {Number} selector - where exactly do you want to insert brush
   * @param {Boolean} text - if an additional paragraph should be added
   * This is generally true for histograms/hexbins to show accumulated data when
   * brushing, otherwise false. */
  enableBrush(node, selector, direction = "xy", text = false) {
    if (text) {
      d3.select('#control').append("p")
                   .attr('class', 'brush-info');
    }

    // append brush:
    var svg = d3.select('svg');
    var brush = (direction === "x") ? d3.brushX() : d3.brush();

    brush.on("start", this.brushstart)
         .on("brush", this.brushmove)
         .on("end", this.brushend);

    d3.select(node)
      .insert("g", "g:nth-child(" + selector + ")")
      .attr("class", "brush")
      .call(brush);

    // make handles invisible:
    d3.selectAll('.handle')
      .style('opacity', 0);
    d3.select('.overlay')
      .attr("width", "100%")
      .attr("height", "100%")
      .style('stroke', 'none');
  }

  brushend() {
    if (!d3.event.selection) {
      d3.select(".selection")
        .style("display", "none");
    }
  }

  // When user begins brushing
  brushstart() {
    d3.select(".selection")
      .style("display", null);
  }

}

/** TABLE CLASS
 * Includes setup with the DataTables library
 * Any additional functions related to the table can be added here */
class Table {

  constructor(chartType) {
    this.chartType = chartType;
  }

  /** Initializing table using DataTables
   * To apply styles and formats onto table from the DataTables library
   * @param {string} chartType - type of chart shown
   * For bar plots, rows and columns ordering should be disabled (to link
   * cells properly to bars + additional rows added)
   * For all other plots, there's an ID column added where each row represents
   * an element (a bar, a point, a region, a hexagon, e.t.c). */
  setDT(chartType) {
    var table;

    if (chartType === "bar" || chartType === "bp-stacked") {
      table = $('#table').DataTable({
          "colReorder": true,
          //disable ordering and sorting
          "ordering": false,
          "aaSorting": []
      });
    } else {
      table = $('#table').DataTable({
          "colReorder": true,
          "columnDefs": [
            { //hide rowID column
              "targets": [0],
              "visible": false
            }
          ]
        });
    }
    this.DT = table;
  }

  // This is used to drive the table button
  // For bar plots, it will hide percent/count conversion buttons
  showTable() {
    $('#table_wrapper').toggleClass('hidden');
    $('.Percentage').toggleClass('hidden');
    $('.Count').toggleClass('hidden');
  }

  // Table to plot linking:
  tableToPlot(table, panel, element) {
    $('#table tbody').on('click', 'tr', function() {
      $(this).toggleClass('active');
          let ind = table.rows('.active')[0];
          let selector = "." + element;
          d3.select(panel).selectAll(selector)
            .attr("class", function(d, i) {
                return (ind.includes(i) ? element + " selected" : element + " none");
        });
      //clear brush
      d3.selectAll('.selection')
        .style("display", "none");
    });
  }

  // This is used to widen columns when there are more than 3
  // columns in the table
  widenColumns() {
    // make tables not go wide when there's only 1 column
    if (chart.type !== "bar") {
      var ncol = $("#table thead tr th").length;
      if (ncol < 3) {
        $('#table').css('width', '50%');
      } else if (ncol <= 4) {
        $('#table').css('width', '75%');
      } else {
        $('#table').css('width', '100%');
      }
    } else {
      $('#table').css('width', '100%');
    }
  }

  // initialize table
  init() {
    this.setDT(this.chartType);
    this.widenColumns();
    // attach event handlers:
    d3.select("#viewTable")
      .on("click", this.showTable);
  }

}


/** BOX PLOT CLASS
 * For setting up interactivity and labels for box plots
 * In cases where there are box plots present on the plot
 * (specifically for dot plots and histograms), this will attach interactivity
 * and label parts of the box plot when the user hovers/clicks on the box plot
 * @param {Object} chart - chart information from R
 * (ie factor levels, generally defaults to 1) */
class Boxplot {

  constructor(chart) {
    this.levelNo = (chart.levNames) ? chart.levNames.length : 1;
    this.boxData = chart.boxData;
    this.polygonBox = document.querySelectorAll('g[id^="inz-box."]');
    this.boxLines = document.querySelectorAll('g[id^="inz-box-line."]');
  }

  /** Main function to process and add labels, along with interactions for the
   * box plot
   * @param {Number} levelNo - number of levels/factors in the plot
   */
  boxMe(levelNo) {
    var polygonBoxes = this.polygonBox;
    var boxLines = this.boxLines;

    for (let i = 1; i <= levelNo; i++) { //separate boxes for each plot
      var polygonBox = polygonBoxes[i-1].children;
      var boxLine = boxLines[i-1].children;
      for (let j = 0; j < 2; j++) {
        polygonBox[j].setAttribute('class', 'box-' + i);
        boxLine[j].setAttribute('class', 'box-' + i);
      }
      this.boxLabelSet(i, 0, 1, 0,'LQ');
      this.boxLabelSet(i, 1, 2, 2, 'UQ');
      this.boxLabelSet(i, 1, 0, 1, 'Median');
      this.boxLabelSet(i, 1, 0, 3, 'Min');
      this.boxLabelSet(i, 2, 1, 4, 'Max');
    }

    //Box Plot interactions:
    for (let j = 1; j <= levelNo; j++) {
    var box = document.getElementsByClassName('box-' + j);
    for (let i = 0; i < box.length; i++) {
      box[i].setAttribute('onmouseover', 'fillBox(' + j + ')');
      box[i].setAttribute('onmouseout', 'normalBox(' + j + ')');
      box[i].setAttribute('onclick', 'showBox(' + j + ')');
      }
    }

  }

  /** Create box labels (min, max, mean, lower quartile, upper quartile)
   * This is used to append text onto the SVG
   * @param {Number} i - designated number that indicates what label it is
   * @param {String} textinput - the label
   * @param panel - where to append these svg labels to (generally this.panel
   * is already identified)
   * @param {Number} x - x-coordinate of where label is positioned
   * @param {Number} y - y-coordinate of where label is positioned
   * @return Appends labels onto SVG in correct positions
   */
  boxLabel(i, textinput, panel, x, y) {
  var boxLabel = document.createElementNS("http://www.w3.org/2000/svg", "text");
    boxLabel.setAttribute('class', 'label boxData-' + i + ' hidden');
    boxLabel.setAttributeNS(null, 'transform', 'translate(' + Number(x) + ',' + (Number(y) + 2) + ') scale(1, -1)');
    boxLabel.setAttributeNS(null, 'id', textinput);

    var textNode = document.createTextNode(textinput);
    boxLabel.appendChild(textNode);
    panel.appendChild(boxLabel);
  }

  /** Sets all labels relative to the box plot and its information
   * This function sets all the labels for the box plot at once.
   * @param {Number} i
   * @param {Number} p
   * @param {Number} r
   * @param {Number} q
   * @param {String} textinput - Label (either: Min, Max, Median, LQ, UQ)
   */
  boxLabelSet(i, p, r, q, textinput) {

    if (textinput == "Min" ||  textinput == "Max") {
      var line = document.getElementById('inz-box-line.1.1.' + i  + '.1.' + p); //i is levelNo.
      // p will either be 1 or 2 -> 1 = minLine, 2 = maxLine
      line.setAttribute('class', 'box-' + i);
      var boxPoints = line.getAttribute('points').split(" ")[r].split(",");
      var panel = line.parentNode;
    } else {
      var box = document.getElementsByClassName('box-' + i)[p];
      // boxplot split into two boxes - lowerbox (p = 0) and upperbox (p = 1)
      var boxPoints = box.getAttribute('points').split(" ")[r].split(",");
      var panel = box.parentNode;
    }

    var x = boxPoints[0], y = boxPoints[1], text;

    if (textinput == "Median") {
      // move median label below the box plot
     y = boxPoints[1] - 11;
    }
    if (this.levelNo === 1) {
      text = textinput + ": " + this.boxData[q].quantiles;
    } else {
      text = textinput + ": " + this.boxData[i-1][q].quantiles;
    }
    // this is associated with the boxData imported from R.
    //q = 0 (LQ), 1 (UQ), 2 (Median), 3 (Min), 4 (Max)
    this.boxLabel(i, text, panel, x, y);
  }

  init() {
    this.boxMe(this.levelNo);
  }

}

// functions for box plot event handling:
fillBox = function(j) {
  d3.selectAll('.box-' + j)
    .classed('fillBox', true);
};

normalBox = function(j) {
  d3.selectAll('.box-' + j)
    .classed('fillBox', false);
  };

showBox = function(j) {
  var boxData = d3.selectAll('.boxData-' + j);
    boxData.classed('hidden', !boxData.classed('hidden'));
}

class MeanIndicator {
  constructor(props) {
    console.log(props);
    this.mean = props.meanData;
    this.levelNo = (chart.levNames) ? chart.levNames.length : 1;
    // this.boxData = chart.boxData;
    this.meanPoint = document.querySelectorAll('g[id^="inz-mean."]');
  }
  
  init() {
    for (let i = 1; i <= this.levelNo; i++) {
      this.addMouseOver(i - 1);
    }
  }
  
  addMouseOver(i) {
    var x = this.meanPoint[i].getElementsByTagName("use")[0].getAttribute('x');
    var y = this.meanPoint[i].getElementsByTagName("use")[0].getAttribute('y');
    var transf = this.meanPoint[i].getElementsByTagName("use")[0].getAttribute('transform').toString();
    
    var d = transf.slice(transf.indexOf('translate(') + 10, -1).split(',');
    
    
    var meanLabel = document.createElementNS("http://www.w3.org/2000/svg", "text");
    // boxLabel.setAttribute('class', 'label boxData-' + i + ' hidden');
    // meanLabel.setAttributeNS(null, 'x', x);
    // meanLabel.setAttributeNS(null, 'y', y);
    meanLabel.setAttributeNS(null, 'transform', 'translate(' + Number(x) + ', ' + (Number(y) - Number(d[1])) + ') scale(1, -1)');
    meanLabel.setAttributeNS(null, 'dominant-baseline', 'baseline');
    // meanLabel.setAttributeNS(null, 'text-anchor', 'start');
    meanLabel.setAttribute('class', 'label hidden group-mean-label');
    meanLabel.setAttribute('id', 'group-mean-' + i);
    // boxLabel.setAttributeNS(null, 'id', textinput);
    
    var textNode = document.createTextNode(this.mean[i]);
    meanLabel.appendChild(textNode);
    this.meanPoint[i].appendChild(meanLabel);
    
    this.meanPoint[i].setAttribute('onclick', 'showMean(' + i + ')');
    this.meanPoint[i].setAttribute('style', 'cursor: pointer');
    
    return this;
  }
}

showMean = function(i) {
  var boxData = d3.select('#group-mean-' + i);
    boxData.classed('hidden', !boxData.classed('hidden'));
}


/** LEGEND CLASS
 * For attaching legend interactivity
 * Includes trend line interactivity for scatter plots
 * Generally for coloured bar plots, colored dot plots and scatter plots
 * @param props - generally refers to the plot class identified */
class Legend {

  constructor(props) {
    this.legendLayout = document.getElementById('inz-leg-layout.1');
    this.legLineLayout = document.getElementById('inz-leg-lines.1');
    this.levelNo = (props.levelNo) ? props.levelNo : 1;
    this.chartType = props.chartType;
    this.colGroupNo = props.colGroupNo;
    this.varNames = props.chart.varNames;
    this.Grob = props.Grob;
    this.tbl = props.tbl;
    this.trendInfo = props.chart.trendInfo;
  }

  // This is used to enable legend interactivity for dot plots and scatter plots
  enableHandlers() {
    if (!this.legendLayout) return;
    let colGroupNo = this.colGroupNo;

    //grab legend text and respective key:
    let keyText = document.querySelectorAll('tspan[id^="inz-leg-txt-"]');
    let key = document.querySelectorAll('use[id^="inz-leg-pt-"]');
    let Grob = this.Grob;
    let table = this.tbl;
    let varNames = this.varNames;

    function subset(i) {
      let ind = i + 1;
      //get the title variable:
      var titleVar = document.getElementById('inz-leg-title.1.1.tspan.1').textContent;
      var key = document.getElementById('inz-leg-pt-' + ind + '.1.1');
      var keyText = document.getElementById('inz-leg-txt-' + ind + '.1.1.tspan.1').textContent;
      var names = varNames;
      var count = document.getElementsByClassName('point').length;
      var el = document.querySelectorAll(Grob);

      for (let j = 1; j <= count; j++) {
          var point = document.getElementById(el[0].id + '.' + j);
          if (key.getAttribute('fill') == point.getAttribute('stroke')) {
              point.setAttribute('class', 'point selected');
          } else {
              point.setAttribute('class', 'point none');
          }
      }

      // find column index + filter: (add one for hidden column)
      var colInd = names.indexOf(titleVar) + 1;
      table.search('').columns().search('').draw();
      table.columns(colInd).search("^" + keyText + "$", true, false).draw();
    }

    this.addHandlers(subset);
  }

  /** Add interactivity to key and text in legend
   * @param {Function} subset - A function to filter by a certain group when
   * user clicks on the group (e.g. dots + table are filtered)
   * In this case, the subset function is different for dot plots/scatter plots
   * and for bar plots. However, the hovers are practically the same (show/out functions) */
  addHandlers(subset) {
    let keyText = document.querySelectorAll('tspan[id^="inz-leg-txt-"]');
    let key = document.querySelectorAll('use[id^="inz-leg-pt-"]');

    function show(i) {
      let ind = i + 1;
      let circle = document.getElementById('inz-leg-pt-' + ind + '.1.1');
      let text = document.getElementById('inz-leg-txt-' + ind + '.1.1.tspan.1');
      if (circle.getAttribute("fill") !== "none") {
            text.setAttribute('fill', circle.getAttribute('fill'));
        } else {
            text.setAttribute('fill', circle.getAttribute('stroke'));
        }
      circle.setAttribute("class", "show");
      text.setAttribute("class", "show");
    }

    function out(i) {
      let ind = i + 1;
      let keyText = document.getElementById('inz-leg-txt-' + ind + '.1.1.tspan.1');
      let key = document.getElementById('inz-leg-pt-' + ind + '.1.1');
      keyText.setAttribute("class", "out keyText");
      key.setAttribute("class", "show");
    }

    // text handlers:
    d3.selectAll(keyText)
      .on("mouseover", (d, i) => show(i))
      .on("mouseout", (d, i) => out(i))
      .on("click", (d, i) => subset(i));

    // circle handlers:
    d3.selectAll(key)
      .on("mouseover", (d, i) => show(i))
      .on("mouseout", (d, i) => out(i))
      .on("click", (d, i) => subset(i));
  }

  /** This is used to enable trend line interactivity
   * Primarily adds interactivity to trend lines
   * @param {String} t - trend line class ("linear", "quadratic", "cubic")
   * @param {String} g - HTML label generated for the trend line
   * @param ptip - d3 object for tooltip
   * Only found on scatter plots only */
  enableTrends(t, g, ptip) {
    let legLineLayout = this.legLineLayout;

    d3.select(legLineLayout).selectAll("." + t)
    .on('mouseover', function() {
        d3.select(this).style("opacity", 0.5);
    })
    .on('mouseout', function() {
        d3.select(this).style("opacity", 1);
    })
    .on('click', function() {
        d3.selectAll('.trend')
        .classed('hidden', function() {
          return((this.getAttribute('class').includes(t) === true) ? false : true);
      });

      return ptip.classed("hidden", false)
                 .html(g);
        });

    d3.selectAll(".trend." + t)
      .on("mouseover", function() {
          d3.select(".tooltip")
            .style("left", d3.event.pageX - 50 + "px")
            .style("top", d3.event.pageY - 50 + "px")
            .style("visibility", "visible")
            .html(g);
          })
      .on("mouseout", function() {
          d3.select(".tooltip")
            .style("visibility", "hidden");
          })
  }

  /** Get the equation of the trend line and create tooltip labels
   * @param t - type of trend identified ("Linear", "Quadratic", "Cubic")
   * @return returns HTML for tooltip label
   * trendInfo is stored in chart information */
  getEquation(t) {
    let trendInfo = this.trendInfo;
    switch(t) {
        case "linear":
        return "Linear fit:" + "<span> y = " + trendInfo[t][0] + " " +
               (trendInfo[t][1] > 0 ? ("+" + trendInfo[t][1]) : trendInfo[t][1])
               + "x " + "</span> <br />" + "R" + "<sup>2</sup> : " + "<span>"
               + trendInfo[t][2] + "</span>";
        break;

        case "quadratic":
        return "Quadratic fit:" + "<span> y = " + trendInfo[t][0] + " "
               + (trendInfo[t][1] > 0 ? ("+" + trendInfo[t][1]) : trendInfo[t][1])
               + "x " + (trendInfo[t][2] > 0 ? ("+" + trendInfo[t][2]) : trendInfo[t][2])
               + "x<sup>2</sup>" + "</span>" + "<br />"+ "R" + "<sup>2</sup> : " + "<span>"
               + trendInfo["rank.cor"][0] + "</span>";
        break;

        case "cubic":
        return "Cubic fit: <span> y = " + trendInfo[t][0] + " " +
               (trendInfo[t][1] > 0 ? ("+" + trendInfo[t][1]) : trendInfo[t][1])
               + "x " + (trendInfo[t][2] > 0 ? ("+" + trendInfo[t][2]) : trendInfo[t][2])
               + "x<sup>2</sup> " + (trendInfo[t][3] > 0 ? ("+" + trendInfo[t][3]) : trendInfo[t][3])
               + "x<sup>3</sup> </span>" + "<br />" +  "R" + "<sup>2</sup> : <span>"
               + trendInfo["rank.cor"][0] + "</span>";
        break;

        default:
        return "Invalid trend!";
    }
  }

  /** For identifying if there is a separate trend line legend present
   * Main function for enabling interactivity for trend lines */
  trendLegend() {
    // if there is no legend, return:
    if (!this.legLineLayout) return;

    let trendInfo = this.trendInfo;
    var lines = d3.select(this.legLineLayout).selectAll('polyline')
                  .attr('class', function() {
                    var id = this.id;
                    var type = id.substring(id.lastIndexOf("-") + 1, id.indexOf("."));
                    return type;
                  });
    var keys = d3.select(this.legLineLayout).selectAll('tspan')
                 .attr('class', function() { return this.innerHTML; });

    //insert text into the control panel:
    var ptip = d3.select('#control')
             .append("p")
             .attr('class', 'trend-info')
             .classed("hidden", true);

   var ll = this.legLineLayout.querySelectorAll('tspan');

   //loop over lines
   for (var i = 0; i < ll.length; i++) {
       var t = ll[i].innerHTML;
       var line = document.getElementById('inz-trend-' + t + '.1.1.1.1');
       line.setAttribute('class', t + ' trend');

       var g = this.getEquation(t);
       this.enableTrends(t, g, ptip);
   }
  }

  // To initialize legend interactivity (both trendlines and groups)
  init() {
    // enable handlers on legend
    this.enableHandlers();
    //enable trend legend handlers:
    this.trendLegend();
  }

}

/** =================================================
  ON LOAD FUNCTION
  Facilitates all toggles on navigation bar and
  initializes appropriate plot
 ================================================== **/

$(function() {
  $(".view-tbl").click(function() {
    $(".tbl-div").removeClass("expand").toggleClass("moveRight");
    $(".svg-div").removeClass("moveLeft").toggleClass("mid");
    // for bars:
    $(".Percentage").toggleClass("hidden");
    $(".Count").toggleClass("hidden");
  });

  $(".plot").click(function() {
    $(".svg-div").removeClass("mid").toggleClass("moveLeft");
    $(".tbl-div").removeClass("moveRight").toggleClass("expand");
  });

  $(".help").click(function() {
    $(".help-overlay").toggleClass("move");
  });

  $(".help-overlay").click(function() {
    $(".help-overlay").toggleClass("move");
  });

  // load chart:
  var plot;

  switch(chart.type[0]) {
    //for inzplots:
    case "hex":
    plot = new Hexbin(chart);
    break;

    case "hist":
    plot = new Histogram(chart);
    break;

    case "dot":
    case "scatter":
    plot = new DotScatter(chart);
    break;

    case "bar":
    plot = new UnstackedBar(chart);
    break;

    case "bp-stacked":
    plot = new StackedBar(chart);
    break;

    //for inzmaps:
    case "region":
    plot = new RegionMap(chart);
    break;

    case "point":
    plot = new CentroidMap(chart);
    break;

    case "sparklines":
    plot = new SparkMap(chart);
    break;

    default:
    return;
  }

  plot.init();
  console.log("Load complete!");

});
