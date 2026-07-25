/** CLASSES FOR BAR PLOTS
 * Generic class for all bar plots
 * Split into subclasses: stacked (type = "bp-stacked"), unstacked (type = "bar")
 * @param props - parameters from Inzplot (chart)
 */
class Barplot extends Inzplot {

  constructor(props) {
    super(props);
    this.plotRegion = document.querySelectorAll('rect[id^="inz-plot-bg"]')[0];
    this.reset = this.reset.bind(this);

    // parameters for bar charts
    this.colorMatch = this.chart.colorMatch;
    this.colCounts = this.chart.colCounts;
    this.group = this.chart.group[0];

    //bind event handlers
    this.changeToPercent = this.changeToPercent.bind(this);
    this.changeToCount = this.changeToCount.bind(this);
    this.reset = this.reset.bind(this);
  }

  // this is used before initializing DataTables
  // for labelling columns and rows for conversion/highlighting
  setTable() {
    let table = document.getElementById("table");
    let tableAttr = {
      table: table,
      tr: table.getElementsByTagName("tr"),
      ncol: table.getElementsByTagName("tr")[0].childElementCount, //no. of columns
      td: table.getElementsByTagName('td'),
      cellNo: table.getElementsByTagName("td").length, //cellNo
      nrow: table.rows.length, // no. of rows
      colorMatch: this.colorMatch,
      colCounts: this.colCounts,
      number: this.data.length,
      count: this.data.length + 1
    };
    this.tableAttr = tableAttr;
    return tableAttr;
  }

  //inserting x-header
  insertXHeader() {
    let table = this.tableAttr.table;
    let ncol = this.tableAttr.ncol;
    var xrow = table.insertRow(0);
    var xhead = xrow.insertCell(0);
    xhead.setAttribute("class", "headings");
    xhead.innerHTML = document.getElementsByTagName("tspan")[2].innerHTML;
    xhead.colSpan = ncol;
    return xhead;
  }

  // inserting y-header
  insertYHeader() {
    var yHeading = document.getElementsByTagName('th')[0];
    yHeading.innerHTML = document.getElementsByTagName('tspan')[3].innerHTML;
    yHeading.setAttribute('class',' headings');
    return yHeading;
  }

  // screening through selecting which bars to be interactive
  // R currently draws bars on top of each other
  screenBars(colorMatch = this.colorMatch) {
    var Grob = this.Grob;
    var type = this.chartType;
    var data = this.data;

    if (colorMatch[0] === null) {
      var bars = d3.select(this.panel).selectAll('polygon');
      return bars;
    }

    //if the bar plot is colored, it has additional bars and polylines to hide
    var p = document.getElementsByTagName('polygon');
    // if stacked:
    if (type === "bp-stacked") {
      d3.selectAll(p)
        .attr("class", function(d, i) {
          return (this.id.indexOf(Grob) >= 0) ? "bar" : "hidden";
        });
    } else {
      d3.selectAll(p)
        .attr("id", function(d, i) {
          if (colorMatch[i] === 1) return Grob + "." + (i/(data.length - 1));
        })
        .attr("class", function(d, i) {
          return (colorMatch[i] === 1) ? "bar" : "hidden";
        });
    }

    //Hiding polylines:
    var barLines = document.getElementById('inz-bar-line.1.1.1');
    d3.select(barLines).selectAll('polyline')
      .classed("hidden", true);
    var bars = d3.select('svg').selectAll('.bar');

    return bars;
  }

  // to enable tooltips
  makeTooltips(bars, data = this.data) {
    let count = this.count;
    let table = this.tbl;
    let tooltip = this.tooltip;
    let type = this.chartType;

    bars.data(data)
        .attr('class', 'bar')
        .on('mouseover', function(d) {
              var el = d3.select(this),
                  coords = el.attr('points'),
                  small = coords.split(" ")[0],
                  sx = Number(small.split(",")[0]),
                  coordsxy = coords.split(" ")[2],
                  x = Number(coordsxy.split(",")[0]),
                  xx = x + (sx-x)/2 - 50,
                  y = Number(coordsxy.split(",")[1]) + 60,
                  //translate to html co-ordinates
                  tm = this.getScreenCTM().translate(+ xx, + y);

              tooltip.style('visibility', 'visible')
                     .style("left", (window.pageXOffset + tm.e)  + "px")
                     .style("top", (window.pageYOffset +tm.f)  + "px");

              if (data[0].var1 !== undefined) { // for 2-way tables
                tooltip.html("<span>" + d.var1 + ' ' + d.var2+ "<br/> " + d.pct +
                "%</span>" + "<br/>" + d.counts);
              } else if (data[0].Var1 !== undefined) { //for stacked
                tooltip.html("<span>" + d.Var1 + ' ' + d.Var2 + "<br> " + d.pct +
                "%</span>" + "<br>" +
                d.counts);
              } else {
                tooltip.html("<span>" + d.varx + "<br/> " + d.pct +
                "%</span>" + "<br/>" + d.counts);
              }
          })
        .on('mouseout', function(){ tooltip.style("visibility", "hidden");})
        .on('click', function(d, i) {
          //clear rows if active:
          $(".tabSelect").removeClass("active tabSelect");
          var selected = this;
          if (type === "bp-stacked") {

            for (let j = 1; j <= count; j++) {
              if (j === i + 1)
                $(table.cell(".td" + j).nodes()).addClass("active tabSelect");
            }

          } else {

            d3.selectAll('.bar')
              .attr("class", function(d, i) {
                if (this == selected) {
                  if (data[0].var1 !== undefined) {
                    $(table.cell("#td" + (i + 1)).nodes()).addClass("active tabSelect");
                  } else {
                    $(table.column(i + 1).nodes()).addClass('active tabSelect');
                  }
                  return ("bar selected");
                } else {
                  return ("bar none");
                }
              });

          }
        });
  }

  // for creating buttons in "controls"
  createButton(name, func) {
    let button = d3.select("#control").append("button")
                   .attr("type", "button")
                   .attr("class", "btn btn-primary " + name)
                   .attr("id", "Button" + name)
                   .html("Show " + name)
                   .on("click", func);
  }

  findCounts(data, tableAttr) {
    let cellNo = tableAttr.cellNo;
    let ncol = tableAttr.ncol;
    let nrow = tableAttr.nrow;
    let table = tableAttr.table;
    let colCounts = this.colCounts;
    let countsTab = [];
    let type = this.chartType;

    if (data[0].var1 !== undefined || data[0].Var1 !== undefined) {
      //Finding the sum of countsTab:
      if (type === "bar") {
        for (let i = 1; i <= cellNo / ncol; i++) {
            var tc = document.getElementById('tc' + i);
            countsTab[i-1] = Number(tc.innerHTML);
        }
      } else {
        for (let j = 1; j < ncol; j++) {
            var totalCounts = document.getElementById('counts' + j); //counts for each group
            countsTab[j-1] = Number(totalCounts.innerHTML);
          }
        }

      //global sum variable
      let sum = countsTab.reduce(function(a, b) { return a + b; }, 0);

      this.sum = sum;
      this.countsTab = countsTab;

      //y heading:
      this.insertYHeader();

      if (type === "bar") {
        //Summation row  - should move this to R?
        var lastRow = table.insertRow(nrow); //nrow + 1
        lastRow.setAttribute('class', 'totalRow');

        for (let i = 1; i <= ncol; i ++) {
          var cell = lastRow.insertCell(i-1);
          cell.id = "totalCell" + (i-1);
          cell.setAttribute("class", "totalCell");
          //fill in column totals:
          cell.innerHTML = colCounts[i-1];
        }

        var sumCell = document.getElementById('totalCell' + (ncol-1));
        sumCell.innerHTML = "N = " + sum;
        var totalCell = document.getElementById('totalCell' + (ncol-2));

        this.tableAttr.sumCell = sumCell;
        this.tableAttr.totalCell = totalCell;

      }
      //insert buttons and conversion functions:
      this.createButton("Percentage", this.changeToPercent);
      this.createButton("Count", this.changeToCount);
    }
  }

  reset() {
    d3.selectAll('.bar')
      .attr("class", "bar");
    // reset tables:
    $(".tabSelect").removeClass("active tabSelect");
  }

  init() {
    this.labelRows();
    this.findCounts(this.data, this.tableAttr);

    // now that's done, reformat table:
    let table = new Table(this.chartType);
    table.init();
    this.tbl = table.DT;

    // set up bars and add tooltips:
    let bars = this.screenBars(this.colorMatch);
    let tooltip = this.setTooltip();
    // additional settings for tooltip
    tooltip.style('min-height', '40px')
           .style('visibility', 'hidden');
    this.makeTooltips(bars, this.data);
    // if it's coloured (but not stacked!), enable interactive legend
    if (!this.colorMatch[0]) {
      this.intLegend();
    }

    // reset:
    d3.select("#reset")
      .on("click", this.reset);
    this.plotRegion.addEventListener("click", this.reset, false);
  }

}

/** CLASS FOR UNSTACKED BAR PLOTS
 * This includes 2 way bar plots + 1 way bar plots
 * Coloured and non-coloured!
 */
class UnstackedBar extends Barplot {

  constructor(props) {
    super(props);
  }

  labelRows() {
    let tableAttr = this.setTable();
    let cellNo = tableAttr.cellNo;
    let ncol = tableAttr.ncol;
    let nrow = tableAttr.nrow;
    let td = tableAttr.td;

    //defining table elements
    d3.selectAll('tr')
      .attr('id', function(d, i) { return ('tr' + i) });

    //finding no. of rows, and labelling
    for (let i = 1; i <= cellNo; i++) {
        if (i % ncol === 0) {
            td[i - 1].setAttribute('id', 'tc' + i / ncol);
            td[i - 1].setAttribute('class', 'tc');
        } else if (i % ncol === 1) {
            td[i - 1].setAttribute('id', 'yGroup' + ((i - 1) / ncol + 1));
            td[i - 1].setAttribute('class', 'yGroup');
        } else if (this.data[0].var1 !== undefined) {
            td[i - 1].setAttribute('id', 'td' + (((i - (i % ncol)) / ncol + 1) + ((nrow - 1) * (i % ncol - 2))));
        } else if (i < ncol) {
            td[i - 1].setAttribute('class', 'td' + (i - 1));
        } else {
            td[i - 1].setAttribute('class', 'td' + ((i - 2) % ncol + 1));
        }
      }
  }

  changeToPercent() {

    d3.select(".Count").classed('dark', false);
    d3.select('.Percentage').classed('dark', true);

    let tableAttr = this.tableAttr;
    let cellNo = tableAttr.cellNo;
    let ncol = tableAttr.ncol;
    let nrow = tableAttr.nrow;
    let sumCell = tableAttr.sumCell;
    let totalCell = tableAttr.totalCell;
    let sum = this.sum;
    let countsTab = this.countsTab;

    //for the column sums:
    for (let i = 1; i < ncol-2; i++) {
      var tCol = document.getElementById('totalCell' + i);
      if ((tCol.innerHTML.indexOf('%') == -1) && (tCol.innerHTML.indexOf(".") >=0)) {
        tCol.innerHTML = (Number(tCol.innerHTML)*100).toFixed(2) + '%';
      } else if (tCol.innerHTML.indexOf('%') >= 0) {
        tCol.innerHTML = tCol.innerHTML;
      } else {
        tCol.innerHTML =(Number(tCol.innerHTML)/sum * 100).toFixed(2) + '%';
      }
    }

    //for all other data:
    for (let j = 1; j <= cellNo/ncol; j++) {
      var tr = document.getElementById('tr' + j);
      for (let i = 1; i <= cellNo - 2*(nrow-1); i ++) {
        var td = document.getElementById('td' + i);
        if ((td.innerHTML.indexOf(".") >= 0) && (td.innerHTML.indexOf('%') == -1)) {
          td.innerHTML = (td.innerHTML*100).toFixed(2) + "%";
        } else if (td.innerHTML.indexOf('%') >= 0) {
          td.innerHTML = td.innerHTML;
        } else {
          if(tr.contains(document.getElementById('tc' + j)) && tr.contains(document.getElementById('td' + i))) {
            td.innerHTML = (Number(td.innerHTML)/countsTab[j-1] * 100).toFixed(2) + '%';
            var tc = document.getElementById('tc' + j);
            tc.innerHTML = countsTab[j-1];
          }
        }
      }
    }

    document.getElementsByTagName('th')[(ncol-1)].innerHTML = "Row N";
    totalCell.innerHTML = "100.00%";
    sumCell.innerHTML = "N = " + sum;
  }

  changeToCount() {

    d3.select(".Count").classed('dark', true);
    d3.select('.Percentage').classed('dark', false);

    let tableAttr = this.tableAttr;
    let cellNo = tableAttr.cellNo;
    let ncol = tableAttr.ncol;
    let nrow = tableAttr.nrow;
    let sum = this.sum;
    let countsTab = this.countsTab;
    let sumCell = tableAttr.sumCell;
    let totalCell = tableAttr.totalCell;

    //for the column sums:
    for (let i = 1; i < ncol-2; i++) {
      var tCol = document.getElementById('totalCell' + i);
      if ((tCol.innerHTML.indexOf('%') == -1) && (tCol.innerHTML.indexOf(".") >=0)) {
        tCol.innerHTML = Math.round(Number(tCol.innerHTML)*sum);
      } else if (tCol.innerHTML.indexOf('%') >= 0) {
        tCol.innerHTML = Math.round(Number(tCol.innerHTML.substring(0,tCol.innerHTML.lastIndexOf('%')))/100 * sum);
      } else {
        tCol.innerHTML = tCol.innerHTML;
      }
    }

  //for all other data:
  for (let j = 1; j <= cellNo/ncol; j ++) {
    var tr = document.getElementById('tr' + j);
    for(let i = 1; i <= cellNo - 2*(nrow-1); i++) {
  if(tr.contains(document.getElementById('tc' + j)) && tr.contains(document.getElementById('td' + i))) {
        var td = document.getElementById('td' + i);
        var tc = document.getElementById('tc' + j);
        if (td.innerHTML.indexOf('%') >= 0){
        td.innerHTML = Math.round(Number(td.innerHTML.substring(0,td.innerHTML.lastIndexOf('%')))/100 * countsTab[j-1]);
        tc.innerHTML = (countsTab[j-1]/sum*100).toFixed(2) + "%";
      } else if ((td.innerHTML.indexOf(".") >= 0) && (td.innerHTML.indexOf('%') == -1)){
        td.innerHTML = Math.round(Number(td.innerHTML) * countsTab[j-1]);
        tc.innerHTML = (countsTab[j-1]/sum*100).toFixed(2) + "%";
      } else {
        td.innerHTML = td.innerHTML;
        tc.innerHTML = tc.innerHTML;
      }
        }
      }
    }
    document.getElementsByTagName('th')[(ncol-1)].innerHTML = "Row N %";
    totalCell.innerHTML = "N = " + sum;
    sumCell.innerHTML = "100.00%";
  }

  //For initializing legend interactions
  // one way colored plots and two-way bar plots only
  intLegend() {
    var legendLayout = document.getElementById('inz-leg-layout.1');
    if (!legendLayout) return;

    var data = this.data,
        count = this.count + 1,
        table = this.tbl,
        nrow = this.tableAttr.nrow,
        // groups in 2nd variable (for 2 way tables)
        group = (data[0].var1 !== undefined) ? this.group + 1 : count;

    function subset(i) {
      let j = i + 1;
      $(".tabSelect").removeClass("active tabSelect");
      d3.selectAll('.bar')
        .attr("class", function(d, i) {
          let ind = i + 1;
          if ((ind == j || (ind % (group - 1)) == j || (ind % (group - 1)) == 0 && j == (group - 1))) {
            if(data[0].var1 !== undefined) { // for 2-way tables
              $(table.row((ind - 1) % (nrow - 1)).nodes()).addClass('active tabSelect');
            } else {
              $(table.column(j).nodes()).addClass('active tabSelect');
            }
            return("bar selected");
          } else {
            $(table.row((ind - 1) % (nrow - 1)).nodes()).removeClass('active tabSelect');
            return("bar none");
          }
      });
    }

    // set up legend interaction
    let legend = new Legend(this);
    legend.addHandlers(subset);
  }

}

/** CLASS FOR STACKED BAR PLOTS
 * The reason for having separate classes is because the nature of stacking
 * is reversed in R. (i.e. bars are created from right to left, rather than
 * from left to right...)
 */
class StackedBar extends Barplot {

  constructor(props) {
    super(props);
  }

  labelRows() {

    let tableAttr = this.setTable();
    let cellNo = tableAttr.cellNo;
    let ncol = tableAttr.ncol;
    let nrow = tableAttr.nrow;
    let tr = tableAttr.tr;
    let td = tableAttr.td;

      // labelling rows:
      for (let i = 1; i < nrow; i ++) {
        // last two rows are the rows containing totals and counts
        if(i === nrow - 2) {
          tr[i].setAttribute('class', 'tc');
        } else if (i === nrow - 1) {
          tr[i].setAttribute('class', 'countRow');
        } else {
          tr[i].setAttribute('class', 'row' + i);
        }
      };

      //finding no. of rows, and labelling
      for (let j = 1; j < ncol; j++) {
        for (let i = 1; i <= cellNo; i ++) {
          if (i % ncol === 1){
            td[i-1].setAttribute('id', 'yGroup' + ((i-1)/ncol + 1));
            td[i-1].setAttribute('class', 'yGroup');
          } else if (document.getElementsByClassName('countRow')[0].contains(td[i-1])){
            td[i-1].setAttribute('id', 'counts' + ((i-1)%ncol));
            td[i-1].innerHTML = Number(td[i-1].innerHTML);
          } else if (document.getElementsByClassName('tc')[0].contains(td[i-1])) {
            td[i-1].setAttribute('id', 'tc' +((i-1)%ncol));
            td[i-1].setAttribute('class', 'tc');
          } else {
            td[i-1].setAttribute('id',  'td' + i);
            td[i-1].setAttribute('class', 'td' + chart.order[i-Math.ceil(i/ncol)-1]);
          }
        }
      };
  }

  changeToPercent() {

    d3.select(".Count").classed('dark', false);
    d3.select('.Percentage').classed('dark', true);

    let tableAttr = this.tableAttr;
    let cellNo = tableAttr.cellNo;
    let ncol = tableAttr.ncol;
    let nrow = tableAttr.nrow;
    let sum = this.sum;
    let countsTab = this.countsTab;

    for (var i = 1; i <= cellNo; i++) {
      var td = document.getElementById('td' + i);
      var total = document.getElementById('tc' + ((i-1) % ncol));
      var countsCol = document.getElementById('counts' + ((i-1) % ncol));
    if (td !== null) {
      if ((td.innerHTML.indexOf(".") >= 0) && (td.innerHTML.indexOf('%') == -1)) { // change proportion to percentages
        td.innerHTML = (td.innerHTML*100).toFixed(2) + "%";
        total.innerHTML = "100.00%"; // this is a bit iffy. Fixed at 100.
        countsCol.innerHTML = countsCol.innerHTML;
      } else if (td.innerHTML.indexOf('%') >= 0) { // remain as percentages
        td.innerHTML = td.innerHTML;
        total.innerHTML = total.innerHTML;
        countsCol.innerHTML = countsCol.innerHTML;
      } else { // change counts to percentages
        td.innerHTML = (Number(td.innerHTML)/countsTab[((i-1)%ncol)-1]*100).toFixed(2) + "%";
        total.innerHTML = "100.00%";
        countsCol.innerHTML = countsTab[((i-1)%ncol)-1];
      }
        document.getElementById('yGroup' + (nrow - 1)).innerHTML = "Col N"; // change from 'Total %' to 'Col N'
      }
    }

  }

  changeToCount() {

    d3.select(".Count").classed('dark', true);
    d3.select('.Percentage').classed('dark', false);

    let tableAttr = this.tableAttr;
    let cellNo = tableAttr.cellNo;
    let ncol = tableAttr.ncol;
    let nrow = tableAttr.nrow;
    let sum = this.sum;
    let countsTab = this.countsTab;

    for(let i = 1; i <= cellNo; i++) {
        var td = document.getElementById('td' + i);
        var total = document.getElementById('tc' + ((i-1)%ncol)); // by columns
        var countsCol = document.getElementById('counts' + ((i-1)%ncol));
        if (td !== null && total !== undefined) {
        if (td.innerHTML.indexOf('%') >= 0) { // convert percentage to counts
        td.innerHTML = Math.round(Number(td.innerHTML.substring(0,td.innerHTML.lastIndexOf('%')))/100 * countsTab[((i-1)%ncol)-1]);
        countsCol.innerHTML = (countsTab[((i-1)%ncol)-1]/sum*100).toFixed(2) + "%";
        total.innerHTML = Math.round(countsTab[((i-1)%ncol)-1]);
      } else if ((td.innerHTML.indexOf(".") >= 0) && (td.innerHTML.indexOf('%') >= -1)){ //converts proportions to counts
        td.innerHTML = Math.round(Number(td.innerHTML) * countsTab[((i-1)%ncol)-1]);
        countsCol.innerHTML = (countsTab[((i-1)%ncol)-1]/sum*100).toFixed(2) + "%";
        total.innerHTML = Math.round(countsTab[((i-1)%ncol)-1]);
      } else { // remain as counts (if already converted to counts)
        td.innerHTML = td.innerHTML;
        total.innerHTML = total.innerHTML;
      }
    }
  }
    document.getElementById('yGroup' + (nrow - 1)).innerHTML = "Total %";
  }

}
