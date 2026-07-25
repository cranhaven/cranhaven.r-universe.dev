// For histogram plots:
class Histogram extends Inzplot {

  constructor(props) {
    super(props);
    //additional parameters:
    this.n = this.chart.n;
    // handlers:
    this.brushmove = this.brushmove.bind(this);
    this.reset = this.reset.bind(this);
  }

  brushmove() {
    var s = d3.event.selection,
        x1 = s[0],
        x2 = s[1],
    // information to extract:
        n = this.n,
        tab = this.data,
        table = this.tbl,
        groupN = [],
        intRange = [],
        ind = [];

    for (let i = 1; i <= this.count; i++) {
      var bar = document.getElementById(this.Grob + '.' + i),
      //obtain end points of the bar
          coords = bar.getAttribute('points').split(" "),
          bottomEdge = coords[0].split(','),
          topEdge =  coords[3].split(','),
          bx = bottomEdge[0],
          tx = topEdge[0];

      if (bar.getAttribute('visibility') == 'hidden') {
        // those that are hidden, remain hidden
        bar.classList.add('hidden');
      } else {
        //points that lie within the  boundary box drawn:
        if (((x1 <= bx && bx <= x2) && (x1 <= tx && tx <= x2))) {
          bar.setAttribute('class', ' histBar selected');
          // store frequency from selected
          groupN.push(tab[i - 1].counts);
          intRange.push(tab[i - 1].lower, tab[i - 1].upper);
          ind.push("^" + i + "$");
        } else {
          bar.setAttribute('class', 'histBar none');
       }
     }
   }
    //summation of array:
    var sum = groupN.reduce(function(a, b) { return a + b; }, 0);
    // report a proportion + interval number:
    var nProp = (sum / n * 100).toFixed(2) + "%";
    var intervalNo = document.getElementsByClassName('selected').length;

   // filter table
   table.columns(0).search(ind.join("|"), true).draw();

   // update tooltip
   d3.select(".brush-info")
     .classed('hidden', false)
     .html("Interval Range: <span>" + intRange[0] + " - " + intRange[intRange.length-1] +
                         "</span> <br> Frequency: <span>" + sum +  "," + nProp + "</span> <br> No. of intervals: <span>" +
                         intervalNo + "</span>");
  }

  setHovers() {
    let table = this.tbl;
    let tooltip = this.tooltip;
    let panel = this.panel;

    d3.select(panel).selectAll('polygon')
      .attr('class', 'histBar');

    d3.selectAll('.histBar')
      .data(this.data)
      .on('mouseover', function(d){
          tooltip.style('visibility', 'visible')
                 .style("left", d3.event.pageX - 40 + "px")
                 .style("top", d3.event.pageY - 55 + "px")
                 .html("Class range: <br> <span>" + d.lower + " - " + d.upper +
                       " </span>" + "<br> N = <span>" + d.counts + ", " + d.pct
                        + "% </span>");})
      .on('mouseout', function() {
            tooltip.style("visibility", "hidden");
          })
      .on('click', function(d, i) {
            var selected = this;

            d3.select(panel).selectAll('polygon')
              .attr("class", function() {
                return (this === selected ? "histBar selected" : "histBar none");
              });

            // because dataTable searches using columns but not rows
            // add an additional column for row num using regex:
            var ind = "^" + (i+1) + "$";
            table.columns(0).search(ind, true).draw();

            // hide box and brush
            d3.selectAll('.boxData')
              .classed('hidden', true);

            d3.select(".brush-info")
              .classed('hidden', true);

            //remove box if present
            d3.selectAll(".selection")
              .style("display", "none");
        });
  }

  reset() {

    d3.select(this.panel).selectAll('.histBar')
      .attr('class', 'histBar');

    // restore table to original state
    this.tbl.search('').columns().search('').draw();
    this.tbl.rows().nodes().to$().removeClass('active');

    d3.select('.brush-info')
      .classed('hidden', true);

    d3.selectAll('.label')
      .classed('hidden', true);

    //remove box
    d3.selectAll(".selection")
      .style("display", "none");

    //hide boxplot data
    d3.selectAll('.boxData')
      .classed('hidden', true);
  }

  init() {
    // init table:
    var tbl = new Table();
    tbl.init();
    tbl.tableToPlot(tbl.DT, this.panel, "histBar");
    this.tbl = tbl.DT;

    if (this.chart.boxData.length > 0) {
      // init box plot:
      var box = new Boxplot(this.chart);
      box.init();
    }
    
    if (this.chart.meanData.length > 0) {
      var meanInds = new MeanIndicator(this.chart);
      meanInds.init();
    }

    // set tooltips, hovers, selection box:
    this.setTooltip(200, 50);
    this.setHovers();
    this.enableBrush(this.panel.parentNode.parentNode, 1, "x", true);

    //attach:
    d3.select("#reset")
      .on("click", this.reset);

    d3.select(".overlay")
      .on("dblclick", this.reset);
  }

}
