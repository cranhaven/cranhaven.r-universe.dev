/** HEXAGONAL BIN PLOTS
  * Initializes and sets up all interactions for single panel
  * hexagonal bin plot
  * @param props - chart.json from R */
class Hexbin extends Inzplot {

  constructor(props) {
    super(props);
    this.n = this.chart.n;
    // bind events:
    this.brushmove = this.brushmove.bind(this);
    this.reset = this.reset.bind(this);
  }

  // set hover interactions on hexagonal bins:
  setHovers() {
    var tooltip = this.tooltip;
    var panel = this.panel;

    d3.select(panel).selectAll('polygon')
        .data(this.data)
        .attr('class', 'hexbin')
        .on('mouseover', function(d) {
              tooltip.style('visibility', 'visible')
                     .style("left", d3.event.pageX - 40 + "px")
                     .style("top", d3.event.pageY - 55 + "px")
                     .html("N = <span>" + d.counts + ", " + d.pct +
                           "% </span>" + "<br> Centered at: <br> <span>" +
                           d.xcm + ", " + d.ycm + "</span>");})
        .on('mouseout', function() {
              tooltip.style("visibility", "hidden");
            })
        .on('click', function() {
            let selected = this;
            d3.select(panel).selectAll('.hexbin')
              .attr("class", function() {
                return (this === selected) ? "hexbin selected" : "hexbin none";
            })
          d3.select("#tt")
            .style('visibility', 'hidden');
        });
  }

  brushmove() {
    var s = d3.event.selection,
        x1 = s[0][0],
        x2 = s[1][0],
        y1 = s[0][1],
        y2 = s[1][1],
    // information to extract:
        groupN = [],
        n = this.n,
        count = this.count;

    for (let i = 1; i <= count; i++) {
        var hexbin = document.getElementById(this.Grob + '.' + i);
        //find the x value at the center of the hexagon (midpoint):
        var coords = hexbin.getAttribute('points');
        var topEdge = coords.split(' ')[5];
        var bottomEdge = coords.split(' ')[2];
        var x = Number(topEdge.split(",")[0]); //midpoint x
        var y = (Number(topEdge.split(",")[1]) + Number(bottomEdge.split(",")[1]))/2; //midpoint y

        if (hexbin.getAttribute('visibility') === 'hidden') {
           // those that are hidden, remain hidden
           hexbin.classList.add('hidden');
         } else {
          //points that lie within the  boundary box drawn:
          if ((x1 <= x && x <= x2) && (y1 <= y && y <= y2)) {
            hexbin.setAttribute('class', ' hexbin selected');
            // store no. of counts from each hexbin that's selected
            groupN.push(this.data[i-1].counts);
          } else {
             hexbin.setAttribute('class', 'hexbin none');
          }
        }
      }

      //summation of array + get proportion:
      var sum = groupN.reduce(function(a, b) { return a + b; }, 0);
      var nProp = (sum / n * 100).toFixed(2) + "%";
      var nbins = document.getElementsByClassName('selected').length;

    //update para
    d3.select('.brush-info')
      .classed('hidden', false)
      .html("Frequency: <span>" + sum + ", " + nProp +
            "</span>" + "<br> bins: <span>" + nbins + "</span>");
  }

  // clear all selections
  reset() {
    d3.select(this.panel).selectAll('polygon')
      .attr("class", "hexbin");
    // hide brush
    d3.selectAll(".selection")
      .style("display", "none");

    d3.select('.brush-info')
      .classed('hidden', true);
  }

  // initialize plot
  init() {
    this.setTooltip(120, 45);
    this.setHovers();
    this.enableBrush(this.panel.parentNode, 4, "xy", true);

    // attach event handlers:
    d3.select("#reset")
      .on("click", this.reset);

    d3.select(".overlay")
      .on("dblclick", this.reset);
  }

}
