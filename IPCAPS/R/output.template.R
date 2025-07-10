#' (Internal object) The HTML output template for IPCAPS
#'
#' \code{output.template} contains \code{$lno_title}, \code{$lno_data},
#' \code{$lno_leafnode}, \code{$lno_body}, and \code{$template} as explained
#' below:
#' \describe{
#'   \item{lno_title}{A index number of \code{$template} that a title of HTML
#'   file is replaced.}
#'   \item{lno_data}{A index number of \code{$template} that a data section in
#'   the HTML file is replaced.}
#'   \item{lno_leafnode}{A index number of \code{$template} that a leaf-node
#'   section in the HTML file is replaced.}
#'   \item{lno_body}{A index number of \code{$template} that a body section in
#'   the HTML file is replaced.}
#'   \item{template}{A vector of characters for HTML file.}
#' }
#'
#' @name output.template
#' @docType data
#' @format A list with with 5 objects
#' @keywords output.template
"output.template"

output.template = list()
output.template$lno_title = 4
output.template$lno_data = 16
output.template$lno_leafnode = 18
output.template$lno_body = 61
output.template$template = rep("",68)
output.template$template[1] = "<html>"
output.template$template[2] = "  <head>"
output.template$template[3] = "  <title>"
output.template$template[4] = "  <!--###REPLACE###title###-->The result of IP2CAPS"
output.template$template[5] = "  </title>"
output.template$template[6] = "    <script type='text/javascript' src='https://www.google.com/jsapi'></script>"
output.template$template[7] = "    <script type='text/javascript'>"
output.template$template[8] = "      google.load('visualization', '1', {packages:['orgchart']});"
output.template$template[9] = "      google.setOnLoadCallback(drawChart);"
output.template$template[10] = "      function drawChart() {"
output.template$template[11] = "        var data = new google.visualization.DataTable();"
output.template$template[12] = "        data.addColumn('string', 'Node');"
output.template$template[13] = "        data.addColumn('string', 'ParentNode');"
output.template$template[14] = "        data.addColumn('string', 'ToolTip');"
output.template$template[15] = "        data.addRows(["
output.template$template[16] = "        /*###REPLACE###data###*/[{v:'Mike', f:'<div class=\"box_class\"><p class=\"head_class\">Head</p>President</div>'}, '', 'The President'],"
output.template$template[17] = "        ]);"
output.template$template[18] = "        /*###REPLACE###leafnode###*/data.setRowProperty(1, 'style', 'border: 2px solid #DB6E6E; background-color:#FFE1E1');"
output.template$template[19] = "        "
output.template$template[20] = "        var chart = new google.visualization.OrgChart(document.getElementById('chart_div'));"
output.template$template[21] = "        chart.draw(data, {allowHtml:true,allowCollapse:true,size:'small',nodeClass:'normal_node'});"
output.template$template[22] = "      }"
output.template$template[23] = "    </script>"
output.template$template[24] = "    <style type=\"text/css\" media=\"all\">"
output.template$template[25] = "    .box_class { "
output.template$template[26] = "            font-size:20px;"
output.template$template[27] = "            font-family:Arial;"
output.template$template[28] = "            font-weight:normal;"
output.template$template[29] = "            vertical-align:text-top;"
output.template$template[30] = "            }"
output.template$template[31] = "    .head_class { "
output.template$template[32] = "            color:purple;"
output.template$template[33] = "            font-size:20px;"
output.template$template[34] = "            font-family:Arial;"
output.template$template[35] = "            font-weight:bold;"
output.template$template[36] = "            text-align:center;"
output.template$template[37] = "            }"
output.template$template[38] = "    .subhead_class { "
output.template$template[39] = "            color:green; "
output.template$template[40] = "            text-align:center;"
output.template$template[41] = "            font-style:italic;"
output.template$template[42] = "            }"
output.template$template[43] = "    .foot_note {"
output.template$template[44] = "            color:grey; "
output.template$template[45] = "            font-style:italic; "
output.template$template[46] = "            font-family:Arial; "
output.template$template[47] = "            font-size:10px;"
output.template$template[48] = "            text-align:center;"
output.template$template[49] = "            }"
output.template$template[50] = "    .normal_node {"
output.template$template[51] = "            border: 3px solid black; "
output.template$template[52] = "            background-color:#E1E4FF;"
output.template$template[53] = "            text-align:left;"
output.template$template[54] = "            vertical-align:text-top;"
output.template$template[55] = "            }"
output.template$template[56] = "    </style>"
output.template$template[57] = "  </head>"
output.template$template[58] = ""
output.template$template[59] = "  <body>"
output.template$template[60] = "    <h2 align=\"center\" style=\"color:black; font-style:bold; font-family:Arial,Helvetica,sans-serif; font-size:28px;\">"
output.template$template[61] = "    <!--###REPLACE###body###-->The result of R.ipPCA"
output.template$template[62] = "    <br><br></h4>"
output.template$template[63] = "    <div id='chart_div'></div>"
output.template$template[64] = "    <div class=\"foot_note\">"
output.template$template[65] = "    <br><br>This page was displayed using the online <a href=\"https://developers.google.com/chart/interactive/docs/gallery/orgchart#\" target=\"_blank\">Google Organizational Chart</a> library."
output.template$template[66] = "    </div>"
output.template$template[67] = "  </body>"
output.template$template[68] = "</html>"

