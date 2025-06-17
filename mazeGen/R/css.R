
css <- function(background="#7abcff" ,boxBackground="#66CDAA"){
css<- paste0("
    <style>
       html {height: 100%}

       #center {
       width: 50%;
       margin: auto;

       }
       body, td {
       font-family: serif;
       background:", background, ";/* Old browsers */
       /* background: -moz-linear-gradient(top, #7abcff 0%, #60abf8 44%, #4096ee 100%);  FF3.6-15 */
       /* background: -webkit-linear-gradient(top, #7abcff 0%,#60abf8 44%,#4096ee 100%);  Chrome10-25,Safari5.1-6 */
       /* background: linear-gradient(to bottom, #7abcff 0%,#60abf8 44%,#4096ee 100%);*/
       font-size: 13 px;
       }\n


       .myButton{
       width:0.001px;
       height:0.001px;
       background-color:#DBFEF8;
       -moz-border-radius:28px;
       -webkit-border-radius:28px;
       border-radius:28px;
       border:1px solid #18ab29;
       cursor:pointer;
       color:#000000;
       font-family:Arial;
       font-size:10px;
       padding:12px 10px;
       margin: -12px -12px;
       text-decoration:none;
       text-shadow:none;
       position:absolute;

       }

       .myButton:hover {
       background-color:#DBFEF8;
       }
       .myButton:active {
       background-color:#DBFEF8;
       }

       .myButtonTwo{
       width:0.001px;
       height:0.001px;
       background-color:gold;
       -moz-border-radius:28px;
       -webkit-border-radius:28px;
       border-radius:28px;
       border:1px solid #18ab29;
       cursor:pointer;
       color:#000000;
       font-family:Arial;
       font-size:10px;
       padding:12px 10px;
       margin: -12px -12px;
       text-decoration:none;
       text-shadow:none;
       position:absolute;

       }
       .colour{
       opacity:0.0;
       }
       /* .myButtonTwo:hover {
       background-color:#FFCF75;
       } */
       /*.myButtonTwo:active {
       background-color:#FFCF75;
       }*/


       .buttonPosition{
       position:absolute;
       }

       .box {
       margin-top:10px;
       margin-bottom:auto;
       border:10px solid white;
       max-width:900px;
       padding:20px;
       border-radius: 20px;
       -webkit-border-radius: 15px;
       -moz-border-radius: 15px;
       background:", boxBackground,";
       color:white;
       font-weight:bold;
       margin:50px auto;
       /*height:550px;*/
       /*width: 600px;*/
       height:850px;
       width: 920px;
       }
       </style>
       ")
  return(css)
}
