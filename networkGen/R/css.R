

## Change the width and height in the .box id.
## In C5 it is slightly scaled to the right. so we need to increase the width by 50 and height by 100.

# adjust padding and margin to make sure there is direct overlap with each other.
#width:20px;
#display:inline-block;
cssC5 <- function(){
button <- paste0("
                <style>
                 html {height: 100%}

                 #center {
                 width: 50%;
                 margin: auto;

                 }


                 body, td {
                 font-family: serif;
                 background: #7abcff; /* Old browsers */
                 /* background: -moz-linear-gradient(top, #7abcff 0%, #60abf8 44%, #4096ee 100%);  FF3.6-15 */
                 /* background: -webkit-linear-gradient(top, #7abcff 0%,#60abf8 44%,#4096ee 100%);  Chrome10-25,Safari5.1-6 */
                 /* background: linear-gradient(to bottom, #7abcff 0%,#60abf8 44%,#4096ee 100%);*/
                 font-size: 13 px;
                 }\n

                 \n
                 .myButton{
                 background-color:#FFCF75;
                 -moz-border-radius:28px;
                 -webkit-border-radius:28px;
                 border-radius:28px;
                 border:1px solid #18ab29;
                 cursor:pointer;
                 color:#000000;
                 font-family:Arial;
                 font-size:10px;
                 padding:12px 10px;
                 margin: -22px -30px;
                 text-decoration:none;
                 text-shadow:none;
                 position:absolute;

                 }

                 .myButton:hover {
                 background-color:#FFCF75;
                 }
                 .myButton:active {
                 background-color:#FFCF75;
                 }


                 .buttonPosition{
                 position:absolute;
                 }

                 .box {
                 margin-top:100px;
                 margin-bottom:auto;
                 border:10px solid white;
                 max-width:800px;
                 padding: 40px;
                 border-radius: 20px;
                 -webkit-border-radius: 15px;
                 -moz-border-radius: 15px;
                 background: #66CDAA;
                 color:white;
                 font-weight:bold;
                 margin:50px auto;
                 height:650px;
                 width: 650px;
                 }
                </style>
                 ")}

## Change the width and height in the .box id.

cssC4 <- function(){
  button <- paste0("
                   <style>
                   html {height: 100%}

                   #center {
                   width: 50%;
                   margin: auto;

}


body, td {
font-family: serif;
background: #7abcff; /* Old browsers */
/* background: -moz-linear-gradient(top, #7abcff 0%, #60abf8 44%, #4096ee 100%);  FF3.6-15 */
/* background: -webkit-linear-gradient(top, #7abcff 0%,#60abf8 44%,#4096ee 100%);  Chrome10-25,Safari5.1-6 */
/* background: linear-gradient(to bottom, #7abcff 0%,#60abf8 44%,#4096ee 100%);*/
font-size: 13 px;
}\n

\n
.myButton{
background-color:#FFCF75;
-moz-border-radius:28px;
-webkit-border-radius:28px;
border-radius:28px;
border:1px solid #18ab29;
cursor:pointer;
color:#000000;
font-family:Arial;
font-size:10px;
padding:12px 10px;
margin: -22px -30px;
text-decoration:none;
text-shadow:none;
position:absolute;

}

.myButton:hover {
background-color:#FFCF75;
}
.myButton:active {
background-color:#FFCF75;
}


.buttonPosition{
position:absolute;
}

.box {
margin-top:100px;
margin-bottom:auto;
border:10px solid white;
max-width:800px;
padding: 40px;
border-radius: 20px;
-webkit-border-radius: 15px;
-moz-border-radius: 15px;
background: #66CDAA;
color:white;
font-weight:bold;
margin:50px auto;
height:550px;
width: 600px;
}
</style>
")}
