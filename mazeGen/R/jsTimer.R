

javaScriptTimerC4 <- function(colourNodePosition,maxScore){
  javaScript<- paste0("
                        ///////////// Count Down Timer Begin ////////////
                      timerHandle = null;

                      function countdown(minutes, seconds)
                      {
                      var endTime, hours, mins, msLeft, time;

                      function twoDigits(n)
                      {
                      return (n <= 9 ? \"0\" + n : n);
                      }
                      function updateTimer()
                      {
                      msLeft = endTime - (+new Date);
                      if (msLeft < 10000 * 6) {
                      document.getElementById(\"countdown2\").style.color = \"white\";
                      }
                      if (msLeft < 1000) {
                      test.submit();
                      } else {
                      time = new Date(msLeft);
                      hours = time.getUTCHours();
                      mins = time.getUTCMinutes();
                      document.getElementById(\"countdown\").innerHTML = (hours ? hours + ':' + twoDigits(mins) : mins) + ':' + twoDigits(time.getUTCSeconds());
                      document.getElementById(\"countdown2\").innerHTML = (hours ? hours + ':' + twoDigits(mins) : mins) + ':' + twoDigits(time.getUTCSeconds());
                      timeStamp = twoDigits(mins) + ':' + twoDigits(time.getUTCSeconds());
                      timerHandle = setTimeout(updateTimer, time.getUTCMilliseconds() + 500);
                      }
                      }

                      endTime = (+new Date) + 1000 * (60 * minutes + seconds) + 500;
                      updateTimer();
                      }

                      countdown(01, 30);


                      var pageInput = document.getElementsByTagName(\"input\");
                      for (i = 0; i < pageInput.length; i++) {
                      console.log(pageInput[i]);
                      pageInput[i].addEventListener('change', function ()
                      {
                      console.log('test')
                      document.getElementById(\"submitButton\").disabled = false;
                      });
                      }


                      ///////////// Timer End ////////////
                        var isFirstNodeClicked=true;
                        var prevNodeId=null;
                        var nodeArray = [];
                        var timerArray = [];
                        var responseArray =[];
                        var node = [];
                        var clickedNodes=[];
                        function captureActionData(node, nodeArray, response, timeStamp){
                        var captureObject = {}; // Create empty JSON array
                        captureObject.node = node
                        captureObject.nodeArray = nodeArray;
                        captureObject.time = timeStamp;
                        captureObject.response = response;
                        responseArray.push(captureObject);
                        console.log(JSON.stringify(responseArray));
                        }



                        function nodeClick(nodeclicked) {
                        console.log('clickedNode = ' + nodeclicked.id + '; previous node = ' + prevNodeId);
                        //console.log(nodeclicked.id)
                        //console.log(edgeArray[0][1]);

                        var response ; // Recorded as right or wrong.

                        // Record first node ///////////
                        if (isFirstNodeClicked == true){
                        if(nodeclicked.id == edgeArray[1][0] && edgeArray[2][0]){
                        node = nodeclicked.id
                        response = \"correct\";
                        timerArray.push(timeStamp);
                        console.log(timerArray);
                        var nodeArray ;

                        captureActionData(node, 'NULL' ,response, timeStamp);

                        // create hidden layer (Dynamic)
                        var form = document.getElementById('hidden');
                        var div = document.createElement('div');
                        var inputElements1 = document.createElement('input');
                        inputElements1.setAttribute(\"type\", \"hidden\");
                        inputElements1.setAttribute(\"name\", \"node\");
                        //inputElements1.setAttribute(\"name\", \"nodeArray[\" + edgeArray[i][0] + \"][\" + edgeArray[i][1] + \"]\");
                        inputElements1.setAttribute(\"value\", node);
                        // inputElements1.innerHTML =  '<input type=\"hidden\" name=\"nodeArray[' + edgeArray[i][0] + '][' + edgeArray[i][1] + ']\" value=\"' + edgeArray[i] + '\"/>'
                        div.appendChild(inputElements1);
                        form.appendChild(div);
                        // create hidden layer (Dynamic) [End] //

                        // create hidden layer (Dynamic) 'timestamp'
                        var inputElements2 = document.createElement('input');
                        inputElements2.setAttribute(\"type\", \"hidden\");
                        inputElements2.setAttribute(\"name\", \"timeStamp\");
                        //inputElements1.setAttribute(\"name\", \"nodeArray[\" + edgeArray[i][0] + \"][\" + edgeArray[i][1] + \"]\");
                        inputElements2.setAttribute(\"value\", timeStamp);
                        // inputElements1.innerHTML =  '<input type=\"hidden\" name=\"nodeArray[' + edgeArray[i][0] + '][' + edgeArray[i][1] + ']\" value=\"' + edgeArray[i] + '\"/>'
                        div.appendChild(inputElements2);
                        form.appendChild(div);
                        // create hidden layer (Dynamic) [End] //

                        // create hidden layer (Dynamic) 'responses'
                        var inputElements3 = document.createElement('input');
                        inputElements3.setAttribute(\"type\", \"hidden\");
                        inputElements3.setAttribute(\"name\", \"response\");
                        //inputElements1.setAttribute(\"name\", \"nodeArray[\" + edgeArray[i][0] + \"][\" + edgeArray[i][1] + \"]\");
                        inputElements3.setAttribute(\"value\", response);
                        // inputElements1.innerHTML =  '<input type=\"hidden\" name=\"nodeArray[' + edgeArray[i][0] + '][' + edgeArray[i][1] + ']\" value=\"' + edgeArray[i] + '\"/>'
                        div.appendChild(inputElements3);
                        form.appendChild(div);
                        // create hidden layer (Dynamic) [End] //

                        // create hidden layer (Dynamic)
                        var inputElements4 = document.createElement('input');
                        inputElements4.setAttribute(\"type\", \"hidden\");
                        inputElements4.setAttribute(\"name\", \"nodePosition\");
                        inputElements4.setAttribute(\"value\", node);

                        div.appendChild(inputElements4);
                        form.appendChild(div);
                        // create hidden layer (Dynamic) [End] //


                        }else{
                        alert(\"Please start from the first node at the bottom\");
                        node = nodeclicked.id
                        response = \"notBottom\";
                        timerArray.push(timeStamp);
                        console.log(timerArray);
                        var nodeArray ;

                        captureActionData(node, 'NULL' ,response, timeStamp);

                        // create hidden layer (Dynamic)
                        var form = document.getElementById('hidden');
                        var div = document.createElement('div');
                        var inputElements1 = document.createElement('input');
                        inputElements1.setAttribute(\"type\", \"hidden\");
                        inputElements1.setAttribute(\"name\", \"node\");
                        //inputElements1.setAttribute(\"name\", \"nodeArray[\" + edgeArray[i][0] + \"][\" + edgeArray[i][1] + \"]\");
                        inputElements1.setAttribute(\"value\", node);
                        // inputElements1.innerHTML =  '<input type=\"hidden\" name=\"nodeArray[' + edgeArray[i][0] + '][' + edgeArray[i][1] + ']\" value=\"' + edgeArray[i] + '\"/>'
                        div.appendChild(inputElements1);
                        form.appendChild(div);
                        // create hidden layer (Dynamic) [End] //

                        // create hidden layer (Dynamic) 'timestamp'
                        var inputElements2 = document.createElement('input');
                        inputElements2.setAttribute(\"type\", \"hidden\");
                        inputElements2.setAttribute(\"name\", \"timeStamp\");
                        //inputElements1.setAttribute(\"name\", \"nodeArray[\" + edgeArray[i][0] + \"][\" + edgeArray[i][1] + \"]\");
                        inputElements2.setAttribute(\"value\", timeStamp);
                        // inputElements1.innerHTML =  '<input type=\"hidden\" name=\"nodeArray[' + edgeArray[i][0] + '][' + edgeArray[i][1] + ']\" value=\"' + edgeArray[i] + '\"/>'
                        div.appendChild(inputElements2);
                        form.appendChild(div);
                        // create hidden layer (Dynamic) [End] //

                        // create hidden layer (Dynamic) 'responses'
                        var inputElements3 = document.createElement('input');
                        inputElements3.setAttribute(\"type\", \"hidden\");
                        inputElements3.setAttribute(\"name\", \"response\");
                        //inputElements1.setAttribute(\"name\", \"nodeArray[\" + edgeArray[i][0] + \"][\" + edgeArray[i][1] + \"]\");
                        inputElements3.setAttribute(\"value\", response);
                        // inputElements1.innerHTML =  '<input type=\"hidden\" name=\"nodeArray[' + edgeArray[i][0] + '][' + edgeArray[i][1] + ']\" value=\"' + edgeArray[i] + '\"/>'
                        div.appendChild(inputElements3);
                        form.appendChild(div);
                        // create hidden layer (Dynamic) [End] //

                        // create hidden layer (Dynamic)
                        var inputElements4 = document.createElement('input');
                        inputElements4.setAttribute(\"type\", \"hidden\");
                        inputElements4.setAttribute(\"name\", \"nodePosition\");
                        inputElements4.setAttribute(\"value\", node);

                        div.appendChild(inputElements4);
                        form.appendChild(div);
                        return;
                        }

                        }


                        /// Second node onwards ////////
                        if (isFirstNodeClicked == false) {
                        var pathExists = false;
                        for(var i=0; i<edgeArray.length; i++) {
                        //alert(edgeArray[i][0]);
                        //alert(edgeArray[i][1]);
                        if((edgeArray[i][0] == nodeclicked.id && edgeArray[i][1] == prevNodeId) || (edgeArray[i][0] == prevNodeId && edgeArray[i][1] == nodeclicked.id)) {
                        if ((edgeArray[i][2] == 0)) {
                        edgeArray[i][2] = 1;",
                        colourNodePosition, "{
                        edgeArray[i][3] = 1;  // count total score
                        console.log(edgeArray[i]);
                        }

                        // JSON array
                        node = nodeclicked.id
                        response = \"correct\";
                        timerArray.push(timeStamp);
                        console.log(timerArray);
                        captureActionData(node, edgeArray[i], response, timeStamp);

                        // create hidden layer (Dynamic)
                        var form = document.getElementById('hidden');
                        var div = document.createElement('div');
                        var inputElements1 = document.createElement('input');
                        inputElements1.setAttribute(\"type\", \"hidden\");
                        inputElements1.setAttribute(\"name\", \"node\");
                        //inputElements1.setAttribute(\"name\", \"nodeArray[\" + edgeArray[i][0] + \"][\" + edgeArray[i][1] + \"]\");
                        inputElements1.setAttribute(\"value\", edgeArray[i]);
                        // inputElements1.innerHTML =  '<input type=\"hidden\" name=\"nodeArray[' + edgeArray[i][0] + '][' + edgeArray[i][1] + ']\" value=\"' + edgeArray[i] + '\"/>'
                        div.appendChild(inputElements1);
                        form.appendChild(div);
                        // create hidden layer (Dynamic) [End] //

                        // create hidden layer (Dynamic) 'time stamp'
                        var inputElements2 = document.createElement('input');
                        inputElements2.setAttribute(\"type\", \"hidden\");
                        inputElements2.setAttribute(\"name\", \"timeStamp\");
                        //inputElements1.setAttribute(\"name\", \"nodeArray[\" + edgeArray[i][0] + \"][\" + edgeArray[i][1] + \"]\");
                        inputElements2.setAttribute(\"value\", timeStamp);
                        // inputElements1.innerHTML =  '<input type=\"hidden\" name=\"nodeArray[' + edgeArray[i][0] + '][' + edgeArray[i][1] + ']\" value=\"' + edgeArray[i] + '\"/>'
                        div.appendChild(inputElements2);
                        form.appendChild(div);
                        // create hidden layer (Dynamic) [End] //

                        // create hidden layer (Dynamic) 'responses'
                        var inputElements3 = document.createElement('input');
                        inputElements3.setAttribute(\"type\", \"hidden\");
                        inputElements3.setAttribute(\"name\", \"response\");
                        //inputElements1.setAttribute(\"name\", \"nodeArray[\" + edgeArray[i][0] + \"][\" + edgeArray[i][1] + \"]\");
                        inputElements3.setAttribute(\"value\", response);
                        // inputElements1.innerHTML =  '<input type=\"hidden\" name=\"nodeArray[' + edgeArray[i][0] + '][' + edgeArray[i][1] + ']\" value=\"' + edgeArray[i] + '\"/>'
                        div.appendChild(inputElements3);
                        form.appendChild(div);
                        // create hidden layer (Dynamic) [End] //

                        // create hidden layer (Dynamic)
                        var inputElements4 = document.createElement('input');
                        inputElements4.setAttribute(\"type\", \"hidden\");
                        inputElements4.setAttribute(\"name\", \"nodePosition\");
                        inputElements4.setAttribute(\"value\", node);

                        div.appendChild(inputElements4);
                        form.appendChild(div);
                        // create hidden layer (Dynamic) [End] //

                        pathExists = true; //hooray path exists :P
                        // prevMyNode = document.getElementById(prevNodeId); // Put into new variable
                        //prevMyNode.style.backgroundColor = '#FFCF75';  // changes the color of the element of previous node ID

                        }

                        //unidirection
                        if(edgeArray[i][4] == 2 && Number(nodeclicked.id) < prevNodeId){
                        edgeArray[i][2] = 0;
                        node = nodeclicked.id
                        timerArray.push(timeStamp);
                        console.log(timerArray);
                        alert(\"Wrong direction\");
                        response = \"wrongDirection\";
                        captureActionData(node, edgeArray[i], response, timeStamp);
                        // create hidden layer (Dynamic)
                        var form = document.getElementById('hidden');
                        var div = document.createElement('div');
                        var inputElements1 = document.createElement('input');
                        inputElements1.setAttribute(\"type\", \"hidden\");
                        inputElements1.setAttribute(\"name\", \"node\");
                        //inputElements1.setAttribute(\"name\", \"nodeArray[\" + edgeArray[i][0] + \"][\" + edgeArray[i][1] + \"]\");
                        inputElements1.setAttribute(\"value\", edgeArray[i]);
                        // inputElements1.innerHTML =  '<input type=\"hidden\" name=\"nodeArray[' + edgeArray[i][0] + '][' + edgeArray[i][1] + ']\" value=\"' + edgeArray[i] + '\"/>'
                        div.appendChild(inputElements1);
                        form.appendChild(div);
                        // create hidden layer (Dynamic) [End] //

                        // create hidden layer (Dynamic) \"time stamp\"
                        var inputElements2 = document.createElement('input');
                        inputElements2.setAttribute(\"type\", \"hidden\");
                        inputElements2.setAttribute(\"name\", \"timeStamp\");
                        //inputElements1.setAttribute(\"name\", \"nodeArray[\" + edgeArray[i][0] + \"][\" + edgeArray[i][1] + \"]\");
                        inputElements2.setAttribute(\"value\", timeStamp);
                        // inputElements1.innerHTML =  '<input type=\"hidden\" name=\"nodeArray[' + edgeArray[i][0] + '][' + edgeArray[i][1] + ']\" value=\"' + edgeArray[i] + '\"/>'
                        div.appendChild(inputElements2);
                        form.appendChild(div);
                        // create hidden layer (Dynamic) [End] //

                        // create hidden layer (Dynamic) 'responses'
                        var inputElements3 = document.createElement('input');
                        inputElements3.setAttribute(\"type\", \"hidden\");
                        inputElements3.setAttribute(\"name\", \"response\");
                        //inputElements1.setAttribute(\"name\", \"nodeArray[\" + edgeArray[i][0] + \"][\" + edgeArray[i][1] + \"]\");
                        inputElements3.setAttribute(\"value\", response);
                        // inputElements1.innerHTML =  '<input type=\"hidden\" name=\"nodeArray[' + edgeArray[i][0] + '][' + edgeArray[i][1] + ']\" value=\"' + edgeArray[i] + '\"/>'
                        div.appendChild(inputElements3);
                        form.appendChild(div);
                        // create hidden layer (Dynamic) [End] //

                        // create hidden layer (Dynamic)
                        var inputElements4 = document.createElement('input');
                        inputElements4.setAttribute(\"type\", \"hidden\");
                        inputElements4.setAttribute(\"name\", \"nodePosition\");
                        inputElements4.setAttribute(\"value\", node);

                        div.appendChild(inputElements4);
                        form.appendChild(div);
                        // create hidden layer (Dynamic) [End] //


                        //prevMyNode.style.backgroundColor = '#218868';
                        return false;
                        }

                        }

                        }



                        //check if the person tried to take a path that don't exists or repeated
                        if (pathExists == false) {
                        for(var i=0; i<edgeArray.length; i++) {
                        if((edgeArray[i][0] !== nodeclicked.id && edgeArray[i][1] !== prevNodeId) || (edgeArray[i][0] !== prevNodeId && edgeArray[i][1] !== nodeclicked.id)) {
                        if ((edgeArray[i][2] == 0)) { //checks that the user hasn't taken this path already
                        edgeArray[i][2] = 0; //makes it so that they can't take that path again
                        console.log(edgeArray[i]);
                        console.log(clickedNodes.length);


                        // Repeated nodes
                        if(nodeclicked.id == prevNodeId) {
                        timerArray.push(timeStamp);
                        console.log(timerArray);
                        alert(\"This is repeated\");
                        response = \"repeated\";
                        captureActionData(clickedNodes[i], clickedNodes[i] ,response, timeStamp);

                        // create hidden layer (Dynamic)
                        var form = document.getElementById('hidden');
                        var div = document.createElement('div');
                        var inputElements1 = document.createElement('input');
                        inputElements1.setAttribute(\"type\", \"hidden\");
                        inputElements1.setAttribute(\"name\", \"node\");
                        //inputElements1.setAttribute(\"name\", \"nodeArray[\" + edgeArray[i][0] + \"][\" + edgeArray[i][1] + \"]\");
                        inputElements1.setAttribute(\"value\", nodeclicked.id);
                        // inputElements1.innerHTML =  '<input type=\"hidden\" name=\"nodeArray[' + edgeArray[i][0] + '][' + edgeArray[i][1] + ']\" value=\"' + edgeArray[i] + '\"/>'
                        div.appendChild(inputElements1);
                        form.appendChild(div);
                        // create hidden layer (Dynamic) [End] //

                        // create hidden layer (Dynamic) \"time stamp\"
                        var inputElements2 = document.createElement('input');
                        inputElements2.setAttribute(\"type\", \"hidden\");
                        inputElements2.setAttribute(\"name\", \"timeStamp\");
                        //inputElements1.setAttribute(\"name\", \"nodeArray[\" + edgeArray[i][0] + \"][\" + edgeArray[i][1] + \"]\");
                        inputElements2.setAttribute(\"value\", timeStamp);
                        // inputElements1.innerHTML =  '<input type=\"hidden\" name=\"nodeArray[' + edgeArray[i][0] + '][' + edgeArray[i][1] + ']\" value=\"' + edgeArray[i] + '\"/>'
                        div.appendChild(inputElements2);
                        form.appendChild(div);
                        // create hidden layer (Dynamic) [End] //

                        // create hidden layer (Dynamic) 'responses'
                        var inputElements3 = document.createElement('input');
                        inputElements3.setAttribute(\"type\", \"hidden\");
                        inputElements3.setAttribute(\"name\", \"response\");
                        //inputElements1.setAttribute(\"name\", \"nodeArray[\" + edgeArray[i][0] + \"][\" + edgeArray[i][1] + \"]\");
                        inputElements3.setAttribute(\"value\", response);
                        // inputElements1.innerHTML =  '<input type=\"hidden\" name=\"nodeArray[' + edgeArray[i][0] + '][' + edgeArray[i][1] + ']\" value=\"' + edgeArray[i] + '\"/>'
                        div.appendChild(inputElements3);
                        form.appendChild(div);
                        // create hidden layer (Dynamic) [End] //

                        // create hidden layer (Dynamic)
                        var inputElements4 = document.createElement('input');
                        inputElements4.setAttribute(\"type\", \"hidden\");
                        inputElements4.setAttribute(\"name\", \"nodePosition\");
                        inputElements4.setAttribute(\"value\", node);

                        div.appendChild(inputElements4);
                        form.appendChild(div);
                        // create hidden layer (Dynamic) [End] //
                        return false;

                        }

                        // JSON array
                        node = nodeclicked.id
                        timerArray.push(timeStamp);
                        console.log(timerArray);
                        alert('You cant go here!');
                        response = \"incorrect\";
                        captureActionData(node, edgeArray[i], response, timeStamp);

                        // create hidden layer (Dynamic)
                        var form = document.getElementById('hidden');
                        var div = document.createElement('div');
                        var inputElements1 = document.createElement('input');
                        inputElements1.setAttribute(\"type\", \"hidden\");
                        inputElements1.setAttribute(\"name\", \"node\");
                        //inputElements1.setAttribute(\"name\",\"nodeArray[\" + edgeArray[i][0] + \"][\" + edgeArray[i][1] + \"]\");
                        inputElements1.setAttribute(\"value\", edgeArray[i]);
                        // inputElements1.innerHTML =  '<input type=\"hidden\" name=\"nodeArray[' + edgeArray[i][0] + '][' + edgeArray[i][1] + ']\" value=\"' + edgeArray[i] + '\"/>'
                        div.appendChild(inputElements1);
                        form.appendChild(div);
                        // create hidden layer (Dynamic) [End] //

                        // create hidden layer (Dynamic) \"time stamp\"
                        var inputElements2 = document.createElement('input');
                        inputElements2.setAttribute(\"type\", \"hidden\");
                        inputElements2.setAttribute(\"name\", \"timeStamp\");
                        //inputElements1.setAttribute(\"name\", \"nodeArray[\" + edgeArray[i][0] + \"][\" + edgeArray[i][1] + \"]\");
                        inputElements2.setAttribute(\"value\", timeStamp);
                        // inputElements1.innerHTML =  '<input type=\"hidden\" name=\"nodeArray[' + edgeArray[i][0] + '][' + edgeArray[i][1] + ']\" value=\"' + edgeArray[i] + '\"/>'
                        div.appendChild(inputElements2);
                        form.appendChild(div);
                        // create hidden layer (Dynamic) [End] //

                        // create hidden layer (Dynamic) 'responses'
                        var inputElements3 = document.createElement('input');
                        inputElements3.setAttribute(\"type\", \"hidden\");
                        inputElements3.setAttribute(\"name\", \"response\");
                        //inputElements1.setAttribute(\"name\", \"nodeArray[\" + edgeArray[i][0] + \"][\" + edgeArray[i][1] + \"]\");
                        inputElements3.setAttribute(\"value\", response);
                        // inputElements1.innerHTML =  '<input type=\"hidden\" name=\"nodeArray[' + edgeArray[i][0] + '][' + edgeArray[i][1] + ']\" value=\"' + edgeArray[i] + '\"/>'
                        div.appendChild(inputElements3);
                        form.appendChild(div);
                        // create hidden layer (Dynamic) [End] //

                        // create hidden layer (Dynamic)
                        var inputElements4 = document.createElement('input');
                        inputElements4.setAttribute(\"type\", \"hidden\");
                        inputElements4.setAttribute(\"name\", \"nodePosition\");
                        inputElements4.setAttribute(\"value\", node);

                        div.appendChild(inputElements4);
                        form.appendChild(div);
                        // create hidden layer (Dynamic) [End] //

                        return false;




                        }
                        }
                        }
                        }

                        }

                        // we successfully clicked on something, so change colour of stuff (line and node)
                        if (isFirstNodeClicked == true){
                        nodeclicked.style.backgroundColor = 'red';
                        }

                        // Create getChildren(gets all the nodes including the node you click)
                        // changes the colour of everything except the node you click on
                        function getChildren(n, skipMe){
                        var r = [];
                        for ( ; n; n = n.nextSibling )
                        if ( n.nodeType == 1 && n != skipMe)
                        r.push( n );
                        return r;
                        };
                        // Create getsiblings(gets all the nodes except the node you click)
                        function getSiblings(n) {
                        return getChildren(n.parentNode.firstChild, n);
                        }


                        if (isFirstNodeClicked == false) { // the line doesnt change colour the first time

                        if (Number(nodeclicked.id) > Number(prevNodeId)) {
                        var elementname = prevNodeId+'_'+nodeclicked.id;
                        } else {
                        var elementname = nodeclicked.id+'_'+prevNodeId;
                        }

                        console.log('element_name = ' + elementname + 'clicked node = ' + nodeclicked.id + '; previous node = ' + prevNodeId);

                        // this breaks if there is no if-statement on line 302. Because it doesn't continue onwards and reset nodeclicked into prevNode.
                        // So you need the if statement. Once that is true, then it continues on with the code.
                        myelement = document.getElementById(elementname);
                        myelement.style.stroke = '#FF0000';


                        }

                        // set things up for next time
                        prevNodeId = nodeclicked.id;
                        isFirstNodeClicked=false;

                        if (isFirstNodeClicked == false){
                        nodeclicked.style.borderColor = 'red';
                        }


                        // suming up the score of the person based on the black dotes crossed
                        var sum = 0;
                        for(i =0; i < edgeArray.length; i++){
                        //console.log(edgeArray[i][3]);
                        sum  += edgeArray[i][3];
                        }
                        console.log(sum); // 6


                        // check stopping rules
                        var completedGame = true;
                        if(sum == ",maxScore,"){ ")
return(javaScript)
}

javaScriptTimerC5 <- function(colourNodePosition,maxScore){
  javaScript<- paste0("
                      ///////////// Count Down Timer Begin ////////////
                     timerHandle = null;

                      function countdown(minutes, seconds)
                      {
                      var endTime, hours, mins, msLeft, time;

                      function twoDigits(n)
                      {
                      return (n <= 9 ? \"0\" + n : n);
                      }
                      function updateTimer()
                      {
                      msLeft = endTime - (+new Date);
                      if (msLeft < 10000 * 6) {
                      document.getElementById(\"countdown2\").style.color = \"red\";
                      }
                      if (msLeft < 1000) {
                      test.submit();
                      } else {
                      time = new Date(msLeft);
                      hours = time.getUTCHours();
                      mins = time.getUTCMinutes();
                      document.getElementById(\"countdown\").innerHTML = (hours ? hours + ':' + twoDigits(mins) : mins) + ':' + twoDigits(time.getUTCSeconds());
                      document.getElementById(\"countdown2\").innerHTML = (hours ? hours + ':' + twoDigits(mins) : mins) + ':' + twoDigits(time.getUTCSeconds());
                      timeStamp = twoDigits(mins) + ':' + twoDigits(time.getUTCSeconds());
                      timerHandle = setTimeout(updateTimer, time.getUTCMilliseconds() + 500);
                      }
                      }

                      endTime = (+new Date) + 1000 * (60 * minutes + seconds) + 500;
                      updateTimer();
                      }

                      countdown(01, 30);


                      var pageInput = document.getElementsByTagName(\"input\");
                      for (i = 0; i < pageInput.length; i++) {
                      console.log(pageInput[i]);
                      pageInput[i].addEventListener('change', function ()
                      {
                      console.log('test')
                      document.getElementById(\"submitButton\").disabled = false;
                      });
                      }



                      ///////////// Timer End ////////////

                      var isFirstNodeClicked=true;
                      var prevNodeId=null;
                      var nodeArray = [];
                      var timerArray = [];
                      var responseArray =[];
                      var node = [];
                      var clickedNodes=[];
                      function captureActionData(node, nodeArray, response, timeStamp){
                      var captureObject = {}; // Create empty JSON array
                      captureObject.node = node
                      captureObject.nodeArray = nodeArray;
                      captureObject.time = timeStamp;
                      captureObject.response = response;
                      responseArray.push(captureObject);
                      console.log(JSON.stringify(responseArray));
                      }



                      function nodeClick(nodeclicked) {
                      console.log('clickedNode = ' + nodeclicked.id + '; previous node = ' + prevNodeId);
                      //console.log(nodeclicked.id)
                      //console.log(edgeArray[0][1]);

                      var response ; // Recorded as right or wrong.

                      // Record first node ///////////
                      if (isFirstNodeClicked == true){
                      if(nodeclicked.id == edgeArray[1][0] && edgeArray[2][0]){
                      node = nodeclicked.id
                      response = \"correct\";
                      timerArray.push(timeStamp);
                      console.log(timerArray);
                      var nodeArray ;

                      captureActionData(node, 'NULL' ,response, timeStamp);

                      // create hidden layer (Dynamic)
                      var form = document.getElementById('hidden');
                      var div = document.createElement('div');
                      var inputElements1 = document.createElement('input');
                      inputElements1.setAttribute(\"type\", \"hidden\");
                      inputElements1.setAttribute(\"name\", \"node\");
                      //inputElements1.setAttribute(\"name\", \"nodeArray[\" + edgeArray[i][0] + \"][\" + edgeArray[i][1] + \"]\");
                      inputElements1.setAttribute(\"value\", node);
                      // inputElements1.innerHTML =  '<input type=\"hidden\" name=\"nodeArray[' + edgeArray[i][0] + '][' + edgeArray[i][1] + ']\" value=\"' + edgeArray[i] + '\"/>'
                      div.appendChild(inputElements1);
                      form.appendChild(div);
                      // create hidden layer (Dynamic) [End] //

                      // create hidden layer (Dynamic) 'timestamp'
                      var inputElements2 = document.createElement('input');
                      inputElements2.setAttribute(\"type\", \"hidden\");
                      inputElements2.setAttribute(\"name\", \"timeStamp\");
                      //inputElements1.setAttribute(\"name\", \"nodeArray[\" + edgeArray[i][0] + \"][\" + edgeArray[i][1] + \"]\");
                      inputElements2.setAttribute(\"value\", timeStamp);
                      // inputElements1.innerHTML =  '<input type=\"hidden\" name=\"nodeArray[' + edgeArray[i][0] + '][' + edgeArray[i][1] + ']\" value=\"' + edgeArray[i] + '\"/>'
                      div.appendChild(inputElements2);
                      form.appendChild(div);
                      // create hidden layer (Dynamic) [End] //

                      // create hidden layer (Dynamic) 'responses'
                      var inputElements3 = document.createElement('input');
                      inputElements3.setAttribute(\"type\", \"hidden\");
                      inputElements3.setAttribute(\"name\", \"response\");
                      //inputElements1.setAttribute(\"name\", \"nodeArray[\" + edgeArray[i][0] + \"][\" + edgeArray[i][1] + \"]\");
                      inputElements3.setAttribute(\"value\", response);
                      // inputElements1.innerHTML =  '<input type=\"hidden\" name=\"nodeArray[' + edgeArray[i][0] + '][' + edgeArray[i][1] + ']\" value=\"' + edgeArray[i] + '\"/>'
                      div.appendChild(inputElements3);
                      form.appendChild(div);
                      // create hidden layer (Dynamic) [End] //

                      // create hidden layer (Dynamic)
                      var inputElements4 = document.createElement('input');
                      inputElements4.setAttribute(\"type\", \"hidden\");
                      inputElements4.setAttribute(\"name\", \"nodePosition\");
                      inputElements4.setAttribute(\"value\", node);

                      div.appendChild(inputElements4);
                      form.appendChild(div);
                      // create hidden layer (Dynamic) [End] //


                      }else{
                      alert(\"Please start from the first node at the bottom\");
                      node = nodeclicked.id
                      response = \"notBottom\";
                      timerArray.push(timeStamp);
                      console.log(timerArray);
                      var nodeArray ;

                      captureActionData(node, 'NULL' ,response, timeStamp);

                      // create hidden layer (Dynamic)
                      var form = document.getElementById('hidden');
                      var div = document.createElement('div');
                      var inputElements1 = document.createElement('input');
                      inputElements1.setAttribute(\"type\", \"hidden\");
                      inputElements1.setAttribute(\"name\", \"node\");
                      //inputElements1.setAttribute(\"name\", \"nodeArray[\" + edgeArray[i][0] + \"][\" + edgeArray[i][1] + \"]\");
                      inputElements1.setAttribute(\"value\", node);
                      // inputElements1.innerHTML =  '<input type=\"hidden\" name=\"nodeArray[' + edgeArray[i][0] + '][' + edgeArray[i][1] + ']\" value=\"' + edgeArray[i] + '\"/>'
                      div.appendChild(inputElements1);
                      form.appendChild(div);
                      // create hidden layer (Dynamic) [End] //

                      // create hidden layer (Dynamic) 'timestamp'
                      var inputElements2 = document.createElement('input');
                      inputElements2.setAttribute(\"type\", \"hidden\");
                      inputElements2.setAttribute(\"name\", \"timeStamp\");
                      //inputElements1.setAttribute(\"name\", \"nodeArray[\" + edgeArray[i][0] + \"][\" + edgeArray[i][1] + \"]\");
                      inputElements2.setAttribute(\"value\", timeStamp);
                      // inputElements1.innerHTML =  '<input type=\"hidden\" name=\"nodeArray[' + edgeArray[i][0] + '][' + edgeArray[i][1] + ']\" value=\"' + edgeArray[i] + '\"/>'
                      div.appendChild(inputElements2);
                      form.appendChild(div);
                      // create hidden layer (Dynamic) [End] //

                      // create hidden layer (Dynamic) 'responses'
                      var inputElements3 = document.createElement('input');
                      inputElements3.setAttribute(\"type\", \"hidden\");
                      inputElements3.setAttribute(\"name\", \"response\");
                      //inputElements1.setAttribute(\"name\", \"nodeArray[\" + edgeArray[i][0] + \"][\" + edgeArray[i][1] + \"]\");
                      inputElements3.setAttribute(\"value\", response);
                      // inputElements1.innerHTML =  '<input type=\"hidden\" name=\"nodeArray[' + edgeArray[i][0] + '][' + edgeArray[i][1] + ']\" value=\"' + edgeArray[i] + '\"/>'
                      div.appendChild(inputElements3);
                      form.appendChild(div);
                      // create hidden layer (Dynamic) [End] //

                      // create hidden layer (Dynamic)
                      var inputElements4 = document.createElement('input');
                      inputElements4.setAttribute(\"type\", \"hidden\");
                      inputElements4.setAttribute(\"name\", \"nodePosition\");
                      inputElements4.setAttribute(\"value\", node);

                      div.appendChild(inputElements4);
                      form.appendChild(div);
                      return;
                      }

                      }


                      /// Second node onwards ////////
                      if (isFirstNodeClicked == false) {
                      var pathExists = false;
                      for(var i=0; i<edgeArray.length; i++) {
                      //alert(edgeArray[i][0]);
                      //alert(edgeArray[i][1]);
                      if((edgeArray[i][0] == nodeclicked.id && edgeArray[i][1] == prevNodeId) || (edgeArray[i][0] == prevNodeId && edgeArray[i][1] == nodeclicked.id)) {
                      if ((edgeArray[i][2] == 0)) {
                      edgeArray[i][2] = 1;",
                      colourNodePosition, "{
                      edgeArray[i][3] = 1;  // count total score
                      console.log(edgeArray[i]);
                      }

                      // JSON array
                      node = nodeclicked.id
                      response = \"correct\";
                      timerArray.push(timeStamp);
                      console.log(timerArray);
                      captureActionData(node, edgeArray[i], response, timeStamp);

                      // create hidden layer (Dynamic)
                      var form = document.getElementById('hidden');
                      var div = document.createElement('div');
                      var inputElements1 = document.createElement('input');
                      inputElements1.setAttribute(\"type\", \"hidden\");
                      inputElements1.setAttribute(\"name\", \"node\");
                      //inputElements1.setAttribute(\"name\", \"nodeArray[\" + edgeArray[i][0] + \"][\" + edgeArray[i][1] + \"]\");
                      inputElements1.setAttribute(\"value\", edgeArray[i]);
                      // inputElements1.innerHTML =  '<input type=\"hidden\" name=\"nodeArray[' + edgeArray[i][0] + '][' + edgeArray[i][1] + ']\" value=\"' + edgeArray[i] + '\"/>'
                      div.appendChild(inputElements1);
                      form.appendChild(div);
                      // create hidden layer (Dynamic) [End] //

                      // create hidden layer (Dynamic) 'time stamp'
                      var inputElements2 = document.createElement('input');
                      inputElements2.setAttribute(\"type\", \"hidden\");
                      inputElements2.setAttribute(\"name\", \"timeStamp\");
                      //inputElements1.setAttribute(\"name\", \"nodeArray[\" + edgeArray[i][0] + \"][\" + edgeArray[i][1] + \"]\");
                      inputElements2.setAttribute(\"value\", timeStamp);
                      // inputElements1.innerHTML =  '<input type=\"hidden\" name=\"nodeArray[' + edgeArray[i][0] + '][' + edgeArray[i][1] + ']\" value=\"' + edgeArray[i] + '\"/>'
                      div.appendChild(inputElements2);
                      form.appendChild(div);
                      // create hidden layer (Dynamic) [End] //

                      // create hidden layer (Dynamic) 'responses'
                      var inputElements3 = document.createElement('input');
                      inputElements3.setAttribute(\"type\", \"hidden\");
                      inputElements3.setAttribute(\"name\", \"response\");
                      //inputElements1.setAttribute(\"name\", \"nodeArray[\" + edgeArray[i][0] + \"][\" + edgeArray[i][1] + \"]\");
                      inputElements3.setAttribute(\"value\", response);
                      // inputElements1.innerHTML =  '<input type=\"hidden\" name=\"nodeArray[' + edgeArray[i][0] + '][' + edgeArray[i][1] + ']\" value=\"' + edgeArray[i] + '\"/>'
                      div.appendChild(inputElements3);
                      form.appendChild(div);
                      // create hidden layer (Dynamic) [End] //

                      // create hidden layer (Dynamic)
                      var inputElements4 = document.createElement('input');
                      inputElements4.setAttribute(\"type\", \"hidden\");
                      inputElements4.setAttribute(\"name\", \"nodePosition\");
                      inputElements4.setAttribute(\"value\", node);

                      div.appendChild(inputElements4);
                      form.appendChild(div);
                      // create hidden layer (Dynamic) [End] //

                      pathExists = true; //hooray path exists :P
                      // prevMyNode = document.getElementById(prevNodeId); // Put into new variable
                      //prevMyNode.style.backgroundColor = '#FFCF75';  // changes the color of the element of previous node ID

                      }

                      //unidirection
                      if(edgeArray[i][4] == 2 && Number(nodeclicked.id) < prevNodeId){
                      edgeArray[i][2] = 0;
                      node = nodeclicked.id
                      timerArray.push(timeStamp);
                      console.log(timerArray);
                      alert(\"Wrong direction\");
                      response = \"wrongDirection\";
                      captureActionData(node, edgeArray[i], response, timeStamp);
                      // create hidden layer (Dynamic)
                      var form = document.getElementById('hidden');
                      var div = document.createElement('div');
                      var inputElements1 = document.createElement('input');
                      inputElements1.setAttribute(\"type\", \"hidden\");
                      inputElements1.setAttribute(\"name\", \"node\");
                      //inputElements1.setAttribute(\"name\", \"nodeArray[\" + edgeArray[i][0] + \"][\" + edgeArray[i][1] + \"]\");
                      inputElements1.setAttribute(\"value\", edgeArray[i]);
                      // inputElements1.innerHTML =  '<input type=\"hidden\" name=\"nodeArray[' + edgeArray[i][0] + '][' + edgeArray[i][1] + ']\" value=\"' + edgeArray[i] + '\"/>'
                      div.appendChild(inputElements1);
                      form.appendChild(div);
                      // create hidden layer (Dynamic) [End] //

                      // create hidden layer (Dynamic) \"time stamp\"
                      var inputElements2 = document.createElement('input');
                      inputElements2.setAttribute(\"type\", \"hidden\");
                      inputElements2.setAttribute(\"name\", \"timeStamp\");
                      //inputElements1.setAttribute(\"name\", \"nodeArray[\" + edgeArray[i][0] + \"][\" + edgeArray[i][1] + \"]\");
                      inputElements2.setAttribute(\"value\", timeStamp);
                      // inputElements1.innerHTML =  '<input type=\"hidden\" name=\"nodeArray[' + edgeArray[i][0] + '][' + edgeArray[i][1] + ']\" value=\"' + edgeArray[i] + '\"/>'
                      div.appendChild(inputElements2);
                      form.appendChild(div);
                      // create hidden layer (Dynamic) [End] //

                      // create hidden layer (Dynamic) 'responses'
                      var inputElements3 = document.createElement('input');
                      inputElements3.setAttribute(\"type\", \"hidden\");
                      inputElements3.setAttribute(\"name\", \"response\");
                      //inputElements1.setAttribute(\"name\", \"nodeArray[\" + edgeArray[i][0] + \"][\" + edgeArray[i][1] + \"]\");
                      inputElements3.setAttribute(\"value\", response);
                      // inputElements1.innerHTML =  '<input type=\"hidden\" name=\"nodeArray[' + edgeArray[i][0] + '][' + edgeArray[i][1] + ']\" value=\"' + edgeArray[i] + '\"/>'
                      div.appendChild(inputElements3);
                      form.appendChild(div);
                      // create hidden layer (Dynamic) [End] //

                      // create hidden layer (Dynamic)
                      var inputElements4 = document.createElement('input');
                      inputElements4.setAttribute(\"type\", \"hidden\");
                      inputElements4.setAttribute(\"name\", \"nodePosition\");
                      inputElements4.setAttribute(\"value\", node);

                      div.appendChild(inputElements4);
                      form.appendChild(div);
                      // create hidden layer (Dynamic) [End] //


                      //prevMyNode.style.backgroundColor = '#218868';
                      return false;
                      }

                      }

                      }



                      //check if the person tried to take a path that don't exists or repeated
                      if (pathExists == false) {
                      for(var i=0; i<edgeArray.length; i++) {
                      if((edgeArray[i][0] !== nodeclicked.id && edgeArray[i][1] !== prevNodeId) || (edgeArray[i][0] !== prevNodeId && edgeArray[i][1] !== nodeclicked.id)) {
                      if ((edgeArray[i][2] == 0)) { //checks that the user hasn't taken this path already
                      edgeArray[i][2] = 0; //makes it so that they can't take that path again
                      console.log(edgeArray[i]);
                      console.log(clickedNodes.length);


                      // Repeated nodes
                      if(nodeclicked.id == prevNodeId) {
                      timerArray.push(timeStamp);
                      console.log(timerArray);
                      alert(\"This is repeated\");
                      response = \"repeated\";
                      captureActionData(clickedNodes[i], clickedNodes[i] ,response, timeStamp);

                      // create hidden layer (Dynamic)
                      var form = document.getElementById('hidden');
                      var div = document.createElement('div');
                      var inputElements1 = document.createElement('input');
                      inputElements1.setAttribute(\"type\", \"hidden\");
                      inputElements1.setAttribute(\"name\", \"node\");
                      //inputElements1.setAttribute(\"name\", \"nodeArray[\" + edgeArray[i][0] + \"][\" + edgeArray[i][1] + \"]\");
                      inputElements1.setAttribute(\"value\", nodeclicked.id);
                      // inputElements1.innerHTML =  '<input type=\"hidden\" name=\"nodeArray[' + edgeArray[i][0] + '][' + edgeArray[i][1] + ']\" value=\"' + edgeArray[i] + '\"/>'
                      div.appendChild(inputElements1);
                      form.appendChild(div);
                      // create hidden layer (Dynamic) [End] //

                      // create hidden layer (Dynamic) \"time stamp\"
                      var inputElements2 = document.createElement('input');
                      inputElements2.setAttribute(\"type\", \"hidden\");
                      inputElements2.setAttribute(\"name\", \"timeStamp\");
                      //inputElements1.setAttribute(\"name\", \"nodeArray[\" + edgeArray[i][0] + \"][\" + edgeArray[i][1] + \"]\");
                      inputElements2.setAttribute(\"value\", timeStamp);
                      // inputElements1.innerHTML =  '<input type=\"hidden\" name=\"nodeArray[' + edgeArray[i][0] + '][' + edgeArray[i][1] + ']\" value=\"' + edgeArray[i] + '\"/>'
                      div.appendChild(inputElements2);
                      form.appendChild(div);
                      // create hidden layer (Dynamic) [End] //

                      // create hidden layer (Dynamic) 'responses'
                      var inputElements3 = document.createElement('input');
                      inputElements3.setAttribute(\"type\", \"hidden\");
                      inputElements3.setAttribute(\"name\", \"response\");
                      //inputElements1.setAttribute(\"name\", \"nodeArray[\" + edgeArray[i][0] + \"][\" + edgeArray[i][1] + \"]\");
                      inputElements3.setAttribute(\"value\", response);
                      // inputElements1.innerHTML =  '<input type=\"hidden\" name=\"nodeArray[' + edgeArray[i][0] + '][' + edgeArray[i][1] + ']\" value=\"' + edgeArray[i] + '\"/>'
                      div.appendChild(inputElements3);
                      form.appendChild(div);
                      // create hidden layer (Dynamic) [End] //

                      // create hidden layer (Dynamic)
                      var inputElements4 = document.createElement('input');
                      inputElements4.setAttribute(\"type\", \"hidden\");
                      inputElements4.setAttribute(\"name\", \"nodePosition\");
                      inputElements4.setAttribute(\"value\", node);

                      div.appendChild(inputElements4);
                      form.appendChild(div);
                      // create hidden layer (Dynamic) [End] //
                      return false;

                      }

                      // JSON array
                      node = nodeclicked.id
                      timerArray.push(timeStamp);
                      console.log(timerArray);
                      alert('You cant go here!');
                      response = \"incorrect\";
                      captureActionData(node, edgeArray[i], response, timeStamp);

                      // create hidden layer (Dynamic)
                      var form = document.getElementById('hidden');
                      var div = document.createElement('div');
                      var inputElements1 = document.createElement('input');
                      inputElements1.setAttribute(\"type\", \"hidden\");
                      inputElements1.setAttribute(\"name\", \"node\");
                      //inputElements1.setAttribute(\"name\",\"nodeArray[\" + edgeArray[i][0] + \"][\" + edgeArray[i][1] + \"]\");
                      inputElements1.setAttribute(\"value\", edgeArray[i]);
                      // inputElements1.innerHTML =  '<input type=\"hidden\" name=\"nodeArray[' + edgeArray[i][0] + '][' + edgeArray[i][1] + ']\" value=\"' + edgeArray[i] + '\"/>'
                      div.appendChild(inputElements1);
                      form.appendChild(div);
                      // create hidden layer (Dynamic) [End] //

                      // create hidden layer (Dynamic) \"time stamp\"
                      var inputElements2 = document.createElement('input');
                      inputElements2.setAttribute(\"type\", \"hidden\");
                      inputElements2.setAttribute(\"name\", \"timeStamp\");
                      //inputElements1.setAttribute(\"name\", \"nodeArray[\" + edgeArray[i][0] + \"][\" + edgeArray[i][1] + \"]\");
                      inputElements2.setAttribute(\"value\", timeStamp);
                      // inputElements1.innerHTML =  '<input type=\"hidden\" name=\"nodeArray[' + edgeArray[i][0] + '][' + edgeArray[i][1] + ']\" value=\"' + edgeArray[i] + '\"/>'
                      div.appendChild(inputElements2);
                      form.appendChild(div);
                      // create hidden layer (Dynamic) [End] //

                      // create hidden layer (Dynamic) 'responses'
                      var inputElements3 = document.createElement('input');
                      inputElements3.setAttribute(\"type\", \"hidden\");
                      inputElements3.setAttribute(\"name\", \"response\");
                      //inputElements1.setAttribute(\"name\", \"nodeArray[\" + edgeArray[i][0] + \"][\" + edgeArray[i][1] + \"]\");
                      inputElements3.setAttribute(\"value\", response);
                      // inputElements1.innerHTML =  '<input type=\"hidden\" name=\"nodeArray[' + edgeArray[i][0] + '][' + edgeArray[i][1] + ']\" value=\"' + edgeArray[i] + '\"/>'
                      div.appendChild(inputElements3);
                      form.appendChild(div);
                      // create hidden layer (Dynamic) [End] //

                      // create hidden layer (Dynamic)
                      var inputElements4 = document.createElement('input');
                      inputElements4.setAttribute(\"type\", \"hidden\");
                      inputElements4.setAttribute(\"name\", \"nodePosition\");
                      inputElements4.setAttribute(\"value\", node);

                      div.appendChild(inputElements4);
                      form.appendChild(div);
                      // create hidden layer (Dynamic) [End] //

                      return false;




                      }
                      }
                      }
                      }

                      }

                      // we successfully clicked on something, so change colour of stuff (line and node)
                      if (isFirstNodeClicked == true){
                      nodeclicked.style.backgroundColor = 'red';
                      }

                      // Create getChildren(gets all the nodes including the node you click)
                      // changes the colour of everything except the node you click on
                      function getChildren(n, skipMe){
                      var r = [];
                      for ( ; n; n = n.nextSibling )
                      if ( n.nodeType == 1 && n != skipMe)
                      r.push( n );
                      return r;
                      };
                      // Create getsiblings(gets all the nodes except the node you click)
                      function getSiblings(n) {
                      return getChildren(n.parentNode.firstChild, n);
                      }


                      if (isFirstNodeClicked == false) { // the line doesnt change colour the first time

                      if (Number(nodeclicked.id) > Number(prevNodeId)) {
                      var elementname = prevNodeId+'_'+nodeclicked.id;
                      } else {
                      var elementname = nodeclicked.id+'_'+prevNodeId;
                      }

                      console.log('element_name = ' + elementname + 'clicked node = ' + nodeclicked.id + '; previous node = ' + prevNodeId);

                      // this breaks if there is no if-statement on line 302. Because it doesn't continue onwards and reset nodeclicked into prevNode.
                      // So you need the if statement. Once that is true, then it continues on with the code.
                      myelement = document.getElementById(elementname);
                      myelement.style.stroke = '#FF0000';


                      }

                      // set things up for next time
                      prevNodeId = nodeclicked.id;
                      isFirstNodeClicked=false;

                      if (isFirstNodeClicked == false){
                      nodeclicked.style.borderColor = 'red';
                      }


                      // suming up the score of the person based on the black dotes crossed
                      var sum = 0;
                      for(i =0; i < edgeArray.length; i++){
                      //console.log(edgeArray[i][3]);
                      sum  += edgeArray[i][3];
                      }
                      console.log(sum); // 6


                      // check stopping rules
                      var completedGame = true;
                      if(sum == ",maxScore,"){ ")
return(javaScript)
}

javaScriptNoTimer <- function(colourNodePosition, maxScore){
    javaScript<- paste0("
   ////////// Timer [Start] //////

                        var time = 0;
                        var running = 0;
                        var timeStamp;

                        function startPause()
                        {
                        if(running == 0){
                        running = 1;
                        increment();
                        //document.getElementById(\"startPause\").innerHTML = \"Pause\";
                        }else{
                        running = 0;
                        //document.getElementById(\"startPause\").innerHTML = \"Resume\";
                        }
                        }


                        function increment()
                        {
                        if(running == 1){
                        setTimeout(function(){
                        time++;
                        var hours = Math.floor(time/10/60/60);
                        var mins = Math.floor(time/10/60 % 60);
                        var secs = Math.floor(time/10 % 60);
                        var tenths = time % 10;

                        if(hours < 10) {
                        hours = \"0\" + hours;
                    }

                    if(mins < 10){
                    mins = \"0\" + mins;
                    }
                    if(secs < 10){
                    secs = \"0\" + secs;
                    }
                    document.getElementById(\"output\").innerHTML = hours + \":\" + mins + \":\" + secs + \":\" + \"0\" + tenths;
                    timeStamp =  hours + \":\" + mins + \":\" + secs + \":\" + \"0\" + tenths;
                    increment();

                    },100);
                    }
                    }

                    window.onload=startPause();

                    ///////////// Timer End ////////////

                        var isFirstNodeClicked=true;
                        var prevNodeId=null;
                        var nodeArray = [];
                        var timerArray = [];
                        var responseArray =[];
                        var node = [];
                        var clickedNodes=[];
                        function captureActionData(node, nodeArray, response, timeStamp){
                        var captureObject = {}; // Create empty JSON array
                        captureObject.node = node
                        captureObject.nodeArray = nodeArray;
                        captureObject.time = timeStamp;
                        captureObject.response = response;
                        responseArray.push(captureObject);
                        console.log(JSON.stringify(responseArray));
                        }



                        function nodeClick(nodeclicked) {
                        console.log('clickedNode = ' + nodeclicked.id + '; previous node = ' + prevNodeId);
                        //console.log(nodeclicked.id)
                        //console.log(edgeArray[0][1]);

                        var response ; // Recorded as right or wrong.

                        // Record first node ///////////
                        if (isFirstNodeClicked == true){
                        if(nodeclicked.id == edgeArray[1][0] && edgeArray[2][0]){
                        node = nodeclicked.id
                        response = \"correct\";
                        timerArray.push(timeStamp);
                        console.log(timerArray);
                        var nodeArray ;

                        captureActionData(node, 'NULL' ,response, timeStamp);

                        // create hidden layer (Dynamic)
                        var form = document.getElementById('hidden');
                        var div = document.createElement('div');
                        var inputElements1 = document.createElement('input');
                        inputElements1.setAttribute(\"type\", \"hidden\");
                        inputElements1.setAttribute(\"name\", \"node\");
                        //inputElements1.setAttribute(\"name\", \"nodeArray[\" + edgeArray[i][0] + \"][\" + edgeArray[i][1] + \"]\");
                        inputElements1.setAttribute(\"value\", node);
                        // inputElements1.innerHTML =  '<input type=\"hidden\" name=\"nodeArray[' + edgeArray[i][0] + '][' + edgeArray[i][1] + ']\" value=\"' + edgeArray[i] + '\"/>'
                        div.appendChild(inputElements1);
                        form.appendChild(div);
                        // create hidden layer (Dynamic) [End] //

                        // create hidden layer (Dynamic) 'timestamp'
                        var inputElements2 = document.createElement('input');
                        inputElements2.setAttribute(\"type\", \"hidden\");
                        inputElements2.setAttribute(\"name\", \"timeStamp\");
                        //inputElements1.setAttribute(\"name\", \"nodeArray[\" + edgeArray[i][0] + \"][\" + edgeArray[i][1] + \"]\");
                        inputElements2.setAttribute(\"value\", timeStamp);
                        // inputElements1.innerHTML =  '<input type=\"hidden\" name=\"nodeArray[' + edgeArray[i][0] + '][' + edgeArray[i][1] + ']\" value=\"' + edgeArray[i] + '\"/>'
                        div.appendChild(inputElements2);
                        form.appendChild(div);
                        // create hidden layer (Dynamic) [End] //

                        // create hidden layer (Dynamic) 'responses'
                        var inputElements3 = document.createElement('input');
                        inputElements3.setAttribute(\"type\", \"hidden\");
                        inputElements3.setAttribute(\"name\", \"response\");
                        //inputElements1.setAttribute(\"name\", \"nodeArray[\" + edgeArray[i][0] + \"][\" + edgeArray[i][1] + \"]\");
                        inputElements3.setAttribute(\"value\", response);
                        // inputElements1.innerHTML =  '<input type=\"hidden\" name=\"nodeArray[' + edgeArray[i][0] + '][' + edgeArray[i][1] + ']\" value=\"' + edgeArray[i] + '\"/>'
                        div.appendChild(inputElements3);
                        form.appendChild(div);
                        // create hidden layer (Dynamic) [End] //

                        // create hidden layer (Dynamic)
                        var inputElements4 = document.createElement('input');
                        inputElements4.setAttribute(\"type\", \"hidden\");
                        inputElements4.setAttribute(\"name\", \"nodePosition\");
                        inputElements4.setAttribute(\"value\", node);

                        div.appendChild(inputElements4);
                        form.appendChild(div);
                        // create hidden layer (Dynamic) [End] //


                        }else{
                        alert(\"Please start from the first node at the bottom\");
                        node = nodeclicked.id
                        response = \"notBottom\";
                        timerArray.push(timeStamp);
                        console.log(timerArray);
                        var nodeArray ;

                        captureActionData(node, 'NULL' ,response, timeStamp);

                        // create hidden layer (Dynamic)
                        var form = document.getElementById('hidden');
                        var div = document.createElement('div');
                        var inputElements1 = document.createElement('input');
                        inputElements1.setAttribute(\"type\", \"hidden\");
                        inputElements1.setAttribute(\"name\", \"node\");
                        //inputElements1.setAttribute(\"name\", \"nodeArray[\" + edgeArray[i][0] + \"][\" + edgeArray[i][1] + \"]\");
                        inputElements1.setAttribute(\"value\", node);
                        // inputElements1.innerHTML =  '<input type=\"hidden\" name=\"nodeArray[' + edgeArray[i][0] + '][' + edgeArray[i][1] + ']\" value=\"' + edgeArray[i] + '\"/>'
                        div.appendChild(inputElements1);
                        form.appendChild(div);
                        // create hidden layer (Dynamic) [End] //

                        // create hidden layer (Dynamic) 'timestamp'
                        var inputElements2 = document.createElement('input');
                        inputElements2.setAttribute(\"type\", \"hidden\");
                        inputElements2.setAttribute(\"name\", \"timeStamp\");
                        //inputElements1.setAttribute(\"name\", \"nodeArray[\" + edgeArray[i][0] + \"][\" + edgeArray[i][1] + \"]\");
                        inputElements2.setAttribute(\"value\", timeStamp);
                        // inputElements1.innerHTML =  '<input type=\"hidden\" name=\"nodeArray[' + edgeArray[i][0] + '][' + edgeArray[i][1] + ']\" value=\"' + edgeArray[i] + '\"/>'
                        div.appendChild(inputElements2);
                        form.appendChild(div);
                        // create hidden layer (Dynamic) [End] //

                        // create hidden layer (Dynamic) 'responses'
                        var inputElements3 = document.createElement('input');
                        inputElements3.setAttribute(\"type\", \"hidden\");
                        inputElements3.setAttribute(\"name\", \"response\");
                        //inputElements1.setAttribute(\"name\", \"nodeArray[\" + edgeArray[i][0] + \"][\" + edgeArray[i][1] + \"]\");
                        inputElements3.setAttribute(\"value\", response);
                        // inputElements1.innerHTML =  '<input type=\"hidden\" name=\"nodeArray[' + edgeArray[i][0] + '][' + edgeArray[i][1] + ']\" value=\"' + edgeArray[i] + '\"/>'
                        div.appendChild(inputElements3);
                        form.appendChild(div);
                        // create hidden layer (Dynamic) [End] //

                        // create hidden layer (Dynamic)
                        var inputElements4 = document.createElement('input');
                        inputElements4.setAttribute(\"type\", \"hidden\");
                        inputElements4.setAttribute(\"name\", \"nodePosition\");
                        inputElements4.setAttribute(\"value\", node);

                        div.appendChild(inputElements4);
                        form.appendChild(div);
                        return;
                        }

                        }


                        /// Second node onwards ////////
                        if (isFirstNodeClicked == false) {
                        var pathExists = false;
                        for(var i=0; i<edgeArray.length; i++) {
                        //alert(edgeArray[i][0]);
                        //alert(edgeArray[i][1]);
                        if((edgeArray[i][0] == nodeclicked.id && edgeArray[i][1] == prevNodeId) || (edgeArray[i][0] == prevNodeId && edgeArray[i][1] == nodeclicked.id)) {
                        if ((edgeArray[i][2] == 0)) {
                        edgeArray[i][2] = 1;",
                        colourNodePosition, "{
                        edgeArray[i][3] = 1;  // count total score
                        console.log(edgeArray[i]);
                        }

                        // JSON array
                        node = nodeclicked.id
                        response = \"correct\";
                        timerArray.push(timeStamp);
                        console.log(timerArray);
                        captureActionData(node, edgeArray[i], response, timeStamp);

                        // create hidden layer (Dynamic)
                        var form = document.getElementById('hidden');
                        var div = document.createElement('div');
                        var inputElements1 = document.createElement('input');
                        inputElements1.setAttribute(\"type\", \"hidden\");
                        inputElements1.setAttribute(\"name\", \"node\");
                        //inputElements1.setAttribute(\"name\", \"nodeArray[\" + edgeArray[i][0] + \"][\" + edgeArray[i][1] + \"]\");
                        inputElements1.setAttribute(\"value\", edgeArray[i]);
                        // inputElements1.innerHTML =  '<input type=\"hidden\" name=\"nodeArray[' + edgeArray[i][0] + '][' + edgeArray[i][1] + ']\" value=\"' + edgeArray[i] + '\"/>'
                        div.appendChild(inputElements1);
                        form.appendChild(div);
                        // create hidden layer (Dynamic) [End] //

                        // create hidden layer (Dynamic) 'time stamp'
                        var inputElements2 = document.createElement('input');
                        inputElements2.setAttribute(\"type\", \"hidden\");
                        inputElements2.setAttribute(\"name\", \"timeStamp\");
                        //inputElements1.setAttribute(\"name\", \"nodeArray[\" + edgeArray[i][0] + \"][\" + edgeArray[i][1] + \"]\");
                        inputElements2.setAttribute(\"value\", timeStamp);
                        // inputElements1.innerHTML =  '<input type=\"hidden\" name=\"nodeArray[' + edgeArray[i][0] + '][' + edgeArray[i][1] + ']\" value=\"' + edgeArray[i] + '\"/>'
                        div.appendChild(inputElements2);
                        form.appendChild(div);
                        // create hidden layer (Dynamic) [End] //

                        // create hidden layer (Dynamic) 'responses'
                        var inputElements3 = document.createElement('input');
                        inputElements3.setAttribute(\"type\", \"hidden\");
                        inputElements3.setAttribute(\"name\", \"response\");
                        //inputElements1.setAttribute(\"name\", \"nodeArray[\" + edgeArray[i][0] + \"][\" + edgeArray[i][1] + \"]\");
                        inputElements3.setAttribute(\"value\", response);
                        // inputElements1.innerHTML =  '<input type=\"hidden\" name=\"nodeArray[' + edgeArray[i][0] + '][' + edgeArray[i][1] + ']\" value=\"' + edgeArray[i] + '\"/>'
                        div.appendChild(inputElements3);
                        form.appendChild(div);
                        // create hidden layer (Dynamic) [End] //

                        // create hidden layer (Dynamic)
                        var inputElements4 = document.createElement('input');
                        inputElements4.setAttribute(\"type\", \"hidden\");
                        inputElements4.setAttribute(\"name\", \"nodePosition\");
                        inputElements4.setAttribute(\"value\", node);

                        div.appendChild(inputElements4);
                        form.appendChild(div);
                        // create hidden layer (Dynamic) [End] //

                        pathExists = true; //hooray path exists :P
                        // prevMyNode = document.getElementById(prevNodeId); // Put into new variable
                        //prevMyNode.style.backgroundColor = '#FFCF75';  // changes the color of the element of previous node ID

                        }

                        //unidirection
                        if(edgeArray[i][4] == 2 && Number(nodeclicked.id) < prevNodeId){
                        edgeArray[i][2] = 0;
                        node = nodeclicked.id
                        timerArray.push(timeStamp);
                        console.log(timerArray);
                        alert(\"Wrong direction\");
                        response = \"wrongDirection\";
                        captureActionData(node, edgeArray[i], response, timeStamp);
                        // create hidden layer (Dynamic)
                        var form = document.getElementById('hidden');
                        var div = document.createElement('div');
                        var inputElements1 = document.createElement('input');
                        inputElements1.setAttribute(\"type\", \"hidden\");
                        inputElements1.setAttribute(\"name\", \"node\");
                        //inputElements1.setAttribute(\"name\", \"nodeArray[\" + edgeArray[i][0] + \"][\" + edgeArray[i][1] + \"]\");
                        inputElements1.setAttribute(\"value\", edgeArray[i]);
                        // inputElements1.innerHTML =  '<input type=\"hidden\" name=\"nodeArray[' + edgeArray[i][0] + '][' + edgeArray[i][1] + ']\" value=\"' + edgeArray[i] + '\"/>'
                        div.appendChild(inputElements1);
                        form.appendChild(div);
                        // create hidden layer (Dynamic) [End] //

                        // create hidden layer (Dynamic) \"time stamp\"
                        var inputElements2 = document.createElement('input');
                        inputElements2.setAttribute(\"type\", \"hidden\");
                        inputElements2.setAttribute(\"name\", \"timeStamp\");
                        //inputElements1.setAttribute(\"name\", \"nodeArray[\" + edgeArray[i][0] + \"][\" + edgeArray[i][1] + \"]\");
                        inputElements2.setAttribute(\"value\", timeStamp);
                        // inputElements1.innerHTML =  '<input type=\"hidden\" name=\"nodeArray[' + edgeArray[i][0] + '][' + edgeArray[i][1] + ']\" value=\"' + edgeArray[i] + '\"/>'
                        div.appendChild(inputElements2);
                        form.appendChild(div);
                        // create hidden layer (Dynamic) [End] //

                        // create hidden layer (Dynamic) 'responses'
                        var inputElements3 = document.createElement('input');
                        inputElements3.setAttribute(\"type\", \"hidden\");
                        inputElements3.setAttribute(\"name\", \"response\");
                        //inputElements1.setAttribute(\"name\", \"nodeArray[\" + edgeArray[i][0] + \"][\" + edgeArray[i][1] + \"]\");
                        inputElements3.setAttribute(\"value\", response);
                        // inputElements1.innerHTML =  '<input type=\"hidden\" name=\"nodeArray[' + edgeArray[i][0] + '][' + edgeArray[i][1] + ']\" value=\"' + edgeArray[i] + '\"/>'
                        div.appendChild(inputElements3);
                        form.appendChild(div);
                        // create hidden layer (Dynamic) [End] //

                        // create hidden layer (Dynamic)
                        var inputElements4 = document.createElement('input');
                        inputElements4.setAttribute(\"type\", \"hidden\");
                        inputElements4.setAttribute(\"name\", \"nodePosition\");
                        inputElements4.setAttribute(\"value\", node);

                        div.appendChild(inputElements4);
                        form.appendChild(div);
                        // create hidden layer (Dynamic) [End] //


                        //prevMyNode.style.backgroundColor = '#218868';
                        return false;
                        }

                        }

                        }



                        //check if the person tried to take a path that don't exists or repeated
                        if (pathExists == false) {
                        for(var i=0; i<edgeArray.length; i++) {
                        if((edgeArray[i][0] !== nodeclicked.id && edgeArray[i][1] !== prevNodeId) || (edgeArray[i][0] !== prevNodeId && edgeArray[i][1] !== nodeclicked.id)) {
                        if ((edgeArray[i][2] == 0)) { //checks that the user hasn't taken this path already
                        edgeArray[i][2] = 0; //makes it so that they can't take that path again
                        console.log(edgeArray[i]);
                        console.log(clickedNodes.length);


                        // Repeated nodes
                        if(nodeclicked.id == prevNodeId) {
                        timerArray.push(timeStamp);
                        console.log(timerArray);
                        alert(\"This is repeated\");
                        response = \"repeated\";
                        captureActionData(clickedNodes[i], clickedNodes[i] ,response, timeStamp);

                        // create hidden layer (Dynamic)
                        var form = document.getElementById('hidden');
                        var div = document.createElement('div');
                        var inputElements1 = document.createElement('input');
                        inputElements1.setAttribute(\"type\", \"hidden\");
                        inputElements1.setAttribute(\"name\", \"node\");
                        //inputElements1.setAttribute(\"name\", \"nodeArray[\" + edgeArray[i][0] + \"][\" + edgeArray[i][1] + \"]\");
                        inputElements1.setAttribute(\"value\", nodeclicked.id);
                        // inputElements1.innerHTML =  '<input type=\"hidden\" name=\"nodeArray[' + edgeArray[i][0] + '][' + edgeArray[i][1] + ']\" value=\"' + edgeArray[i] + '\"/>'
                        div.appendChild(inputElements1);
                        form.appendChild(div);
                        // create hidden layer (Dynamic) [End] //

                        // create hidden layer (Dynamic) \"time stamp\"
                        var inputElements2 = document.createElement('input');
                        inputElements2.setAttribute(\"type\", \"hidden\");
                        inputElements2.setAttribute(\"name\", \"timeStamp\");
                        //inputElements1.setAttribute(\"name\", \"nodeArray[\" + edgeArray[i][0] + \"][\" + edgeArray[i][1] + \"]\");
                        inputElements2.setAttribute(\"value\", timeStamp);
                        // inputElements1.innerHTML =  '<input type=\"hidden\" name=\"nodeArray[' + edgeArray[i][0] + '][' + edgeArray[i][1] + ']\" value=\"' + edgeArray[i] + '\"/>'
                        div.appendChild(inputElements2);
                        form.appendChild(div);
                        // create hidden layer (Dynamic) [End] //

                        // create hidden layer (Dynamic) 'responses'
                        var inputElements3 = document.createElement('input');
                        inputElements3.setAttribute(\"type\", \"hidden\");
                        inputElements3.setAttribute(\"name\", \"response\");
                        //inputElements1.setAttribute(\"name\", \"nodeArray[\" + edgeArray[i][0] + \"][\" + edgeArray[i][1] + \"]\");
                        inputElements3.setAttribute(\"value\", response);
                        // inputElements1.innerHTML =  '<input type=\"hidden\" name=\"nodeArray[' + edgeArray[i][0] + '][' + edgeArray[i][1] + ']\" value=\"' + edgeArray[i] + '\"/>'
                        div.appendChild(inputElements3);
                        form.appendChild(div);
                        // create hidden layer (Dynamic) [End] //

                        // create hidden layer (Dynamic)
                        var inputElements4 = document.createElement('input');
                        inputElements4.setAttribute(\"type\", \"hidden\");
                        inputElements4.setAttribute(\"name\", \"nodePosition\");
                        inputElements4.setAttribute(\"value\", node);

                        div.appendChild(inputElements4);
                        form.appendChild(div);
                        // create hidden layer (Dynamic) [End] //
                        return false;

                        }

                        // JSON array
                        node = nodeclicked.id
                        timerArray.push(timeStamp);
                        console.log(timerArray);
                        alert('You cant go here!');
                        response = \"incorrect\";
                        captureActionData(node, edgeArray[i], response, timeStamp);

                        // create hidden layer (Dynamic)
                        var form = document.getElementById('hidden');
                        var div = document.createElement('div');
                        var inputElements1 = document.createElement('input');
                        inputElements1.setAttribute(\"type\", \"hidden\");
                        inputElements1.setAttribute(\"name\", \"node\");
                        //inputElements1.setAttribute(\"name\",\"nodeArray[\" + edgeArray[i][0] + \"][\" + edgeArray[i][1] + \"]\");
                        inputElements1.setAttribute(\"value\", edgeArray[i]);
                        // inputElements1.innerHTML =  '<input type=\"hidden\" name=\"nodeArray[' + edgeArray[i][0] + '][' + edgeArray[i][1] + ']\" value=\"' + edgeArray[i] + '\"/>'
                        div.appendChild(inputElements1);
                        form.appendChild(div);
                        // create hidden layer (Dynamic) [End] //

                        // create hidden layer (Dynamic) \"time stamp\"
                        var inputElements2 = document.createElement('input');
                        inputElements2.setAttribute(\"type\", \"hidden\");
                        inputElements2.setAttribute(\"name\", \"timeStamp\");
                        //inputElements1.setAttribute(\"name\", \"nodeArray[\" + edgeArray[i][0] + \"][\" + edgeArray[i][1] + \"]\");
                        inputElements2.setAttribute(\"value\", timeStamp);
                        // inputElements1.innerHTML =  '<input type=\"hidden\" name=\"nodeArray[' + edgeArray[i][0] + '][' + edgeArray[i][1] + ']\" value=\"' + edgeArray[i] + '\"/>'
                        div.appendChild(inputElements2);
                        form.appendChild(div);
                        // create hidden layer (Dynamic) [End] //

                        // create hidden layer (Dynamic) 'responses'
                        var inputElements3 = document.createElement('input');
                        inputElements3.setAttribute(\"type\", \"hidden\");
                        inputElements3.setAttribute(\"name\", \"response\");
                        //inputElements1.setAttribute(\"name\", \"nodeArray[\" + edgeArray[i][0] + \"][\" + edgeArray[i][1] + \"]\");
                        inputElements3.setAttribute(\"value\", response);
                        // inputElements1.innerHTML =  '<input type=\"hidden\" name=\"nodeArray[' + edgeArray[i][0] + '][' + edgeArray[i][1] + ']\" value=\"' + edgeArray[i] + '\"/>'
                        div.appendChild(inputElements3);
                        form.appendChild(div);
                        // create hidden layer (Dynamic) [End] //

                        // create hidden layer (Dynamic)
                        var inputElements4 = document.createElement('input');
                        inputElements4.setAttribute(\"type\", \"hidden\");
                        inputElements4.setAttribute(\"name\", \"nodePosition\");
                        inputElements4.setAttribute(\"value\", node);

                        div.appendChild(inputElements4);
                        form.appendChild(div);
                        // create hidden layer (Dynamic) [End] //

                        return false;




                        }
                        }
                        }
                        }

                        }

                        // we successfully clicked on something, so change colour of stuff (line and node)
                        if (isFirstNodeClicked == true){
                        nodeclicked.style.backgroundColor = 'red';
                        }

                        // Create getChildren(gets all the nodes including the node you click)
                        // changes the colour of everything except the node you click on
                        function getChildren(n, skipMe){
                        var r = [];
                        for ( ; n; n = n.nextSibling )
                        if ( n.nodeType == 1 && n != skipMe)
                        r.push( n );
                        return r;
                        };
                        // Create getsiblings(gets all the nodes except the node you click)
                        function getSiblings(n) {
                        return getChildren(n.parentNode.firstChild, n);
                        }


                        if (isFirstNodeClicked == false) { // the line doesnt change colour the first time

                        if (Number(nodeclicked.id) > Number(prevNodeId)) {
                        var elementname = prevNodeId+'_'+nodeclicked.id;
                        } else {
                        var elementname = nodeclicked.id+'_'+prevNodeId;
                        }

                        console.log('element_name = ' + elementname + 'clicked node = ' + nodeclicked.id + '; previous node = ' + prevNodeId);

                        // this breaks if there is no if-statement on line 302. Because it doesn't continue onwards and reset nodeclicked into prevNode.
                        // So you need the if statement. Once that is true, then it continues on with the code.
                        myelement = document.getElementById(elementname);
                        myelement.style.stroke = '#FF0000';


                        }

                        // set things up for next time
                        prevNodeId = nodeclicked.id;
                        isFirstNodeClicked=false;

                        if (isFirstNodeClicked == false){
                        nodeclicked.style.borderColor = 'red';
                        }


                        // suming up the score of the person based on the black dotes crossed
                        var sum = 0;
                        for(i =0; i < edgeArray.length; i++){
                        //console.log(edgeArray[i][3]);
                        sum  += edgeArray[i][3];
                        }
                        console.log(sum); // 6


                        // check stopping rules
                        var completedGame = true;
                        if(sum == ",maxScore,"){ ")
    return(javaScript)
  }
# maxScore
javaScriptTwoC4 <-function(finalRow){
javaScript2<- paste0(
  finalRow,"{
  console.log(edgeArray[4][1]);
  alert('Congratulations! You solved the puzzle!');

  var form = document.getElementById('hidden2');
  var div = document.createElement('div');
  var inputElements4 = document.createElement('input');
  inputElements4.setAttribute(\"type\", \"hidden\");
  inputElements4.setAttribute(\"name\", \"completedGame\");
  inputElements4.setAttribute(\"value\", 1);
  div.appendChild(inputElements4);
  form.appendChild(div);

  var form = document.getElementById('hidden3');
  var div = document.createElement('div');
  var inputElements5 = document.createElement('input');
  inputElements5.setAttribute(\"type\", \"hidden\");
  inputElements5.setAttribute(\"name\", \"totalGoldScore\");
  inputElements5.setAttribute(\"value\", sum);
  div.appendChild(inputElements5);
  form.appendChild(div);


 clearTimeout(timerHandle);
  test.submit('next'); // concerto
  return true; // stop the script.
  }

  }else{",
                      finalRow,"{
  console.log(edgeArray[4][1]);
  alert(\"Sorry, that was not the maximum score\");

  var form = document.getElementById('hidden2');
  var div = document.createElement('div');
  var inputElements4 = document.createElement('input');
  inputElements4.setAttribute(\"type\", \"hidden\");
  inputElements4.setAttribute(\"name\", \"completedGame\");
  inputElements4.setAttribute(\"value\", 0);
  div.appendChild(inputElements4);
  form.appendChild(div);

  var form = document.getElementById('hidden3');
  var div = document.createElement('div');
  var inputElements5 = document.createElement('input');
  inputElements5.setAttribute(\"type\", \"hidden\");
  inputElements5.setAttribute(\"name\", \"totalGoldScore\");
  inputElements5.setAttribute(\"value\", sum);
  div.appendChild(inputElements5);
  form.appendChild(div);
  clearTimeout(timerHandle);
  test.submit('next'); // concerto
  return true; // stop the script.

                      }
                    }

  clickedNodes.push(nodeclicked.id); // array for repeated logic



                    }

  ")

return(javaScript2)
}

javaScriptTwoC5 <-function(finalRow){
  javaScript2<- paste0(
    finalRow,"{
    console.log(edgeArray[4][1]);
    alert('Congratulations! You solved the puzzle!');

    var form = document.getElementById('hidden2');
    var div = document.createElement('div');
    var inputElements4 = document.createElement('input');
    inputElements4.setAttribute(\"type\", \"hidden\");
    inputElements4.setAttribute(\"name\", \"completedGame\");
    inputElements4.setAttribute(\"value\", 1);
    div.appendChild(inputElements4);
    form.appendChild(div);

    var form = document.getElementById('hidden3');
    var div = document.createElement('div');
    var inputElements5 = document.createElement('input');
    inputElements5.setAttribute(\"type\", \"hidden\");
    inputElements5.setAttribute(\"name\", \"totalGoldScore\");
    inputElements5.setAttribute(\"value\", sum);
    div.appendChild(inputElements5);
    form.appendChild(div);


    clearTimeout(timerHandle);
    testRunner.submitView('next'); // concerto
    return true; // stop the script.
}

}else{",
                      finalRow,"{
console.log(edgeArray[4][1]);
alert(\"Sorry, that was not the maximum score\");

var form = document.getElementById('hidden2');
var div = document.createElement('div');
var inputElements4 = document.createElement('input');
inputElements4.setAttribute(\"type\", \"hidden\");
inputElements4.setAttribute(\"name\", \"completedGame\");
inputElements4.setAttribute(\"value\", 0);
  div.appendChild(inputElements4);
  form.appendChild(div);

  var form = document.getElementById('hidden3');
  var div = document.createElement('div');
  var inputElements5 = document.createElement('input');
  inputElements5.setAttribute(\"type\", \"hidden\");
  inputElements5.setAttribute(\"name\", \"totalGoldScore\");
  inputElements5.setAttribute(\"value\", sum);
  div.appendChild(inputElements5);
  form.appendChild(div);
  clearTimeout(timerHandle);
  testRunner.submitView('next'); // concerto
  return true; // stop the script.

                      }
                    }

  clickedNodes.push(nodeclicked.id); // array for repeated logic



                    }

  ")

  return(javaScript2)
                      }

