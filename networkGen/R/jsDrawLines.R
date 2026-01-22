jsDrawLines <- function(){
jsDrawLines <- paste0("


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

                      // function reset()
                      // {
                      //     running = 0;
                      //     time = 0;
                      //     document.getElementById(\"startPause\").innerHTML = \"Start\";
                      //     document.getElementById(\"output\").innerHTML = \"00:00:00:00\";
                      // }

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

                      // <p style =\"width:150px; height:20px; background-color:#fff; border: 1px solid #999\" id=\"output\"></p>
                      //       <div id=\"controls\">
                      //       <button id=\"startPause\" onclick=\"startPause()\">Start</button>
                      //       <button onclick=\"reset()\">Reset</button>
                      //        </div>
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
    //  console.log(nodeclicked.id)


    var response ; // Recorded as right or wrong.

    // Record first node ///////////
    if (isFirstNodeClicked == true){
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


    // Here you can use the R JSON package and convert it to an R matrix
    // Because One button can only submit one type infor.

    }

    /// Second node onwards ////////
    if (isFirstNodeClicked == false) {
    var pathExists = false;
    for(var i=0; i<edgeArray.length; i++) {
    //alert(edgeArray[i][0]);
    //alert(edgeArray[i][1]);
    if((edgeArray[i][0] == nodeclicked.id && edgeArray[i][1] == prevNodeId) || (edgeArray[i][0] == prevNodeId && edgeArray[i][1] == nodeclicked.id)) {
    if ((edgeArray[i][2] == 0)) { //checks that the user hasn't taken this path already
    edgeArray[i][2] = 1; //makes it so that they can't take that path again
    console.log(edgeArray[i]);

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
    prevMyNode = document.getElementById(prevNodeId); // Put into new variable
    prevMyNode.style.backgroundColor = '#FFCF75';  // changes the color of the element of previous node ID

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


    // Repeated nodes
    for(var i = 0; i <clickedNodes.length; i++){
    if(nodeclicked.id == clickedNodes[i]) {
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

    return false;




    }
    }
    }
    }
    // if pathExists == same
    // response == \"repeated\"
    //  for(var i=0; i<edgeArray;length, i++){
    //       captureObject.node = edgeArray[i];
    //     }
    // captureObject.time = timeStamp;
    // captureObject.response = response;
    // responseArray.push(captureObject);
    // console.log(JSON.stringify(responseArray));

    }

    // we successfully clicked on something, so change colour of stuff (line and node)
    nodeclicked.style.backgroundColor = '#218868';

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


    // Gets information from the other non-clicked nodes.
    //   var siblings = getSiblings(nodeclicked);
    // for (var i=0; i<siblings.length; i++){
    //  siblings[i].style.backgroundColor = '#44c767';
    // }

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


    // check stopping rules
    // rule 1 - has user finished?
    var completedGame = true;
    for( i=0; i < edgeArray.length; i++){
    if ((edgeArray[i][2] == 0)) {
    completedGame = false;

    }

    }
    if (completedGame == true){
    alert('Congratulations! You solved the puzzle!');

    var form = document.getElementById('hidden2');
    var div = document.createElement('div');
    var inputElements4 = document.createElement('input');
    inputElements4.setAttribute(\"type\", \"hidden\");
    inputElements4.setAttribute(\"name\", \"completedGame\");
    inputElements4.setAttribute(\"value\", 1);
    div.appendChild(inputElements4);
    form.appendChild(div);

    //// Hidden Layer input (Static - You get the result only if you complete the test)
    //var form = document.getElementById('foo');
    //var inputElements1 = null;
    //for (var i=0; i< edgeArray.length; i++){
    //inputElements1 = inputElements1 + '<input type=\"hidden\" cname=\"nodeArray[' + edgeArray[i][0] + '][' + edgeArray[i][1] + ']\" value=\"' + edgeArray[i] + '\"/>'
    //return inputElements1;
    // }
    //  console.log(inputElements1);
    //form.innerHTML = inputElements1;
    // TODO: show next button
    //document.getElementById(\"foo\").submit();
    test.submit('next'); // concerto


    return true; // stop the script.
    }

    // check stopping rule
    // rule 2- dead end means you failed

    var possibleRoute = false;
    for(i =0; i < edgeArray.length; i++){
    if((edgeArray[i][0]==Number(nodeclicked.id) && edgeArray[i][2]==0)){
    possibleRoute = true;            }
    if((edgeArray[i][1]==Number(nodeclicked.id) && edgeArray[i][2]==0)){
    possibleRoute = true;
    }
    }
    if (possibleRoute == false){

    alert('You lost :(');
    var form = document.getElementById('hidden2');
    var div = document.createElement('div');
    var inputElements4 = document.createElement('input');
    inputElements4.setAttribute(\"type\", \"hidden\");
              inputElements4.setAttribute(\"name\", \"completedGame\");
              inputElements4.setAttribute(\"value\", 0);
              test.submit('next'); // concerto
              // document.getElementById(\"foo\").submit();
              return false;
            }


            clickedNodes.push(nodeclicked.id); // array for repeated logic



            }


      ")
return(jsDrawLines)
}


