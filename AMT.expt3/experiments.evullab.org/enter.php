<?php
require_once('countrylist.php');
$condition = rand(-1,1);
?>

<!DOCTYPE html> 
<html>
    <head>
	<title>Number estimation experiment</title>
	<script type="text/javascript" src="jquery.js"></script>
    <script type="text/javascript" src="colorconversion.js"></script>
	<script type="text/javascript">
	var WIDTH;
	var HEIGHT;
	var ndots = 100;
	var R = 5;
	var x = Array();
	var y = Array();
	var ctx;
	
	function showdots(){
		ctx.clearRect(0,0,WIDTH,HEIGHT);
		for(var i = 0; i<ndots; i++){
			circle(x[i],y[i],R,0, 0, 0);
		}
	}
	
	function init(){
		$("#edscanvas").focus();
		ctx = $('#edscanvas')[0].getContext("2d");
		WIDTH = $('#edscanvas')[0].width;
		HEIGHT = $('#edscanvas')[0].height;
		
		for(var i=0; i<ndots; i++){
			x[i] = Math.random()*(WIDTH-2*R)+R;
			y[i] = Math.random()*(HEIGHT-2*R)+R;
		}
		$("#response").focus();
		showdots();
	}
	
	
	function circle(x,y,r,cr,cg,cb){
		var col = "rgba("+cr+","+cg+","+cb+",1)";
		ctx.fillStyle = col;
		ctx.beginPath();
		ctx.arc(x,y,r,0,Math.PI*2, true);
		ctx.closePath();
		ctx.fill();
	}
	
	</script>
	
	<script type="text/javascript">
	function updateCountry(){
	 var e = document.getElementById("selCountry"); 
	 var country=e.options[e.selectedIndex].text;
	 document.getElementById('country').value=country;
	  if((country=='Select Country')){
	 	document.getElementById('sub').disabled=true; }


	 if(!(country=='Select Country')){
	 	document.getElementById('sub').disabled=false; }
	 }

	</script>
	</head>
	<body style='background-color:#55AADD'>
		<div align="center">
			<div align="left" style="border:1px dotted black;padding:1em;width:600px;">        
					<p   >In this HIT you will have to estimate the number of dots that flashed on the screen.  On some trials we might give you feedback (that is: we might tell you how many dots there really were).  On other trials we will only give you your score.  The closer you get to the correct answer, the better your score, and the higher your total score at the end, the bigger your bonus!  This experiment should last about 10 minutes.</p>

					<p> Press <b>Start Tutorial </b> below to see an example of the HIT.<br> 
						For this example the display of dots will remain onscreen for longer than they will in the real experiment. </p>

					<p> Once you see the dots appear, just type in your guess (using digits) and press Enter/Return. (in this demonstration, your response will not be submitted.)</p>


				</div>	
				
				<button slign="middle" type="button" id="testspin" onclick="init()" ><b>Start Tutorial</b></button>
				

				<div align="center" style="padding:1em">
					
					<canvas id="edscanvas" width="600" height="600" style="background-color:white;border: 1px solid black;">
						Your browser does not support the HTML5 canvas tag. Please do not take this HIT.
					</canvas>
					<br>
					<strong>How many dots were there? </strong> 
					<input type=text name='response' id="response" style='background-color:#FFFFFF;border: 1px solid black;'> (press return to submit)<br>
					<em>(note, in this demonstration, your response will not be submitted!)</em>
				</div>
				<div align="center" style="border:1px dotted black;padding:1em;width:600px;"> 
					<p>	If you are ready to begin the real experiment, proceed below, but before we start, please tell us: what country are you currently in?</p>

					<select onmouseout="updateCountry()" id='selCountry'  style="width:200px" > <?=$countrylist?> </select>

					<form id="record" action="display.php" method="post">

						<input type="hidden" id="turkerId" name="workerId" value="<?=$_REQUEST['workerId']?>"> 
						<input type="hidden" id="turkSubmitTo" name="turkSubmitTo" value="<?=$_REQUEST['turkSubmitTo']?>"> 
						<input type="hidden" id="assignmentId" name="assignmentId" value="<?=$_REQUEST['assignmentId'] ?>"> 
						<input type="hidden" id="hitId" name="hitId" value="<?=$_REQUEST['hitId'] ?>">

						<input type="hidden" id="country" name="country" value=0>
						<input type="hidden" name="score" id="score" value="0" />
						<input type=hidden name='trialn' id="trialn" value="0">
						<input type=hidden name='condition' id="condition" value="<?=$condition?>">
						<input type=hidden name='phase' id="phase" value="<0">

						<input type="submit" name="sub"  value="To Quiz" class="sub" id="sub" align="center" <?php if ($_REQUEST['workerId'] == "") { echo 'onclick="alert(\'Please accept this HIT before continuing.\'); return false;"'; } ?> disabled/>

					</form>
				</div>
			</div>
	</body>
</html>
