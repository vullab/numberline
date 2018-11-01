<?php
require_once('config.php');
$conn=mysql_connect(DB_SERVER_WEB, DB_USER_WEB, DB_PASS_WEB);	
mysql_select_db(DB_DATABASE_WEB);
$IPaddress = $_SERVER['REMOTE_ADDR'];

if($_REQUEST['submitted']=='yes')
{
	$Tsub = array();
	$Tsub['workerId'] = $_REQUEST['workerId'];
	$Tsub['assignmentId'] = $_REQUEST['assignmentId'];
	$Tsub['hitId'] = $_REQUEST['hitId'];
	$Tsub['curtime'] = time();
	$Tsub['OSName'] = $_REQUEST['OSName'];
	$Tsub['browserName'] = $_REQUEST['browserName'];
	$Tsub['country'] = $_REQUEST['country'];
	$Tsub['IP'] = $_REQUEST['IP'];
	$Tsub['trialn'] = $_REQUEST['trialn'];
	$Tsub['shown'] = $_REQUEST['shown'];
	$Tsub['response'] = $_REQUEST['response'];
	$Tsub['feedback'] = $_REQUEST['feedback'];
	$Tsub['responseTime'] = $_REQUEST['responsetime'];
	$Tsub['fbcondition'] = $_REQUEST['condition'];
	$Tsub['phase'] = $_REQUEST['phase'];
	$Tsub['score'] = $_REQUEST['score'];
	
	$colnames = '(';
	$colvals = '(';
	$terms = count($Tsub);
	foreach($Tsub as $k => $v)
	{	
		$terms--;
		$colnames .= $k;
		$colvals .= "'" . $v ."'";
		if($terms){
			$colnames .= ', ';
			$colvals .= ', ';
		}
	}
	$colnames .= ")";
	$colvals .= ")";
	
	$query = "INSERT INTO " . DB_TABLE . " " . $colnames . " VALUES " . $colvals . ";";
	$result = mysql_query($query,$conn) or die('failed on ' . $query);
}	

$curscore = $_REQUEST['score'];
$curtrial = $_REQUEST['trialn']+1;

$condition = $_REQUEST['condition'];
if($curtrial <= EXPT_p0n){
	$phase = -1;
} elseif($curtrial <= (EXPT_p0n + EXPT_p1n)){
	$phase = 1;
} elseif($curtrial <= (EXPT_p0n + EXPT_p1n + EXPT_p2n)){
	$phase = 2;
} elseif($curtrial <= (EXPT_p0n + EXPT_p1n + EXPT_p2n + EXPT_p3n)){
	$phase = 3;
} else {
	$phase = 0;
}

# choose a number to show.
$curshow = rand(8,500);

if($phase == -1){ // pre-training phase
	$curfeedback = $curshow;
} elseif($phase == 2){ // feedback phase
	if($curshow > EXPT_fbthresh){
		$curfeedback = EXPT_fbthresh + round(pow(10,(log10($curshow-EXPT_fbthresh)*(1+$condition*EXPT_cfb))));
	} else {
		$curfeedback = $curshow;		// to maintain illusion, give correct fb for small n.
	}
} else {
	$curfeedback = 0;
}

?>

<html>
	<head><title>My test</title>
		<!--[if IE]><script type="text/javascript" src="../excanvas.js"></script><![endif]-->
	    <script type="text/javascript" src="jquery.js"></script>
	    <script type="text/javascript" src="colorconversion.js"></script>
	    <script type="text/javascript" src="browserdetect.js"></script>
		<script type="text/javascript">
		var WIDTH;
		var HEIGHT;
		var R = 5;
		var firstsub = 1;
		var fbshown = 0;
		var ndots;
		var starttime;
		var score = <?=$curscore?>;
		var thisscore = 0;
		
		var x = Array();
		var y = Array();
		
		function init(n){
			ndots = n;
			ctx = $('#edscanvas')[0].getContext("2d");
			WIDTH = $('#edscanvas')[0].width;
			HEIGHT = $('#edscanvas')[0].height;
			
			// make dots positions.
			for(var i=0; i<ndots; i++){
				x[i] = Math.random()*(WIDTH-2*R)+R;
				y[i] = Math.random()*(HEIGHT-2*R)+R;
			}
			
			// set catcher for response.
			//$("#response").keyup(function(event){
			//    if(event.keyCode == 13){
			//        submitme();
			//    }
			//});
			
			$("#browserName").val(BrowserDetect.browser + "-" + BrowserDetect.version);
			$("#OSName").val(BrowserDetect.OS);
			
			setTimeout("start();",<?=EXPT_pretime?>);
		}
		
		function log10(val) {
		  return Math.log(val) / Math.log(10);
		}
		
		function showdots(){
			ctx.clearRect(0,0,WIDTH,HEIGHT);
			for(var i = 0; i<ndots; i++){
				circle(x[i],y[i],R,0, 0, 0);
			}
		}
		
		function circle(x,y,r,cr,cg,cb){
			var col = "rgba("+cr+","+cg+","+cb+",1)";
			ctx.fillStyle = col;
			ctx.beginPath();
			ctx.arc(x,y,r,0,Math.PI*2, true);
			ctx.closePath();
			ctx.fill();
		}
		
		function start(){
			showdots();
			
			starttime = new Date();
			
			$("#response").focus();
			
			setTimeout("ctx.clearRect(0,0,WIDTH,HEIGHT);",<?=($phase==-1)?EXPT_traintime:EXPT_showtime?>);
		}
		
		function showfeedback(){
			$("#feedback").html('There are <?=$curfeedback?> dots.');
			showdots();
		}
		function showscore(){
			$("#scoreshow").html('You got '+thisscore+' points.  Your score is now: ' + score);
		}
		
		function submitme(){
			//console.log("running submitme: " + fbshown);
			if(firstsub == 1){
				//console.log("firstsub: " + ((new Date()).getTime()-starttime.getTime()));
				// record response time, scores but only once.
				$("#responsetime").val((new Date()).getTime()-starttime.getTime());
				var ratio = $("#response").val()/<?=($curfeedback>0)?$curfeedback:$curshow?>;
				var logr = log10(ratio);
				var sqlogr = Math.pow(logr,2);
				var sqsclogr = sqlogr/<?=EXPT_accslope?>;
				var scaledscore = Math.pow(2, -sqsclogr);
				thisscore = Math.round(Math.max(0,10*scaledscore));
				// console.log($("#response").val());
				// console.log(ratio);
				// console.log(logr);
				// console.log(sqlogr);
				// console.log(sqsclogr);
				// console.log(thisscore);
				score = score + thisscore;
				$("#score").val(score);
				firstsub = 0;
			}
			$("#respform").hide();
			
			if(fbshown == 0 && <?=$curfeedback?> > 0){
				//console.log("fb show: " + ((new Date()).getTime()-starttime.getTime()));
				showfeedback();
				showscore();
				fbshown = 1;
				setTimeout("$('#respform').submit();",<?=EXPT_fbtime?>);
				return false;
			} else if(fbshown == 0) {	
				//console.log("score show: " + ((new Date()).getTime()-starttime.getTime()));
				showscore();
				fbshown = 1;
				setTimeout("$('#respform').submit();",<?=EXPT_fbtime?>);
				return false;
			} else {
				//console.log("submitting: " + ((new Date()).getTime()-starttime.getTime()));
				return true;
			}
		}
		// 
		</script>
		<style type="text/css">
		  canvas { border: 1px solid black; background-color:white}
		</style>
		</head>
		
		</head>
	<body onload='init(<?=$curshow?>);' style='background-color:#55AADD'>
	<div align="center">
		<span id="counter" name="counter">Trial <?=$curtrial?> of <?=(EXPT_p0n + EXPT_p1n + EXPT_p2n + EXPT_p3n)?>.  Current score: <?=$curscore?></span><br>
		<canvas id="edscanvas" width="600" height="600"></canvas><br>
		<form id='respform' name='respform' action='<? echo ($phase==0) ? "exit.php" : "display.php" ?>' method='POST' onSubmit="return submitme();">
			<strong>How many dots were there? </strong> 
			<input type=text name='response' id="response" style='background-color:#FFFFFF;border: 1px solid black;'> (press return to submit)
			
			<input type="hidden" id="submitted" name="submitted" value="yes">
			
			<input type=hidden name='shown' id="shownn" value="<?=$curshow?>">
			<input type=hidden name='feedback' id="fbnum" value="<?=$curfeedback?>">
			<input type=hidden name='responsetime' id="responsetime" value="">
			<input type=hidden name='trialn' id="trialn" value="<?=$curtrial?>">
			<input type=hidden name='condition' id="condition" value="<?=$condition?>">
			<input type=hidden name='phase' id="phase" value="<?=$phase?>">
			<input type=hidden name='score' id="score" value="<?=$curscore?>">
			
			
			<input type="hidden" id="workerId" name="workerId" value="<?=$_REQUEST['workerId']?>">
			<input type="hidden" id="turkSubmitTo" name="turkSubmitTo" value="<?=$_REQUEST['turkSubmitTo']?>">
			<input type="hidden" id="assignmentId" name="assignmentId" value="<?=$_REQUEST['assignmentId']?>">
			<input type="hidden" id="hitId" name="hitId" value="<?=$_REQUEST['hitId']?>">
			<input type="hidden" id="country" name="country" value="<?=$_REQUEST['country']?>">
			<input type="hidden" id="OSName" name="OSName" value="">
			<input type="hidden" id="browserName" name="browserName" value="">
			<input type="hidden" name="IP" id="IP" value="<?=$IPaddress?>" />
			
		</form>
		<span id="feedback" name="feedback" style='font-weight:bold;color:#DD0000;'></span><br>
		<span id="scoreshow" name="scoreshow" style='font-style:italic;'></span><br>
<?/*
		<span id="debugging" name="debugging">
			Shown: <?=$curshow?><br>
			Feedback: <?=$curfeedback?><br>
		</span>
*/?>
	</div>
	

	</body>
</html>

