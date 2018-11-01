<?php
$P=$_POST['P'];

$U=$_POST['U'];
$upCountry=$_POST['upCountry'];
$upCountry=str_replace(' ','',$upCountry);
$score=$_POST['score'];
$count=$_POST['count'];

$q1="If I bet 4 points on Black and 6 points on Red and the spinner lands on Red, how many points do I earn?";
$q2="The payoff table for Red is always on the left and the payoff table for Black is always on the right. True or false?";
$q4="In the picture above, the Turker has bet 7 points on Black and 3 on Red. True or false?";
$q3="In the picture above, if I bet on B and the wheel lands on Black, how many points do I gain/lose?";

?>

<html>

<body>

<style type="text/css">
#picbox {

		width: 28%;
		height: 28%

	}
</style>

<script>
var q1correct=false;
var q2correct=false;
var q3correct=false;
var q4correct=false;
function ret1(r){
	q2correct=(r=="F");
}

function ret2(r){
	q4correct=(r=="T");
}

function qallfun(){
	q1correct=(document.getElementById("q1").value==6);

	q3correct=(document.getElementById("q3").value==-25);
	


	if(q1correct  && q3correct && q2correct  && q4correct){
		document.getElementById("mturk_form").action="trial.php";
		alert("You got them all right! You can now begin the HIT!")
	}
	if(!(q1correct  && q3correct && q2correct  && q4correct )){

		var incorrect = '';
		if(!q1correct){
			incorrect=incorrect+'1 ';
		}
		if(!q2correct){
			incorrect=incorrect+'2 ';
		}
		if(!q4correct){
			incorrect=incorrect+'3 ';
		}
		if(!q3correct){
			incorrect=incorrect+'4 ';
		}
		alert("Question(s) " + incorrect +" is/are incorrect. Please read the instructions again.")
	}
}


</script>

	



	<div align="center">
	<p > Before beginning the HIT, you must correctly answer several questions about the instructions. Once you have answered all the questions, press Submit. If you answer any questions incorrectly, you will be returned to the instructions and then be asked to answer the questions again. </p>



	<br>

<div style="border:1px dotted black;padding:1em;">
	<p> <b>Question 1:</b> <?php echo $q1 ?> </p>
	<input type="text" id="q1" name="Please type a number:" value=""/>Please type a number
</div>
<br>

<div style="border:1px dotted black;padding:1em;">
	<p> <b>Question 2:</b> <?php echo $q2 ?> </p>
	<input type="radio" name="q2" id="q2" onclick="ret1('T')"/> True <br/>
	<input type="radio" name="q2" id="q2" onclick="ret1('F')"/> False <br/>

</div>
<br>

<div style="border:1px dotted black;padding:1em;">
	<img border="0" id="picbox" src="quiz3.jpg" style="width:30%;height:12%" />
	<p> <b>Question 3:</b> <?php echo $q4 ?> </p>
	<input type="radio" name="q4" id="q4" onclick="ret2('T')"/> True <br/>
	<input type="radio" name="q4" id="q4" onclick="ret2('F')"/> False <br/>

</div>
<br>

<div style="border:1px dotted black;padding:1em;">
	<img border="0" id="picbox" src="quiz4.jpg" style="width:250px;height:300px" />
	<p> <b>Question 4:</b> <?php echo $q3 ?> </p>
	<input type="text" id="q3" name="Please type a number:" value=""/> Please type a number

	
</div>
<br>

<form id="mturk_form" action="enter.php" method="post">

	<input type="hidden" id="turkerId" name="workerId" value="<?php echo $_REQUEST['workerId']?>">
	<input type="hidden" id="turkSubmitTo" name="turkSubmitTo" value="<?php echo $_REQUEST['turkSubmitTo']?>">
	<input type="hidden" id="assignmentId" name="assignmentId" value="<?php echo $_REQUEST['assignmentId'] ?>">
	<input type="hidden" id="hitId" name="hitId" value="<?php echo $_REQUEST['hitId'] ?>">
	
	<input type="hidden" id="upCountry" name="upCountry" value=<?php echo $upCountry; ?>>
	<input type="hidden" name="score" id="score" value=<?php echo $score ?> />
	<input type="hidden" name="U" id="U" value=<?php echo $U ?> />
	<input type="hidden" name="P" id="P" value=<?php echo $P ?> />
	<input type="hidden" id="browserName" name="browserName" value="">
	<input type="hidden" name="count" id="count" value=<?php echo $count ?> />
	<input id="submitButton" type="submit" style="width:5%;height:2.5%" name="Submit" value="Submit" onclick="qallfun()" >
</form>

<script type="text/javascript">


var BrowserDetect = { init: function () { this.browser = this.searchString(this.dataBrowser) || "An unknown browser"; this.version = this.searchVersion(navigator.userAgent) ||
this.searchVersion(navigator.appVersion) || "an unknown version"; this.OS = this.searchString(this.dataOS) || "an unknown OS"; }, searchString: function (data) { for (var
i=0;i<data.length;i++)	{ var dataString = data[i].string; var dataProp = data[i].prop; this.versionSearchString = data[i].versionSearch || data[i].identity; if (dataString) { if
(dataString.indexOf(data[i].subString) != -1) return data[i].identity; } else if (dataProp) return data[i].identity; } }, searchVersion: function (dataString) { var index =
dataString.indexOf(this.versionSearchString); if (index == -1) return; return parseFloat(dataString.substring(index+this.versionSearchString.length+1)); }, dataBrowser: [ { string:
navigator.userAgent, subString: "Chrome", identity: "Chrome" }, { 	string: navigator.userAgent, subString: "OmniWeb", versionSearch: "OmniWeb/", identity: "OmniWeb" }, { string:
navigator.vendor, subString: "Apple", identity: "Safari", versionSearch: "Version" }, { prop: window.opera, identity: "Opera", versionSearch: "Version" }, { string: navigator.vendor,
subString: "iCab", identity: "iCab" }, { string: navigator.vendor, subString: "KDE", identity: "Konqueror" }, { string: navigator.userAgent, subString: "Firefox", identity: "Firefox" },
{ string: navigator.vendor, subString: "Camino", identity: "Camino" }, {		// for newer Netscapes (6+) string: navigator.userAgent, subString: "Netscape", identity: "Netscape" }, {
string: navigator.userAgent, subString: "MSIE", identity: "Explorer", versionSearch: "MSIE" }, { string: navigator.userAgent, subString: "Gecko", identity: "Mozilla", versionSearch:
"rv" }, { 		// for older Netscapes (4-) string: navigator.userAgent, subString: "Mozilla", identity: "Netscape", versionSearch: "Mozilla" } ], dataOS : [ { string:
navigator.platform, subString: "Win", identity: "Windows" }, { string: navigator.platform, subString: "Mac", identity: "Mac" }, { string: navigator.userAgent, subString: "iPhone",
identity: "iPhone/iPod" }, { string: navigator.platform, subString: "Linux", identity: "Linux" } ]

}; BrowserDetect.init();

function navCheck(){ document.getElementById('browserName').value=BrowserDetect.browser+BrowserDetect.version; }

window.onload=navCheck();



</script>

</html>