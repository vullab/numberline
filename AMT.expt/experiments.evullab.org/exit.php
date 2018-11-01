<?php 

require_once('config.php');
$conn=mysql_connect(DB_SERVER_WEB, DB_USER_WEB, DB_PASS_WEB);	
mysql_select_db(DB_DATABASE_WEB);
$IPaddress = $_SERVER['REMOTE_ADDR'];

if($_REQUEST['submitted']=='yes')
{
	$Tsub = array();
	$Tsub['workerId'] = $_REQUEST['workerId'];
	$Tsub['assignmentId'] = $_REQUEST['workassignmentIderId'];
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

$assignment_id = $_REQUEST['assignmentId'];
$score=$_REQUEST["score"];


$score = round(.25/(1+exp(-($score-240)/100)),2);

?>
<div align="center">
<b>Excellent! You're done. You earned a bonus of $<?php echo $score ?>. Just click submit below to complete the HIT!</b><br><br>


<form id="mturk_form" method="POST" action="<?php echo $_REQUEST['turkSubmitTo'] ?>/mturk/externalSubmit">
	<input type="hidden" id="assignmentId" name="assignmentId" value="<?php echo $assignment_id ?>">
	<input type="hidden" id="score" name="correctBonus" value="<?php echo $score ?>">
	Please enter any feedback you have for us in the form below. <br>

	<textarea rows="10" cols="80" id="feedback" name="feedback">  </textarea>
<br>
	<input id="submitButton" type="submit" name="Sub" value="Complete HIT">
</form>
</div>
