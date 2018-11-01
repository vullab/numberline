<?php

	$world= $_POST["world"];
	$choices= $_POST["choices"];
	$outcomes= $_POST["outcomes"];
	$score= $_POST["score"];
	$count=$_POST["count"];
	$selection=$_POST['selection'];
	$mtime=$_POST['mtime'];
	$P=$_POST['P'];
	$U=$_POST['U'];
	$real=$_POST['real'];
	$flip=$_POST['flip'];
	$P=unserialize($P);
	$U=unserialize($U);
	$prob=$P[$count];
	$util=$U[$count];
	$count=$count+1;

	if($world=="BLACK"){
		$score=$score+$outcomes+10-$selection;
		$dscore=10-$selection+$outcomes;
	}else{
		$score=$score+$outcomes+$selection;
		$dscore=$selection+$outcomes;
	}
	
	if($count<10){
		$nextPage="trial.php";
	}else{
		$nextPage="exit.php";
	}

	$OSName=$_POST["OSName"];
	$browserName=$_POST["browserName"];
	$country=$_POST["upCountry"];
	$workerId=$_POST['workerId'];
	$assignmentId=$_POST['assignmentId'];
	$hitId=$_POST['hitId'];
	$IP=$_SERVER['REMOTE_ADDR'];
	$timestamp = time();
	
	require_once('config.php');
	$conn=mysql_connect(DB_SERVER_WEB, DB_USER_WEB, DB_PASS_WEB);	
	mysql_select_db(DB_DATABASE_WEB);
	$query="INSERT INTO `ContMem1` (`workerId`,`assignmentId`,`hitId`,`time`,`world`,`choices`,`outcomes`,`score`,`selection`,`prob`,`util`,`mtime`,`OSName`,`browserName`,`country`,`IP`,`real`,`flip`) VALUES ('" . $workerId . "','" . $assignmentId . "','" . $hitId . "', '" . $timestamp . "', '" .  $world  .  "','"  .  $choices  .  "'," .  $outcomes  .  "," . $score . "," . $selection . "," . $prob . "," . $util . "," . $mtime . ",'" . $OSName . "','" . $browserName . "','" . $country . "','" . $IP . "','" . $real . "','" . $flip ."')";
	//echo $query;
	$result = mysql_query($query,$conn);
?>

<html>
<body>



<p  align="middle"> The wheel landed on <?php echo $world ?>. You bet <?php echo 10-$selection ?> points on BLACK and <?php echo $selection ?> points on RED.</p>

<p align="middle"> You selected <?php echo $choices ?>, and won <?php echo $outcomes ?> points! </p>

<p align="middle"> Together you earned a total of <?php echo $dscore ?> points. </p>


<p  align="middle"> <b>NEW SCORE </b></p>
<p  align="middle"> <b><?php echo $score  ?> points</b></p>

<div align="center">
	<form id="record" method="post" action=<?php echo $nextPage ?> >

	<input type="hidden" id="turkerId" name="workerId" value="<?php echo $_REQUEST['workerId']?>">
	<input type="hidden" id="turkSubmitTo" name="turkSubmitTo" value="<?php echo $_REQUEST['turkSubmitTo']?>">
	<input type="hidden" id="assignmentId" name="assignmentId" value="<?php echo $_REQUEST['assignmentId'] ?>">
	<input type="hidden" id="hitId" name="hitId" value="<?php echo $_REQUEST['hitId'] ?>">

		<input type="hidden" size =50 id = "score" name="score" value=<?php echo $score ?>>  </input>
		<input type="hidden" size =50 id = "count" name="count" value=<?php echo $count ?>>  </input>

		<input type="hidden" id="upCountry" name="upCountry" value=<?php echo $country; ?>>



		<input type="hidden" id="P" name="P" value="<?php echo serialize($P) ?>">
		<input type="hidden" id="U" name="U" value="<?php echo serialize($U) ?>">


        <input type="submit"  value="Next"></input>
	
        </form>
</div>


  </body>
</html>
