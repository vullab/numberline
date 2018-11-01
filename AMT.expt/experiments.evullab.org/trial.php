<?php
$P=$_POST['P'];
$P=unserialize($P);

$U=$_POST['U'];
$U=unserialize($U);

$count=$_POST['count'];
$score=$_POST['score'];
$Pcurr=$P[$count];
$U4=$U[$count];

$upCountry=$_POST['upCountry'];
if($U4>0){
	$U1=1;
	$U2=0;
	$U3=0;
}
if($U4<0){
	$U1=1;
	$U2=2;
	$U3=0;
}

?>
<html>

<head>

<style type="text/css">

.leftbox 
{
float:left;
margin-left:40%;
width:.5%;
height:.6%;
padding:.5%;
border:1px solid silver;
}

.midbox 
{
float:left;
width:.5%;
height:.6%;
padding:.5%;
border:1px solid silver;
}

.rightbox 
{
float:left;
width:.5%;
height:.6%;
padding:.5%;
border:1px solid silver;
margin-right:0%;
}

.bval 
{
float:left;
margin-left:39%;
margin-right:6%;
}

.rval 
{
float:left;
margin-left:7.5%;
margin-right:0%;
}

.sub 
{
float:left;
margin-left:48%;

}
</style>

</head>

<script>
var dr=.04
var start=0;
var world;
var choices;
var outcomes;
var lim=999;
var spinON=false;
var rON=false;
var butON=false;
var optON=true;
var st;
<?php echo(" var prob=$Pcurr") ?>
</script>


<body onload="startUp()">



<p align="middle" style="font-size:30px;"> <b> Current Score: <?php echo $score ?></b> </p>

<div align="center">
<canvas id="myCanvas" width="400" height="400" >
Your browser does not support the HTML5 Canvas tag
</canvas>
</div>


<script type="text/javascript">

var startpoint = -Math.PI/2;
var endpoint = 2*prob*Math.PI-(Math.PI/2);
var middle= Math.abs(((-Math.PI/2)-(2*prob*Math.PI-(Math.PI/2))))/2;
var flip;
var col2nam;
var col1nam;
var col2;
var col1;

function startUp(){
flip=Math.random()
if(flip<=.5){
col1='#000000';
col2="#FF0000";
col2nam='RED';
col1nam='BLACK';
}else{
col2nam='BLACK';
col1nam='RED';
col2='#000000';
col1="#FF0000";
}
document.getElementById('flip').value=col1nam;

draw_roul(999);
startTime();
}

function draw_roul(lim){
var c=document.getElementById("myCanvas");
var ctx=c.getContext("2d");

if(start<lim){
ctx.clearRect ( 0 , 0 , 900 , 450 );

ctx.fillStyle='#000000';
ctx.beginPath();
ctx.moveTo(200, 165);
ctx.arc(200,165,161,0,2*Math.PI,true);
ctx.closePath();
ctx.fill();

ctx.fillStyle='#8A4117';
ctx.beginPath();
ctx.moveTo(200, 165);
ctx.arc(200,165,160,0,2*Math.PI,true);
ctx.closePath();
ctx.fill();

ctx.fillStyle='#000000';
ctx.beginPath();
ctx.moveTo(200, 165);
ctx.arc(200,165,151,0,2*Math.PI,true);
ctx.closePath();
ctx.fill();
 
ctx.fillStyle=col1;
ctx.beginPath();
ctx.moveTo(200, 165);
ctx.arc(200,165,150,0,2*Math.PI,true);
ctx.closePath();
ctx.fill();


ctx.fillStyle=col2;
ctx.beginPath();
ctx.moveTo(200, 165);
ctx.arc(200,165,150,start-Math.PI/2,start+2*prob*Math.PI-(Math.PI/2),false);
ctx.closePath();
ctx.fill();



ctx.beginPath();
ctx.fillStyle=col1;
ctx.strokeStyle=col1;
ctx.moveTo(65,265);
ctx.lineTo(100,300);
ctx.lineTo(80,320);
ctx.lineTo(85,325);
ctx.lineTo(45,315);
ctx.lineTo(40,280);
ctx.lineTo(45,285);
ctx.closePath();
ctx.fill()
ctx.stroke()

ctx.beginPath();
ctx.fillStyle=col2;
ctx.strokeStyle=col2;
ctx.moveTo(298,300);
ctx.lineTo(333,265);
ctx.lineTo(353,285);
ctx.lineTo(358,280);
ctx.lineTo(353,315);
ctx.lineTo(313,325);
ctx.lineTo(318,320);
ctx.lineTo(298,300);
ctx.closePath();
ctx.fill();
ctx.stroke();

ctx.fillStyle='#FFC89D';
ctx.beginPath();
ctx.moveTo(200, 165);
ctx.arc(200,165,10,0,2*Math.PI,true);
ctx.closePath();
ctx.fill();

ctx.beginPath();
ctx.moveTo(200, 165);
ctx.arc(350,165,5,0,2*Math.PI,true);
ctx.closePath();
ctx.fill();

ctx.beginPath();
ctx.moveTo(200, 165);
ctx.arc(200,315,5,0,2*Math.PI,true);
ctx.closePath();
ctx.fill();

ctx.beginPath();
ctx.moveTo(200, 165);
ctx.arc(200,15,5,0,2*Math.PI,true);
ctx.closePath();
ctx.fill();



ctx.beginPath();
ctx.moveTo(200, 165);
ctx.arc(50,165,5,0,2*Math.PI,true);
ctx.closePath();
ctx.fill();

ctx.fillStyle="#FFFF00";
ctx.strokeStyle="#000000";
ctx.beginPath();
ctx.moveTo(50,165);
ctx.lineTo(35,150);
ctx.lineTo(35,180);
ctx.closePath();
ctx.fill();
ctx.stroke();


ctx.beginPath()
ctx.fillStyle="#FFFFFF";
ctx.strokeStyle=col2;
ctx.rect(340,330,60,50);
ctx.rect(280,330,60,50);
ctx.fill()
ctx.stroke()

ctx.beginPath()
ctx.strokeStyle=col1;
ctx.rect(1,330,60,50);
ctx.rect(60,330,60,50);
ctx.fill()
ctx.stroke()



ctx.font="24px times new roman";
ctx.fillStyle=col1;
ctx.fillText("A",25,350);
ctx.fillText("B",85,350);
ctx.fillStyle=col2;
ctx.fillText("A",305,350);
ctx.fillText("B",365,350);

ctx.fillStyle=col1;
ctx.fillText(<?php echo $U1 ?>,25,373);
ctx.fillText(<?php echo $U2 ?>,85,373);
ctx.fillStyle=col2;
ctx.fillText(<?php echo $U3 ?>,305,373);
ctx.fillText(<?php echo $U4 ?>,350,373);

start=start+dr;
}

}

function startTime()
{
var d = new Date();
st=d.getTime();
}

function endTime()
{
var d = new Date();
var x = document.getElementById("mtime");
x.value=d.getTime()-st;
}

function spin(){
	endTime();
	document.getElementById("sub").disabled=true;
	
	var OSName="Unknown OS";
                if (navigator.appVersion.indexOf("Win")!=-1) OSName="Windows";
                if (navigator.appVersion.indexOf("Mac")!=-1) OSName="MacOS";
                if (navigator.appVersion.indexOf("X11")!=-1) OSName="UNIX";
                if (navigator.appVersion.indexOf("Linux")!=-1) OSName="Linux";

var BrowserDetect = {
	init: function () {
		this.browser = this.searchString(this.dataBrowser) || "An unknown browser";
		this.version = this.searchVersion(navigator.userAgent)
			|| this.searchVersion(navigator.appVersion)
			|| "an unknown version";
		this.OS = this.searchString(this.dataOS) || "an unknown OS";
	},
	searchString: function (data) {
		for (var i=0;i<data.length;i++)	{
			var dataString = data[i].string;
			var dataProp = data[i].prop;
			this.versionSearchString = data[i].versionSearch || data[i].identity;
			if (dataString) {
				if (dataString.indexOf(data[i].subString) != -1)
					return data[i].identity;
			}
			else if (dataProp)
				return data[i].identity;
		}
	},
	searchVersion: function (dataString) {
		var index = dataString.indexOf(this.versionSearchString);
		if (index == -1) return;
		return parseFloat(dataString.substring(index+this.versionSearchString.length+1));
	},
	dataBrowser: [
		{
			string: navigator.userAgent,
			subString: "Chrome",
			identity: "Chrome"
		},
		{ 	string: navigator.userAgent,
			subString: "OmniWeb",
			versionSearch: "OmniWeb/",
			identity: "OmniWeb"
		},
		{
			string: navigator.vendor,
			subString: "Apple",
			identity: "Safari",
			versionSearch: "Version"
		},
		{
			prop: window.opera,
			identity: "Opera",
			versionSearch: "Version"
		},
		{
			string: navigator.vendor,
			subString: "iCab",
			identity: "iCab"
		},
		{
			string: navigator.vendor,
			subString: "KDE",
			identity: "Konqueror"
		},
		{
			string: navigator.userAgent,
			subString: "Firefox",
			identity: "Firefox"
		},
		{
			string: navigator.vendor,
			subString: "Camino",
			identity: "Camino"
		},
		{		// for newer Netscapes (6+)
			string: navigator.userAgent,
			subString: "Netscape",
			identity: "Netscape"
		},
		{
			string: navigator.userAgent,
			subString: "MSIE",
			identity: "Explorer",
			versionSearch: "MSIE"
		},
		{
			string: navigator.userAgent,
			subString: "Gecko",
			identity: "Mozilla",
			versionSearch: "rv"
		},
		{ 		// for older Netscapes (4-)
			string: navigator.userAgent,
			subString: "Mozilla",
			identity: "Netscape",
			versionSearch: "Mozilla"
		}
	],
	dataOS : [
		{
			string: navigator.platform,
			subString: "Win",
			identity: "Windows"
		},
		{
			string: navigator.platform,
			subString: "Mac",
			identity: "Mac"
		},
		{
			   string: navigator.userAgent,
			   subString: "iPhone",
			   identity: "iPhone/iPod"
	    },
		{
			string: navigator.platform,
			subString: "Linux",
			identity: "Linux"
		}
	]

};
BrowserDetect.init();



	var browserName= BrowserDetect.browser+BrowserDetect.version;

	document.getElementById('OSName').value=OSName;
	document.getElementById('browserName').value=browserName; 


	optON=false;
	document.getElementById("betA").disabled=true;
	document.getElementById("betB").disabled=true;

	var rnum = Math.random();
	if (rnum>prob)
	{ state="big"}
	else
	{state="small"}
	lim=3.5*Math.PI+2*Math.PI*rnum-2*middle;
	
	if(state=="big"){
		var real="LEFT";
		if(flip<=.5){
			world='BLACK';
		}else{
			world="RED";
		}
	}else{
		var real="RIGHT";
		if(flip<=.5){
			world='RED';
		}else{
			world="BLACK";
		}
	}

	document.getElementById("real").value=real;
	choice=document.getElementById("choices").value;
	
	if (choice=='A' && real=="LEFT"){outcomes=<?php echo $U1 ?>}
	else if (choice=='B' && real=="LEFT"){outcomes=<?php echo $U2 ?>}
	else if (choice=='A' && real=="RIGHT"){outcomes=<?php echo $U3 ?>}
	else if (choice=='B' && real=="RIGHT"){outcomes=<?php echo $U4 ?>}
	
	document.getElementById("world").value=world;
	document.getElementById("outcomes").value=outcomes;
	
	var time = 6000;
	var refreshIntervalId = setInterval(function(){draw_roul(lim)},10);
	setTimeout(function(){clearInterval(refreshIntervalId)},time)
	setTimeout(function(){document.getElementById("sub").value="Next"},time+60)
	setTimeout(function(){document.getElementById("sub").onclick=""},time+60)
	setTimeout(function(){document.getElementById("sub").disabled=false},time+60)
	setTimeout(function(){document.getElementById("sub").type="submit"},time+60)


}


</script>



<script>


function mDown(obj,num){
if(optON){
rON=true;
var bval=num-1;
var rval=11-num;
document.getElementById("selection").value=rval;
document.getElementById("bval").innerHTML="Black: "+bval;
document.getElementById("rval").innerHTML="Red: "+rval;

  for (i=1; i<=11; i++){

    if(i<num){
      change=document.getElementById("box"+i);
      change.style.backgroundColor="#000000";
    }
    if(i>num){
      change=document.getElementById("box"+i);
      change.style.backgroundColor="#FF0000";
    }
    if(i==num){
      change=document.getElementById("box"+num);
      change.style.backgroundColor="#800000";
    }
  }
}
if(rON && butON &&optON){
	spinON=true;
	document.getElementById("sub").disabled=false;

}
}

function check(choice){

document.getElementById("choices").value=choice;
butON=true;
if(rON && butON){
	spinON=true;
	document.getElementById("sub").disabled=false;

}
}
</script>

	

<form id="record" action="submit.php" method="post">

	<input type="hidden" id="workerId" name="workerId" value="<?php echo $_REQUEST['workerId']?>">
	<input type="hidden" id="turkSubmitTo" name="turkSubmitTo" value="<?php echo $_REQUEST['turkSubmitTo']?>">
	<input type="hidden" id="assignmentId" name="assignmentId" value="<?php echo $_REQUEST['assignmentId'] ?>">
	<input type="hidden" id="hitId" name="hitId" value="<?php echo $_REQUEST['hitId'] ?>">

	<input type="hidden" id="upCountry" name="upCountry" value=<?php echo $upCountry ?>>
	<input type="hidden" id="OSName" name="OSName" value="">
	<input type="hidden" id="browserName" name="browserName" value="">
	<input type="hidden" name="U" id="U" value=<?php echo serialize($U) ?> />
	<input type="hidden" name="P" id="P" value=<?php echo serialize($P) ?> />
	<input type="hidden" name="count" id="count" value=<?php echo $count ?> />
	<input type="hidden" size =50 id = "world" name="world" value="1"></input>
    <input type="hidden" size =50 id = "choices" name="choices" value="0"></input>
	<input type="hidden" size =50 id = "outcomes" name="outcomes" value="0"></input>
	<input type="hidden" size =50 id = "selection" name="selection" value="0"></input>
	<input type="hidden" name="score" id="score" value=<?php echo $score ?> />
	<input type="hidden" name="real" id="real" value="" />
	<input type="hidden" name="flip" id="flip" value="" />
	<input type="hidden" id="mtime" name="mtime" value="">	
		
	<p align="middle" ><b>Do you bet on A or B? </b></p>
	<div align="center">
	<input type="radio" name="bet"  id="betA" onclick="check('A')" value="A" style="color:Red;" align="center" /> A <br />
	<input type="radio"  name="bet" id="betB" onclick="check('B')" value="B" style="color:Green;"  align="center"/>B <br />
	</div>
	
	<p align="middle" ><b>Will the wheel land on Black or Red? </b></p>

	<div id="box1" class="leftbox" onclick="mDown(this,1)" style="background-color:#808080;width:.5%;height:.6%;padding:.6%;"></div>
	<div id="box2" class="midbox"  onclick="mDown(this,2)" style="background-color:#808080;width:.5%;height:.6%;padding:.6%;"></div>
	<div id="box3" class="midbox"  onclick="mDown(this,3)" style="background-color:#808080;width:.5%;height:.6%;padding:.6%;"></div>
	<div id="box4" class="midbox"  onclick="mDown(this,4)" style="background-color:#808080;width:.5%;height:.6%;padding:.6%;"></div>
	<div id="box5" class="midbox"  onclick="mDown(this,5)" style="background-color:#808080;width:.5%;height:.6%;padding:.6%;"></div>
	<div id="box6" class="midbox"  onclick="mDown(this,6)" style="background-color:#808080;width:.5%;height:.6%;padding:.6%;"></div>
	<div id="box7" class="midbox"  onclick="mDown(this,7)" style="background-color:#808080;width:.5%;height:.6%;padding:.6%;"></div>
	<div id="box8" class="midbox"  onclick="mDown(this,8)" style="background-color:#808080;width:.5%;height:.6%;padding:.6%;"></div>
	<div id="box9" class="midbox"  onclick="mDown(this,9)" style="background-color:#808080;width:.5%;height:.6%;padding:.6%;"></div>
	<div id="box10" class="midbox"  onclick="mDown(this,10)" style="background-color:#808080;width:.5%;height:.6%;padding:.6%;"></div>
	<div id="box11" class="rightbox" onclick="mDown(this,11)" style="background-color:#808080;width:.5%;height:.6%;padding:.6%;"></div>

	<p class="bval" id="bval">Black: ? </p>
	<p class="rval" id="rval">Red: ? </p>

	<div align="center">
	<input type="button" name="sub"  onclick="spin()" class="sub" id="sub" value="Spin" disabled />
	</div>

</form>


</body>

</html>