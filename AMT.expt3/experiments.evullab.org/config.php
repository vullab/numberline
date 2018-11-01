<?php
//website database server
DEFINE('DB_SERVER_WEB', "mysql.evullab.org");
//DEFINE('DB_SERVER_WEB', "localhost:8888/");
//website database login name
DEFINE('DB_USER_WEB', "evullab_expts");
//DEFINE('DB_USER_WEB', "root");
//website database login password
DEFINE('DB_PASS_WEB', "evullabexpts3509mandler");
//DEFINE('DB_PASS_WEB', "root");
//website database name
DEFINE('DB_DATABASE_WEB', "evullab_expts");


// table name
DEFINE('DB_TABLE', "numberline2");
// expt configuration
DEFINE('EXPT_p0n', 10);		// number of training trials
DEFINE('EXPT_p1n', 30);		// number of trials in pre feedback phase
DEFINE('EXPT_p2n', 30);		// number of trials in feedback phase
DEFINE('EXPT_p3n', 30);		// number of trials in post-feedback phase
DEFINE('EXPT_cfb', 0.03);	// condition feedback slope (for cond: -1, slope = 1+(C))
DEFINE('EXPT_fbthresh', 10);	// condition 3 feedback slope


DEFINE('EXPT_pretime', 300);	// blank time before dots shown
DEFINE('EXPT_showtime', 500);	// display dots for this long
DEFINE('EXPT_traintime', 1000);	// display dots for this long
DEFINE('EXPT_fbtime', 1500);	// display feedback for this long

DEFINE('EXPT_accslope', 0.03);	// scoring function slope


?>
