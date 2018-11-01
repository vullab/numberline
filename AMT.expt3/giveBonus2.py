#!/usr/bin/python

import os, sys
input_file = sys.argv[1]
records = open(input_file, 'r').read().replace("\r", "\n").split("\n")

output_file = sys.argv[2]
f = open(output_file, 'w')

f.write("#!/bin/bash\n")

f.write("pushd $MTURK_CMD_HOME/bin/\n")
titles=records[0].strip().split('\t')

assignIDIndex =  titles.index('"assignmentid"')
workerIDindex =  titles.index('"workerid"')
correctIDindex =  titles.index('"Answer.correctBonus"')
submittedIDindex =  titles.index('"assignmentstatus"')

for r in records[1:]:
	r2=r.strip().split('\t')
	if len(r2) == len(titles) and r2[submittedIDindex] == 	'"Submitted"':
		(assignmentID,workerID,correct,comment) = (r2[assignIDIndex],r2[workerIDindex],r2[correctIDindex],"performance bonus")
		bonus=(correct.strip('[\s"]+'))
		#print(bonus)
		cmd="./grantBonus.sh -workerid %s -assignment %s -amount %s -reason %s" %(workerID,assignmentID,bonus,comment)
		print(cmd)
		f.write(cmd + "\n")

f.write("popd\n")
f.close()
os.system("chmod +x " + output_file)
