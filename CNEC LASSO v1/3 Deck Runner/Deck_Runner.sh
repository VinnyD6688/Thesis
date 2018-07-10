#!/bin/bash

cd Outputs
rm *.out
cd ..

flist='h_p1 c_p1 o_p1 cl_p1 si_p1 h2o_pn crude_p1 sio2_p1'

cd Temp

for f1 in $flist
do
	echo $f1
	mv /home/apfeinbe/Deck\ Runner/Inputs/$f1.inp /home/apfeinbe/Deck\ Runner/Temp/$f1.inp

	nohup mpiexec -f /home/apfeinbe/Deck\ Runner/Temp/hosts.mpi mcnp6.mpi i=$f1.inp o=$f1.out < /dev/null > screen

	mv $f1.out /home/apfeinbe/Deck\ Runner/Outputs/$f1.out
	mv $f1.inp /home/apfeinbe/Deck\ Runner/Inputs/$f1.inp
	
	rm runtpe
	rm screen
	
	
done

cd ..