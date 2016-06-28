all:
	make -C ts335793-pbmcts
	make -C ts335793-affmcucb
	make -C ts335793-affnmc
	make -C ts335793-pbnmc
	make -C ts335793-ffmc

chempionship:
	make
	./judge -v -i setup.in -T 5m -m 10s ./ts335793-pbmcts/ts335793-pbmcts ./ts335793-affmcucb/ts335793-affmcucb ./ts335793-ffmc/ts335793-ffmc ./ts335793-pbnmc/ts335793-pbnmc

upload:
	make clean

	tar cvzf ts335793-pbmcts.tgz ts335793-pbmcts
	tar cvzf ts335793-affmcucb.tgz ts335793-affmcucb
	tar cvzf ts335793-affnmc.tgz ts335793-affnmc
	tar cvzf ts335793-pbnmc.tgz ts335793-pbnmc
	tar cvzf ts335793-ffmc.tgz ts335793-ffmc

	scp ts335793-pbmcts.tgz ts335793@students.mimuw.edu.pl:~
	scp ts335793-affmcucb.tgz ts335793@students.mimuw.edu.pl:~
	scp ts335793-affnmc.tgz ts335793@students.mimuw.edu.pl:~
	scp ts335793-pbnmc.tgz ts335793@students.mimuw.edu.pl:~
	scp ts335793-ffmc.tgz ts335793@students.mimuw.edu.pl:~

clean:
	make -C ts335793-pbmcts clean
	make -C ts335793-affmcucb clean
	make -C ts335793-affnmc clean
	make -C ts335793-pbnmc clean
	make -C ts335793-ffmc clean

	-rm ts335793-pbmcts.tgz
	-rm ts335793-affmcucb.tgz
	-rm ts335793-affnmc.tgz
	-rm ts335793-pbnmc.tgz
	-rm ts335793-ffmc.tgz
