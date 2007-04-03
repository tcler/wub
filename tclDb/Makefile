backup: clean
	(cd ..; tar czv --exclude='tclDb/backup/*' --exclude='tclDb/trials/*' -f tclDb-`date +%F`.tar.gz tclDb)

clean:
	-rm -r *.o *~ */*~ mkdict_tie.db

