all: krc.txt krc.pdf krc.html

krc.txt: krc.1 Makefile
	nroff -Tascii -t -c -man krc.1 | col -b > krc.txt

krc.html: krc.1 Makefile
#	groff -Thtml -man krc.1 > krc.html #groff doesn't like tables
	man2html krc.1 > krc.html

krc.pdf: krc.1 Makefile
	groff -Tps -t -man krc.1 | ps2pdf - $@

clean:
	rm -f krc.text krc.pdf krc.html
