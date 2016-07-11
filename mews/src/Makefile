all: LSN.so OU.so

LSN.so: LSN.cpp
	Rscript --vanilla -e "TMB::compile('LSN.cpp')"

OU.so: OU.cpp
	Rscript --vanilla -e "TMB::compile('OU.cpp')"

clean:
	rm -rf *o