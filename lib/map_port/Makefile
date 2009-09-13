SRC=src
C_SRC=c_src
PRIV=priv

all:    subdirs
	
subdirs: 
	cd ${SRC}; make
	cd ${C_SRC}; make	
	
# remove all the code
clean: 
	rm -rf ebin/*.beam erl_crash.dump
	rm $(PRIV)/port_drv.so

