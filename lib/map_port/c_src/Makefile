PRIV=../priv

all:    port_drv.so
		
port_drv.so: port_driver.c map.c
	gcc -o $(PRIV)/port_drv.so -fpic -shared map.c port_driver.c
	
clean:
	rm $(PRIV)/port_drv.so