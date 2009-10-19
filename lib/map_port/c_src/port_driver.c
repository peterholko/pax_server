/* port_driver.c */

#include <stdio.h>
#include "erl_driver.h"
#include "map.h"

typedef struct {
    ErlDrvPort port;
} port_data;

static ErlDrvData port_drv_start(ErlDrvPort port, char *buff)
{
    port_data* d = (port_data*)driver_alloc(sizeof(port_data));
    d->port = port;
    return (ErlDrvData)d;
}

static void port_drv_stop(ErlDrvData handle)
{
    driver_free((char*)handle);
}

static void port_drv_output(ErlDrvData handle, char *buff, int bufflen)
{
    port_data* d = (port_data*)handle;
    char fn = buff[0];
    char *buffer = (char*) malloc(100);

    if (fn == 1) 
	{
		buffer[0] = loadMap();
		
		driver_output(d->port, buffer, 1);
    } 
	else if(fn == 2) 
	{	
		unsigned char high = (unsigned char)buff[1];
		unsigned char low = (unsigned char)buff[2];
	
		short tileIndex = (high << 8) + low; 		
		buffer[0] = getTileType(tileIndex);
		
		driver_output(d->port, buffer, 1);
    }
	else if(fn == 3)
	{
		int numResources;
		
		unsigned char high = (unsigned char)buff[1];
		unsigned char low = (unsigned char)buff[2];
	
		short tileIndex = (high << 8) + low; 	

		buffer = getResources(tileIndex);
		
		numResources = buffer[0];
		
		driver_output(d->port, buffer, (numResources * 2) + 1);
	}
}

ErlDrvEntry port_driver_entry = {
    NULL,               /* F_PTR init, N/A */
    port_drv_start,  	/* L_PTR start, called when port is opened */
    port_drv_stop,   	/* F_PTR stop, called when port is closed */
    port_drv_output, 	/* F_PTR output, called when erlang has sent data to the port */
    NULL,               /* F_PTR ready_input, called when input descriptor ready to read*/
    NULL,               /* F_PTR ready_output, called when output descriptor ready to write */
    "port_drv",     	/* char *driver_name, the argument to open_port */
    NULL,               /* F_PTR finish, called when unloaded */
    NULL,               /* F_PTR control, port_command callback */
    NULL,               /* F_PTR timeout, reserved */
    NULL                /* F_PTR outputv, reserved */
};

DRIVER_INIT(port_drv) /* must match name in driver_entry */
{
    return &port_driver_entry;
}


