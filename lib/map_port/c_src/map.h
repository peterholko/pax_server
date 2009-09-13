#ifndef _MAP_H
#define _MAP_H

#include <stdio.h>
#include <stdlib.h>

#define FALSE 0
#define TRUE 1

typedef unsigned char byte;
typedef int bool;

typedef struct{
	byte type;
	byte yield;
} resource_type;

typedef struct{
	byte type;
	byte numResources;
	resource_type *resources;
} tile_type;

char **resourceList;
tile_type *mapTiles;

bool loadResourceList();
bool loadResource(char *filename, int resourceType);
bool loadTiles();
int loadMap();
void destroyMap();

char *getResources(int index);
byte getTileType(int index);

int main();

#endif
