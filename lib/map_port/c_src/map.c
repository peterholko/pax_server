#include "map.h"

bool loadResourceList()
{
	FILE *pFile;
	size_t result;
	char filename[80];
	bool resourceStatus;
	int resourceType = 0;
	
	pFile = fopen("resourceList.txt", "r");
	
	if(pFile == NULL)
		return FALSE;
		
	while(!feof(pFile))
	{
		fscanf (pFile, "%s", filename);
		resourceStatus = loadResource(filename, resourceType);
		resourceType++;
		
		if(!resourceStatus)
			break;
	}
	
	fclose(pFile);

	return resourceStatus;
}

bool loadResource(char *filename, int resourceType)
{
	FILE *pFile;
	int i = 0;
	long lSize;
	byte yield;
	size_t result;
				
	pFile = fopen(filename, "rb");
	
	if(pFile == NULL)	
		return FALSE;
		
	while(!feof(pFile))
	{
		result = fread(&yield, sizeof(byte), 1, pFile);
		
		resource_type resource;
		
		resource.type = resourceType;
		resource.yield = yield;
				
		tile_type tile = mapTiles[i];
		
		if(tile.numResources == 0)
		{
			tile.resources = (resource_type *) malloc(sizeof(resource_type));
		}
		else
		{
			tile.resources = (resource_type *) realloc(tile.resources, tile.numResources * sizeof(resource_type));
		}
		
		tile.numResources++;
				
		if(tile.resources == NULL)
			return FALSE;
			
		tile.resources[tile.numResources - 1] = resource;
			
		mapTiles[i] = tile;	
		i++;
	}
	
	fclose(pFile);
		 		
	return TRUE;
}

bool loadTiles()
{
	FILE *pFile;
	size_t result;
	byte tileTypeNum;
	int index = 0;
	
	mapTiles = (tile_type*) malloc( sizeof(tile_type) * 2500);
	
	if(mapTiles == NULL) 
		return FALSE;	
	
	pFile = fopen("tiles.bin", "rb");
	
	if(pFile == NULL)	
		return FALSE;	
	
	while(!feof(pFile))
	{
		result = fread(&tileTypeNum, sizeof(byte), 1, pFile);
				
		tile_type tile;
		tile.type = tileTypeNum;
		tile.numResources = 0;
		
		mapTiles[index] = tile;
		index++;
	}
	
	return TRUE;
}

int loadMap()
{	
	return loadTiles() && loadResourceList();
}

void destroyMap()
{
	free(mapTiles);
}

char *getResources(int index)
{			
	int i;
	tile_type tile = mapTiles[index];	
	
	char *resources = (char *) malloc (tile.numResources * 2 + 1);
	
	if(resources == NULL)
		return NULL;
	
	resources[0] = tile.numResources;
		
	for(i = 0; i < tile.numResources; i++)
	{
		resource_type resource = tile.resources[i];
		
		resources[i * 2 + 1] = resource.type;
		resources[(i * 2) + 2] = resource.yield;
	}	
		
	return resources;
}

byte getTileType(int index)
{
	tile_type tile = mapTiles[index];
	
	return tile.type;
}

int main()
{
	int i = 0;
	int index = 0;
	int numResources = 0;
	char *resources;

	loadTiles();
	loadResourceList();
	
	while(1)
	{
		printf("\nIndex: ");
		scanf("%d", &index);
		
		resources = getResources(index);
		numResources = resources[0];
		
		printf("Resources: ");
		printf("%d", resources[0]);
		
		for(i = 0; i < numResources; i++)
		{
			printf("%d", resources[i * 2 + 1] );
			printf("%d", resources[(i * 2) + 2]);
		}
	}
	return 0;
}







