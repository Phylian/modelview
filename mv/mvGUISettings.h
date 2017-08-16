#ifndef __mvGUISettings_h
#define __mvGUISettings_h

#include "mvUtil.h"

class mvHashTable;

class MV_EXPORT mvGUISettings  
{
public:
	float m_version;

	// Crop
	float cropBoundsXDelta;
	float cropBoundsYDelta;
	float cropBoundsZDelta;

	// Animation
	float animationRotate;
	float animationElevate;
	float animationDelay;

	// Lighting - lights
	int headlightOn;
	int auxiliaryLightOn;
	float headlightIntensity;
	float auxiliaryLightIntensity;
	float auxiliaryLightDirection[3];

	// Lighting -- background
	int customBackground;
	float background[3];

	// Camera
	double cameraPosition[3];
	double focalPoint[3];
	double viewUp[3];
	int parallelProjection;
	double parallelScale;

	// Particles
	float particleCropBoundsXDelta;
	float particleCropBoundsYDelta;
	float particleCropBoundsZDelta;

	mvGUISettings();
	int Serialize(ofstream *out);
	int Deserialize(mvHashTable *hashTable, float version);
};

#endif
