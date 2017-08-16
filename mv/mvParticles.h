#ifndef __mvParticles_h
#define __mvParticles_h

#include "mvDisplayObject.h"

class vtkCubeSource;
class vtkGlyph3D;
class vtkPolyData;
class vtkThreshold;
class vtkThresholdPoints;
class vtkTransform;
class vtkTransformPolyDataFilter;
class vtkLookupTable;
class vtkLogLookupTable;
class vtkClipPolyData;
class mvClipParticles;

class MV_EXPORT mvParticles : public mvDisplayObject  
{
public:
	mvParticles();
	virtual ~mvParticles();
	void SetParticleCoordinates(float *particleCoordinates);
	float *GetParticleCoordinates() {return m_ParticleCoordinates;}
	void SetParticleConcentrations(float *particleConcentrations);
	float *GetParticleConcentrations() {return m_ParticleConcentrations;}
	float GetMaxConcentration() {return m_maxConc;}
	float GetMinConcentration() {return m_minConc;}
	float GetMaxDisplayedConcentration() {return m_MaxDisplayedConcentration;}
	float GetMinDisplayedConcentration() {return m_MinDisplayedConcentration;}
	int ParticleCount() {return m_ParticleCount;}
	void SetParticleCount(int particleCount);
	void Build();
	void SetDefaultGlyphSize(float s);
	void EnlargeGlyphs();
	void ShrinkGlyphs();
	float GetGlyphSize();
	void SetScale(float xScale, float yScale, float zScale);
	void SetDisplayedConcentrations(float minConcentration, float maxConcentration);
	void CropParticles(int ShouldCrop, float xmin, float xmax, float ymin, float ymax, float zmin, float zmax, float ModelBounds[6]);
	int GetDisplayedConcentrationsHaveBeenSet() {return m_ConcentrationsSet;}
	int GetParticlesCropped() {return m_CropParticles;}
	float GetParticleCropXMin() {return m_bounds[0];}
	float GetParticleCropXMax() {return m_bounds[1];}
	float GetParticleCropYMin() {return m_bounds[2];}
	float GetParticleCropYMax() {return m_bounds[3];}
	float GetParticleCropZMin() {return m_bounds[4];}
	float GetParticleCropZMax() {return m_bounds[5];}
	void SetColorBarEndPoints(float valueBlue, float valueRed);
	void Reset();

protected:
	vtkPolyData *m_PolyData;
	vtkGlyph3D *m_Glyph;
	vtkCubeSource *m_CubeSource;
	vtkThresholdPoints *m_ThresholdPoints;
	vtkTransform *m_Transform;
	vtkTransformPolyDataFilter *m_TransformFilter;
	vtkClipPolyData *m_CropFilter;
	mvClipParticles *m_ClipParticles;

	// Lookup tables
	vtkLookupTable *m_LutRedToBlue;
	vtkLookupTable *m_LutBlueToRed;
	vtkLogLookupTable *m_LogLutRedToBlue;
	vtkLogLookupTable *m_LogLutBlueToRed;

	float m_DefaultGlyphSize;
	// the arrays pointed to by m_ParticleCoordinates and m_ParticleConcentrations are owned by
	// the data source that supplies them to mvParticles so mvParticles does not release that 
	// memory.
	float m_minConc;
	float m_maxConc;
	float m_MaxDisplayedConcentration;
	float m_MinDisplayedConcentration;
	float *m_ParticleCoordinates;
	float *m_ParticleConcentrations;
	int m_ParticleCount;
	int m_CropParticles;
	int m_ConcentrationsSet;
	int m_ColorsSet;
	float m_bounds[6];
    float minConcentration();
	float maxConcentration();
/*	
	void SetMaxDisplayedConcentration(float concentration);
	void SetMinDisplayedConcentration(float concentration);
*/
};

#endif
