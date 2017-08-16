#include "mvModelFeatures.h"
#include "mvDataSource.h"
#include "vtkConeSource.h"
#include "vtkPolyData.h"
#include "vtkPoints.h"
#include "vtkGlyph3D.h"
#include "vtkCubeSource.h"
#include "vtkStructuredGrid.h"
#include "vtkThreshold.h"
#include "vtkThresholdPoints.h"
#include "vtkTransform.h"
#include "vtkTransformPolyDataFilter.h"
#include "vtkFloatArray.h"
#include "vtkLookupTable.h"
#include "vtkFloatArray.h"

mvModelFeatures::mvModelFeatures()
{
	m_DisplayMode = MV_DISPLAY_MODEL_FEATURES_AS_CELLS;
	// Objects for cell display
	m_StructuredGrid = vtkStructuredGrid::New();
	m_ThresholdCells = vtkThreshold::New();
	m_ThresholdCells->SetInput(m_StructuredGrid);
	m_ThresholdCells->ThresholdBetween(0.5, 10000);
	m_ThresholdCells->SetAttributeModeToUseCellData();
	SetMapperInput(m_ThresholdCells->GetOutput());

	m_Lut = vtkLookupTable::New();
	m_Mapper->SetLookupTable(m_Lut);

	// Objects for glyph display
	m_PolyData = vtkPolyData::New();
	m_Glyph = vtkGlyph3D::New();
	m_CubeSource = vtkCubeSource::New();
	m_ThresholdPoints = vtkThresholdPoints::New();
	m_ThresholdPoints->ThresholdBetween(0.5, 10000);
	m_Transform = vtkTransform::New();
	m_TransformFilter = vtkTransformPolyDataFilter::New();
	m_ThresholdPoints->SetInput(m_PolyData);
	m_TransformFilter->SetInput(m_ThresholdPoints->GetOutput());
	m_TransformFilter->SetTransform(m_Transform);
	m_Glyph->SetInput(m_TransformFilter->GetOutput());
	m_Glyph->SetScaleModeToDataScalingOff();
	m_Glyph->SetColorModeToColorByScalar();
	m_Glyph->SetSource(m_CubeSource->GetOutput());
	m_DefaultGlyphSize = 1;
	m_NumberOfGlyphs = 0;
	m_Dim[0] = 0;
	m_Dim[1] = 0;
	m_Dim[2] = 0;
	m_Subgrid[0] = 0;
	m_Subgrid[1] = 0;
	m_Subgrid[2] = 0;
	m_Subgrid[3] = 0;
	m_Subgrid[4] = 0;
	m_Subgrid[5] = 0;
	m_GlyphScalarArray = 0;
	m_DisplayOrder = 0;
	m_NumberOfCells = 0;
	m_CellScalarArray = 0;
	m_ModelFeatureArray = 0;
	m_SubgridIsActivated = 0;
	m_CellTypes = 0;
	m_Connectivity = 0;
}

mvModelFeatures::~mvModelFeatures()
{
	m_StructuredGrid->Delete();
	m_ThresholdCells->Delete();
	m_PolyData->Delete();
	m_Glyph->Delete();
	m_CubeSource->Delete();
	m_ThresholdPoints->Delete();
	m_Lut->Delete();
	m_Transform->Delete();
	m_TransformFilter->Delete();
	if (m_GlyphScalarArray != 0)
	{
		delete [] m_GlyphScalarArray;
	}
	if (m_DisplayOrder != 0)
	{
		delete [] m_DisplayOrder;
	}
	if (m_CellScalarArray != 0)
	{
		delete [] m_CellScalarArray;
	}
	if (m_CellTypes != 0)
	{
		delete [] m_CellTypes;
	}
	if (m_Connectivity != 0)
	{
		m_Connectivity->Delete();
	}
}

void mvModelFeatures::SetDisplayMode(int displayMode)
{
	m_DisplayMode = displayMode;
}

int mvModelFeatures::GetDisplayMode() const
{
	return m_DisplayMode;
}

int *mvModelFeatures::GetDisplayOrder()
{
	return m_DisplayOrder;
}

void mvModelFeatures::GetColor(int i, float *rgba)
{
	if (m_Lut)
	{
		float *color = m_Lut->GetTableValue(i);
		rgba[0] = color[0];
		rgba[1] = color[1];
		rgba[2] = color[2];
		rgba[3] = color[3];
	}
}

void mvModelFeatures::SetColor(int i, float *rgba)
{
	if (m_Lut)
	{
		m_Lut->SetTableValue(i, rgba);
	}
}

void mvModelFeatures::SetNumberOfModelFeatureTypes(int numModelFeatureTypes)
{
	m_NumberOfModelFeatureTypes = numModelFeatureTypes;
	m_Lut->SetNumberOfColors(m_NumberOfModelFeatureTypes);
}

void mvModelFeatures::SetModelFeatureArray(int *modelFeatureArray)
{

	m_ModelFeatureArray = modelFeatureArray;


	if (m_ModelFeatureArray == 0 || m_CellScalarArray == 0)
	{
		return;
	}

	if (m_DisplayOrder != 0)
	{
		int k = 0;
		for (int i=0; i<m_NumberOfModelFeatureTypes; i++)
		{
			if (m_ModelFeatureArray[k] >= 0)
			{
				k += m_ModelFeatureArray[k];
			}
			else
			{
				m_DisplayOrder[i] = -2;
			}
			k ++;
		}
	}

	if (m_DisplayMode == MV_DISPLAY_MODEL_FEATURES_AS_GLYPHS)
	{
		// TO DO: Implement this
	}
	else
	{
		m_StructuredGrid->Modified();
		SetDisplayOrderForCells();
	}
		
}

void mvModelFeatures::SetGridPoints(vtkPoints *gridPoints)
{
	m_GridPoints = gridPoints;
}

void mvModelFeatures::Build()
{
	int i, j, k, m;
	if (m_ModelFeatureArray == 0)
	{
		return;
	}
	m_Mapper->SetScalarRange(1, m_NumberOfModelFeatureTypes);
	if (m_DisplayOrder != 0)
	{
		delete [] m_DisplayOrder;
	}
	m_DisplayOrder = new int[m_NumberOfModelFeatureTypes];
	for (i=0; i<m_NumberOfModelFeatureTypes; i++)
	{
		m_DisplayOrder[i] = -1;
	}
	m_Lut->Build();
	m_SubgridIsActivated = 0;
	m_Actor->SetScale(1, 1, 1);
	if (m_DisplayMode == MV_DISPLAY_MODEL_FEATURES_AS_GLYPHS)
	{
		SetMapperInput(m_Glyph->GetOutput());
		m_ThresholdPoints->ThresholdBetween(0.5, m_NumberOfModelFeatureTypes + 0.5);
		int count = 0;
		k=0;
		for (i=0; i<m_NumberOfModelFeatureTypes; i++)
		{
			count += m_ModelFeatureArray[k];
			k += (m_ModelFeatureArray[k] + 1);
		}
		m_NumberOfGlyphs = count;
		if (m_GlyphScalarArray != 0)
		{
			delete [] m_GlyphScalarArray;
		}
		m_GlyphScalarArray = new float[m_NumberOfGlyphs];
		vtkPoints *points = vtkPoints::New();
		vtkFloatArray *scalars = vtkFloatArray::New();
		m = 0;
		k = 0;
		for (i=0; i<m_NumberOfModelFeatureTypes; i++)
		{
			int numGlyph = m_ModelFeatureArray[k];
			k++;
			for (j=0; j<numGlyph; j++)
			{
				float *p = m_GridPoints->GetPoint(m_ModelFeatureArray[k]);
				points->InsertNextPoint(p[0], p[1], p[2]);
				// Note that we start counting from 1. Also, all glyphs are
				// set to be initially invisible.
				m_GlyphScalarArray[m] = (float) (i+1+m_NumberOfModelFeatureTypes);
				k++;
				m++;
			}
		}
		scalars->SetArray(m_GlyphScalarArray, m_NumberOfGlyphs, 1);

		m_PolyData->SetPoints(points);
		m_PolyData->GetPointData()->SetScalars(scalars);
		points->Delete();
		scalars->Delete();
	}
	else
	{
		SetMapperInput(m_ThresholdCells->GetOutput());
		m_ThresholdCells->ThresholdBetween(0.5, m_NumberOfModelFeatureTypes + 0.5);
		if (m_GridType == MV_UNSTRUCTED_GRID)
		{
			dynamic_cast<vtkUnstructuredGrid*>(m_StructuredGrid)->SetCells(m_CellTypes, m_Connectivity);
		}
		else
		{
			dynamic_cast<vtkStructuredGrid*>(m_StructuredGrid)->SetDimensions(m_Dim[0], m_Dim[1], m_Dim[2]);
		}
		m_StructuredGrid->SetPoints(m_GridPoints);
		m_NumberOfCells = (m_Dim[0]-1) * (m_Dim[1]-1) * (m_Dim[2]-1);
		if (m_CellScalarArray != 0)
		{
			delete [] m_CellScalarArray;
		}
		m_CellScalarArray = new float[m_NumberOfCells];
		for (i=0; i<m_NumberOfCells; i++)
		{
			m_CellScalarArray[i] = 0;
		}
		vtkFloatArray *scalars = vtkFloatArray::New();
		scalars->SetNumberOfComponents(1);
		scalars->SetArray(m_CellScalarArray, m_NumberOfCells, 1);
		m_StructuredGrid->GetCellData()->SetScalars(scalars);
		scalars->Delete();
	}
}

void mvModelFeatures::SetScale(float xScale, float yScale, float zScale)
{
	if (m_DisplayMode == MV_DISPLAY_MODEL_FEATURES_AS_GLYPHS)
	{
//		m_Actor->SetScale(1, 1, 1);
		float ps[3];
		m_Transform->GetScale(ps);
		m_Transform->Scale(xScale/ps[0], yScale/ps[1], zScale/ps[2]);
	}
	else
	{
		m_Actor->SetScale(xScale, yScale, zScale);
	}
}

void mvModelFeatures::SetDisplayOrder(int *displayOrder)
{
	if (m_ModelFeatureArray == 0 || m_DisplayOrder == 0)
	{
		return;
	}
	for (int i=0; i<m_NumberOfModelFeatureTypes; i++)
	{
		m_DisplayOrder[i] = displayOrder[i];
	}
	if (m_DisplayMode == MV_DISPLAY_MODEL_FEATURES_AS_GLYPHS)
	{
		SetDisplayOrderForGlyphs();
	}
	else
	{
		SetDisplayOrderForCells();
	}
}

void mvModelFeatures::SetDefaultGlyphSize(float gs)
{
	m_CubeSource->SetXLength(gs);
	m_CubeSource->SetYLength(gs);
	m_CubeSource->SetZLength(gs);
}

void mvModelFeatures::EnlargeGlyphs() 
{
	m_CubeSource->SetXLength(m_CubeSource->GetXLength()*1.5);
	m_CubeSource->SetYLength(m_CubeSource->GetYLength()*1.5);
	m_CubeSource->SetZLength(m_CubeSource->GetZLength()*1.5);
}

void mvModelFeatures::ShrinkGlyphs() 
{
	m_CubeSource->SetXLength(m_CubeSource->GetXLength()/1.5);
	m_CubeSource->SetYLength(m_CubeSource->GetYLength()/1.5);
	m_CubeSource->SetZLength(m_CubeSource->GetZLength()/1.5);
}

float mvModelFeatures::GetGlyphSize()
{
	return m_CubeSource->GetXLength();
}

void mvModelFeatures::SetFullGridDimensions(const int *dim)
{
	m_Dim[0] = dim[0];
	m_Dim[1] = dim[1]; 
	m_Dim[2] = dim[2];
}

void mvModelFeatures::SetSubgridExtent(int imin, int imax, 
					int jmin, int jmax, int kmin, int kmax)
{
	m_Subgrid[0] = imin;
	m_Subgrid[1] = imax;
	m_Subgrid[2] = jmin;
	m_Subgrid[3] = jmax;
	m_Subgrid[4] = kmin;
	m_Subgrid[5] = kmax;
}

void mvModelFeatures::SubgridOn()
{
	int i, j, index, ii, jj, kk;
	if (m_DisplayMode == MV_DISPLAY_MODEL_FEATURES_AS_GLYPHS)
	{
		int m = 0;
		int k = 0;
		for (i=0; i<m_NumberOfModelFeatureTypes; i++)
		{
			int numGlyph = m_ModelFeatureArray[k];
			k++;
			for (j=0; j<numGlyph; j++)
			{
				index = m_ModelFeatureArray[k];
				kk = index/(m_Dim[0]*m_Dim[1]);
				index -= kk*(m_Dim[0]*m_Dim[1]);
				jj = index/m_Dim[0];
				ii = index - jj*m_Dim[0];
				if (ii < m_Subgrid[0] || ii > m_Subgrid[1] ||
					jj < m_Subgrid[2] || jj > m_Subgrid[3] ||
					kk < m_Subgrid[4] || kk > m_Subgrid[5])
				{
					m_GlyphScalarArray[m] = -abs(m_GlyphScalarArray[m]);
				}
				else
				{
					m_GlyphScalarArray[m] = abs(m_GlyphScalarArray[m]);
				}
				k++;
				m++;
			}
		}
		m_PolyData->Modified();
	}
	else
	{
		int i, j, k, m;
		m = 0;
		for (k=0; k<m_Dim[2]-1; k++)
		{
			for (j=0; j<m_Dim[1]-1; j++)
			{
				for (i=0; i<m_Dim[0]-1; i++)
				{
					if (i < m_Subgrid[0] || i > m_Subgrid[1]-1 ||
						j < m_Subgrid[2] || j > m_Subgrid[3]-1 ||
						k < m_Subgrid[4] || k > m_Subgrid[5]-1)
					{
						m_CellScalarArray[m] = -abs(m_CellScalarArray[m]);
					}
					else
					{
						m_CellScalarArray[m] = abs(m_CellScalarArray[m]);
					}
					m++;
				}
			}
		}
		m_StructuredGrid->Modified();
	}
	m_SubgridIsActivated = 1;
}


void mvModelFeatures::SubgridOff()
{
	if (m_DisplayMode == MV_DISPLAY_MODEL_FEATURES_AS_GLYPHS)
	{
		for (int i=0; i<m_NumberOfGlyphs; i++)
		{
			m_GlyphScalarArray[i] = abs(m_GlyphScalarArray[i]);
		}
		m_PolyData->Modified();
	}
	else
	{
		for (int i=0; i<(m_Dim[0]-1)*(m_Dim[1]-1)*(m_Dim[2]-1); i++)
		{
			m_CellScalarArray[i] = abs(m_CellScalarArray[i]);
		}
		m_StructuredGrid->Modified();
	}
	m_SubgridIsActivated = 0;
}

void mvModelFeatures::SetDisplayOrderForGlyphs()
{
	int i, j, k, m, p, q;
	// To begin, hide all glyphs
	for (j=0; j<m_NumberOfGlyphs; j++)
	{
		if (m_GlyphScalarArray[j] > 0 && 
			m_GlyphScalarArray[j] <= m_NumberOfModelFeatureTypes)
		{
			m_GlyphScalarArray[j] += m_NumberOfModelFeatureTypes;
		}
		else if (m_GlyphScalarArray[j] < 0 && 
			m_GlyphScalarArray[j] >= -m_NumberOfModelFeatureTypes)
		{
			m_GlyphScalarArray[j] -= m_NumberOfModelFeatureTypes;
		}

	}
	m_PolyData->Modified();
	int np = m_GridPoints->GetNumberOfPoints();
	int *marker = new int[np];
	for (i=0; i<np; i++)
	{
		marker[i] = 0;
	}

	// Turn on glyphs respresnting features that are displayed
	for (p=0; p<m_NumberOfModelFeatureTypes; p++)
	{
		// p is the display order
		for (j=0; j<m_NumberOfModelFeatureTypes; j++)
		{
			if (m_DisplayOrder[j] == p)
			{
				// j is the index of the model feature to be displayed
				k = 0;
				m = 0;
				for (i=0; i<j; i++)
				{
					m += m_ModelFeatureArray[k];
					k += m_ModelFeatureArray[k] + 1;
				}
				// m is the starting index of the glyph scalar array where
				// the scalar is to be modified to indicate that the glyph is shown.
				// k is the index of the model feature array containing the
				// number of glyphs representing the feature that is displayed
				for (i=m, q=1; i<m+m_ModelFeatureArray[k]; i++, q++)
				{
					// Display a glyph only when the node is unoccupied.
					int mp = m_ModelFeatureArray[k+q];
					if (marker[mp] == 0)
					{
						// mark the node as occupied.
						marker[mp] = 1;
						if (m_GlyphScalarArray[i] > 0)
						{
							m_GlyphScalarArray[i] -= m_NumberOfModelFeatureTypes;
						}
						else
						{
							m_GlyphScalarArray[i] += m_NumberOfModelFeatureTypes;
						}
					}
				}
			}
		}
	}
	delete [] marker;
}

void mvModelFeatures::SetDisplayOrderForCells()
{
	int i, j, k, m, p;
	for (j=0; j<m_NumberOfCells; j++)
	{
		m_CellScalarArray[j] = 0;

	}
	m_StructuredGrid->Modified();

	// Turn on cells respresnting features that are displayed,
	// starting from last in order toward first
	for (p=m_NumberOfModelFeatureTypes-1; p>=0; p--)
	{
		// p is the display order
		for (j=0; j<m_NumberOfModelFeatureTypes; j++)
		{
			if (m_DisplayOrder[j] == p)
			{
				// j is the index of the model feature to be displayed
				k = 0;
				for (i=0; i<j; i++)
				{
					if (m_ModelFeatureArray[k] > 0)
					{
						k += m_ModelFeatureArray[k];
					}
					k++;
				}
				// k is the index of the model feature array containing 
				// how many cells represent the feature that is displayed
				for (i=1; i<=m_ModelFeatureArray[k]; i++)
				{
					m = m_ModelFeatureArray[k+i];
					if (m >= 0)
					{
						m_CellScalarArray[m] = j+1;
					}
				}
			}
		}
	}
	if (m_SubgridIsActivated)
	{
		SubgridOn();
	}
}

void mvModelFeatures::AssignConnectivity(int *types, vtkCellArray* connectivity)
{
	if (m_CellTypes != 0)
	{
		delete [] m_CellTypes;
	}
	m_CellTypes = new int[connectivity->GetNumberOfCells()];

	for (int i = 0; i < connectivity->GetNumberOfCells(); i++)
	{
		m_CellTypes[i] = types[i];
	}
	if (m_Connectivity != 0)
	{
		m_Connectivity->Delete();
	}
	m_Connectivity = vtkCellArray::New();
	m_Connectivity->DeepCopy(connectivity);
}

void mvModelFeatures::SetGridType(int i)
{
	m_GridType = i;
}