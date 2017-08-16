#include "mvColorBandFilter.h"
#include "vtkFloatArray.h"

mvColorBandFilter::mvColorBandFilter()
{
	m_Values = 0;
	m_NumberOfValues = 0;
	m_Clipper = 0;
	m_Append = 0;
}

mvColorBandFilter::~mvColorBandFilter()
{
	ReleaseMemory();
}

void mvColorBandFilter::ReleaseMemory()
{
	if (m_NumberOfValues > 0)
	{
		delete [] m_Values;
		m_Values = 0;
	}
	if (m_NumberOfValues > 2)
	{
		for (int i=0; i<m_NumberOfValues-2; i++)
		{
			m_Clipper[i]->Delete();
		}
		delete [] m_Clipper;
		m_Clipper = 0;
		m_Append->Delete();
		m_Append = 0;
	}
}

void mvColorBandFilter::SetValues(float *values, int numValues)
{
	ReleaseMemory();
	m_NumberOfValues = numValues;
	if (numValues > 0)
	{
		m_Values = new float[numValues];
		for (int i=0; i<numValues; i++)
		{
			m_Values[i] = values[i];
		}

	}
	if (numValues > 2)
	{
		m_Append = vtkAppendPolyData::New();
		m_Clipper = new vtkClipPolyData *[numValues - 2];
		for (int i=0; i<numValues-2; i++)
		{
			m_Clipper[i] = vtkClipPolyData::New();
			m_Clipper[i]->GenerateClippedOutputOn();
			m_Clipper[i]->InsideOutOn();
			m_Clipper[i]->SetValue(values[i+1]);
			if (i > 0)
			{
				m_Clipper[i]->SetInput(m_Clipper[i-1]->GetClippedOutput());
			}
			m_Append->AddInput(m_Clipper[i]->GetOutput());
		}
		m_Append->AddInput(m_Clipper[numValues-3]->GetClippedOutput());
	}
	this->Modified();
}

void mvColorBandFilter::PrintSelf(ostream& os, vtkIndent indent)
{
  vtkPolyDataToPolyDataFilter::PrintSelf(os,indent);
  // TO DO: implement this method properly.
}

void mvColorBandFilter::Execute()
{
	int i, j, last, numCells;
	float v;
	vtkFloatArray *scalars;

	vtkPolyData *input=(vtkPolyData *)this->GetInput();
	vtkPolyData *output = this->GetOutput();

	if (m_NumberOfValues < 3)
	{
		output->CopyStructure(input);

		if (m_NumberOfValues == 0)
		{
			output->GetCellData()->SetScalars(input->GetCellData()->GetScalars());
			output->GetPointData()->SetScalars(input->GetPointData()->GetScalars());
		}
		else
		{
			numCells = input->GetNumberOfCells();
			scalars = vtkFloatArray::New();
			scalars->SetNumberOfComponents(1);
			if (numCells > 0)
			{
				v = m_Values[0];
				if (m_NumberOfValues == 2)
				{
					v = (v + m_Values[1])/2;
				}
				scalars->SetNumberOfTuples(numCells);
				for (j=0; j<numCells; j++)
				{
					scalars->SetTuple1(j, v);
				}
			}
			output->GetCellData()->SetScalars(scalars);
			scalars->Delete();
		}
		return;
	}

	m_Clipper[0]->SetInput(input);

	for (i=0; i<m_NumberOfValues - 2; i++)
	{
		m_Clipper[i]->Update();
		numCells = m_Clipper[i]->GetOutput()->GetNumberOfCells();
		scalars = vtkFloatArray::New();
		scalars->SetNumberOfComponents(1);
		if (numCells > 0)
		{
			v = (m_Values[i] + m_Values[i+1])/2;
			scalars->SetNumberOfTuples(numCells);
			for (j=0; j<numCells; j++)
			{
				scalars->SetTuple1(j, v);
			}
		}
		m_Clipper[i]->GetOutput()->GetCellData()->SetScalars(scalars);
		scalars->Delete();
	}
	last = m_NumberOfValues - 3;
	numCells = m_Clipper[last]->GetClippedOutput()->GetNumberOfCells();
	scalars = vtkFloatArray::New();
	scalars->SetNumberOfComponents(1);
	if (numCells > 0)
	{
		v = (m_Values[last + 1] + m_Values[last + 2])/2;
		scalars->SetNumberOfTuples(numCells);
		for (j=0; j<numCells; j++)
		{
			scalars->SetTuple1(j, v);
		}
	}
	m_Clipper[last]->GetClippedOutput()->GetCellData()->SetScalars(scalars);
	scalars->Delete();
	m_Append->Update();
	output->CopyStructure(m_Append->GetOutput());
	output->GetCellData()->SetScalars(m_Append->GetOutput()->GetCellData()->GetScalars());
}
