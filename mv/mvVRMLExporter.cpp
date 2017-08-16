#include "mvVRMLExporter.h"

#include "vtkVRMLExporter.h"
#include "vtkGeometryFilter.h"
#include "vtkPolyDataMapper.h"
#include "vtkMath.h"
#include "vtkAssemblyNode.h"

mvVRMLExporter::mvVRMLExporter()
{

}

mvVRMLExporter::~mvVRMLExporter()
{

}
 
void mvVRMLExporter::WriteData()
{
  vtkRenderer *ren;
  vtkActorCollection *ac;
  vtkActor *anActor, *aPart;
  vtkLightCollection *lc;
  vtkLight *aLight;
  vtkCamera *cam;
//  float *tempf;
  FILE *fp;
  
  // make sure the user specified a FileName or FilePointer
  if (!this->FilePointer && (this->FileName == NULL))
    {
    vtkErrorMacro(<< "Please specify FileName to use");
    return;
    }

  // first make sure there is only one renderer in this rendering window
  if (this->RenderWindow->GetRenderers()->GetNumberOfItems() > 1)
    {
    vtkErrorMacro(<< "VRML files only support one renderer per window.");
    return;
    }

  // get the renderer
  this->RenderWindow->GetRenderers()->InitTraversal();
  ren = this->RenderWindow->GetRenderers()->GetNextItem();
  
  // make sure it has at least one actor
  if (ren->GetActors()->GetNumberOfItems() < 1)
    {
    vtkErrorMacro(<< "no actors found for writing VRML file.");
    return;
    }
    
  // try opening the files
  if (!this->FilePointer)
    {
    fp = fopen(this->FileName,"w");
    if (!fp)
      {
      vtkErrorMacro(<< "unable to open VRML file " << this->FileName);
      return;
      }
    }
  else
    {
    fp = this->FilePointer;
    }
  
  //
  //  Write header
  //
  vtkDebugMacro("Writing VRML file");
  fprintf(fp,"#VRML V2.0 utf8\n");
  fprintf(fp,"# VRML file written by the visualization toolkit\n\n");

  // Start write the Background
  float background[3];
  ren->GetBackground(background);
  fprintf(fp,"    Background {\n ");
  fprintf(fp,"   skyColor [%f %f %f, ]\n", background[0], 
          background[1], background[2]);
  fprintf(fp,"    }\n ");
  // End of Background

  // do the camera
  cam = ren->GetActiveCamera();
  fprintf(fp,"    Viewpoint\n      {\n      fieldOfView %f\n",
          cam->GetViewAngle()*3.1415926/180.0);
  fprintf(fp,"      position %f %f %f\n",cam->GetPosition()[0],
          cam->GetPosition()[1], cam->GetPosition()[2]);
  fprintf(fp,"      description \"Default View\"\n");
  /****Start of modification for Model Viewer****/
  //tempf = cam->GetOrientationWXYZ();
  //fprintf(fp,"      orientation %g %g %g %g\n      }\n", tempf[1], tempf[2], 
  //        tempf[3], tempf[0]*3.1415926/180.0);
  // The above 2 statements in the original code is replaced by revised code below,
  // which is a modified version of vtkTransform::GetOrientationWXYZ
  int i;
  cam->GetViewTransformObject()->Update();
  // convenient access to matrix
  double (*matrix)[4] = cam->GetViewTransformObject()->GetMatrix()->Element;
  double ortho[3][3], wxyz[4];

  for (i = 0; i < 3; i++)
    {
    ortho[i][0] = matrix[0][i];  //ortho is transposed from original code
    ortho[i][1] = matrix[1][i];
    ortho[i][2] = matrix[2][i];
    }
  if (vtkMath::Determinant3x3(ortho) < 0)
    {
    ortho[0][i] = -ortho[0][i];
    ortho[1][i] = -ortho[1][i];
    ortho[2][i] = -ortho[2][i];
    }

  vtkMath::Matrix3x3ToQuaternion(ortho, wxyz);

  // calc the return value wxyz
 double mag = sqrt(wxyz[1]*wxyz[1] + wxyz[2]*wxyz[2] + wxyz[3]*wxyz[3]);

  if (mag)
    {
    wxyz[0] = 2.0*acos(wxyz[0])/vtkMath::DoubleDegreesToRadians();
    wxyz[1] /= mag;
    wxyz[2] /= mag;
    wxyz[3] /= mag;
    }
  else
    {
    wxyz[0] = 0.0;
    wxyz[1] = 0.0;
    wxyz[2] = 0.0;
    wxyz[3] = 1.0;
    }

  fprintf(fp,"      orientation %g %g %g %g\n      }\n", (float) wxyz[1], (float) wxyz[2], 
          (float) wxyz[3], ((float) wxyz[0])*3.1415926/180.0);
  /****End of modification for Model Viewer****/

  // do the lights first the ambient then the others
  fprintf(fp,"    NavigationInfo {\n      type [\"EXAMINE\",\"FLY\"]\n      speed 4.0\n");
  if (ren->GetLights()->GetNumberOfItems() == 0)
    {
    fprintf(fp,"      headlight TRUE}\n\n");
    }
  else
    {
    fprintf(fp,"      headlight FALSE}\n\n");
    }
  fprintf(fp,"    DirectionalLight { ambientIntensity 1 intensity 0 # ambient light\n");
  fprintf(fp,"      color %f %f %f }\n\n", ren->GetAmbient()[0],
          ren->GetAmbient()[1], ren->GetAmbient()[2]);
  
  // make sure we have a default light
  // if we dont then use a headlight
  lc = ren->GetLights();
  for (lc->InitTraversal(); (aLight = lc->GetNextItem()); )
    {
    this->WriteALight(aLight, fp);
    }

  // do the actors now
  ac = ren->GetActors();
  vtkAssemblyPath *apath;
  for (ac->InitTraversal(); (anActor = ac->GetNextActor()); )
    {
    for (anActor->InitPathTraversal(); (apath=anActor->GetNextPath()); )
      {
      aPart=(vtkActor *)apath->GetLastNode()->GetProp();
      this->WriteAnActor(aPart, fp);
      }
    }

  if (!this->FilePointer)
    {
    fclose(fp);
    }
}

void mvVRMLExporter::WriteAnActor(vtkActor *anActor, FILE *fp)
{
    /**** Start of modification for Model Viewer ****/
	//    Ignore the actor if it is not visible
	if (!anActor->GetVisibility())
	{
		return;
	}
    /**** End of modification for Model Viewer ****/
  vtkDataSet *ds;
  vtkPolyData *pd;
  vtkGeometryFilter *gf = NULL;
  vtkPointData *pntData;
  vtkPoints *points = NULL;
  vtkDataArray *normals = NULL;
  vtkDataArray *tcoords = NULL;
  int i, i1, i2;
  vtkProperty *prop;
  float *tempf;
  vtkCellArray *cells;
  vtkIdType npts = 0;
  vtkIdType *indx = 0;
  float tempf2;
  int pointDataWritten = 0;
  vtkPolyDataMapper *pm;
  vtkUnsignedCharArray *colors;
  float *p;
  unsigned char *c;
  vtkTransform *trans;
  int totalValues;
  
  // see if the actor has a mapper. it could be an assembly
  if (anActor->GetMapper() == NULL)
    {
    return;
    }

  // first stuff out the transform
  trans = vtkTransform::New();
  trans->SetMatrix(anActor->vtkProp3D::GetMatrix());
  
  fprintf(fp,"    Transform {\n");
  tempf = trans->GetPosition();
  fprintf(fp,"      translation %g %g %g\n", tempf[0], tempf[1], tempf[2]);
  tempf = trans->GetOrientationWXYZ();
  fprintf(fp,"      rotation %g %g %g %g\n", tempf[1], tempf[2], 
          tempf[3], tempf[0]*3.1415926/180.0);
  tempf = trans->GetScale();
  fprintf(fp,"      scale %g %g %g\n", tempf[0], tempf[1], tempf[2]);
  fprintf(fp,"      children [\n");
  trans->Delete();
  
  // get the mappers input and matrix
  ds = anActor->GetMapper()->GetInput();
  
  // we really want polydata
  if ( ds->GetDataObjectType() != VTK_POLY_DATA )
    {
    gf = vtkGeometryFilter::New();
    gf->SetInput(ds);
    gf->Update();
    pd = gf->GetOutput();
    }
  else
    {
    pd = (vtkPolyData *)ds;
    }

  pm = vtkPolyDataMapper::New();
  pm->SetInput(pd);
  pm->SetScalarRange(anActor->GetMapper()->GetScalarRange());
  pm->SetScalarVisibility(anActor->GetMapper()->GetScalarVisibility());
  pm->SetLookupTable(anActor->GetMapper()->GetLookupTable());
  pm->SetScalarMode(anActor->GetMapper()->GetScalarMode());

  points = pd->GetPoints();
  pntData = pd->GetPointData();
  normals = pntData->GetNormals();
  tcoords = pntData->GetTCoords();
  colors  = pm->MapScalars(1.0);
  
  fprintf(fp,"        Shape {\n");
  
  // write out the material properties to the mat file
  fprintf(fp,"          appearance Appearance {\n");
  fprintf(fp,"            material Material {\n");
  prop = anActor->GetProperty();
  fprintf(fp,"              ambientIntensity %g\n", prop->GetAmbient());
  // if we don't have colors and we have only lines & points
  // use emissive to color them
  if (!(normals || colors || pd->GetNumberOfPolys() || 
        pd->GetNumberOfStrips()))
    {
    tempf2 = prop->GetAmbient();
    tempf = prop->GetAmbientColor();
    fprintf(fp,"              emissiveColor %g %g %g\n",
	        tempf[0]*tempf2, tempf[1]*tempf2, tempf[2]*tempf2);
    }
  tempf2 = prop->GetDiffuse();
  tempf = prop->GetDiffuseColor();
  fprintf(fp,"              diffuseColor %g %g %g\n",
          tempf[0]*tempf2, tempf[1]*tempf2, tempf[2]*tempf2);
  tempf2 = prop->GetSpecular();
  tempf = prop->GetSpecularColor();
  fprintf(fp,"              specularColor %g %g %g\n",
          tempf[0]*tempf2, tempf[1]*tempf2, tempf[2]*tempf2);
  fprintf(fp,"              shininess %g\n",prop->GetSpecularPower()/128.0);
  fprintf(fp,"              transparency %g\n",1.0 - prop->GetOpacity());
  fprintf(fp,"              }\n"); // close matrial

  // is there a texture map
  if (anActor->GetTexture())
    {
    vtkTexture *aTexture = anActor->GetTexture();
    int *size, xsize, ysize, bpp;
    vtkDataArray *scalars;
    vtkDataArray *mappedScalars;
    unsigned char *txtrData;
    
    // make sure it is updated and then get some info
    if (aTexture->GetInput() == NULL)
      {
      vtkErrorMacro(<< "texture has no input!\n");
      return;
      }
    aTexture->GetInput()->Update();
    size = aTexture->GetInput()->GetDimensions();
    scalars = aTexture->GetInput()->GetPointData()->GetScalars();

    // make sure scalars are non null
    if (!scalars) 
      {
      vtkErrorMacro(<< "No scalar values found for texture input!\n");
      return;
      }

    // make sure using unsigned char data of color scalars type
    if (aTexture->GetMapColorScalarsThroughLookupTable () ||
        (scalars->GetDataType() != VTK_UNSIGNED_CHAR) )
      {
      mappedScalars = aTexture->GetMappedScalars ();
      }
    else
      {
      mappedScalars = scalars;
      }

    // we only support 2d texture maps right now
    // so one of the three sizes must be 1, but it 
    // could be any of them, so lets find it
    if (size[0] == 1)
      {
      xsize = size[1]; ysize = size[2];
      }
    else
      {
      xsize = size[0];
      if (size[1] == 1)
        {
        ysize = size[2];
        }
      else
        {
        ysize = size[1];
        if (size[2] != 1)
          {
          vtkErrorMacro(<< "3D texture maps currently are not supported!\n");
          return;
          }
        }
      }

    fprintf(fp,"            texture PixelTexture {\n");
    bpp = mappedScalars->GetNumberOfComponents();
    fprintf(fp,"              image %i %i %i\n", xsize, ysize, bpp);
    txtrData = static_cast<vtkUnsignedCharArray*>(mappedScalars)->GetPointer(0);
    totalValues = xsize*ysize;
    for (i = 0; i < totalValues; i++)
      {
      fprintf(fp,"0x%.2x",*txtrData);
      txtrData++;
      if (bpp > 1) 
		{
		fprintf(fp,"%.2x",*txtrData);
		txtrData++;
		}
      if (bpp > 2) 
		{
		fprintf(fp,"%.2x",*txtrData);
		txtrData++;
		}
      if (bpp > 3) 
		{
		fprintf(fp,"%.2x",*txtrData);
		txtrData++;
		}
      if (i%8 == 0)
		{
		fprintf(fp,"\n");
		}
      else
		{
		fprintf(fp," ");
		}
      }
    if (!(aTexture->GetRepeat()))
      {
      fprintf(fp,"              repeatS FALSE\n");
      fprintf(fp,"              repeatT FALSE\n");
      }
    fprintf(fp,"              }\n"); // close texture
    }
  fprintf(fp,"            }\n"); // close appearance

  /**** Start of modification for Model Viewer ****/
  int colorPerFace = (anActor->GetMapper()->GetScalarMode() == VTK_SCALAR_MODE_USE_CELL_DATA)
		|| ((pntData->GetScalars() == NULL && pd->GetCellData()->GetScalars != NULL));

  /**** End of modification for Model Viewer ****/

  // write out polys if any
  if (pd->GetNumberOfPolys() > 0)
    {
    fprintf(fp,"          geometry IndexedFaceSet {\n");
    // two sided lighting ? for now assume it is on
    fprintf(fp,"            solid FALSE\n");
    if (!pointDataWritten)
      {
      this->WritePointData(points, normals, tcoords, colors, fp);
      pointDataWritten = 1;
      }
    else
      {
      fprintf(fp,"            coord  USE VTKcoordinates\n");
      if (normals)
		{
		fprintf(fp,"            normal  USE VTKnormals\n");
		}
      if (tcoords)
		{
		fprintf(fp,"            texCoord  USE VTKtcoords\n");
		}
      if (colors)
		{
		fprintf(fp,"            color  USE VTKcolors\n");
		}
      }
    
    fprintf(fp,"            coordIndex  [\n");
    
    cells = pd->GetPolys();
    for (cells->InitTraversal(); cells->GetNextCell(npts,indx); )
      {
      fprintf(fp,"              ");
      for (i = 0; i < npts; i++)
        {
        // treating vtkIdType as int
        fprintf(fp,"%i, ", (int)indx[i]);
        }
      fprintf(fp,"-1,\n");
      }
    fprintf(fp,"            ]\n");
    /**** Start of modification for Model Viewer ****/
	if (colorPerFace)
	{
      fprintf(fp,"            colorPerVertex FALSE\n");
	}
    /**** End of modification for Model Viewer ****/
    fprintf(fp,"          }\n");
    }

  // write out tstrips if any
  if (pd->GetNumberOfStrips() > 0)
    {
    fprintf(fp,"          geometry IndexedFaceSet {\n");
    if (!pointDataWritten)
      {
      this->WritePointData(points, normals, tcoords, colors, fp);
      pointDataWritten = 1;
      }
    else
      {
      fprintf(fp,"            coord  USE VTKcoordinates\n");
      if (normals)
		{
		fprintf(fp,"            normal  USE VTKnormals\n");
		}
      if (tcoords)
		{
		fprintf(fp,"            texCoord  USE VTKtcoords\n");
		}
      if (colors)
		{
		fprintf(fp,"            color  USE VTKcolors\n");
		}
      }
    fprintf(fp,"            coordIndex  [\n");
    cells = pd->GetStrips();
    for (cells->InitTraversal(); cells->GetNextCell(npts,indx); )
      {
      for (i = 2; i < npts; i++)
		{
		if (i%2)
		  {
		  i1 = i - 1;
		  i2 = i - 2;
		  }
		else
		  {
		  i1 = i - 2;
		  i2 = i - 1;
		  }
        // treating vtkIdType as int
        fprintf(fp,"              %i, %i, %i, -1,\n", (int)indx[i1], 
                (int)indx[i2], (int)indx[i]);
		}
      }
    fprintf(fp,"            ]\n");
    fprintf(fp,"          }\n");
    }
  
  // write out lines if any
  if (pd->GetNumberOfLines() > 0)
    {
    fprintf(fp,"          geometry IndexedLineSet {\n");
    if (!pointDataWritten)
      {
      this->WritePointData(points, NULL, NULL, colors, fp);
      pointDataWritten = 1;
      }
    else
      {
      fprintf(fp,"            coord  USE VTKcoordinates\n");
      if (colors)
		{
		fprintf(fp,"            color  USE VTKcolors\n");
		}
      }
    
    fprintf(fp,"            coordIndex  [\n");
    
    cells = pd->GetLines();
    for (cells->InitTraversal(); cells->GetNextCell(npts,indx); )
      {
      fprintf(fp,"              ");
      for (i = 0; i < npts; i++)
		{
        // treating vtkIdType as int
        fprintf(fp,"%i, ", (int)indx[i]);
		}
      fprintf(fp,"-1,\n");
      }
    fprintf(fp,"            ]\n");
    /**** Start of modification for Model Viewer ****/
	if (colorPerFace)
	{
      fprintf(fp,"            colorPerVertex FALSE\n");
	}
    /**** End of modification for Model Viewer ****/
    fprintf(fp,"          }\n");
    }

  // write out verts if any
  if (pd->GetNumberOfVerts() > 0)
    {
    fprintf(fp,"          geometry PointSet {\n");
    cells = pd->GetVerts();
    fprintf(fp,"            coord Coordinate {");
    fprintf(fp,"              point [");
    for (cells->InitTraversal(); cells->GetNextCell(npts,indx); )
      {
      fprintf(fp,"              ");
      for (i = 0; i < npts; i++)
		{
		p = points->GetPoint(indx[i]);
		fprintf (fp,"              %g %g %g,\n", p[0], p[1], p[2]);
		}
      }
    fprintf(fp,"              ]\n");
    fprintf(fp,"            }\n");
    if (colors)
		{
		fprintf(fp,"            color Color {");
		fprintf(fp,"              color [");
		for (cells->InitTraversal(); cells->GetNextCell(npts,indx); )
		  {
		  fprintf(fp,"              ");
		  for (i = 0; i < npts; i++)
			{
			c = colors->GetPointer(4*indx[i]);
			fprintf (fp,"           %g %g %g,\n", c[0]/255.0, c[1]/255.0, 
				 c[2]/255.0);
			}
		  }
		fprintf(fp,"              ]\n");
		fprintf(fp,"            }\n");
		}
  
    fprintf(fp,"          }\n");
    }

  fprintf(fp,"        }\n"); // close the  Shape
  fprintf(fp,"      ]\n"); // close the original transforms children
  fprintf(fp,"    }\n"); // close the original transform
  
  if (gf)
    {
    gf->Delete();
    }
  pm->Delete();
}


void mvVRMLExporter::WritePointData(vtkPoints *points, vtkDataArray *normals,
                                     vtkDataArray *tcoords, 
                                     vtkUnsignedCharArray *colors, FILE *fp)
{
  float *p;
  int i;
  unsigned char *c;
  
  // write out the points
  fprintf(fp,"            coord DEF VTKcoordinates Coordinate {\n");
  fprintf(fp,"              point [\n");
  for (i = 0; i < points->GetNumberOfPoints(); i++)
    {
    p = points->GetPoint(i);
    fprintf (fp,"              %g %g %g,\n", p[0], p[1], p[2]);
    }
  fprintf(fp,"              ]\n");
  fprintf(fp,"            }\n");
  
  // write out the point data
  if (normals)
    {
    fprintf(fp,"            normal DEF VTKnormals Normal {\n");
    fprintf(fp,"              vector [\n");
    for (i = 0; i < normals->GetNumberOfTuples(); i++)
      {
      p = normals->GetTuple(i);
      fprintf (fp,"           %g %g %g,\n", p[0], p[1], p[2]);
      }
    fprintf(fp,"            ]\n");
    fprintf(fp,"          }\n");
    }

  // write out the point data
  if (tcoords)
    {
    fprintf(fp,"            texCoord DEF VTKtcoords TextureCoordinate {\n");
    fprintf(fp,"              point [\n");
    for (i = 0; i < tcoords->GetNumberOfTuples(); i++)
      {
      p = tcoords->GetTuple(i);
      fprintf (fp,"           %g %g,\n", p[0], p[1]);
      }
    fprintf(fp,"            ]\n");
    fprintf(fp,"          }\n");
    }

  // write out the point data
  if (colors)
    {
    fprintf(fp,"            color DEF VTKcolors Color {\n");
    fprintf(fp,"              color [\n");
    for (i = 0; i < colors->GetNumberOfTuples(); i++)
      {
      c = colors->GetPointer(4*i);
      fprintf (fp,"           %g %g %g,\n", c[0]/255.0, c[1]/255.0, 
	       c[2]/255.0);
      }
    fprintf(fp,"            ]\n");
    fprintf(fp,"          }\n");
    }
}

