/**
    Alegría 2D Game Engine Copyright (C) 2010 J.G. Camarasa <pepius@gmail.com>
    
    This file is part of Alegría.
    
    Alegría is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.

*/

#include <fstream>
#include <iostream>


#include "AIO_Sound.hpp"


using namespace std;




AIO_Sound::AIO_Sound():m_loaded(false),m_name("NONAME"){}

AIO_Sound::~AIO_Sound(){
    Free();
}

string AIO_Sound::GetName() const{
    return m_name;
}

const string& AIO_Sound::GetPath() const{
	return m_path;
}

void AIO_Sound::SetName(const string &name){
    m_name = name;
}

/*
 * Struct that holds the RIFF data of the Wave file.
 * The RIFF data is the meta data information that holds,
 * the ID, size and format of the wave file
 */
struct RIFF_Header {
  char chunkID[4];
  long chunkSize;//size not including chunkSize or chunkID
  char format[4];
};

	
/*
 * Struct to hold fmt subchunk data for WAVE files.
 */
struct WAVE_Format {
  char subChunkID[4];
  long subChunkSize;
  short audioFormat;
  short numChannels;
  long sampleRate;
  long byteRate;
  short blockAlign;
  short bitsPerSample;
};

/*
* Struct to hold the data of the wave file
*/
struct WAVE_Data {
  char subChunkID[4]; //should contain the word data
  long subChunk2Size; //Stores the size of the data block
};

/*
 * Load wave file function. No need for ALUT with this
 */
bool loadWavFile(const string &filename, ALuint* buffer,
                 ALsizei* size, ALsizei* frequency,
                 ALenum* format) {
  //Local Declarations
  FILE* soundFile = NULL;
  WAVE_Format wave_format;
  RIFF_Header riff_header;
  WAVE_Data wave_data;
  unsigned char* data;
 
  try {
    soundFile = fopen(filename.c_str(), "rb");
    if (!soundFile)
      throw (filename);
 
    // Read in the first chunk into the struct
    fread(&riff_header, sizeof(RIFF_Header), 1, soundFile);
 
    //check for RIFF and WAVE tag in memeory
    if ((riff_header.chunkID[0] != 'R' ||
         riff_header.chunkID[1] != 'I' ||
         riff_header.chunkID[2] != 'F' ||
         riff_header.chunkID[3] != 'F') ||
        (riff_header.format[0] != 'W' ||
         riff_header.format[1] != 'A' ||
         riff_header.format[2] != 'V' ||
         riff_header.format[3] != 'E'))
             throw ("Invalid RIFF or WAVE Header");
 
    //Read in the 2nd chunk for the wave info
    fread(&wave_format, sizeof(WAVE_Format), 1, soundFile);
    //check for fmt tag in memory
    if (wave_format.subChunkID[0] != 'f' ||
        wave_format.subChunkID[1] != 'm' ||
        wave_format.subChunkID[2] != 't' ||
        wave_format.subChunkID[3] != ' ')
             throw ("Invalid Wave Format");
 
    //check for extra parameters;
    if (wave_format.subChunkSize > 16)
        fseek(soundFile, sizeof(short), SEEK_CUR);
 
    //Read in the the last byte of data before the sound file
    fread(&wave_data, sizeof(WAVE_Data), 1, soundFile);
    //check for data tag in memory
    if (wave_data.subChunkID[0] != 'd' ||
        wave_data.subChunkID[1] != 'a' ||
        wave_data.subChunkID[2] != 't' ||
        wave_data.subChunkID[3] != 'a')
             throw ("Invalid data header");
 
    //Allocate memory for data
    data = new unsigned char[wave_data.subChunk2Size];
 
    // Read in the sound data into the soundData variable
    if (!fread(data, wave_data.subChunk2Size, 1, soundFile))
        throw ("error loading WAVE data into struct!");
 
    //Now we set the variables that we passed in with the
    //data from the structs
    *size = wave_data.subChunk2Size;
    *frequency = wave_format.sampleRate;
    //The format is worked out by looking at the number of
    //channels and the bits per sample.
    if (wave_format.numChannels == 1) {
        if (wave_format.bitsPerSample == 8 )
            *format = AL_FORMAT_MONO8;
        else if (wave_format.bitsPerSample == 16)
            *format = AL_FORMAT_MONO16;
    } else if (wave_format.numChannels == 2) {
        if (wave_format.bitsPerSample == 8 )
            *format = AL_FORMAT_STEREO8;
        else if (wave_format.bitsPerSample == 16)
            *format = AL_FORMAT_STEREO16;
    }

	alGetError();

    //create our openAL buffer and check for success
    alGenBuffers(1, buffer);

	if(alGetError()!=AL_NO_ERROR)
		throw("error alGenBuffers");
    //now we put our data into the openAL buffer and
    //check for success
    alBufferData(*buffer, *format, (void*)data,
                 *size, *frequency);

	if(alGetError()!=AL_NO_ERROR)
		throw("error alBufferData");

    //clean up and return true if successful
    fclose(soundFile);
    return true;
  } catch(std::string error) {
    //our catch statement for if we throw a string
    std::cerr << error << " : trying to load "
              << filename << std::endl;
    //clean up memory if wave loading fails
    if (soundFile != NULL)
        fclose(soundFile);
    //return false to indicate the failure to load wave
    return false;
  }
}

int AIO_Sound::Load(const string  &filename, const string &idpath){
	
	m_loaded = loadWavFile(filename, &m_buffername, &m_size, &m_frequency, &m_format);

	if(!m_loaded)
		return 0;

	alGenSources(1,&m_sourcename);
	


	m_path = idpath;

    return 1;

}


ALuint AIO_Sound::GetBufferName() const{
    return m_buffername;
}

ALuint AIO_Sound::GetSourceName() const{
    return m_sourcename;
}
   
   

int AIO_Sound::Free(){
    if(!m_loaded){
        return 0;
    }
   
	alDeleteBuffers(1, &m_buffername);
    
    m_loaded = false;
    return 1;

}

