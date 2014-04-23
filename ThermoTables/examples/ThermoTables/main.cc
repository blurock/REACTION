/*  FILE     
**  PACKAGE     REACTION    
**  AUTHOR   Edward S. Blurock
**
**  CONTENT
**    
**
**  REFERENCES
**
**  COPYRIGHT (C) 1995  REACTION Project / Edward S. Blurock 
*/
 
#include "System.hh"
#include "Vector.hh"
#include "MatrixNumeric.hh"
#include "MatrixUtilities.hh"
#include "PrimitiveStats.hh"
#include "ThermoTables.hh"


void ChemkinTest();

main (int argc, char *argv[])
     {
     ChemkinTest();
     }
void ChemkinTest()
     {
     ChemkinBaseTableObject ChemkinSample;
     
     ifstream myin("inchemkin");
     if(!myin) 
	  {
	  cout <<  "Cannot open file\n";
	  };

     myin >> ChemkinSample;
     cout << " ----------------------------" << endl;
     cout << " made chemkin input " << endl;
     ChemkinSample.print(cout);
     cout << " ----------------------------" << endl; 
     cout << "Copy Chemkin\n";
     ChemkinBaseTableObject *newobject = new ChemkinBaseTableObject;
     *newobject = ChemkinSample;
     newobject->print(cout);
     cout << endl << " ----------------------------" << endl; 
     CommBuffer buffer(COMM_BUFF_ENCODE);
     newobject->EncodeThis(buffer);
     delete newobject;
     CommBuffer newbuffer(buffer.GetBuffer(),buffer.GetBufferSize());
     ChemkinBaseTableObject dobject;
     dobject.DecodeThis(newbuffer);
     dobject.print(cout);
     cout << "\n-----------------------------\n";
     cout << "Benson from Chemkin\n";
     ChemkinSample.print(cout);
     cout << endl;
     BensonBaseTableObject benson(ChemkinSample);
     benson.print(cout);
     cout << "\n-----------------------------\n";
     cout << "EncodeDecode\n";
     CommBuffer bbuffer(COMM_BUFF_ENCODE);
     benson.EncodeThis(bbuffer);
     CommBuffer newbbuffer(bbuffer.GetBuffer(),bbuffer.GetBufferSize());
     BensonBaseTableObject newbenson;
     newbenson.DecodeThis(newbbuffer);
     cout << "\n-----------------------------\n";     
     cout << "Benson After Decode" << endl;
     newbenson.print(cout);
     cout << "\n-----------------------------\n";
     String title("Benson");
     OutputThermoTableStyle thermout;
     thermout.OutputThermoElementLaTeX(cout,newbenson,title);
     thermout.OutputThermoElementASCII(cout,newbenson,title);
     }

     
