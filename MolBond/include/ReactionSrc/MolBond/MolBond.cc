/*  FILE     MolBond.cc
**  PACKAGE  MolBond
**  AUTHOR   Edward S. Blurock
**
**  CONTENT
**    Exported functions for the "MolBond" package.
**
**  REFERENCES
**
**  COPYRIGHT (C) REACTION Project, Edward S. Blurock
*/

 
/*I  . . . INCLUDES  . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
*/
#define TEMPLATE_INSTANTIATION
#include "Basis/System.hh"
#include "Basis/Pairs.hh"
#include "Reaction/MolBond.hh"

/*S IO
*/
/*F out = operator<<(out,connect) . . . . . . . . . . . . .  SimpleConnection
**
**  DESCRIPTION
**    out: The output stream
**    connect: The bond
**
**  REMARKS
**
*/
ostream& operator<<(ostream& out, const SimpleConnection& connect)
     {
     return connect.print(out);
     }

/*F out = operator<<(out,connect) . . . . . . . . . . . . . . . .  SimpleBond
**
**  DESCRIPTION
**    out: The output stream
**    connect: The bond
**
**  REMARKS
**
*/
ostream& operator<<(ostream& out, const SimpleBond& bond)     
     {
     return bond.print(out);
     }
/*F out = operator<<(out,bond)  . . . . . . . . . . . . . . . . . MolFileBond
**
**  DESCRIPTION
**    out: The output stream
**    bond: The bond
**
**  REMARKS
**
*/
ostream& operator<<(ostream& out, const MolFileBond& bond)
     {
     return bond.print(out);
     } 
/*F out = operator<<(out,bond)  . . . . . . . . . . . . . . . .  MoleculeBond
**
**  DESCRIPTION
**    out: The output stream
**
**  REMARKS
**
*/
ostream& operator<<(ostream& out, const MoleculeBond& bond)
     {
     return bond.print(out);
     }





 
/*F ans = operator==(b1,b2) . . . . . . . . . . . . . . . .  SimpleConnection
**
**  DESCRIPTION
**    b1,b2: The bond
**    ans: true if equal
**  REMARKS
**
*/
bool operator==(const SimpleConnection& b1, const SimpleConnection& b2)
{
  return ( (b1.I == b2.I) && (b1.J == b2.J) ) ||
    ( (b1.I == b2.J) && (b1.J == b2.I) );
}
/*F ans = operator==(b1,b2) . . . . . . . . . . . . . . . . . . .  SimpleBond
**
**  DESCRIPTION
**    b1,b2: The bond
**    ans: true if equal
**  REMARKS
**
*/
bool operator==(const SimpleBond& b1, const SimpleBond& b2)
{
  return (b1.BondOrder == b2.BondOrder) &&
    (( (b1.I == b2.I) && (b1.J == b2.J) ) ||
     ( (b1.I == b2.J) && (b1.J == b2.I) ));
}
/*F ans = operator==(b1,b2) . . . . . . . . . . . . . . . . . . . MolFileBond
**
**  DESCRIPTION
**    b1,b2: The bond
**    ans: true if equal
**  REMARKS
**
*/
bool operator==(const MolFileBond& b1, const MolFileBond& b2)
{
  return (b1.BondOrder == b2.BondOrder) &&
    (( (b1.I == b2.I) && (b1.J == b2.J) ) ||
     ( (b1.I == b2.J) && (b1.J == b2.I) ));
}
/*F ans = operator==(b1,b2) . . . . . . . . . . . . . . . . . . . MoleculeBond
**
**  DESCRIPTION
**    b1,b2: The bond
**    ans: true if equal
**  REMARKS
**
*/
bool operator==(const MoleculeBond& b1, const MoleculeBond& b2)
{
  return (b1.BondOrder == b2.BondOrder) &&
    (( (b1.I == b2.I) && (b1.J == b2.J) ) ||
     ( (b1.I == b2.J) && (b1.J == b2.I) ));
}
/*F ans = operator<(b1,b2) . . . . . . . . . . . . . . . . . . . MolFileBond
**
**  DESCRIPTION
**    b1,b2: The bond
**    ans: true if equal
**  REMARKS
**
*/
bool operator<(const MolFileBond& b1, const MolFileBond& b2)
{
  bool ans = true;
  if (b1.BondOrder > b2.BondOrder)
    ans = false;
  else if (b1.BondOrder < b2.BondOrder)
    ans = true;
  else if(b1.I > b2.I)
    ans = false;
  else if(b1.I < b2.I)
    ans = true;
  else if(b1.I < b2.I)
    ans = true;
  else 
    ans = false;
  
  return ans;
}
/*F ans = operator<(b1,b2)  . . . . . . . . . . . . . . . . . .  MoleculeBond
**
**  DESCRIPTION
**    b1,b2: The bond
**    ans: true if equal
**  REMARKS
**
*/
bool operator<(const MoleculeBond& b1, const MoleculeBond& b2)
{
  bool ans = true;
  if (b1.BondOrder > b2.BondOrder)
    ans = false;
  else if (b1.BondOrder < b2.BondOrder)
    ans = true;
  else if(b1.I > b2.I)
    ans = false;
  else if(b1.I < b2.I)
    ans = true;
  else if(b1.I < b2.I)
    ans = true;
  else 
    ans = false;
  
  return ans;
}

/*S EnocdeDecode
 */
/*F ans = Encode(buffer,bond) . . . . . . . . . . . . . . .  SimpleConnection
**
**  DESCRIPTION
**    buffer: The buffer
**    bond: The bond
**    ans: True if successful
**
**  REMARKS
**
*/
bool Encode(CommBuffer& buffer, SimpleConnection& bond)
     {
     return bond.EncodeThis(buffer);
     }
/*F ans = Decode(buffer,bond) . . . . . . . . . . . . . . .  SimpleConnection
**
**  DESCRIPTION
**    buffer: The buffer
**    bond: The bond
**    ans: True if successful
**
**  REMARKS
**
*/
bool Decode(CommBuffer& buffer, SimpleConnection& bond)
     {
     return bond.DecodeThis(buffer);
     }
/*F ans = Encode(buffer,bond) . . . . . . . . . . . . . . . . . .  SimpleBond
**
**  DESCRIPTION
**    buffer: The buffer
**    bond: The bond
**    ans: True if successful
**
**  REMARKS
**
*/
bool Encode(CommBuffer& buffer, SimpleBond& bond)
     {
     return bond.EncodeThis(buffer);
     }
/*F ans = Decode(buffer,bond) . . . . . . . . . . . . . . . . . .  SimpleBond
**
**  DESCRIPTION
**    buffer: The buffer
**    bond: The bond
**    ans: True if successful
**
**  REMARKS
**
*/
bool Decode(CommBuffer& buffer, SimpleBond& bond)
     {
     return bond.DecodeThis(buffer);
     }
/*F ans = Encode(buffer,bond) . . . . . . . . . . . . . . . . . . MolFileBond
**
**  DESCRIPTION
**    buffer: The buffer
**    bond: The bond
**    ans: True if successful
**
**  REMARKS
**
*/
bool Encode(CommBuffer& buffer, MolFileBond& bond)
     {
     return bond.EncodeThis(buffer);
     }
/*F ans = Decode(buffer,bond) . . . . . . . . . . . . . . . . . . MolFileBond
**
**  DESCRIPTION
**    buffer: The buffer
**    bond: The bond
**    ans: True if successful
**
**  REMARKS
**
*/
bool Decode(CommBuffer& buffer, MolFileBond& bond)
     {
     return bond.DecodeThis(buffer);
     }
/*F ans = Encode(buffer,bond) . . . . . . . . . . . . . . . . .  MoleculeBond
**
**  DESCRIPTION
**    buffer: The buffer
**    bond: The bond
**    ans: True if successful
**
**  REMARKS
**
*/
bool Encode(CommBuffer& buffer, MoleculeBond& bond)
     {
     return bond.EncodeThis(buffer);
     }
/*F ans = Decode(buffer,bond) . . . . . . . . . . . . . . . . .  MoleculeBond
**
**  DESCRIPTION
**    buffer: The buffer
**    bond: The bond
**    ans: True if successful
**
**  REMARKS
**
*/
bool Decode(CommBuffer& buffer, MoleculeBond& bond)
     {
     return bond.DecodeThis(buffer);
     }

