/*  FILE     MechanismGraph.cc
**  PACKAGE  MechanismGraph
**  AUTHOR   Edward S. Blurock
**
**  CONTENT
**    Exported functions for the "MechanismGraph" package.
**
**  REFERENCES
**
**  COPYRIGHT (C) 1997 Edward S. Blurock
*/

 
/*I  . . . INCLUDES  . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
*/
#include "FullSystem.hh"
#include "Dbase.hh"
#include "GeneralGraph.hh"
#include "StaticAtom.hh"
#include "MolAtom.hh"
#include "MolBond.hh"
#include "Molecule.hh"
#include "ThermoProps.hh"
#include "Rxn.hh"
#include "Mechanism.hh"
#include "MechanismGraph.hh"

/*S RxnDataMechanismNode
 */ 
/*F RxnDataMechanismNode()  . . . . . . . . . . . . . . . . . . . empty constructor
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
RxnDataMechanismNode::RxnDataMechanismNode()
{
  Identification = MECHANISMGRAPH_NODE_ID;
  NameTag = MECHANISMGRAPH_NODE_NAME;
  SetType(Identification);
  String name("Object.");
  name.AppendToEnd(NameTag);
  EncodeDecodeClass = name;
} 
/*F RxnDataMechanismNode(data)  . . . . . . . . . . . . . . . . .  copy constructor
**
**  DESCRIPTION
**    data: data to be copied
**
**  REMARKS
**
*/
RxnDataMechanismNode::RxnDataMechanismNode(const RxnDataMechanismNode& data)
  : BaseDataNode(data)
{
}
/*F Read(in,objc) . . . . . . . . . . . . . . . . . .  Read in RxnDataMechanismNode
**
**  DESCRIPTION
**    in: The input stream
**    objc: The class definition
**
**  REMARKS
**
*/
bool RxnDataMechanismNode::Read(istream& in, DataObjectClass* objc)
{
  StreamObjectInput str(in,' ');
  NameTag = str.ReadNext();
  return Read(in,objc,NameTag);
}
/*F Read(in,objc,name)  . . . . . . . . . . . . . . .  Read in RxnDataMechanismNode
**
**  DESCRIPTION
**    in: The input stream
**    objc: The class definition
**    name: The name of the object (already assigned)
**
**  REMARKS
**
*/
bool RxnDataMechanismNode::Read(istream& in, DataObjectClass* objc, const String& name)
{
  bool result = BaseDataNode::Read(in,objc,name);
  
  return result;
}
 
/*F out1 = print(out) . . . . . . . . . . . . . . . . . . . .  RxnDataMechanismNode
**
**  DESCRIPTION
**    out,out1: The output buffer
**
**  REMARKS
**
*/
ostream& RxnDataMechanismNode::print(ostream& out) const
{
  BaseDataNode::print(out);
  //PointerPrint(out,"The List of Parameters: ","No Parameters",Parameters);
  // The rest

  return out;
}
/*F obj = Clone() . . . . . . . . . . . . . . . . . . . . . . .RxnDataMechanismNode
**
**  DESCRIPTION
**    obj: The copy
**
**  REMARKS
**
*/
Identify * RxnDataMechanismNode::Clone()
{
  RxnDataMechanismNode *obj = new RxnDataMechanismNode(*this);
  return obj;
}
 
/*F CopyClone(obj)  . . . . . . . . . . . . . . . . . . . . . .RxnDataMechanismNode
**
**  DESCRIPTION
**    obj: The object to be copied
**
**  REMARKS
**
*/
void RxnDataMechanismNode::CopyClone(Identify * obj)
{
  RxnDataMechanismNode *objfull = (RxnDataMechanismNode *) obj;
  *this = *objfull;
  //Parameter = (BaseData... *) PointerClone(objfull->Parameter)
}
 
/*F ans = EncodeThis(buffer)  . . . . . . . . . . . . . . . .  RxnDataMechanismNode
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: True if successful
**
**  REMARKS
**
*/
bool RxnDataMechanismNode::EncodeThis(CommBuffer& buffer)
{
  bool result = BaseDataNode::EncodeThis(buffer);
  //result = result && ---.EncodeThis(buffer);
  //result = result && Encode(buffer,---);
  //result = result && PointerEncode(buffer,Parameters);

  return result;
}
/*F ans = DecodeThis(buffer)  . . . . . . . . . . . . . . . .  RxnDataMechanismNode
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: True if successful
**
**  REMARKS
**
*/
bool RxnDataMechanismNode::DecodeThis(CommBuffer& buffer)
{
  bool result = BaseDataNode::DecodeThis(buffer);
  // The rest

  return result;
}
 
 
/*S RxnMechanismNodeClass
 */
/*F RxnMechanismNodeClass() . . . . . . . . . . . . . . . . . . . empty constructor
**
**  DESCRIPTION
**
**  REMARKS
**
*/
RxnMechanismNodeClass::RxnMechanismNodeClass()
{
  Identification = MECHANISMGRAPH_NODE_ID;
  NameTag = MECHANISMGRAPH_NODE_NAME;
  SubClass = "Node";
  EncodeDecodeClass = NameTag;
} 
/*F RxnMechanismNodeClass(data) . . . . . . . . . . . . . . . . .  copy constructor
**
**  DESCRIPTION
**     data: The class to be copied
**
**  REMARKS
**
*/
RxnMechanismNodeClass::RxnMechanismNodeClass(const RxnMechanismNodeClass& data)
  : DataNodeClass(data)
{
} 
 
/*F RxnMechanismNodeClass(id,name,descr)  . . . . . . . . . . . . . . . constructor
**
**  DESCRIPTION
**    id: The ID of the object class
**    name: The name of the object class
**    descr: A text description of the object
**
**
**  REMARKS
**
*/
RxnMechanismNodeClass::RxnMechanismNodeClass(const int id, 
				 const String& name,
				 const String& descr)
  : DataNodeClass(id,name,descr)
{
  SubClass = "Node";
  EncodeDecodeClass = "MechanismNode";
}
 
/*F out1 = print(out) . . . . . . . . . . . . . . . . . . . . RxnMechanismNodeClass
**
**  DESCRIPTION
**    out1,out: The output buffer
**
**  REMARKS
**
*/
ostream& RxnMechanismNodeClass::print(ostream& out) const
{
  DataNodeClass::print(out);
  //PointerPrint(out,"  The Class: "," No Class Defined ",Class);
  // the rest
       
       return out;
}
 
/*F in1 = Read(in)  . . . . . . . . . . . . . . . . . . . . . RxnMechanismNodeClass
**
**  DESCRIPTION
**    in,in1: The input buffer
**
**    This reads in the rest of the information (after the type
**    has been determined from the first line.  For the
**    base RxnMechanismNodeClass, there is no further information.
**
**  REMARKS
**
*/
bool RxnMechanismNodeClass::Read(istream& in, DataSetOfObjectsClass &set)
{
  bool result = DataNodeClass::Read(in,set);
  //result = result && PointerClassRead(in,(DataObjectClass *&) Class,
  //COREOBJECTS_BASE_NAME,
  //set," No Class ");
  return result;
}
 
/*F CopyClone(objc) . . . . . . . . . . . . . . . . . . . . . RxnMechanismNodeClass
**
**  DESCRIPTION
**    objc: The class to be copied
**
**  REMARKS
**
*/
void RxnMechanismNodeClass::CopyClone(Identify *  objc)
{
  RxnMechanismNodeClass *objcfull = (RxnMechanismNodeClass *) objc;
  *this = *objcfull;
  //ParameterClass = (DataSetOfObjectsClass *) PointerClone(objcfull->ParameterClass);
}
/*F objc = Clone()  . . . . . . . . . . . . . . . . . . . . . RxnMechanismNodeClass
**
**  DESCRIPTION
**    objc: The object class that was created
**
**  REMARKS
**
*/
Identify * RxnMechanismNodeClass::Clone()
    {
      RxnMechanismNodeClass* id = new RxnMechanismNodeClass(*this);
      return (Identify *) id;
    }
/*F ans = EncodeThis(buffer)  . . . . . . . . . . . . . . . . RxnMechanismNodeClass
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: true if successful
**
**  REMARKS
**
*/
bool RxnMechanismNodeClass::EncodeThis(CommBuffer& buffer)
{
  bool result = DataNodeClass::EncodeThis(buffer);
  //result = result && PointerEncode(buffer,Class);
  // result = result && Encode(buffer,------);

  return result;
}
/*F ans = DecodeThis(buffer)  . . . . . . . . . . . . . . . . RxnMechanismNodeClass
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: true if successful
**
**  REMARKS
**
*/
bool RxnMechanismNodeClass::DecodeThis(CommBuffer& buffer)
{
  bool result = DataNodeClass::DecodeThis(buffer);
  //result = result && PointerDecode(buffer,(BaseDataObject *&) Class);
  //result = result && Decode(buffer,-----);

  return result;
}
/*F obj = BaseDataObjectExample() . . . . . . . . . . . . .  create an object
**
**  DESCRIPTION
**    obj: The created object
**
**    This function is used to create an empty instance of a object 
**    given the class.  This is used so that the virtual functions
**    of the object can be used.
**
**  REMARKS
**
*/
BaseDataObject * RxnMechanismNodeClass::BaseDataObjectExample()
{ 
  BaseDataObject *obj = new RxnDataMechanismNode();
  obj->SetType(Identification);
  return obj;
}
/*F ans = TopDecode(buffer,obj) . . . . . . . . . . . . . . . RxnMechanismNodeClass
**
**  DESCRIPTION
**    buffer: The buffer 
**    obj: The object
**    ans: True if successful
**
**  REMARKS
**
*/
bool TopDecode(CommBuffer& buffer, RxnMechanismNodeClass*& obj)
     {
     obj = new RxnMechanismNodeClass;
     return obj->DecodeThis(buffer);
     }
/*F ans = TopDecode(buffer,obj) . . . . . . . . . . . . . . . RxnDataMechanismNode
**
**  DESCRIPTION
**    buffer: The buffer 
**    obj: The object
**    ans: True if successful
**
**  REMARKS
**
*/
bool TopDecode(CommBuffer& buffer, RxnDataMechanismNode*& obj)
     {
     obj = new RxnDataMechanismNode;
     return obj->DecodeThis(buffer);
     }
/*S RxnDataMechanismEdge
 */ 
/*F RxnDataMechanismEdge()  . . . . . . . . . . . . . . . . . . . empty constructor
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
RxnDataMechanismEdge::RxnDataMechanismEdge()
{
  Identification = MECHANISMGRAPH_EDGE_ID;
  NameTag = MECHANISMGRAPH_EDGE_NAME;
  SetType(Identification);
  String name("Object.");
  name.AppendToEnd(NameTag);
  EncodeDecodeClass = name;
} 
/*F RxnDataMechanismEdge(data)  . . . . . . . . . . . . . . . . .  copy constructor
**
**  DESCRIPTION
**    data: data to be copied
**
**  REMARKS
**
*/
RxnDataMechanismEdge::RxnDataMechanismEdge(const RxnDataMechanismEdge& data)
  : BaseDataEdge(data),
    Name(data.Name),
    reverseReaction(data.reverseReaction)
{
}
/*F Read(in,objc) . . . . . . . . . . . . . . . . . .  Read in RxnDataMechanismEdge
**
**  DESCRIPTION
**    in: The input stream
**    objc: The class definition
**
**  REMARKS
**
*/
bool RxnDataMechanismEdge::Read(istream& in, DataObjectClass* objc)
{
  StreamObjectInput str(in,' ');
  NameTag = str.ReadNext();
  return Read(in,objc,NameTag);
}
/*F Read(in,objc,name)  . . . . . . . . . . . . . . .  Read in RxnDataMechanismEdge
**
**  DESCRIPTION
**    in: The input stream
**    objc: The class definition
**    name: The name of the object (already assigned)
**
**  REMARKS
**
*/
bool RxnDataMechanismEdge::Read(istream& in, DataObjectClass* objc, const String& name)
{
  bool result = BaseDataEdge::Read(in,objc,name);
  StreamObjectInput str(in,' ');
  Name = str.ReadNext();
  String rev = str.ReadNext();
  if(rev == "Reverse")
    reverseReaction = true;
  else
    reverseReaction = false;
  return result;
}
 
/*F out1 = print(out) . . . . . . . . . . . . . . . . . . . .  RxnDataMechanismEdge
**
**  DESCRIPTION
**    out,out1: The output buffer
**
**  REMARKS
**
*/
ostream& RxnDataMechanismEdge::print(ostream& out) const
{
  BaseDataEdge::print(out);
  out << " Object Name: " << Name << " ";
  if(reverseReaction) 
    out << "(with Reverse) ";
  return out;
}
/*F obj = Clone() . . . . . . . . . . . . . . . . . . . . . . .RxnDataMechanismEdge
**
**  DESCRIPTION
**    obj: The copy
**
**  REMARKS
**
*/
Identify * RxnDataMechanismEdge::Clone()
{
  RxnDataMechanismEdge *obj = new RxnDataMechanismEdge(*this);
  return obj;
}
 
/*F CopyClone(obj)  . . . . . . . . . . . . . . . . . . . . . .RxnDataMechanismEdge
**
**  DESCRIPTION
**    obj: The object to be copied
**
**  REMARKS
**
*/
void RxnDataMechanismEdge::CopyClone(Identify * obj)
{
  RxnDataMechanismEdge *objfull = (RxnDataMechanismEdge *) obj;
  *this = *objfull;
  //Parameter = (BaseData... *) PointerClone(objfull->Parameter)
}
 
/*F ans = EncodeThis(buffer)  . . . . . . . . . . . . . . . .  RxnDataMechanismEdge
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: True if successful
**
**  REMARKS
**
*/
bool RxnDataMechanismEdge::EncodeThis(CommBuffer& buffer)
{
  bool result = BaseDataEdge::EncodeThis(buffer);
  //result = result && ---.EncodeThis(buffer);
  result = result && Encode(buffer,Name);
  result = result && BoolEncode(buffer,reverseReaction);
  //result = result && PointerEncode(buffer,Parameters);

  return result;
}
/*F ans = DecodeThis(buffer)  . . . . . . . . . . . . . . . .  RxnDataMechanismEdge
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: True if successful
**
**  REMARKS
**
*/
bool RxnDataMechanismEdge::DecodeThis(CommBuffer& buffer)
{
  bool result = BaseDataEdge::DecodeThis(buffer);
  result = result && Decode(buffer,Name);
  result = result && BoolDecode(buffer,reverseReaction);

  return result;
}
/*S RxnMechanismEdgeClass
 */
/*F RxnMechanismEdgeClass() . . . . . . . . . . . . . . . . . . . empty constructor
**
**  DESCRIPTION
**
**  REMARKS
**
*/
RxnMechanismEdgeClass::RxnMechanismEdgeClass()
{
  Identification = MECHANISMGRAPH_EDGE_ID;
  NameTag = MECHANISMGRAPH_EDGE_NAME;
  SubClass = "Edge";
  EncodeDecodeClass = NameTag;
} 
/*F RxnMechanismEdgeClass(data) . . . . . . . . . . . . . . . . .  copy constructor
**
**  DESCRIPTION
**     data: The class to be copied
**
**  REMARKS
**
*/
RxnMechanismEdgeClass::RxnMechanismEdgeClass(const RxnMechanismEdgeClass& data)
  : DataEdgeClass(data)
{
} 
 
/*F RxnMechanismEdgeClass(id,name,descr)  . . . . . . . . . . . . . . . constructor
**
**  DESCRIPTION
**    id: The ID of the object class
**    name: The name of the object class
**    descr: A text description of the object
**
**
**  REMARKS
**
*/
RxnMechanismEdgeClass::RxnMechanismEdgeClass(const int id, 
				 const String& name,
				 const String& descr)
  : DataEdgeClass(id,name,descr)
{
  SubClass = "Edge";
  EncodeDecodeClass = "MechanismEdge";
}
 
/*F out1 = print(out) . . . . . . . . . . . . . . . . . . . . RxnMechanismEdgeClass
**
**  DESCRIPTION
**    out1,out: The output buffer
**
**  REMARKS
**
*/
ostream& RxnMechanismEdgeClass::print(ostream& out) const
{
  DataEdgeClass::print(out);
  //PointerPrint(out,"  The Class: "," No Class Defined ",Class);
  // the rest
       
       return out;
}
 
/*F in1 = Read(in)  . . . . . . . . . . . . . . . . . . . . . RxnMechanismEdgeClass
**
**  DESCRIPTION
**    in,in1: The input buffer
**
**    This reads in the rest of the information (after the type
**    has been determined from the first line.  For the
**    base RxnMechanismEdgeClass, there is no further information.
**
**  REMARKS
**
*/
bool RxnMechanismEdgeClass::Read(istream& in, DataSetOfObjectsClass &set)
{
  bool result = DataEdgeClass::Read(in,set);
  //result = result && PointerClassRead(in,(DataObjectClass *&) Class,
  //COREOBJECTS_BASE_NAME,
  //set," No Class ");
  return result;
}
 
/*F CopyClone(objc) . . . . . . . . . . . . . . . . . . . . . RxnMechanismEdgeClass
**
**  DESCRIPTION
**    objc: The class to be copied
**
**  REMARKS
**
*/
void RxnMechanismEdgeClass::CopyClone(Identify *  objc)
{
  RxnMechanismEdgeClass *objcfull = (RxnMechanismEdgeClass *) objc;
  *this = *objcfull;
  //ParameterClass = (DataSetOfObjectsClass *) PointerClone(objcfull->ParameterClass);
}
/*F objc = Clone()  . . . . . . . . . . . . . . . . . . . . . RxnMechanismEdgeClass
**
**  DESCRIPTION
**    objc: The object class that was created
**
**  REMARKS
**
*/
Identify * RxnMechanismEdgeClass::Clone()
    {
      RxnMechanismEdgeClass* id = new RxnMechanismEdgeClass(*this);
      return (Identify *) id;
    }
/*F ans = EncodeThis(buffer)  . . . . . . . . . . . . . . . . RxnMechanismEdgeClass
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: true if successful
**
**  REMARKS
**
*/
bool RxnMechanismEdgeClass::EncodeThis(CommBuffer& buffer)
{
  bool result = DataEdgeClass::EncodeThis(buffer);
  //result = result && PointerEncode(buffer,Class);
  // result = result && Encode(buffer,------);

  return result;
}
/*F ans = DecodeThis(buffer)  . . . . . . . . . . . . . . . . RxnMechanismEdgeClass
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: true if successful
**
**  REMARKS
**
*/
bool RxnMechanismEdgeClass::DecodeThis(CommBuffer& buffer)
{
  bool result = DataEdgeClass::DecodeThis(buffer);
  //result = result && PointerDecode(buffer,(BaseDataObject *&) Class);
  //result = result && Decode(buffer,-----);

  return result;
}
/*F obj = BaseDataObjectExample() . . . . . . . . . . . . .  create an object
**
**  DESCRIPTION
**    obj: The created object
**
**    This function is used to create an empty instance of a object 
**    given the class.  This is used so that the virtual functions
**    of the object can be used.
**
**  REMARKS
**
*/
BaseDataObject * RxnMechanismEdgeClass::BaseDataObjectExample()
{ 
  BaseDataObject *obj = new RxnDataMechanismEdge();
  obj->SetType(Identification);
  return obj;
}
/*F ans = TopDecode(buffer,obj) . . . . . . . . . . . . . . . RxnMechanismEdgeClass
**
**  DESCRIPTION
**    buffer: The buffer 
**    obj: The object
**    ans: True if successful
**
**  REMARKS
**
*/
bool TopDecode(CommBuffer& buffer, RxnMechanismEdgeClass*& obj)
     {
     obj = new RxnMechanismEdgeClass;
     return obj->DecodeThis(buffer);
     }
/*F ans = TopDecode(buffer,obj) . . . . . . . . . . . . . . . RxnDataMechanismEdge
**
**  DESCRIPTION
**    buffer: The buffer 
**    obj: The object
**    ans: True if successful
**
**  REMARKS
**
*/
bool TopDecode(CommBuffer& buffer, RxnDataMechanismEdge*& obj)
     {
     obj = new RxnDataMechanismEdge;
     return obj->DecodeThis(buffer);
     }
/*S RxnDataMechanismGraph
 */ 
/*F RxnDataMechanismGraph()  . . . . . . . . . . . . . . . . . . . empty constructor
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
RxnDataMechanismGraph::RxnDataMechanismGraph()
{
  Identification = MECHANISMGRAPH_GRAPH_ID;
  NameTag = MECHANISMGRAPH_GRAPH_NAME;
  SetType(Identification);
  String name("Object.");
  name.AppendToEnd(NameTag);
  EncodeDecodeClass = name;
} 
/*F RxnDataMechanismGraph(data)  . . . . . . . . . . . . . . . . .  copy constructor
**
**  DESCRIPTION
**    data: data to be copied
**
**  REMARKS
**
*/
RxnDataMechanismGraph::RxnDataMechanismGraph(const RxnDataMechanismGraph& data)
  : BaseDataGraph(data)
{
}
/*F Read(in,objc) . . . . . . . . . . . . . . . . . .  Read in RxnDataMechanismGraph
**
**  DESCRIPTION
**    in: The input stream
**    objc: The class definition
**
**  REMARKS
**
*/
bool RxnDataMechanismGraph::Read(istream& in, DataObjectClass* objc)
{
  StreamObjectInput str(in,' ');
  NameTag = str.ReadNext();
  return Read(in,objc,NameTag);
}
/*F Read(in,objc,name)  . . . . . . . . . . . . . . .  Read in RxnDataMechanismGraph
**
**  DESCRIPTION
**    in: The input stream
**    objc: The class definition
**    name: The name of the object (already assigned)
**
**  REMARKS
**
*/
bool RxnDataMechanismGraph::Read(istream& in, DataObjectClass* objc, const String& name)
{
  bool result = BaseDataGraph::Read(in,objc,name);
  
  return result;
}
 
/*F out1 = print(out) . . . . . . . . . . . . . . . . . . . .  RxnDataMechanismGraph
**
**  DESCRIPTION
**    out,out1: The output buffer
**
**  REMARKS
**
*/
ostream& RxnDataMechanismGraph::print(ostream& out) const
{
  BaseDataGraph::print(out);
  //PointerPrint(out,"The List of Parameters: ","No Parameters",Parameters);
  // The rest

  return out;
}
/*F obj = Clone() . . . . . . . . . . . . . . . . . . . . . . .RxnDataMechanismGraph
**
**  DESCRIPTION
**    obj: The copy
**
**  REMARKS
**
*/
Identify * RxnDataMechanismGraph::Clone()
{
  RxnDataMechanismGraph *obj = new RxnDataMechanismGraph(*this);
  return obj;
}
 
/*F CopyClone(obj)  . . . . . . . . . . . . . . . . . . . . . .RxnDataMechanismGraph
**
**  DESCRIPTION
**    obj: The object to be copied
**
**  REMARKS
**
*/
void RxnDataMechanismGraph::CopyClone(Identify * obj)
{
  RxnDataMechanismGraph *objfull = (RxnDataMechanismGraph *) obj;
  *this = *objfull;
  //Parameter = (BaseData... *) PointerClone(objfull->Parameter)
}
 
/*F ans = EncodeThis(buffer)  . . . . . . . . . . . . . . . .  RxnDataMechanismGraph
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: True if successful
**
**  REMARKS
**
*/
bool RxnDataMechanismGraph::EncodeThis(CommBuffer& buffer)
{
  bool result = BaseDataGraph::EncodeThis(buffer);
  //result = result && ---.EncodeThis(buffer);
  //result = result && Encode(buffer,---);
  //result = result && PointerEncode(buffer,Parameters);

  return result;
}
/*F ans = DecodeThis(buffer)  . . . . . . . . . . . . . . . .  RxnDataMechanismGraph
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: True if successful
**
**  REMARKS
**
*/
bool RxnDataMechanismGraph::DecodeThis(CommBuffer& buffer)
{
  bool result = BaseDataGraph::DecodeThis(buffer);
  // The rest

  return result;
}
 
 
/*S RxnMechanismGraphClass
 */
/*F RxnMechanismGraphClass() . . . . . . . . . . . . . . . . . . . empty constructor
**
**  DESCRIPTION
**
**  REMARKS
**
*/
RxnMechanismGraphClass::RxnMechanismGraphClass()
  : MoleculeNodeClass(NULL),
    ReactionNodeClass(NULL),
    EdgeClass(NULL)
{
  Identification = MECHANISMGRAPH_GRAPH_ID;
  NameTag = MECHANISMGRAPH_GRAPH_NAME;
  SubClass = "Graph";
  EncodeDecodeClass = NameTag;
} 
/*F RxnMechanismGraphClass(data) . . . . . . . . . . . . . . . . .  copy constructor
**
**  DESCRIPTION
**     data: The class to be copied
**
**  REMARKS
**
*/
RxnMechanismGraphClass::RxnMechanismGraphClass(const RxnMechanismGraphClass& data)
  : DataGraphClass(data)
{
  MoleculeNodeClass = (RxnMechanismNodeClass *) PointerClone(data.MoleculeNodeClass);
  ReactionNodeClass = (RxnMechanismNodeClass *) PointerClone(data.ReactionNodeClass);
  EdgeClass = (RxnMechanismEdgeClass *) PointerClone(data.EdgeClass);
} 
 
/*F RxnMechanismGraphClass(id,name,descr)  . . . . . . . . . . . . . . . constructor
**
**  DESCRIPTION
**    id: The ID of the object class
**    name: The name of the object class
**    descr: A text description of the object
**
**
**  REMARKS
**
*/
RxnMechanismGraphClass::RxnMechanismGraphClass(const int id, 
				 const String& name,
				 const String& descr)
  : DataGraphClass(id,name,descr),
    MoleculeNodeClass(NULL),
    ReactionNodeClass(NULL),
    EdgeClass(NULL)
{
  SubClass = "Graph";
  EncodeDecodeClass = "MechanismGraph";
}
 
/*F out1 = print(out) . . . . . . . . . . . . . . . . . . . . RxnMechanismGraphClass
**
**  DESCRIPTION
**    out1,out: The output buffer
**
**  REMARKS
**
*/
ostream& RxnMechanismGraphClass::print(ostream& out) const
{
  DataGraphClass::print(out);
  PointerPrint(out,"  The MoleculeNode Class: "," No Class Defined ",MoleculeNodeClass);
  PointerPrint(out,"  The ReactionNode Class: "," No Class Defined ",ReactionNodeClass);
  PointerPrint(out,"  The Edge Class: "," No Class Defined ",EdgeClass);
  return out;
}
 
/*F in1 = Read(in)  . . . . . . . . . . . . . . . . . . . . . RxnMechanismGraphClass
**
**  DESCRIPTION
**    in,in1: The input buffer
**
**    This reads in the rest of the information (after the type
**    has been determined from the first line.  For the
**    base RxnMechanismGraphClass, there is no further information.
**
**  REMARKS
**
*/
bool RxnMechanismGraphClass::Read(istream& in, DataSetOfObjectsClass &set)
{
  bool result = DataGraphClass::Read(in,set);
  result = result && PointerClassRead(in,(DataObjectClass *&) MoleculeNodeClass,
				      MECHANISMGRAPH_NODE_NAME,
				      set," No MoleculeNode Class ");
  result = result && PointerClassRead(in,(DataObjectClass *&) ReactionNodeClass,
				      MECHANISMGRAPH_NODE_NAME,
				      set," No ReactionNode Class ");
  result = result && PointerClassRead(in,(DataObjectClass *&) EdgeClass,
				      MECHANISMGRAPH_EDGE_NAME,
				      set," No Edge Class ");
  return result;
}
 
/*F CopyClone(objc) . . . . . . . . . . . . . . . . . . . . . RxnMechanismGraphClass
**
**  DESCRIPTION
**    objc: The class to be copied
**
**  REMARKS
**
*/
void RxnMechanismGraphClass::CopyClone(Identify *  objc)
{
  RxnMechanismGraphClass *objcfull = (RxnMechanismGraphClass *) objc;
  *this = *objcfull;
  MoleculeNodeClass = (RxnMechanismNodeClass *) PointerClone(objcfull->MoleculeNodeClass);
  ReactionNodeClass = (RxnMechanismNodeClass *) PointerClone(objcfull->ReactionNodeClass);
  EdgeClass         = (RxnMechanismEdgeClass *) PointerClone(objcfull->EdgeClass);
}
/*F objc = Clone()  . . . . . . . . . . . . . . . . . . . . . RxnMechanismGraphClass
**
**  DESCRIPTION
**    objc: The object class that was created
**
**  REMARKS
**
*/
Identify * RxnMechanismGraphClass::Clone()
    {
      RxnMechanismGraphClass* id = new RxnMechanismGraphClass(*this);
      return (Identify *) id;
    }
/*F ans = EncodeThis(buffer)  . . . . . . . . . . . . . . . . RxnMechanismGraphClass
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: true if successful
**
**  REMARKS
**
*/
bool RxnMechanismGraphClass::EncodeThis(CommBuffer& buffer)
{
  bool result = DataGraphClass::EncodeThis(buffer);
  result = result && PointerEncode(buffer,MoleculeNodeClass);
  result = result && PointerEncode(buffer,ReactionNodeClass);
  result = result && PointerEncode(buffer,EdgeClass);
  return result;
}
/*F ans = DecodeThis(buffer)  . . . . . . . . . . . . . . . . RxnMechanismGraphClass
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: true if successful
**
**  REMARKS
**
*/
bool RxnMechanismGraphClass::DecodeThis(CommBuffer& buffer)
{
  bool result = DataGraphClass::DecodeThis(buffer);
  result = result && PointerDecode(buffer,(BaseDataObject *&) MoleculeNodeClass);
  result = result && PointerDecode(buffer,(BaseDataObject *&) ReactionNodeClass);
  result = result && PointerDecode(buffer,(BaseDataObject *&) EdgeClass);
  return result;
}
RxnMechanismNodeClass *RxnMechanismGraphClass::getMoleculeNodeClass()
{
  return MoleculeNodeClass;
}
RxnMechanismNodeClass *RxnMechanismGraphClass::getReactionNodeClass()
{
  return ReactionNodeClass;
}
RxnMechanismEdgeClass *RxnMechanismGraphClass::getEdgeClass()
{
  return EdgeClass;
}
/*F obj = BaseDataObjectExample() . . . . . . . . . . . . .  create an object
**
**  DESCRIPTION
**    obj: The created object
**
**    This function is used to create an empty instance of a object 
**    given the class.  This is used so that the virtual functions
**    of the object can be used.
**
**  REMARKS
**
*/
BaseDataObject * RxnMechanismGraphClass::BaseDataObjectExample()
{ 
  BaseDataObject *obj = new RxnDataMechanismGraph();
  obj->SetType(Identification);
  return obj;
}
/*F ans = TopDecode(buffer,obj) . . . . . . . . . . . . . . . RxnMechanismGraphClass
**
**  DESCRIPTION
**    buffer: The buffer 
**    obj: The object
**    ans: True if successful
**
**  REMARKS
**
*/
bool TopDecode(CommBuffer& buffer, RxnMechanismGraphClass*& obj)
     {
     obj = new RxnMechanismGraphClass;
     return obj->DecodeThis(buffer);
     }
/*F ans = TopDecode(buffer,obj) . . . . . . . . . . . . . . . RxnDataMechanismGraph
**
**  DESCRIPTION
**    buffer: The buffer 
**    obj: The object
**    ans: True if successful
**
**  REMARKS
**
*/
bool TopDecode(CommBuffer& buffer, RxnDataMechanismGraph*& obj)
     {
     obj = new RxnDataMechanismGraph;
     return obj->DecodeThis(buffer);
     }
/*S BaseDataCreateMechanismGraph
 */ 
/*F RxnDataCreateMechanismGraph()  . . . . . . . . . . . . . . . . . . . empty constructor
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
RxnDataCreateMechanismGraph::RxnDataCreateMechanismGraph()
  : MechanismS("Mechanism"),
    GraphClassNameS("GraphClassName")
{
  Identification = MECHANISMGRAPH_CREATEGRAPH_ID;
  NameTag = MECHANISMGRAPH_CREATEGRAPH_NAME;
  SetType(Identification);
  String name("Object.");
  name.AppendToEnd(NameTag);
  EncodeDecodeClass = name;
} 
/*F RxnDataCreateMechanismGraph(data)  . . . . . . . . . . . . . . . . .  copy constructor
**
**  DESCRIPTION
**    data: data to be copied
**
**  REMARKS
**
*/
RxnDataCreateMechanismGraph::RxnDataCreateMechanismGraph(const RxnDataCreateMechanismGraph& data)
  : BaseDataAlgorithmOperation(data),
    MechanismS(data.MechanismS),
    GraphClassNameS(data.GraphClassNameS)
{
}
/*F Read(in,objc) . . . . . . . . . . . . . . . . . .  Read in RxnDataCreateMechanismGraph
**
**  DESCRIPTION
**    in: The input stream
**    objc: The class definition
**
**  REMARKS
**
*/
bool RxnDataCreateMechanismGraph::Read(istream& in, DataObjectClass* objc)
{
  StreamObjectInput str(in,' ');
  NameTag = str.ReadNext();
  return Read(in,objc,NameTag);
}
/*F Read(in,objc,name)  . . . . . . . . . . . . . . .  Read in RxnDataCreateMechanismGraph
**
**  DESCRIPTION
**    in: The input stream
**    objc: The class definition
**    name: The name of the object (already assigned)
**
**  REMARKS
**
*/
bool RxnDataCreateMechanismGraph::Read(istream& in, DataObjectClass* objc, const String& name)
{
  bool result = BaseDataAlgorithmOperation::Read(in,objc,name);
  
  return result;
}
 
/*F out1 = print(out) . . . . . . . . . . . . . . . . . . . .  RxnDataCreateMechanismGraph
**
**  DESCRIPTION
**    out,out1: The output buffer
**
**  REMARKS
**
*/
ostream& RxnDataCreateMechanismGraph::print(ostream& out) const
{
  BaseDataAlgorithmOperation::print(out);
  out << "Mechanism Name Parameter:     " << MechanismS << endl;
  out << "Graph Class Names:            " << GraphClassNameS << endl;
  return out;
}
/*F obj = Clone() . . . . . . . . . . . . . . . . . . . . . . .RxnDataCreateMechanismGraph
**
**  DESCRIPTION
**    obj: The copy
**
**  REMARKS
**
*/
Identify * RxnDataCreateMechanismGraph::Clone()
{
  RxnDataCreateMechanismGraph *obj = new RxnDataCreateMechanismGraph(*this);
  return obj;
}
 
/*F CopyClone(obj)  . . . . . . . . . . . . . . . . . . . . . .RxnDataCreateMechanismGraph
**
**  DESCRIPTION
**    obj: The object to be copied
**
**  REMARKS
**
*/
void RxnDataCreateMechanismGraph::CopyClone(Identify * obj)
{
  RxnDataCreateMechanismGraph *objfull = (RxnDataCreateMechanismGraph *) obj;
  *this = *objfull;
  //Parameter = (BaseData... *) PointerClone(objfull->Parameter)
}
 
/*F ans = EncodeThis(buffer)  . . . . . . . . . . . . . . . .  RxnDataCreateMechanismGraph
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: True if successful
**
**  REMARKS
**
*/
bool RxnDataCreateMechanismGraph::EncodeThis(CommBuffer& buffer)
{
  bool result = BaseDataAlgorithmOperation::EncodeThis(buffer);
  result = result && Encode(buffer,MechanismS);
  result = result && Encode(buffer,GraphClassNameS);
  //result = result && ---.EncodeThis(buffer);
  //result = result && PointerEncode(buffer,Parameters);

  return result;
}
/*F ans = DecodeThis(buffer)  . . . . . . . . . . . . . . . .  RxnDataCreateMechanismGraph
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: True if successful
**
**  REMARKS
**
*/
bool RxnDataCreateMechanismGraph::DecodeThis(CommBuffer& buffer)
{
  bool result = BaseDataAlgorithmOperation::DecodeThis(buffer);
  result = result && Decode(buffer,MechanismS);
  result = result && Decode(buffer,GraphClassNameS);
  return result;
}
/*F ans = SetUpAlgorithms(instances,instancesclass,run,runclass)  . algorithm
**
**  DESCRIPTION
**    instances: The set of instances
**    instancesclass: The set of instance object classes
**    run: The algorithm run information
**    runclass: The run class
**    
**  REMARKS
**
*/
bool RxnDataCreateMechanismGraph::SetUpAlgorithms(BaseDataSetOfInstances *instances,
				       DataSetOfInstancesClass *instancesclass,
				       BaseDataAlgorithmRun *run,
				       DataAlgorithmRunClass *runclass)
{
  return true;
}
/*F ans = CheckInput(instances,instancesclass,run,runclass) . . . . algorithm
**
**  DESCRIPTION
**    instances: The set of instances
**    instancesclass: The set of instance object classes
**    run: The algorithm run information
**    runclass: The run class
**
**  REMARKS
**
*/
bool RxnDataCreateMechanismGraph::CheckInput(BaseDataSetOfInstances *instances,
					     DataSetOfInstancesClass *instancesclass,
					     BaseDataAlgorithmRun *run,
					     DataAlgorithmRunClass *runclass)
{
  bool result = true;
  result = result && CheckInputVariable(MechanismS,"Mechanism Name",run);
  result = result && CheckInputVariable(GraphClassNameS,"Node and Edge Classes",run);
  return result;
}
/*F ans = SetUpInput(instances,instancesclass,run,runclass) . . . . algorithm
**
**  DESCRIPTION
**    instances: The set of instances
**    instancesclass: The set of instance object classes
**    run: The algorithm run information
**    runclass: The run class
**    
**  REMARKS
**
*/
bool RxnDataCreateMechanismGraph::SetUpInput(BaseDataSetOfInstances *instances,
				  DataSetOfInstancesClass *instancesclass,
				  BaseDataAlgorithmRun *run,
				  DataAlgorithmRunClass *runclass)
{
  bool result = true;
  BaseDataString *InstanceS = (BaseDataString *) run->ParameterValue(MechanismS);
  if(instances->InstanceInSet(InstanceS->getString())) {
    BaseDataInstance *instance = instances->GetInstance(InstanceS->getString());
    if(instance->IsInList("Mechanism")) {
      Mechanism = (RxnDataMechanism *) instance->GetObject("Mechanism");
      GraphClassName = (BaseDataString *) run->ParameterValue(GraphClassNameS);
      GraphClassS    = GraphClassName->getString();
      DataSetOfObjectsClass *classes = instancesclass->PointerToAllowedClasses();
      if(classes->IsInList(GraphClassS)) {
	GraphClass = (RxnMechanismGraphClass *) classes->GetObjectClass(GraphClassS);
	Graph      = (RxnDataMechanismGraph *) GraphClass->BaseDataObjectExample();
	Graph->NameTag = "MechanismGraph";
	instance->AddObject(Graph);
	delete Graph;
	Graph = (RxnDataMechanismGraph *) instance->GetObject("MechanismGraph");
	
	MoleculeNodeClass = (RxnMechanismNodeClass *) GraphClass->getMoleculeNodeClass();
	ReactionNodeClass = (RxnMechanismNodeClass *) GraphClass->getReactionNodeClass();
	EdgeClass         = (RxnMechanismEdgeClass *) GraphClass->getEdgeClass();
      } else {
	cerr << "The Graph Class '" << GraphClassS << "' was not found in list of classes" << endl;
	result = false;
      }
    } else {
      cerr << "'Mechanism' not found in instance '" << instance->NameTag << "'" << endl;
      result = false;
    }
  } else {
    cerr << "'" << InstanceS->getString() << "' instance not found" << endl;
    result = false;
  }
  return result;
}
/*F ans = Calculate(instances,instancesclass,run,runclass)  . . . . algorithm
**
**  DESCRIPTION
**    instances: The set of instances
**    instancesclass: The set of instance object classes
**    run: The algorithm run information
**    runclass: The run class
**    
**  REMARKS
**
*/
bool RxnDataCreateMechanismGraph::Calculate(BaseDataSetOfInstances *instances,
					    DataSetOfInstancesClass *instancesclass,
					    BaseDataAlgorithmRun *run,
					    DataAlgorithmRunClass *runclass)
{
  bool result = true;
  result = result && AddMechanismNodes(MoleculeNodeClass,ReactionNodeClass);
  unsigned int mechtype = Mechanism->GetType();
  RxnMechanismClass *mechclass = (RxnMechanismClass *) instancesclass->PointerToAllowedClasses()->GetObjectClass(mechtype);
  result = result && AddMechanismEdges(EdgeClass,mechclass,instances,instancesclass);
  return result;
}
/*F ans = AddMechanismNodes()  . . . . add mechanism nodes
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
bool RxnDataCreateMechanismGraph::AddMechanismNodes(RxnMechanismNodeClass *moleculenode,
						    RxnMechanismNodeClass *reactionnode)
{
  bool result = true;
  ObjectList<String>::iterator name;
  ObjectList<String> molnames = Mechanism->getMoleculeNames().GetKeyWords();
  for(name = molnames.begin(); name != molnames.end();name++)
    {
      BaseDataNode *node = (BaseDataNode *) moleculenode->BaseDataObjectExample();
      node->NameTag = *name;
      String nodename = Graph->addNode((BaseDataNode *) node);
    }
  /*
  ObjectList<String> rxnnames = Mechanism->getReactionNames().GetKeyWords();
  for(name = rxnnames.begin(); name != rxnnames.end(); name++)
    {
      BaseDataNode *node = (BaseDataNode *) moleculenode->BaseDataObjectExample();
      node->NameTag = *name;
      String nodename = Graph->addNode((BaseDataNode *) node);
  */

  return result;
}
/*F ans = AddMechanismEdges()  . . . . add mechanism edges
**
**  DESCRIPTION
**    This routine adds edges between molecules.
**  REMARKS
**
*/
bool RxnDataCreateMechanismGraph::AddMechanismEdges(RxnMechanismEdgeClass *edgeclass,
						    RxnMechanismClass *mechclass,
						    BaseDataSetOfInstances *instances,
						    DataSetOfInstancesClass *instancesclass)
{
  bool result = true;
  Graph->setAllowMultipleEdges(true);
  ObjectList<String> rxnnames = Mechanism->getReactionNames().GetKeyWords();
  ObjectList<String>::iterator name;
  for(name = rxnnames.begin(); name != rxnnames.end(); name++)
    {
      cout << "Add Reaction        " << *name << endl;
      RxnDataReactionSummary *summary = (RxnDataReactionSummary *) Mechanism->getReactionSummary(*name);
      RxnDataReaction *rxn = Mechanism->getReaction(summary->getReactionName(),
						    mechclass->getReactionClass(),
						    instances,instancesclass);
      /*
      ObjectList<String>::iterator molname;
      BaseDataKeySet reactantkeys = rxn->getReactantNames();
      ObjectListString reactants = reactantkeys.GetKeyWords();
      for(molname = reactants.begin(); molname != reactants.end(); molname++)
	{
	  String edgename = Graph->addEdge(*molname,*name,GraphClass);
	}
      BaseDataKeySet productkeys = rxn->getProductNames();
      ObjectListString products = productkeys.GetKeyWords();

      for(molname = products.begin(); molname != products.end(); molname++)
	{
	  String edgename = Graph->addEdge(*name,*molname,GraphClass);
	}
      */
      ObjectList<String>::iterator reactname,prodname;
      BaseDataKeySet reactantkeys = rxn->getReactantNames();
      ObjectListString reactants = reactantkeys.GetKeyWords();
      for(reactname = reactants.begin(); reactname != reactants.end(); reactname++)
	{
	  BaseDataKeySet productkeys = rxn->getProductNames();
	  ObjectListString products = productkeys.GetKeyWords();
	  
	  for(prodname = products.begin(); prodname != products.end(); prodname++)
	    {
	      String edgename = Graph->addEdge(*reactname,*prodname,GraphClass);
	      RxnDataMechanismEdge *e = (RxnDataMechanismEdge *) Graph->getEdge(edgename);
	      e->Name = rxn->NameTag;
	      e->reverseReaction = true;
	      if(rxn->IsInList("Reverse")) {
		RxnDataReactionRates *rate = (RxnDataReactionRates *) rxn->GetObject("Reverse");
		RxnReactionRatesClass *rclass = (RxnReactionRatesClass *) 
		  instancesclass->PointerToAllowedClasses()->GetObjectClass(rate->GetType());
		double r = rate->getArrheniusValue(rclass);
		if(r == 0.0)
		  e->reverseReaction = false;
	      } else {
		e->reverseReaction = false;
	      }
	    }
	}      
    }
  
  return result;
}
/*F ans = WriteOutputValues(instances,instancesclass,run,runclass)  algorithm
**
**  DESCRIPTION
**    instances: The set of instances
**    instancesclass: The set of instance object classes
**    run: The algorithm run information
**    runclass: The run class
**    
**  REMARKS
**
*/
bool RxnDataCreateMechanismGraph::WriteOutputValues(BaseDataSetOfInstances *instances,
					 DataSetOfInstancesClass *instancesclass,
					 BaseDataAlgorithmRun *run,
					 DataAlgorithmRunClass *runclass)
{
  bool result = true;
  return result;
}
/*F ans = ConcludeRun(instances,instancesclass,run,runclass)  . . . algorithm
**
**  DESCRIPTION
**    instances: The set of instances
**    instancesclass: The set of instance object classes
**    run: The algorithm run information
**    runclass: The run class
**    
**  REMARKS
**
*/
bool RxnDataCreateMechanismGraph::ConcludeRun(BaseDataSetOfInstances *instances,
				   DataSetOfInstancesClass *instancesclass,
				   BaseDataAlgorithmRun *run,
				   DataAlgorithmRunClass *runclass)
{
  bool result = true;
//  delete something
  return result;
}
 
 
 
/*S RxnCreateMechanismGraphClass
 */
/*F RxnCreateMechanismGraphClass() . . . . . . . . . . . . . . . . . . . empty constructor
**
**  DESCRIPTION
**
**  REMARKS
**
*/
RxnCreateMechanismGraphClass::RxnCreateMechanismGraphClass()
{
  Identification = MECHANISMGRAPH_CREATEGRAPH_ID;
  NameTag = MECHANISMGRAPH_CREATEGRAPH_NAME;
  SubClass = "AlgorithmOperation";
  EncodeDecodeClass = NameTag;
} 
/*F RxnCreateMechanismGraphClass(data) . . . . . . . . . . . . . . . . .  copy constructor
**
**  DESCRIPTION
**     data: The class to be copied
**
**  REMARKS
**
*/
RxnCreateMechanismGraphClass::RxnCreateMechanismGraphClass(const RxnCreateMechanismGraphClass& data)
  : DataAlgorithmOperationClass(data)
{
} 
 
/*F RxnCreateMechanismGraphClass(id,name,descr)  . . . . . . . . . . . . . . . constructor
**
**  DESCRIPTION
**    id: The ID of the object class
**    name: The name of the object class
**    descr: A text description of the object
**
**
**  REMARKS
**
*/
RxnCreateMechanismGraphClass::RxnCreateMechanismGraphClass(const int id, 
				 const String& name,
				 const String& descr)
  : DataAlgorithmOperationClass(id,name,descr)
{
  SubClass = "AlgorithmOperation";
  EncodeDecodeClass = "CreateMechanismGraph";
}
 
/*F out1 = print(out) . . . . . . . . . . . . . . . . . . . . RxnCreateMechanismGraphClass
**
**  DESCRIPTION
**    out1,out: The output buffer
**
**  REMARKS
**
*/
ostream& RxnCreateMechanismGraphClass::print(ostream& out) const
{
  DataAlgorithmOperationClass::print(out);
  //PointerPrint(out,"  The Class: "," No Class Defined ",Class);
  // the rest
       
       return out;
}
 
/*F in1 = Read(in)  . . . . . . . . . . . . . . . . . . . . . RxnCreateMechanismGraphClass
**
**  DESCRIPTION
**    in,in1: The input buffer
**
**    This reads in the rest of the information (after the type
**    has been determined from the first line.  For the
**    base RxnCreateMechanismGraphClass, there is no further information.
**
**  REMARKS
**
*/
bool RxnCreateMechanismGraphClass::Read(istream& in, DataSetOfObjectsClass &set)
{
  bool result = DataAlgorithmOperationClass::Read(in,set);
  //result = result && PointerClassRead(in,(DataObjectClass *&) Class,
  //COREOBJECTS_BASE_NAME,
  //set," No Class ");
  return result;
}
 
/*F CopyClone(objc) . . . . . . . . . . . . . . . . . . . . . RxnCreateMechanismGraphClass
**
**  DESCRIPTION
**    objc: The class to be copied
**
**  REMARKS
**
*/
void RxnCreateMechanismGraphClass::CopyClone(Identify *  objc)
{
  RxnCreateMechanismGraphClass *objcfull = (RxnCreateMechanismGraphClass *) objc;
  *this = *objcfull;
  //ParameterClass = (DataSetOfObjectsClass *) PointerClone(objcfull->ParameterClass);
}
/*F objc = Clone()  . . . . . . . . . . . . . . . . . . . . . RxnCreateMechanismGraphClass
**
**  DESCRIPTION
**    objc: The object class that was created
**
**  REMARKS
**
*/
Identify * RxnCreateMechanismGraphClass::Clone()
    {
      RxnCreateMechanismGraphClass* id = new RxnCreateMechanismGraphClass(*this);
      return (Identify *) id;
    }
/*F ans = EncodeThis(buffer)  . . . . . . . . . . . . . . . . RxnCreateMechanismGraphClass
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: true if successful
**
**  REMARKS
**
*/
bool RxnCreateMechanismGraphClass::EncodeThis(CommBuffer& buffer)
{
  bool result = DataAlgorithmOperationClass::EncodeThis(buffer);
  //result = result && PointerEncode(buffer,Class);
  // result = result && Encode(buffer,------);

  return result;
}
/*F ans = DecodeThis(buffer)  . . . . . . . . . . . . . . . . RxnCreateMechanismGraphClass
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: true if successful
**
**  REMARKS
**
*/
bool RxnCreateMechanismGraphClass::DecodeThis(CommBuffer& buffer)
{
  bool result = DataAlgorithmOperationClass::DecodeThis(buffer);
  //result = result && PointerDecode(buffer,(BaseDataObject *&) Class);
  //result = result && Decode(buffer,-----);

  return result;
}
/*F obj = BaseDataObjectExample() . . . . . . . . . . . . .  create an object
**
**  DESCRIPTION
**    obj: The created object
**
**    This function is used to create an empty instance of a object 
**    given the class.  This is used so that the virtual functions
**    of the object can be used.
**
**  REMARKS
**
*/
BaseDataObject * RxnCreateMechanismGraphClass::BaseDataObjectExample()
{ 
  BaseDataObject *obj = new RxnDataCreateMechanismGraph();
  obj->SetType(Identification);
  return obj;
}
/*F ans = TopDecode(buffer,obj) . . . . . . . . . . . . . . . RxnCreateMechanismGraphClass
**
**  DESCRIPTION
**    buffer: The buffer 
**    obj: The object
**    ans: True if successful
**
**  REMARKS
**
*/
bool TopDecode(CommBuffer& buffer, RxnCreateMechanismGraphClass*& obj)
     {
     obj = new RxnCreateMechanismGraphClass;
     return obj->DecodeThis(buffer);
     }
/*F ans = TopDecode(buffer,obj) . . . . . . . . . . . . . . . RxnDataCreateMechanismGraph
**
**  DESCRIPTION
**    buffer: The buffer 
**    obj: The object
**    ans: True if successful
**
**  REMARKS
**
*/
bool TopDecode(CommBuffer& buffer, RxnDataCreateMechanismGraph*& obj)
     {
     obj = new RxnDataCreateMechanismGraph;
     return obj->DecodeThis(buffer);
     }
/*S BaseDataMechanismGraphReduction
 */ 
/*F BaseDataMechanismGraphReduction()  . . . . . . . . . . . . . . . . . . . empty constructor
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
BaseDataMechanismGraphReduction::BaseDataMechanismGraphReduction()
  : MechanismInstanceNameS("Mechanism"),
    StandardMoleculeSummaryS("StandardMoleculeSummary"),
    MechanismGraphS("MechanismGraph"),
    LeavesS("Leaves"),
    findLeaves(false),
    allowReverse(false),
    iterate(false)
{
  Identification = MECHANISMGRAPH_REDUCTION_ID;
  NameTag = MECHANISMGRAPH_REDUCTION_NAME;
  SetType(Identification);
  String name("Object.");
  name.AppendToEnd(NameTag);
  EncodeDecodeClass = name;
} 
/*F BaseDataMechanismGraphReduction(data)  . . . . . . . . . . . . . . . . .  copy constructor
**
**  DESCRIPTION
**    data: data to be copied
**
**  REMARKS
**
*/
BaseDataMechanismGraphReduction::BaseDataMechanismGraphReduction(const BaseDataMechanismGraphReduction& data)
  : BaseDataAlgorithmOperation(data),
    MechanismInstanceNameS(data.MechanismInstanceNameS),
    StandardMoleculeSummaryS(data.StandardMoleculeSummaryS),
    MechanismGraphS(data.MechanismGraphS),
    LeavesS(data.LeavesS),
    findLeaves(data.findLeaves),
    allowReverse(data.allowReverse),
    iterate(data.iterate)
{
}
/*F Read(in,objc) . . . . . . . . . . . . . . . . . .  Read in BaseDataMechanismGraphReduction
**
**  DESCRIPTION
**    in: The input stream
**    objc: The class definition
**
**  REMARKS
**
*/
bool BaseDataMechanismGraphReduction::Read(istream& in, DataObjectClass* objc)
{
  StreamObjectInput str(in,' ');
  NameTag = str.ReadNext();
  return Read(in,objc,NameTag);
}
/*F Read(in,objc,name)  . . . . . . . . . . . . . . .  Read in BaseDataMechanismGraphReduction
**
**  DESCRIPTION
**    in: The input stream
**    objc: The class definition
**    name: The name of the object (already assigned)
**
**  REMARKS
**
*/
bool BaseDataMechanismGraphReduction::Read(istream& in, DataObjectClass* objc, const String& name)
{
  bool result = BaseDataAlgorithmOperation::Read(in,objc,name);
  
  return result;
}
 
/*F out1 = print(out) . . . . . . . . . . . . . . . . . . . .  BaseDataMechanismGraphReduction
**
**  DESCRIPTION
**    out,out1: The output buffer
**
**  REMARKS
**
*/
ostream& BaseDataMechanismGraphReduction::print(ostream& out) const
{
  BaseDataAlgorithmOperation::print(out);
  //PointerPrint(out,"The List of Parameters: ","No Parameters",Parameters);
  // The rest

  return out;
}
/*F obj = Clone() . . . . . . . . . . . . . . . . . . . . . . .BaseDataMechanismGraphReduction
**
**  DESCRIPTION
**    obj: The copy
**
**  REMARKS
**
*/
Identify * BaseDataMechanismGraphReduction::Clone()
{
  BaseDataMechanismGraphReduction *obj = new BaseDataMechanismGraphReduction(*this);
  return obj;
}
 
/*F CopyClone(obj)  . . . . . . . . . . . . . . . . . . . . . .BaseDataMechanismGraphReduction
**
**  DESCRIPTION
**    obj: The object to be copied
**
**  REMARKS
**
*/
void BaseDataMechanismGraphReduction::CopyClone(Identify * obj)
{
  BaseDataMechanismGraphReduction *objfull = (BaseDataMechanismGraphReduction *) obj;
  *this = *objfull;
  //Parameter = (BaseData... *) PointerClone(objfull->Parameter)
}
 
/*F ans = EncodeThis(buffer)  . . . . . . . . . . . . . . . .  BaseDataMechanismGraphReduction
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: True if successful
**
**  REMARKS
**
*/
bool BaseDataMechanismGraphReduction::EncodeThis(CommBuffer& buffer)
{
  bool result = BaseDataAlgorithmOperation::EncodeThis(buffer);
  //result = result && ---.EncodeThis(buffer);
  result = result && Encode(buffer,MechanismInstanceNameS);
  result = result && Encode(buffer,StandardMoleculeSummaryS);
  result = result && Encode(buffer,MechanismGraphS);
  result = result && Encode(buffer,LeavesS);
  //result = result && PointerEncode(buffer,Parameters);

  return result;
}
/*F ans = DecodeThis(buffer)  . . . . . . . . . . . . . . . .  BaseDataMechanismGraphReduction
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: True if successful
**
**  REMARKS
**
*/
bool BaseDataMechanismGraphReduction::DecodeThis(CommBuffer& buffer)
{
  bool result = BaseDataAlgorithmOperation::DecodeThis(buffer);
  result = result && Decode(buffer,MechanismInstanceNameS);
  result = result && Decode(buffer,StandardMoleculeSummaryS);
  result = result && Decode(buffer,MechanismGraphS);
  result = result && Decode(buffer,LeavesS);

  return result;
}
/*F ans = SetUpAlgorithms(instances,instancesclass,run,runclass)  . algorithm
**
**  DESCRIPTION
**    instances: The set of instances
**    instancesclass: The set of instance object classes
**    run: The algorithm run information
**    runclass: The run class
**    
**  REMARKS
**
*/
bool BaseDataMechanismGraphReduction::SetUpAlgorithms(BaseDataSetOfInstances *instances,
				       DataSetOfInstancesClass *instancesclass,
				       BaseDataAlgorithmRun *run,
				       DataAlgorithmRunClass *runclass)
{
  return true;
}
/*F ans = CheckInput(instances,instancesclass,run,runclass) . . . . algorithm
**
**  DESCRIPTION
**    instances: The set of instances
**    instancesclass: The set of instance object classes
**    run: The algorithm run information
**    runclass: The run class
**
**  REMARKS
**
*/
bool BaseDataMechanismGraphReduction::CheckInput(BaseDataSetOfInstances *instances,
				  DataSetOfInstancesClass *instancesclass,
				  BaseDataAlgorithmRun *run,
				  DataAlgorithmRunClass *runclass)
{
  bool result = true;
  result = result && CheckInputVariable(MechanismInstanceNameS,"Mechanism Instance Name of this description",run);
  result = result && CheckInputVariable(StandardMoleculeSummaryS,"List of Molecules from base mechanism",run);
  return result;
}
/*F ans = SetUpInput(instances,instancesclass,run,runclass) . . . . algorithm
**
**  DESCRIPTION
**    instances: The set of instances
**    instancesclass: The set of instance object classes
**    run: The algorithm run information
**    runclass: The run class
**    
**  REMARKS
**
*/
bool BaseDataMechanismGraphReduction::SetUpInput(BaseDataSetOfInstances *instances,
				  DataSetOfInstancesClass *instancesclass,
				  BaseDataAlgorithmRun *run,
				  DataAlgorithmRunClass *runclass)
{
  bool result = true;
  BaseDataString *MechanismInstanceName = (BaseDataString *) run->ParameterValue(MechanismInstanceNameS);
  StandardMoleculeSummary = (RxnDataMoleculeSummarySet *) run->ParameterValue(StandardMoleculeSummaryS);
  if(instances->InstanceInSet(MechanismInstanceName->getString())) {
    Instance = instances->GetInstance(MechanismInstanceName->getString());
    if(Instance->IsInList(MechanismGraphS)) {
      MechanismGraph = (RxnDataMechanismGraph *) Instance->GetObject(MechanismGraphS)->Clone();
      if(run->AlgorithmSummary.KeyWordInList("FindLeaves")) {
	findLeaves = true;
      }
      if(run->AlgorithmSummary.KeyWordInList("Iterate")){
	iterate = true;
      }
      if(run->AlgorithmSummary.KeyWordInList("AllowReverse")){
	allowReverse = true;
      }
    } else {
      cerr << "Mechanism within instance '" << MechanismInstanceName->getString() << "' (" << MechanismGraphS << ") not found" << endl;
      result = false;
    }
  } else {
    cerr << "Instance: '" << MechanismInstanceName->getString() << "' not found" << endl;
    result = false;
  }
  return result;
}
/*F ans = Calculate(instances,instancesclass,run,runclass)  . . . . algorithm
**
**  DESCRIPTION
**    instances: The set of instances
**    instancesclass: The set of instance object classes
**    run: The algorithm run information
**    runclass: The run class
**    
**  REMARKS
**
*/
bool BaseDataMechanismGraphReduction::Calculate(BaseDataSetOfInstances *instances,
				 DataSetOfInstancesClass *instancesclass,
				 BaseDataAlgorithmRun *run,
				 DataAlgorithmRunClass *runclass)
{
  bool result = true;
  if(findLeaves) { 
    cout << "BaseDataMechanismGraphReduction" << endl;
    String LevelBaseNameS("ReduceLevel");
    String LevelListS("LevelList");

    BaseDataKeyWords *levels = new BaseDataKeyWords();
    String totalname(LevelBaseNameS);
    totalname.AppendToEnd("Total");
    levels->NameTag = totalname;
    BaseDataKeyWords *LevelList = new BaseDataKeyWords();
    LevelList->NameTag = LevelListS;
    FindLeaves();
    unsigned int level = 0;
    if(iterate) {
      while(Leaves->SizeOf() > 0) {
	cout << "=========== Level " << level << " Nodes To Eliminate" << endl;
	Leaves->NameTag = PositveIntegerToString(level,LevelBaseNameS,2);
	Leaves->print(cout);
	cout << endl;
	levels->AddKeyWords(*Leaves);
	Instance->AddObject(Leaves);
	LevelList->AddKeyWord(Leaves->NameTag);
	EliminateNodes(MechanismGraph,Leaves);
	delete Leaves;
	FindLeaves();
	level++;
      }
    } else {
	cout << "=========== Level " << level << " Nodes To Eliminate" << endl;
	Leaves->NameTag = PositveIntegerToString(level,LevelBaseNameS,2);
	Leaves->print(cout);
	cout << endl;
	levels->AddKeyWords(*Leaves);
	Instance->AddObject(Leaves);
	EliminateNodes(MechanismGraph,Leaves);
    }
    Instance->AddObject(levels);
    Instance->AddObject(LevelList);
    delete levels;
    delete LevelList;
  }
  return result;
}
void BaseDataMechanismGraphReduction::FindLeaves() 
{
  cout << "BaseDataMechanismGraphReduction::FindLeaves" << endl;
  BaseDataSetOfObjects *nodes = MechanismGraph->getNodes();
  ObjectList<String> nodenamelist = nodes->ListOfObjectNames();
  BaseDataKeyWords *nodenames = new BaseDataKeyWords(nodenamelist);
  //cout << "Node Names: ";
  //nodenames->print(cout);
  //cout << endl;
  //cout << "Base Molecules: ";
  //StandardMoleculeSummary->print(cout);
  //cout << endl;
  //cout << "Mechanism Graph" << endl;
  //MechanismGraph->print(cout);
  //cout << endl;
  Leaves = GetLeaves(MechanismGraph, nodenames);
}

BaseDataKeyWords *BaseDataMechanismGraphReduction::GetLeaves(BaseDataGraph *graph, BaseDataKeyWords *nodes)
{
  BaseDataKeyWords *leaves = new BaseDataKeyWords();

  ObjectList<String> names = nodes->GetKeyWords();
  ObjectList<String>::iterator name;
  for(name = names.begin();name != names.end();name++) {
    cout << "Node: '" << *name << "'" << endl;
    if(!StandardMoleculeSummary->IsInList(*name)) {
      cout << "Get Edges" << endl;
      ObjectList<String> edges = graph->getNeighboredEdges(*name);
      bool leaf = true;
      ObjectList<String>::iterator edge = edges.begin();
      while(leaf && edge != edges.end()) {
	RxnDataMechanismEdge *e = (RxnDataMechanismEdge *) graph->getEdge(*edge);
	cout << "Check Reaction: " << e->Name << " Node1" << e->getNode1() << 
	  " Node2: " << e->getNode2() << endl;
	String edgename = e->getNode1();
	if(e->getNode1() == *name) {
	  leaf = false;
	} else if(e->reverseReaction) {
	  cout << "Reaction: " << e->Name << " has a reverse for " << *name << endl;
	  if(allowReverse)
	    leaf = false;
	  else
	    cout << "      (But reverse reaction not used)" << endl;
	}
	edge++;
      }
      if(leaf) {
	leaves->AddKeyWord(*name);
	cout << "Node: '" << *name << "' is a Leaf node" << endl;
	//for(edge=edges.begin();edge != edges.end();edge++) {
	//RxnDataMechanismEdge *e = (RxnDataMechanismEdge *) graph->getEdge(*edge);
	  //cout << "Leaf Reaction: '" << e->Name << "'" << endl;
	//}
      }
    } else {
      cout << "A Base Mechanism Molecule: '" << *name << "'" << endl;
    }
  }
  cout << "Done Checking for Leaves" << endl;
  return leaves;
}
void BaseDataMechanismGraphReduction::EliminateNodes(BaseDataGraph *graph, BaseDataKeyWords *nodes) {
  cout << "Deleting Nodes" << endl;
  ObjectList<String> names = nodes->GetKeyWords();
  ObjectList<String>::iterator name;
  for(name = names.begin();name != names.end();name++) {
    graph->delNode(*name);
  }
}
/*F ans = WriteOutputValues(instances,instancesclass,run,runclass)  algorithm
**
**  DESCRIPTION
**    instances: The set of instances
**    instancesclass: The set of instance object classes
**    run: The algorithm run information
**    runclass: The run class
**    
**  REMARKS
**
*/
bool BaseDataMechanismGraphReduction::WriteOutputValues(BaseDataSetOfInstances *instances,
					 DataSetOfInstancesClass *instancesclass,
					 BaseDataAlgorithmRun *run,
					 DataAlgorithmRunClass *runclass)
{
  bool result = true;
  
//  run->AddParameter(ctree);

  return result;
}
/*F ans = ConcludeRun(instances,instancesclass,run,runclass)  . . . algorithm
**
**  DESCRIPTION
**    instances: The set of instances
**    instancesclass: The set of instance object classes
**    run: The algorithm run information
**    runclass: The run class
**    
**  REMARKS
**
*/
bool BaseDataMechanismGraphReduction::ConcludeRun(BaseDataSetOfInstances *instances,
				   DataSetOfInstancesClass *instancesclass,
				   BaseDataAlgorithmRun *run,
				   DataAlgorithmRunClass *runclass)
{
  bool result = true;
//  delete something
  return result;
}
 
 
 
/*S DataMechanismGraphReductionClass
 */
/*F DataMechanismGraphReductionClass() . . . . . . . . . . . . . . . . . . . empty constructor
**
**  DESCRIPTION
**
**  REMARKS
**
*/
DataMechanismGraphReductionClass::DataMechanismGraphReductionClass()
{
  Identification = MECHANISMGRAPH_REDUCTION_ID;
  NameTag = MECHANISMGRAPH_REDUCTION_NAME;
  SubClass = "AlgorithmOperation";
  EncodeDecodeClass = NameTag;
} 
/*F DataMechanismGraphReductionClass(data) . . . . . . . . . . . . . . . . .  copy constructor
**
**  DESCRIPTION
**     data: The class to be copied
**
**  REMARKS
**
*/
DataMechanismGraphReductionClass::DataMechanismGraphReductionClass(const DataMechanismGraphReductionClass& data)
  : DataAlgorithmOperationClass(data)
{
} 
 
/*F DataMechanismGraphReductionClass(id,name,descr)  . . . . . . . . . . . . . . . constructor
**
**  DESCRIPTION
**    id: The ID of the object class
**    name: The name of the object class
**    descr: A text description of the object
**
**
**  REMARKS
**
*/
DataMechanismGraphReductionClass::DataMechanismGraphReductionClass(const int id, 
				 const String& name,
				 const String& descr)
  : DataAlgorithmOperationClass(id,name,descr)
{
  SubClass = "AlgorithmOperation";
  EncodeDecodeClass = "MechanismGraphReduction";
}
 
/*F out1 = print(out) . . . . . . . . . . . . . . . . . . . . DataMechanismGraphReductionClass
**
**  DESCRIPTION
**    out1,out: The output buffer
**
**  REMARKS
**
*/
ostream& DataMechanismGraphReductionClass::print(ostream& out) const
{
  DataAlgorithmOperationClass::print(out);
  //PointerPrint(out,"  The Class: "," No Class Defined ",Class);
  // the rest
       
       return out;
}
 
/*F in1 = Read(in)  . . . . . . . . . . . . . . . . . . . . . DataMechanismGraphReductionClass
**
**  DESCRIPTION
**    in,in1: The input buffer
**
**    This reads in the rest of the information (after the type
**    has been determined from the first line.  For the
**    base DataMechanismGraphReductionClass, there is no further information.
**
**  REMARKS
**
*/
bool DataMechanismGraphReductionClass::Read(istream& in, DataSetOfObjectsClass &set)
{
  bool result = DataAlgorithmOperationClass::Read(in,set);
  //result = result && PointerClassRead(in,(DataObjectClass *&) Class,
  //COREOBJECTS_BASE_NAME,
  //set," No Class ");
  return result;
}
 
/*F CopyClone(objc) . . . . . . . . . . . . . . . . . . . . . DataMechanismGraphReductionClass
**
**  DESCRIPTION
**    objc: The class to be copied
**
**  REMARKS
**
*/
void DataMechanismGraphReductionClass::CopyClone(Identify *  objc)
{
  DataMechanismGraphReductionClass *objcfull = (DataMechanismGraphReductionClass *) objc;
  *this = *objcfull;
  //ParameterClass = (DataSetOfObjectsClass *) PointerClone(objcfull->ParameterClass);
}
/*F objc = Clone()  . . . . . . . . . . . . . . . . . . . . . DataMechanismGraphReductionClass
**
**  DESCRIPTION
**    objc: The object class that was created
**
**  REMARKS
**
*/
Identify * DataMechanismGraphReductionClass::Clone()
    {
      DataMechanismGraphReductionClass* id = new DataMechanismGraphReductionClass(*this);
      return (Identify *) id;
    }
/*F ans = EncodeThis(buffer)  . . . . . . . . . . . . . . . . DataMechanismGraphReductionClass
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: true if successful
**
**  REMARKS
**
*/
bool DataMechanismGraphReductionClass::EncodeThis(CommBuffer& buffer)
{
  bool result = DataAlgorithmOperationClass::EncodeThis(buffer);
  //result = result && PointerEncode(buffer,Class);
  // result = result && Encode(buffer,------);

  return result;
}
/*F ans = DecodeThis(buffer)  . . . . . . . . . . . . . . . . DataMechanismGraphReductionClass
**
**  DESCRIPTION
**    buffer: The buffer
**    ans: true if successful
**
**  REMARKS
**
*/
bool DataMechanismGraphReductionClass::DecodeThis(CommBuffer& buffer)
{
  bool result = DataAlgorithmOperationClass::DecodeThis(buffer);
  //result = result && PointerDecode(buffer,(BaseDataObject *&) Class);
  //result = result && Decode(buffer,-----);

  return result;
}
/*F obj = BaseDataObjectExample() . . . . . . . . . . . . .  create an object
**
**  DESCRIPTION
**    obj: The created object
**
**    This function is used to create an empty instance of a object 
**    given the class.  This is used so that the virtual functions
**    of the object can be used.
**
**  REMARKS
**
*/
BaseDataObject * DataMechanismGraphReductionClass::BaseDataObjectExample()
{ 
  BaseDataObject *obj = new BaseDataMechanismGraphReduction();
  obj->SetType(Identification);
  return obj;
}
/*F ans = TopDecode(buffer,obj) . . . . . . . . . . . . . . . DataMechanismGraphReductionClass
**
**  DESCRIPTION
**    buffer: The buffer 
**    obj: The object
**    ans: True if successful
**
**  REMARKS
**
*/
bool TopDecode(CommBuffer& buffer, DataMechanismGraphReductionClass*& obj)
     {
     obj = new DataMechanismGraphReductionClass;
     return obj->DecodeThis(buffer);
     }
/*F ans = TopDecode(buffer,obj) . . . . . . . . . . . . . . . BaseDataMechanismGraphReduction
**
**  DESCRIPTION
**    buffer: The buffer 
**    obj: The object
**    ans: True if successful
**
**  REMARKS
**
*/
bool TopDecode(CommBuffer& buffer, BaseDataMechanismGraphReduction*& obj)
     {
     obj = new BaseDataMechanismGraphReduction;
     return obj->DecodeThis(buffer);
     }


/*S Utilities
 */
/*F InitializeMechanismGraphDecodeFunctions()  . . . . . . . . . . . . Reactions
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
void InitialMechanismGraphDecodeFunctions()
{
  EncodeDecodeRegisterClass(RxnMechanismNodeClass,RxnDataMechanismNode,MECHANISMGRAPH_NODE_NAME);
  EncodeDecodeRegisterClass(RxnMechanismEdgeClass,RxnDataMechanismEdge,MECHANISMGRAPH_EDGE_NAME);
  EncodeDecodeRegisterClass(RxnMechanismGraphClass,RxnDataMechanismGraph,MECHANISMGRAPH_GRAPH_NAME);
  EncodeDecodeRegisterClass(RxnCreateMechanismGraphClass,RxnDataCreateMechanismGraph,MECHANISMGRAPH_CREATEGRAPH_NAME);
EncodeDecodeRegisterClass(DataMechanismGraphReductionClass,BaseDataMechanismGraphReduction,MECHANISMGRAPH_REDUCTION_NAME);
}
/*F AddMechanismGraphClasses(set) . . . . . . . . . . . .  EquilibriumConst
**
**  DESCRIPTION
**    set: The set of classes to add them to
**
**  REMARKS
**
*/
void AddMechanismGraphClasses(DataSetOfObjectsClass& set)
{
  String nodedescr("The Mechanism Node Class");
  RxnMechanismNodeClass nodeclass(MECHANISMGRAPH_NODE_ID,MECHANISMGRAPH_NODE_NAME,nodedescr);
  set.AddObjectClass(nodeclass);

  String edgedescr("The Mechanism Edge Class");
  RxnMechanismEdgeClass edgeclass(MECHANISMGRAPH_EDGE_ID,MECHANISMGRAPH_EDGE_NAME,edgedescr);
  set.AddObjectClass(edgeclass);

  String graphdescr("The Mechanism Graph Class");
  RxnMechanismGraphClass graphclass(MECHANISMGRAPH_GRAPH_ID,MECHANISMGRAPH_GRAPH_NAME,graphdescr);
  set.AddObjectClass(graphclass);

  String creategraphdescr("The Create Graph From Mechanism Algorithm Class");
  RxnCreateMechanismGraphClass creategraphclass(MECHANISMGRAPH_CREATEGRAPH_ID,MECHANISMGRAPH_CREATEGRAPH_NAME,creategraphdescr);
  set.AddObjectClass(creategraphclass);

  String reductiondescr("The Mechanism Graph Reduction Algorithm Class");
  DataMechanismGraphReductionClass reductionclass(MECHANISMGRAPH_REDUCTION_ID,MECHANISMGRAPH_REDUCTION_NAME,reductiondescr);
  set.AddObjectClass(reductionclass);
}
