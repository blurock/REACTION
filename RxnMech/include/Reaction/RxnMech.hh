/*  FILE     RxnMech.hh
**  PACKAGE  RxnMech
**  AUTHOR   Edward S. Blurock
**
**  CONTENT
**    Definitions for the "RxnMech" package.
**
**  OVERVIEW
**
**  IMPLEMENTATION
**
**  REFERENCES
**
**  COPYRIGHT (C) 1996 Reaction, Edward S. Blurock
*/
 
#ifndef REACTION_RXNMECH_HH
#define REACTION_RXNMECH_HH
 
 
/*I  . . . INCLUDES  . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
*/
#define MECHANISM_NAMES 99999999
#define MOLECULE_NODE   100
#define REACTION_NODE   200
void InitializeMechanismPropDecodeFunctions();
 
#include "Reaction/RxnMechType.hh"

/*P  . . . PROTOTYPES  . . . . . . . . . . . . . . . . . . . . . . . . . . . 
*/
template <class ReactionClass>
ostream& operator<<(ostream& out, const ReactionMechanism<ReactionClass>& mech);
template <class ReactionClass>
bool Encode(CommBuffer& buffer, ReactionMechanism<ReactionClass>& mech);
template <class ReactionClass>
bool Decode(CommBuffer& buffer, ReactionMechanism<ReactionClass>& mech);
ostream& operator<<(ostream& out, DbaseMechanism& mech);
ostream& operator<<(ostream& out, const SenkinMoleculeDataPoints& data);
ostream& operator<<(ostream& out, const MechanismSenkinDataPoints& data);
ostream& operator<<(ostream& out, const SenkinInitialConditions& conditions);
ostream& operator<<(ostream& out, const DbaseMechPaths& paths);

bool Encode(CommBuffer& buffer, SenkinMoleculeDataPoints& data);
bool Decode(CommBuffer& buffer, SenkinMoleculeDataPoints& data);
bool Encode(CommBuffer& buffer, MechanismSenkinDataPoints& data);
bool Decode(CommBuffer& buffer, MechanismSenkinDataPoints& data);
bool Encode(CommBuffer& buffer, SenkinInitialConditions& conditions);
bool Decode(CommBuffer& buffer, SenkinInitialConditions& conditions);
bool Encode(CommBuffer& buffer, BasicMechanismGraph& mgraph);
bool Decode(CommBuffer& buffer, BasicMechanismGraph& mgraph);
bool Encode(CommBuffer& buffer, DbaseMechPaths& mgraph);
bool Decode(CommBuffer& buffer, DbaseMechPaths& mgraph);

bool operator==(const SenkinMoleculeDataPoints& data1, const SenkinMoleculeDataPoints& data2);
bool operator<(const SenkinMoleculeDataPoints& data1, const SenkinMoleculeDataPoints& data2);

ostream& PrintMoleculeGraphNode(ostream& out,
				GraphNode<Identify,Identify> node,
				const String& prefix,
				const int maxline);
ostream& PrintMechGraphNode(ostream& out,
			    GraphNode<Identify,Identify> node,
			    const String& prefix,
			    const int maxline);
ostream& PrintReactionGraphNode(ostream& out,
				GraphNode<Identify,Identify> node,
				const String& prefix,
				const int maxline);
ostream& operator<<(ostream& out, const BasicMechanismGraph& mgraph);
ostream& PrintIdentifyList(ostream& out,
			   const ObjectList<Identify>& ilist,
			   const String& prefix,
			   const int maxline);
ostream& PrintIdentifyListByName(ostream& out,
				 const ObjectList<Identify>& ilist,
				 const String& prefix,
				 const int maxline);
ostream& PrintIdentifyListByID(ostream& out,
			       const ObjectList<Identify>& ilist,
			       const String& prefix,
			       const int maxline);


/*I  . . . INCLUDES  . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
*/
#include "ReactionSrc/RxnMech/RxnMech.icc"

#ifdef TEMPLATE_INSTANTIATION
#include "ReactionSrc/RxnMech/RxnMech.itc"
#endif

#endif


