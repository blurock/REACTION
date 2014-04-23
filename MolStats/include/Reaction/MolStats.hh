/*  FILE     MolStats.hh
**  PACKAGE  MolStats
**  AUTHOR   Edward S. Blurock
**
**  CONTENT
**    Definitions for the "MolStats" package.
**
**  OVERVIEW
**
**  IMPLEMENTATION
**
**  REFERENCES
**
**  COPYRIGHT (C) 1996 Reaction, Edward S. Blurock
*/
 
#ifndef REACTION_MOLSTATS_HH
#define REACTION_MOLSTATS_HH
 
#define MOLSTATS_BASE        50130
#define MOLSTATS_ATOMS_NAME       "AtomStatistics"
#define MOLSTATS_ATOMS_ID         MOLSTATS_BASE + 1
#define MOLSTATS_FORMSTATS_NAME   "FormMoleculeSetStatistics"
#define MOLSTATS_FORMSTATS_ID     MOLSTATS_BASE + 2

#define MoleculeInInstance  "Molecule"

/*I  . . . INCLUDES  . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
*/

#include "MolStatsType.hh" 
 
/*P  . . . PROTOTYPES  . . . . . . . . . . . . . . . . . . . . . . . . . . . 
*/
void AddMolStatsClasses(DataSetOfObjectsClass& set);
void InitialSetOfMolStatsDecodeFunctions();
template <class ValenceType>
bool operator==(const AtomicNumberCount<ValenceType>& x, 
		const AtomicNumberCount<ValenceType>& y);
template <class ValenceType>
bool operator<(const AtomicNumberCount<ValenceType>& x, 
	       const AtomicNumberCount<ValenceType>& y);
template <class ValenceType>
bool operator==(const AtomCountList<ValenceType>& x,
		const AtomCountList<ValenceType>& y);
template <class ValenceType>
ostream&  operator<<(ostream& out,
		     const AtomicNumberCount<ValenceType>& counts);
template <class ValenceType>
ostream& operator<<(ostream& out,
		    const AtomCountList<ValenceType>& counts);

template <class ValenceType>
bool Encode(CommBuffer& buffer, AtomicNumberCount<ValenceType>& cnt);
template <class ValenceType>
bool Decode(CommBuffer& buffer, AtomicNumberCount<ValenceType>& cnt);
template <class ValenceType>
bool Encode(CommBuffer& buffer, AtomCountList<ValenceType>& cnt);
template <class ValenceType>
bool Decode(CommBuffer& buffer, AtomCountList<ValenceType>& cnt);

template <class ValenceType>
bool operator==(const AtomicNumberCount<ValenceType>& x, 
		const AtomicNumberCount<ValenceType>& y);
template <class ValenceType>
bool operator<(const AtomicNumberCount<ValenceType>& x, 
	       const AtomicNumberCount<ValenceType>& y);
template <class ValenceType>
ostream&  operator<<(ostream& out,
		     const AtomicNumberCount<ValenceType>& counts);
template <class ValenceType>
bool operator==(const AtomCountList<ValenceType>& x,
		const AtomCountList<ValenceType>& y);





/*I  . . . INCLUDES  . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
*/
#ifdef TEMPLATE_INSTANTIATION
#include "MolStats/MolStats.itc"
#endif

#endif


