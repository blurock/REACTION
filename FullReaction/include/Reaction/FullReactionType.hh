/*  FILE     FullReactionType.hh
**  PACKAGE  FullReaction
**  AUTHOR   Edward S. Blurock
**
**  CONTENT
**    Class definitions for the "FullReaction" package.
**
**  OVERVIEW
**
**  IMPLEMENTATION
**
**  REFERENCES
**
**  COPYRIGHT (C) 1996 Reaction Project, RISC Linz
*/
 
#ifndef Reaction_FULLREACTIONTYPE_HH
#define Reaction_FULLREACTIONTYPE_HH

/*I  . . . INCLUDES  . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
*/

class ReactionSystemSave : public AnalysisSystemSave
{
public:
  ReactionSystemSave(int argc, char *argv[])
    : AnalysisSystemSave(argc,argv)
    {
    }
  virtual void EncodeDecodeObjectsSetUp();
  virtual void StandardObjectsSetUp();
  virtual void CommandSetUp();
  virtual void Initialization();
};


#endif
