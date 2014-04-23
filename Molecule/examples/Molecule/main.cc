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
/*I  . . . INCLUDES  . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
*/
#define TEMPLATE_INSTANTIATION
#define TEMPLATE_INSTANTIATION
#include "FullReaction.hh"

int ElementExists(ReactionSystemBase* sys);

class MolSystemTest : public MoleculeSystemBase
{
public:
  MolSystemTest(int argc, char *argv[])
    : MoleculeSystemBase(argc,argv)
    {
    }
};

/*F main  . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .  
**
**  DESCRIPTION
**    
**  REMARKS
**
*/
int main(int argc, char *argv[])
{
  MolSystemTest test(argc,argv);
  
  test.Initialization();
  test.Run();
  test.Exit();

}
