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
#define TEMPLATE_INSTANTIATION
#include "FullReaction.hh"
#include "BasisSystem.hh"
void *STATICATOMINFO = NULL;
/*C
**
**  DESCRIPTION
**
**  REMARKS
**
*/
class MechanismTest : public MechanismSystemBase
{
 public:
  MechanismTest(int argc, char *argv[])
    : MechanismSystemBase(argc,argv)
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
  MechanismTest test(argc,argv);
  test.Initialization();
  test.Run();
  test.Exit();
}
