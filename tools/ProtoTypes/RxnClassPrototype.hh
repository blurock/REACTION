/*C RxnData=Object=  . . . . . . . . . . . . . . . . . . . . . . .  data
**
**  DESCRIPTION
**    This is the =Object= class definitions
**
**  REMARKS
**    Inheirits BaseData=SubClass=
*/
class RxnData=Object= : public BaseData=SubClass=
{
public:
  RxnData=Object=();
  RxnData=Object=(const RxnData=Object=& data);

  STANDARD_VIRTUAL_METHODS_OBJECT
};
/*C Rxn=Object=Class . . . . . . . . . . . .  the info about a data type
**
**  DESCRIPTION
**
**  REMARKS
**     Inheirits Data=SubClass=Class
*/
class Rxn=Object=Class : public Data=SubClass=Class
{
public:
  Rxn=Object=Class();
  Rxn=Object=Class(const Rxn=Object=Class& data);
  Rxn=Object=Class(const int id, 
		    const String& name,
		    const String& descr);
  STANDARD_VIRTUAL_METHODS_CLASS
};

