#include "basic.h"
#include <math.h>

int main(int argc, char *argv[])
     {
       double A1,A2,n1,n2,E1,E2;
       double error,temp;
       double ratio,k1,k2,R;
       double a1,a2;

       R = 1.98 * 4.189/1000.0;

       A1 = ConvertStringToFloat(argv[1]);
       A2 = ConvertStringToFloat(argv[4]);
       n1 = ConvertStringToFloat(argv[2]);
       n2 = ConvertStringToFloat(argv[5]);
       E1 = ConvertStringToFloat(argv[3]);
       E2 = ConvertStringToFloat(argv[6]);

       printf(" %f * T^(%f) * exp(%f/RT)\n", A1,n1,E1);
       printf(" %f * T^(%f) * exp(%f/RT)\n", A2,n2,E2);
       error = 0.0;
       for(temp=300;temp < 2000;temp += 100.0)
	 {
	   k1 = A1 * pow(temp,n1) * exp(-(E1/(temp * R)));
	   k2 = A2 * pow(temp,n2) * exp(-(E2/(temp * R)));
	   a1 = pow(temp,fabs(n1));
	   a2 = pow(temp,fabs(n2));
	   printf("%10f %10f      ",
		  log10(A1*a1),log10(A2*a2));
	   if(n1 < 0.0) a1 = 1.0/a1;
	   if(n2 < 0.0) a2 = 1.0/a2;
	   
	   printf("%10f %10f   %10f %10f   ",
		  a1,a2,
		  exp(-(E1/(temp * R))),
		  exp(-(E2/(temp * R))));
	   ratio = fabs( (k1 - k2)/(k1 + k2) );
	   error += ratio;
	   printf(" Temperature: %10f  Ratio: %f K1: %10f K2: %10f \n", temp,ratio,k1,k2);
	 }
       printf("Total Error: %10f\n",error);
       return 0;
     }
