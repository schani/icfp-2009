
#include <math.h>
#include <stdio.h>
#include <fenv.h>
#include <getopt.h>
#include <string.h>
#include <stdlib.h>

#define	G_PI	3.1415926784

/* MATH STUFF */

#define	SIM_G	6.67428e-11
#define SIM_M	6.0e+24
#define SIM_R	6.357e+6

#define SIM_mu	(SIM_G*SIM_M)

static double
m_period(double semi_major)
{
    return 2.0*G_PI * sqrt((semi_major * semi_major * semi_major) / SIM_mu);
}


/* solve T3+T4 numerically */

static double
m_solve_t3t4(double a, double b, double T, double precision)
{
    double interval[2] = { 0, 1e+6 };
    double val, h = -1;
    
    do {
	h = (interval[0] + interval[1])/2.0;
	val = m_period((a+h)/2.0) + m_period((b+h)/2.0);
	
	if (val < T) {
	    interval[0] = h;
	} else {
	    interval[1] = h;
	}
    } while (fabs(T - val) > precision);
    return h;
}




int main(int argc, char *argv[]) 
{
	
	double a = atof(argv[1]);	
	double b = atof(argv[2]);	
	double T = atof(argv[3]);	
	double p = atof(argv[4]);

	double h = m_solve_t3t4(a, b, T, p);

	printf ("a=%f, b=%f, T=%f, p=%f -> h=%f\n", a, b, T, p, h);	

	exit(0);
}
