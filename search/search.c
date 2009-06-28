#include <math.h>
#include <stdio.h>
#include <stdlib.h>

#define	SEARCH_DO_CLAMP	1

#ifdef	SEARCH_MAIN
#define DIV_GRID	6
#define DIV_BEST	3
#else
#define DIV_GRID	5
#define DIV_BEST	4
#endif


double	calc_score(void *userdata, double *value)
{
	return sin(value[0])*cos(value[1])+1.0;
}

double	calc_score_int(void *userdata, int *value)
{
	return sin(value[0]/100.0)*cos(value[1]/100.0)+1.0;
}




double	clamp(double val, double a, double b)
{
	return (val > b) ? b : ((val < a) ? a : val);
}



void	search_area(double (*score_f)(void *, double *),
	void *user, double area[2][2], int depth)
{
	double best_score[DIV_BEST] = {0};
	double best_point[DIV_BEST][2];
	
	double div[2] = { (area[1][0] - area[0][0])/DIV_GRID,
			  (area[1][1] - area[0][1])/DIV_GRID };
	
	// printf("division is [%f,%f]\n", div[0], div[1]);
		
	
	for (int i=0; i<DIV_GRID; i++) {
	    for (int j=0; j<DIV_GRID; j++) {
		double point[2] = { area[0][0] + (0.5 + i)*div[0],
				    area[0][1] + (0.5 + j)*div[1] };
		double score = (*score_f)(user, point);

		printf("@[%f,%f] = %f [%d]\n",
			point[0], point[1], score, depth);

		for (int n=0; n<DIV_BEST; n++) {
		    if (best_score[n] < score) {
			for (int m=n+1; m<DIV_BEST; m++) {
			    best_score[m] = best_score[m-1];
			    best_point[m][0] = best_point[m-1][0];
			    best_point[m][1] = best_point[m-1][1];
			}
			best_score[n] = score;
			best_point[n][0] = point[0];
			best_point[n][1] = point[1];
			break;
		    }
		}
	    }
	}

	for (int n=0; n<DIV_BEST; n++) {
	    if (best_score[n] <= 0.0)
		break;

	    if (n == 0)
		printf("&[%f,%f] = %f [%d,%d]\n",
			best_point[n][0], best_point[n][1],
			best_score[n], depth, n);

	    double new_area[2][2];

	    new_area[0][0] = best_point[n][0] - div[0]/2.0;
	    new_area[0][1] = best_point[n][1] - div[1]/2.0;
	    new_area[1][0] = best_point[n][0] + div[0]/2.0;
	    new_area[1][1] = best_point[n][1] + div[1]/2.0;

	    if (depth > 0) {
		printf("[%d] now search area [%f,%f-%f,%f] ...\n",
			depth,
			new_area[0][0], new_area[0][1],
			new_area[1][0], new_area[1][1]);

		search_area(score_f, user, new_area, depth - 1);			
	    }
	}
}



void	search_area_int(double (*score_f)(void *, int *),
	void *user, int area[2][2], int depth)
{
	double best_score[DIV_BEST] = {0};
	double best_point[DIV_BEST][2];
	
	double div[2] = { (area[1][0] - area[0][0])*1.0/DIV_GRID,
			  (area[1][1] - area[0][1])*1.0/DIV_GRID };

	printf("division is [%f,%f]\n", div[0], div[1]);

	if (div[0] < 1.0)
	    div[0] = 1.0;
	if (div[1] < 1.0)
	    div[1] = 1.0;

	for (double i=area[0][0] + div[0]/2.0; i<area[1][0]; i+=div[0]) {
	    for (double j=area[0][1] + div[1]/2.0; j<area[1][1]; j+=div[1]) {
		int point[2] = { i, j };
		double score = (*score_f)(user, point);
		
		printf("@[%d,%d] = %f [%d]\n",
			point[0], point[1], score, depth);

		for (int n=0; n<DIV_BEST; n++) {
		    if (best_score[n] < score) {
			for (int m=n+1; m<DIV_BEST; m++) {
			    best_score[m] = best_score[m-1];
			    best_point[m][0] = best_point[m-1][0];
			    best_point[m][1] = best_point[m-1][1];
			}
			best_score[n] = score;
			best_point[n][0] = point[0];
			best_point[n][1] = point[1];
			break;
		    }
		}
	    }
	}
	
	for (int n=0; n<DIV_BEST; n++) {
	    if (best_score[n] <= 0.0)
		break;

	    if (n == 0)
		printf("&[%d,%d] = %f [%d,%d]\n",
			(int)best_point[n][0], (int)best_point[n][1],
			best_score[n], depth, n);

	    int new_area[2][2];

	    new_area[0][0] = best_point[n][0] - (int)(div[0]/2.0);
	    new_area[0][1] = best_point[n][1] - (int)(div[1]/2.0);
	    new_area[1][0] = best_point[n][0] + (int)(div[0]/2.0);
	    new_area[1][1] = best_point[n][1] + (int)(div[1]/2.0);

#ifdef	SEARCH_DO_CLAMP
	    for (int i=0; i<2; i++) {
		for (int j=0; j<2; j++) {
		    new_area[i][j] = clamp(new_area[i][j], area[0][j], area[1][j]);
		}
	    }
#endif

	    if ((depth > 0) && (div[0] >= 1.0 || div[1] >= 1.0)) {
		printf("[%d] now search area [%d,%d-%d,%d] ...\n",
			depth,
			new_area[0][0], new_area[0][1],
			new_area[1][0], new_area[1][1]);

		search_area_int(score_f, user, new_area, depth - 1);
	    }
	}
}
	
	
#ifdef	SEARCH_MAIN
	
int	main(int argc, char *argv[])
{
	double start_area[2][2] = {{0, 0}, {4, 4}};
	int start_area_int[2][2] = {{0, 0}, {50, 30}};

	search_area(&calc_score, NULL, start_area, 4);
	search_area_int(&calc_score_int, NULL, start_area_int, 4);
	
	exit(0);

}

#endif

