#include <math.h>
#include <stdio.h>
#include <stdlib.h>




double	calc_score(void *userdata, double *value)
{
	return sin(value[0])*cos(value[1])+1.0;
}



#define DIV_GRID	5
#define DIV_BEST	3

double	mix(double a, double b, double f)
{
	return (a * (1.0 -f)) + (b * f);
}


void	search_area(double area[2][2], int depth)
{
	/* remove that later */
	double grid[DIV_GRID][DIV_GRID][2];
	
	double best_score[DIV_BEST] = {0};
	double best_point[DIV_BEST][2];
	
	double div[2] = { (area[1][0] - area[0][0])/(DIV_GRID-1),
			  (area[1][1] - area[0][1])/(DIV_GRID-1) };
	
	printf("division is [%f,%f]\n", div[0], div[1]);
		
	
	for (int i=0; i<DIV_GRID; i++) {
	    for (int j=0; j<DIV_GRID; j++) {
		grid[i][j][0] = area[0][0] + i*div[0];
		grid[i][j][1] = area[0][1] + j*div[1];
		
		double score = calc_score(NULL, grid[i][j]);
		
		for (int n=0; n<DIV_BEST; n++) {
		    if (best_score[n] < score) {
		    	for (int m=n+1; m<DIV_BEST; m++) {
			    best_score[m] = best_score[m-1];
			    best_point[m][0] = best_point[m-1][0];
			    best_point[m][1] = best_point[m-1][1];
			}
			best_score[n] = score;
			best_point[n][0] = grid[i][j][0];
			best_point[n][1] = grid[i][j][1];
		    	break;
		    }
		}
		
		/* printf("score [%d,%d] (%f,%f) = %f\n",
			i, j, grid[i][j][0], grid[i][j][1],
			score); */
	    }
	}
	
	for (int n=0; n<DIV_BEST; n++) {
	    if (n == 0)
	    	printf("[%d] best [%d] (%f,%f) = %f\n",
	    		depth, n, best_point[n][0], best_point[n][1],
			best_score[n]);
	
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

	    	search_area(new_area, depth - 1);			
	    }
	}
}
	
int	main(int argc, char *argv[])
{
	double start_area[2][2] = {{0, 0}, {4, 4}};
	
	search_area(start_area, 4);
	
	exit(0);

}


