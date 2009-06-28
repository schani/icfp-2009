#ifndef __SEARCH_H__
#define __SEARCH_H__

void	search_area(double (*score_f)(void *, double *),
		    void *user, double area[2][2], int depth);

void	search_area_int(double (*score_f)(void *, int *),
			void *user, int area[2][2], int depth);

#endif
