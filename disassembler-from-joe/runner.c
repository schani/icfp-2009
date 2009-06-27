#include <math.h>
#include <stdio.h>

#include "out.c"

int
main (void)
{
    int iter;
    machine_state_t state;
    init_machine(&state);
    state.input_16000 = 1001.0;
    iter = 0;
    while (++iter < 3000000 && state.output[0] == 0.0) {
	timestep(&state);
	printf("%d %f %f %f %f %f\n", iter, state.output[0], state.output[1], state.output[2], state.output[3], state.output[4]);
    }
    return 0;
}
