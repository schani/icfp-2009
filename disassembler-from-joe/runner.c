#include <glib.h>
#include <math.h>
#include <stdio.h>

int iter;

typedef void (*compare_init_func_t) (guint32 n, gpointer user_data);
typedef void (*set_new_value_func_t) (guint32 addr, double new_value, gpointer user_data);

#include "out.c"

static FILE*
open_trace_file (char *filename, guint32 team_id, guint32 scenario)
{
    FILE *f = fopen(filename, "w");
    guint32 magic = 0xcafebabe;

    g_assert(f != NULL);

    fwrite(&magic, 4, 1, f);
    fwrite(&team_id, 4, 1, f);
    fwrite(&scenario, 4, 1, f);

    return f;
}

static void
write_count (guint32 count, FILE *f)
{
    fwrite(&iter, 4, 1, f);
    fwrite(&count, 4, 1, f);
}

static void
write_count_func (guint32 count, gpointer user_data)
{
    FILE *f = user_data;
    if (count > 0)
	write_count(count, f);
}

static void
write_value_func (guint32 addr, double val, gpointer user_data)
{
    FILE *f = user_data;
    fwrite(&addr, 4, 1, f);
    fwrite(&val, 8, 1, f);
}

static void
write_timestep (FILE *trace, machine_inputs_t *old, machine_inputs_t *new)
{
    if (trace == NULL)
	return;
    compare_inputs(old, new, write_count_func, write_value_func, trace);
}

#define SCENARIO	1001

int
main (void)
{
    machine_state_t state;
    machine_inputs_t old_inputs;
    FILE *trace = open_trace_file("/tmp/trace", 19, SCENARIO);

    init_machine(&state);
    old_inputs = state.inputs;

    iter = 0;

    state.inputs.input_16000 = SCENARIO;
    write_timestep(trace, &old_inputs, &state.inputs);

    while (++iter < 3000000 && state.output[0] == 0.0) {
	double sx, sy;

	timestep(&state);

	old_inputs = state.inputs;
	if (iter == 2) {
	    state.inputs.input_2 = -5.87933562337077564;
	    state.inputs.input_3 = -2466.47900495061549;
	} else if (iter == 18875) {
	    state.inputs.input_2 = 3.53485723689101805;
	    state.inputs.input_3 = 1470.93135803171094;
	    //state.inputs.input_3 = 1482.93135803171094;
	} else {
	    state.inputs.input_2 = 0.0;
	    state.inputs.input_3 = 0.0;
	}
	write_timestep(trace, &old_inputs, &state.inputs);

	sx = state.output[2];
	sy = state.output[3];
	if (iter % 1 == 0)
	    //printf("%d %f %f %f %f %f\n", iter, state.output[0], sx, sy, state.output[4],
	    //sqrt(sx * sx + sy * sy));
	    printf("%d %f %f %f %f %f\n", iter, state.output[0], state.output[1], state.output[2], state.output[3], state.output[4]);
    }

    write_count(0, trace);

    if (trace != NULL)
	fclose(trace);
    return 0;
}
