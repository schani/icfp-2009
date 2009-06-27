#include <glib.h>
#include <math.h>
#include <stdio.h>

typedef struct
{
    double x;
    double y;
} vector_t;

static const vector_t v_zero = { 0.0, 0.0 };

typedef void (*compare_init_func_t) (guint32 n, gpointer user_data);
typedef void (*set_new_value_func_t) (guint32 addr, double new_value, gpointer user_data);

#include "out.c"

FILE *global_trace;
int global_iter;
machine_state_t global_state;
machine_inputs_t global_old_inputs;

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
    fwrite(&global_iter, 4, 1, f);
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

static void
print_timestep (machine_state_t *state)
{
    double sx = state->output[2];
    double sy = state->output[3];

    printf("%d %f %f %f %f %f\n", global_iter, state->output[0], sx, sy, state->output[4],
	   sqrt(sx * sx + sy * sy));

    //printf("%d %f %f %f %f %f\n", iter, state.output[0], state.output[1], state.output[2], state.output[3], state.output[4]);
}

static void
global_timestep (void)
{
    write_timestep(global_trace, &global_old_inputs, &global_state.inputs);
    global_old_inputs = global_state.inputs;
    timestep(&global_state);
    ++global_iter;
    print_timestep(&global_state);
}

static vector_t
get_pos (machine_state_t *state)
{
    vector_t v;

    v.x = -state->output[2];
    v.y = -state->output[3];

    return v;
}

static vector_t
v_sub (vector_t a, vector_t b)
{
    vector_t c = { a.x - b.x, a.y - b.y };
    return c;
}

static double
v_abs (vector_t v)
{
    return sqrt(v.x * v.x + v.y * v.y);
}

static vector_t
v_mul_scal (vector_t v, double a)
{
    vector_t w = { v.x * a, v.y * a };
    return w;
}

static vector_t
v_norm (vector_t v)
{
    double a = v_abs(v);
    return v_mul_scal(v, 1.0 / a);
}

static void
print_vec (vector_t v)
{
    g_print("(%f,%f)", v.x, v.y);
}

static vector_t
get_speed (machine_state_t *state)
{
    machine_state_t copy = *state;
    vector_t old = get_pos(state);
    vector_t new;

    timestep(&copy);

    new = get_pos(&copy);

    return v_sub(new, old);
}

static double
distance_from_earth (machine_state_t *state)
{
    return v_abs(get_pos(state));
}

static void
set_thrust (machine_state_t *state, vector_t thrust)
{
    state->inputs.input_2 = thrust.x;
    state->inputs.input_3 = thrust.y;
}

static double
calc_apogee (machine_state_t *state, vector_t thrust, double max_dist, int *num_iters)
{
    machine_state_t copy = *state;
    int i = 0;

    set_thrust(&copy, thrust);

    for (;;) {
	double old_dist;
	double new_dist;

	old_dist = distance_from_earth(&copy);
	timestep(&copy);
	new_dist = distance_from_earth(&copy);
	set_thrust(&copy, v_zero);

	if (new_dist >= max_dist) {
	    if (num_iters != NULL)
		*num_iters = i;
	    return max_dist;
	}

	if (new_dist <= old_dist) {
	    if (num_iters != NULL)
		*num_iters = i;
	    return old_dist;
	}

	++i;
    }
}

static int
inject_circular_to_elliptical (machine_state_t *state, double dest_apogee, double tolerance)
{
    double min = 0, max = state->output[1];
    vector_t speed, speed_norm;

    g_assert(dest_apogee > distance_from_earth(state));

    speed = get_speed(state);
    speed_norm = v_norm(speed);

    g_print("speed is ");
    print_vec(speed);
    print_vec(speed_norm);
    g_print("\n");

    while (min < max) {
	double middle = (min + max) / 2;
	vector_t thrust = v_mul_scal(speed_norm, middle);
	int num_iters;

	g_print("trying thrust %f ", middle);
	print_vec(thrust);
	g_print("\n");

	double apogee = calc_apogee(state, thrust, dest_apogee * 2.0, &num_iters);

	g_print("apogee is %f after %d iterations\n", apogee, num_iters);

	if (fabs(apogee - dest_apogee) <= tolerance) {
	    set_thrust(state, thrust);
	    return num_iters;
	}

	if (apogee < dest_apogee)
	    min = middle;
	else
	    max = middle;
    }

    g_assert_not_reached();
}

static double
calc_circular_orbit_difference (machine_state_t *state, vector_t thrust, int num_timesteps)
{
    machine_state_t copy = *state;
    double first_dist = distance_from_earth(&copy);
    int i = 0;

    set_thrust(&copy, thrust);

    for (i = 0; i < num_timesteps; ++i) {
	timestep(&copy);
	set_thrust(&copy, v_zero);
    }

    return distance_from_earth(&copy) - first_dist;
}

static void
inject_elliptical_to_circular (machine_state_t *state, double dest_apogee, int num_timesteps, double tolerance)
{
    double min = 0, max = state->output[1];
    vector_t speed, speed_norm;

    g_assert(fabs(distance_from_earth(state) - dest_apogee) <= tolerance);

    speed = get_speed(state);
    speed_norm = v_norm(speed);

    g_print("speed is ");
    print_vec(speed);
    print_vec(speed_norm);
    g_print("\n");

    while (min < max) {
	double middle = (min + max) / 2;
	vector_t thrust = v_mul_scal(speed_norm, middle);

	g_print("trying thrust %f ", middle);
	print_vec(thrust);
	g_print("\n");

	double diff = calc_circular_orbit_difference(state, thrust, num_timesteps);

	g_print("diff is %f\n", diff);

	if (fabs(diff) <= tolerance) {
	    set_thrust(state, thrust);
	    return;
	}

	if (diff < 0)
	    min = middle;
	else
	    max = middle;
    }

    g_assert_not_reached();
}

#define SCENARIO	1001

int
main (void)
{
    int num_iters, i;
    double dest_apogee;

    global_trace = open_trace_file("/tmp/trace", 19, SCENARIO);

    init_machine(&global_state);
    global_old_inputs = global_state.inputs;
    global_iter = 0;

    global_state.inputs.input_16000 = SCENARIO;

    global_timestep();

    dest_apogee = global_state.output[4];

    num_iters = inject_circular_to_elliptical(&global_state, dest_apogee, 0.000001);
    for (i = 0; i < num_iters; ++i) {
	global_timestep();
	set_thrust(&global_state, v_zero);
    }

    inject_elliptical_to_circular(&global_state, dest_apogee, 900, 0.000001);
    while (global_iter < 3000000 && global_state.output[0] == 0.0) {
	global_timestep();
	set_thrust(&global_state, v_zero);
    }

    g_print("score is %f\n", global_state.output[0]);

    write_count(0, global_trace);

    if (global_trace != NULL)
	fclose(global_trace);
    return 0;
}
