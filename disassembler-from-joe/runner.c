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

#if defined(BIN1)
#define SCENARIO	1001
#include "bin1.c"
#elif defined(BIN2)
#define SCENARIO	2001
#include "bin2.c"
#elif defined(BIN3)
#define SCENARIO	3003
#include "bin3.c"
#else
#error bla
#endif

FILE *dump_file = NULL;
FILE *global_trace = NULL;
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

#if defined(BIN1)
static void
print_timestep (machine_state_t *state)
{
    double sx = -state->output[2];
    double sy = -state->output[3];

    if (dump_file != NULL)
	fprintf(dump_file, "%d %f %f %f %f 1 0 %f %f\n", global_iter, state->output[0], state->output[1],
		sx, sy, state->output[4], sqrt(sx * sx + sy * sy));
}
#elif defined(BIN2) || defined(BIN3)
static void
print_timestep (machine_state_t *state)
{
    double sx = -state->output[2];
    double sy = -state->output[3];

    double dx = state->output[4];
    double dy = state->output[5];

    if (dump_file != NULL)
	fprintf(dump_file, "%d %f %f %f %f 0 1 %f %f %f\n", global_iter, state->output[0], state->output[1],
		sx, sy, sx + dx, sy + dy, sqrt(sx * sx + sy * sy));
}
#else
#error bla
#endif

static void
global_timestep (void)
{
    write_timestep(global_trace, &global_old_inputs, &global_state.inputs);
    global_old_inputs = global_state.inputs;
    timestep(&global_state);
    ++global_iter;
    print_timestep(&global_state);
}

static void
do_timestep (machine_state_t *state)
{
    if (state == &global_state)
	global_timestep();
    else
	timestep(state);
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
get_thrust (machine_state_t *state)
{
    vector_t v;

    v.x = state->inputs.input_2;
    v.y = state->inputs.input_3;

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

static double
v_angle (vector_t v)
{
    double angle = atan2(v.y, v.x);
    if (angle < 0.0)
	angle += 2.0 * G_PI;
    return angle;
}

static double
a_norm (double angle)
{
    while (angle > G_PI)
	angle -= G_PI;
    while (angle <= -G_PI)
	angle += G_PI;
    return angle;
}

static double
a_delta (double a1, double a2)
{
    return a_norm(a2-a1);
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

static vector_t
v_rotate (vector_t v, double angle)
{
    vector_t new;

    new.x = v.x*cos(angle)-v.y*sin(angle);
    new.y = v.x*sin(angle)+v.y*cos(angle);
    return new;
}

static vector_t
get_norm_speed (machine_state_t *state)
{
    machine_state_t copy = *state;
    vector_t old = get_pos(state);
    vector_t new;

    timestep(&copy);

    new = get_pos(&copy);

    {
	double a1 = atan2(old.y, old.x);
	double a2 = atan2(new.y, new.x);
	double da = a_delta(a1, a2);

	vector_t veld = v_sub(new, old);
	vector_t velc;

        if (0) {
	    velc = v_rotate(veld, da);
	} else {
	    velc = v_rotate(old, (da >= 0) ? G_PI/2.0 : -G_PI/2.0);
	}

	g_print("[%f,%f] -> %f  (%f,%f) -> (%f,%f)\n",
		a1, a2, da, veld.x, veld.y, velc.x, velc.y);
	return v_norm(velc);
	
    }

    // return v_norm(v_sub(new, old));
}

static double
distance_from_earth (machine_state_t *state)
{
    return v_abs(get_pos(state));
}

static double
get_angle (machine_state_t *state)
{
    vector_t pos = get_pos(state);
    return v_angle(pos);
}

static void
set_thrust (machine_state_t *state, vector_t thrust)
{
    state->inputs.input_2 = thrust.x;
    state->inputs.input_3 = thrust.y;
}

static void
do_n_timesteps (machine_state_t *state, int num_iters)
{
    for (int i = 0; i < num_iters; ++i) {
	do_timestep(state);
	set_thrust(state, v_zero);
    }
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

static void
calc_ellipse (machine_state_t *state, vector_t *apogee, vector_t *perigee, int *t_to_apogee, int *t_to_perigee)
{
    machine_state_t copy = *state;
    double dist = distance_from_earth(&copy);
    gboolean ascending;
    gboolean have_apogee = FALSE, have_perigee = FALSE;
    vector_t pos;
    int iter = 0;

    timestep(&copy);
    ++iter;
    set_thrust(&copy, v_zero);

    ascending = distance_from_earth(&copy) > dist;
    pos = get_pos(&copy);
    dist = distance_from_earth(&copy);

    while (!have_apogee || !have_perigee) {
	gboolean new_ascending;

	timestep(&copy);
	new_ascending = distance_from_earth(&copy) > dist;

	if (new_ascending != ascending) {
	    if (ascending) {
		g_assert(!have_apogee);
		have_apogee = TRUE;
		*apogee = pos;
		*t_to_apogee = iter;
	    } else {
		g_assert(!have_perigee);
		have_perigee = TRUE;
		*perigee = pos;
		*t_to_perigee = iter;
	    }
	}

	ascending = new_ascending;
	dist = distance_from_earth(&copy);
	pos = get_pos(&copy);

	++iter;
    }
}

static int
inject_circular_to_elliptical (machine_state_t *state, double dest_apogee, double tolerance)
{
    double min = 0, max = state->output[1];
    vector_t speed_norm;

    g_assert(dest_apogee > distance_from_earth(state));

    speed_norm = get_norm_speed(state);

    g_print("norm speed is ");
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
inject_elliptical_to_circular (machine_state_t *state, double sign,
			       double dest_apogee, int num_timesteps, double tolerance)
{
    double min = 0, max = state->output[1];
    vector_t speed_norm;

    g_assert(fabs(distance_from_earth(state) - dest_apogee) <= tolerance);

    speed_norm = get_norm_speed(state);
    if (sign < 0)
	speed_norm = v_mul_scal(speed_norm, -1.0);

    g_print("norm speed is ");
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

	if (sign >= 0) {
	    if (diff < 0)
		min = middle;
	    else
		max = middle;
	} else {
	    if (diff < 0)
		max = middle;
	    else
		min = middle;
	}
    }

    g_assert_not_reached();
}

static gboolean
between_angles (double angle, double a1, double a2, double max_diff)
{
    g_assert(angle >= 0 && angle <= 2.0 * G_PI);
    g_assert(a1 >= 0 && a1 <= 2.0 * G_PI);
    g_assert(a2 >= 0 && a2 <= 2.0 * G_PI);

    if (fabs(a1 - a2) > max_diff) {
	if (a1 < a2)
	    return angle <= a1 || angle >= a2;
	else
	    return angle <= a2 || angle >= a1;
    }

    if (a1 < a2)
	return angle >= a1 && angle <= a2;
    else
	return angle >= a2 && angle <= a1;
}

static int
timestep_until_angle (machine_state_t *state, double angle, double max_dist, gboolean *have_angle)
{
    double start_angle = get_angle(state);
    double dest_angle = start_angle + angle;
    double old_angle = start_angle;
    int i = 0;

    g_assert(angle >= 0.0);
    if (dest_angle > 2.0 * G_PI)
	dest_angle -= 2.0 * G_PI;

    for (;;) {
	do_timestep(state);
	set_thrust(state, v_zero);
	++i;

	if (distance_from_earth(state) >= max_dist) {
	    if (have_angle != NULL)
		*have_angle = FALSE;
	    return i;
	}

	double new_angle = get_angle(state);

	//g_print("%d %f %f %f %f\n", i, old_angle, new_angle, dest_angle, distance_from_earth(&copy));

	if (between_angles(dest_angle, old_angle, new_angle, 0.2)) {
	    g_print("at angle %f (dest angle %f) - dist %f\n", new_angle, dest_angle, distance_from_earth(state));
	    if (have_angle != NULL)
		*have_angle = TRUE;
	    return i;
	}

	old_angle = new_angle;
    }

    g_assert_not_reached();
}

static double
calc_distance_after_angle (machine_state_t *state, vector_t thrust, double angle, int *num_iters)
{
    machine_state_t copy = *state;
    double max_dist = 2.0 * distance_from_earth(&copy);
    int i;

    set_thrust(&copy, thrust);

    i = timestep_until_angle(&copy, angle, max_dist, NULL);
    if (num_iters != NULL)
	*num_iters = i;

    return distance_from_earth(&copy);
}

static int
inject_bielliptical_to_circular (machine_state_t *state, double dest_apogee, double tolerance)
{
    double min = 0, max = state->output[1];
    vector_t speed_norm;
    int num_iters = 0;

    g_assert(distance_from_earth(state) > dest_apogee);

    speed_norm = get_norm_speed(state);

    g_print("norm sspeed is ");
    print_vec(speed_norm);
    g_print("\n");

    while (min < max) {
	double middle = (min + max) / 2;
	vector_t thrust = v_mul_scal(speed_norm, middle);

	g_print("trying thrust %f ", middle);
	print_vec(thrust);
	g_print("\n");

	double dist = calc_distance_after_angle(state, thrust, G_PI, &num_iters);

	g_print("dist is %f\n", dist);

	if (fabs(dist - dest_apogee) <= tolerance) {
	    set_thrust(state, thrust);
	    return num_iters;
	}

	if (dist < dest_apogee)
	    min = middle;
	else
	    max = middle;
    }

    g_assert_not_reached();
}

int
main (int argc, char *argv[])
{
    int num_iters, i;
    double dest_apogee;
    gboolean have_angle = FALSE;

    if (argc > 1)
	dump_file = fopen(argv[1], "w");

    global_trace = open_trace_file("/tmp/trace.osf", 19, SCENARIO);

    init_machine(&global_state);
    global_old_inputs = global_state.inputs;
    global_iter = 0;

    global_state.inputs.input_16000 = SCENARIO;

    global_timestep();

#if defined(BIN1)
    dest_apogee = global_state.output[4];

    num_iters = inject_circular_to_elliptical(&global_state, dest_apogee, 1.0);

#if 1
    do_n_timesteps(&global_state, num_iters);
    inject_elliptical_to_circular(&global_state, 1.0, dest_apogee, 900, 1.0);
#else
    // thrust more
    set_thrust(&global_state, v_mul_scal(get_thrust(&global_state), 1.25));
    timestep_until_angle(&global_state, G_PI, dest_apogee * 100.0, &have_angle);
    if (!have_angle) {
	g_print("don't have angle\n");
	return 1;
    }

    num_iters = inject_bielliptical_to_circular(&global_state, dest_apogee, 1.0);
    for (i = 0; i < num_iters; ++i) {
	global_timestep();
	set_thrust(&global_state, v_zero);
    }

    inject_elliptical_to_circular(&global_state, -1.0, dest_apogee, 900, 1.0);

    /*
    for (i = 0; i < 900; ++i) {
	global_timestep();
	set_thrust(&global_state, v_zero);
    }
    */
#endif
#elif defined(BIN2)
    g_assert_not_reached();
#elif defined(BIN3)

    vector_t apogee, perigee;
    int t_to_apogee, t_to_perigee;

    calc_ellipse(&global_state, &apogee, &perigee, &t_to_apogee, &t_to_perigee);

    g_print("t to apogee: %d  perigee: %d\n", t_to_apogee, t_to_perigee);
    g_print("apogee ");
    print_vec(apogee);
    g_print("   perigee ");
    print_vec(perigee);
    g_print("\n");
#else
#error bla
#endif

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
