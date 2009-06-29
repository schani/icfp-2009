#include <glib.h>
#include <math.h>
#include <stdio.h>
#include <fenv.h>
#include <getopt.h>
#include <string.h>
#include <stdlib.h>

#include "search.h"

#define MAX_TIMESTEPS		3000000
#define WINNING_RADIUS		1000.0
#define TARGET_RADIUS		10.0
#define WINNING_TIMESTEPS	900

typedef struct
{
    double x;
    double y;
} vector_t;

static vector_t v_sub (vector_t a, vector_t b);
static double v_abs (vector_t v);

static const vector_t v_zero = { 0.0, 0.0 };

typedef void (*compare_init_func_t) (guint32 n, gpointer user_data);
typedef void (*set_new_value_func_t) (guint32 addr, double new_value, gpointer user_data);

#if defined(BIN1)
#include "bin1.c"
#elif defined(BIN2)
#include "bin2.c"
#elif defined(BIN3)
#include "bin3.c"
#elif defined(BIN4)
#include "bin4.c"
#else
#error bla
#endif

typedef vector_t (*get_pos_func_t) (machine_state_t *state);

typedef double (*fuel_divisor_func_t) (machine_state_t *state, gpointer user_data);
typedef int (*skip_size_func_t) (machine_state_t *state, gpointer user_data);
typedef gboolean (*termination_condition_func_t) (machine_state_t *state, get_pos_func_t get_pos_func,
						  gpointer user_data);

FILE *dump_file = NULL;
FILE *global_trace = NULL;
machine_state_t global_state;
machine_inputs_t global_old_inputs;
double dump_orbit = -1;
vector_t debug_point = { 0.0, 0.0 };

static void
set_input (machine_state_t *state, guint32 addr, double value)
{
    switch (addr) {
	case 2 :
	    state->inputs.input_2 = value;
	    break;
	case 3 :
	    state->inputs.input_3 = value;
	    break;
	case 16000 :
	    state->inputs.input_16000 = value;
	    break;
	default :
	    g_assert_not_reached();
    }
}

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
write_count (machine_state_t *state, guint32 count, FILE *f)
{
    fwrite(&state->num_timesteps_executed, 4, 1, f);
    fwrite(&count, 4, 1, f);
}

static void
write_count_func (guint32 count, gpointer user_data)
{
    FILE *f = user_data;
    if (count > 0)
	write_count(&global_state, count, f);
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

static vector_t
get_pos (machine_state_t *state)
{
    vector_t v;

    v.x = -state->output[2];
    v.y = -state->output[3];

    return v;
}

static void
set_dump_orbit (double d)
{
    dump_orbit = d;
}

static void
clear_dump_orbit (void)
{
    dump_orbit = -1;
}

static void
set_debug_point (vector_t p)
{
    debug_point = p;
}


/* Format of dump file:
 *
 * <time> <score> <fuel> <our-x> <our-y>
 *     <number-or-orbits> <number-of-satellites> <number-of-moons>
 *     <number-of-fueling-stations> <number-of-debug-points>
 *
 * <orbit-0> <orbit-1> ... <orbit-n-1>
 * <sat-0-x> <sat-0-y> <sat-1-x> <sat-1-y> ... <sat-m-1-x> <sat-m-1-y>
 * <moon-0-x> <moon-0-y> <moon-1-x> <moon-1-y> ... <moon-k-1-x> <moon-k-1-y>
 * <fuel-0-x> <fuel-0-y> <fuel-1-x> <fuel-1-y> ... <fuel-l-1-x> <fuel-l-1-y>
 * <point-0-x> <point-0-y> <point-1-x> <point-1-y> ... <point-l-1-x> <point-l-1-y>
 */
#if defined(BIN1)
static void
print_timestep (machine_state_t *state)
{
    double sx = -state->output[2];
    double sy = -state->output[3];
    int count_debugpoints = 1;
    
    if ((debug_point.x == 0.0) && (debug_point.y == 0.0)){
    	count_debugpoints = 0;
    }

    if (dump_file != NULL){
    	//general
		fprintf(dump_file, "%d %f %f %f %f ",
			state->num_timesteps_executed, state->output[0], state->output[1], sx, sy);
		//counts
		fprintf(dump_file, "1 0 0 0 %d ", count_debugpoints);
		//orbits 
		fprintf(dump_file, "%f " ,state->output[4]);
		//satellites
		//moons
		//fueling stations
		//debug point (if there is one)
		if (count_debugpoints != 0)
			fprintf(dump_file, "%f %f ", debug_point.x, debug_point.y);
		//comment (distance)
		fprintf(dump_file, "%f\n", sqrt(sx * sx + sy * sy));
    }

}
#elif defined(BIN2) || defined(BIN3)
static void
print_timestep (machine_state_t *state)
{
    double sx = -state->output[2];
    double sy = -state->output[3];

    double dx = state->output[4];
    double dy = state->output[5];
    
    int count_debugpoints = 1;
    
    if ((debug_point.x == 0.0) && (debug_point.y == 0.0)){
    	count_debugpoints = 0;
    }    

    if (dump_file != NULL) {
    	//general
		fprintf(dump_file, "%d %f %f %f %f ", state->num_timesteps_executed,
			state->output[0], state->output[1],	sx, sy); 
		//counts
		fprintf(dump_file, "%d 1 0 0 %d ", dump_orbit <= 0.0 ? 0 : 1, count_debugpoints);
		//orbits
		if (dump_orbit > 0.0)
		    fprintf(dump_file, "%f ", dump_orbit);
		//satellites
		fprintf(dump_file, "%f %f ", sx + dx, sy + dy);
		//moons
		//fueling stations
		//debug point (if there is one)	
		if (count_debugpoints != 0)
			fprintf(dump_file, "%f %f ", debug_point.x, debug_point.y);
		//comment (distance)	 
	 	fprintf(dump_file, "%f\n", sqrt(sx * sx + sy * sy));
    }
}

static vector_t
get_meet_greet_sat_pos (machine_state_t *state)
{
    vector_t pos = get_pos(state);

    double dx = state->output[4];
    double dy = state->output[5];

    vector_t v = { pos.x + dx, pos.y + dy };

    return v;
}
#elif defined(BIN4)

static void
print_timestep (machine_state_t *state)
{
    int max_sat = 11; //how much satellites should be visualized

    double sx = -state->output[2];
    double sy = -state->output[3];
    
    double fuelx = state->output[4];
    double fuely = state->output[5];

    double moonx = state->output[0x64];
    double moony = state->output[0x65];

    int count_debugpoints = 1;
    
    if ((debug_point.x == 0.0) && (debug_point.y == 0.0)){
    	count_debugpoints = 0;
    }    

    
    if (dump_file != NULL) {
    	//general
		fprintf(dump_file, "%d %f %f %f %f ", state->num_timesteps_executed,
			state->output[0], state->output[1],	sx, sy); 
		//counts
		fprintf(dump_file, "%d %d 1 1 %d ", dump_orbit <= 0.0 ? 0 : 1,
			max_sat, count_debugpoints);
		//orbits
		if (dump_orbit > 0.0)
		    fprintf(dump_file, "%f ", dump_orbit);
		//satellites
		for (int i=0; i<max_sat; ++i){
			double dx = state->output[3*i+7];
        	double dy = state->output[3*i+8];
			fprintf(dump_file, "%f %f ", sx + dx, sy + dy);
		}
		//moon
		fprintf(dump_file, "%f %f ", sx + moonx, sy + moony); 
		//fueling station
		fprintf(dump_file, "%f %f ", sx + fuelx, sy + fuely); 
		//debug point (if there is one)	
		if (count_debugpoints != 0)
			fprintf(dump_file, "%f %f ", debug_point.x, debug_point.y);
		//comment	 
	 	fprintf(dump_file, "\n");
    }
}   

static vector_t
get_fuel_station_pos (machine_state_t *state)
{
    vector_t pos = get_pos(state);

    double dx = state->output[4];
    double dy = state->output[5];

    vector_t v = { pos.x + dx, pos.y + dy };

    return v;
}

static vector_t
get_sat_n_pos (machine_state_t *state, int n)
{
    vector_t pos = get_pos(state);

    g_assert(n >= 0 && n < 11);

    double dx = state->output[3 * n + 7];
    double dy = state->output[3 * n + 8];

    vector_t v = { pos.x + dx, pos.y + dy };

    return v;
}

static gboolean
get_sat_n_collected (machine_state_t *state, int n)
{
    g_assert(n >= 0 && n < 11);

    double dx = state->output[3 * n + 9];

    return dx != 0.0;
}

static vector_t get_sat_0_pos (machine_state_t *state) { return get_sat_n_pos(state, 0); }
static vector_t get_sat_1_pos (machine_state_t *state) { return get_sat_n_pos(state, 1); }
static vector_t get_sat_2_pos (machine_state_t *state) { return get_sat_n_pos(state, 2); }
static vector_t get_sat_3_pos (machine_state_t *state) { return get_sat_n_pos(state, 3); }
static vector_t get_sat_4_pos (machine_state_t *state) { return get_sat_n_pos(state, 4); }
static vector_t get_sat_5_pos (machine_state_t *state) { return get_sat_n_pos(state, 5); }
static vector_t get_sat_6_pos (machine_state_t *state) { return get_sat_n_pos(state, 6); }
static vector_t get_sat_7_pos (machine_state_t *state) { return get_sat_n_pos(state, 7); }
static vector_t get_sat_8_pos (machine_state_t *state) { return get_sat_n_pos(state, 8); }
static vector_t get_sat_9_pos (machine_state_t *state) { return get_sat_n_pos(state, 9); }
static vector_t get_sat_10_pos (machine_state_t *state) { return get_sat_n_pos(state, 10); }

static get_pos_func_t get_sat_pos_funcs[] = { get_sat_0_pos, get_sat_1_pos, get_sat_2_pos,
					      get_sat_3_pos, get_sat_4_pos, get_sat_5_pos,
					      get_sat_6_pos, get_sat_7_pos, get_sat_8_pos,
					      get_sat_9_pos, get_sat_10_pos };

static get_pos_func_t
get_get_sat_pos_func (int n)
{
    g_assert(n >= 0 && n < 11);
    return get_sat_pos_funcs[n];
}
#else
#error bla
#endif

static void
global_timestep (void)
{
    /*
    if (global_state.output[0] != 0.0) {
	write_count(0, global_trace);
	if (global_trace != NULL)
	    fclose(global_trace);
	g_print("score is %f\n", global_state.output[0]);
	exit(0);
    }
    */
    if (global_trace != NULL) 
    	write_timestep(global_trace, &global_old_inputs, &global_state.inputs);
    global_old_inputs = global_state.inputs;
    timestep(&global_state);
    print_timestep(&global_state);
}

static void
do_timestep (machine_state_t *state)
{
    if (state == &global_state) {
	global_timestep();
#if defined(BIN2) || defined(BIN3)
	//g_print("%d %f\n", state->num_timesteps_executed, v_abs(v_sub(get_pos(state), get_meet_greet_sat_pos(state))));
#endif
    } else
	timestep(state);
    //g_print("ts %d score %f\n", state->num_timesteps_executed, state->output[0]);
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
v_make (double x, double y)
{
    vector_t v = { x, y };
    return v;
}

static vector_t
v_add (vector_t a, vector_t b)
{
    vector_t c = { a.x + b.x, a.y + b.y };
    return c;
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
    return angle;
}

static double
a_norm (double angle)
{
    while (angle > G_PI)
	angle -= 2*G_PI;
    while (angle <= -G_PI)
	angle += 2*G_PI;
    return angle;
}

static double
a_delta (double a1, double a2)
{
    return a_norm(a2-a1);
}

static double
a_sign (double angle)
{
    return (angle >= 0) ? 1.0 : -1.0;
}

static void
print_vec (vector_t v)
{
    g_print("(%f,%f)", v.x, v.y);
}

static vector_t
get_speed_generic (machine_state_t *state, get_pos_func_t get_pos_func)
{
    machine_state_t copy = *state;
    vector_t old = get_pos_func(state);
    vector_t new;

    timestep(&copy);

    new = get_pos_func(&copy);

    return v_sub(new, old);
}

static vector_t
get_speed (machine_state_t *state)
{
    return get_speed_generic(state, get_pos);
}

static vector_t
v_rotate (vector_t v, double angle)
{
    vector_t new;

    new.x = v.x*cos(angle)-v.y*sin(angle);
    new.y = v.x*sin(angle)+v.y*cos(angle);
    return new;
}


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

static double
m_focal_dist(double apoapsis, double periapsis)
{
    return apoapsis - periapsis;
}

static double
m_major(double apoapsis, double periapsis)
{
    return apoapsis + periapsis;
}

static double
m_semi_major(double apoapsis, double periapsis)
{
    return m_major(apoapsis, periapsis)/2.0;
}

static double
m_eccentricity(double apoapsis, double periapsis)
{
    return m_focal_dist(apoapsis, periapsis) / m_major(apoapsis, periapsis);
}


/* solve T3+T4 numerically */

static double
m_solve_t3t4(double a, double b, double T, double precision)
{
    double interval[2] = { 0, 1e+12 };
    double val, h = -1;

    do {
	h = (interval[0] + interval[1])/2.0;
	val = m_period((a+h)/2.0)/2.0 + m_period((b+h)/2.0)/2.0;

	if (val < T) {
	    interval[0] = h;
	} else {
	    interval[1] = h;
	}
    } while (fabs(T - val) > precision);
    return h;
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
	double a1 = v_angle(old);
	double a2 = v_angle(new);
	double da = a_delta(a1, a2);

	vector_t veld = v_sub(new, old);
	vector_t velc;

        if (0) {
	    velc = v_rotate(veld, da);
	} else {
	    velc = v_rotate(old, (da >= 0) ? G_PI/2.0 : -G_PI/2.0);
	}

	if (0) { 
		vector_t vel = get_speed (state);

		g_print("[%f,%f] -> %f  (%f,%f) : (%f,%f) -> (%f,%f)\n",
			a1, a2, da, vel.x, vel.y, veld.x, veld.y, velc.x, velc.y);
	}
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

static gboolean
between_angles (double angle, double a1, double a2, double max_diff)
{
    g_assert(angle >= -G_PI && angle <= G_PI);
    g_assert(a1 >= -G_PI && a1 <= G_PI);
    g_assert(a2 >= -G_PI && a2 <= G_PI);

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
timestep_until_angle (machine_state_t *state, double dest_angle, double max_dist, gboolean *have_angle)
{
    double old_angle = get_angle(state);
    int i = 0;

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

	if (state->num_timesteps_executed >= MAX_TIMESTEPS) {
	    if (have_angle != NULL)
		*have_angle = FALSE;
	    return i;
	}

	old_angle = new_angle;
    }

    g_assert_not_reached();
}

static int
timestep_until_angle_delta (machine_state_t *state, double angle_delta, double max_dist, gboolean *have_angle)
{
    double dest_angle = a_norm(get_angle(state) + angle_delta);
    int num_steps = 0;

    g_assert(angle_delta >= 0.0);

    if (angle_delta > G_PI) {
	num_steps += timestep_until_angle(state, a_norm(get_angle(state) + G_PI), max_dist, have_angle);
	if (have_angle != NULL && !*have_angle)
	    return num_steps;
    }

    return num_steps + timestep_until_angle(state, dest_angle, max_dist, have_angle);
}

static double
calc_apogee_or_perigee (machine_state_t *state, vector_t thrust, double max_dist, int *num_iters)
{
    machine_state_t copy = *state;
    gboolean have_angle;
    int i;

    set_thrust(&copy, thrust);

    i = timestep_until_angle_delta(&copy, G_PI, max_dist, &have_angle);
    if (num_iters != NULL)
	*num_iters = i;
    if (!have_angle)
	return max_dist;
    return distance_from_earth(&copy);
}

static void
calc_ellipse (machine_state_t *state, vector_t *apogee, vector_t *perigee, int *t_to_apogee, int *t_to_perigee,
	      get_pos_func_t get_pos_func)
{
    machine_state_t copy = *state;
    double dist = v_abs(get_pos_func(&copy));
    gboolean ascending;
    gboolean have_apogee = FALSE, have_perigee = FALSE;
    vector_t pos;
    int iter = 0;

    timestep(&copy);
    ++iter;
    set_thrust(&copy, v_zero);

    ascending = v_abs(get_pos_func(&copy)) > dist;
    pos = get_pos_func(&copy);
    dist = v_abs(pos);

    while (!have_apogee || !have_perigee) {
	gboolean new_ascending;

	timestep(&copy);
	new_ascending = v_abs(get_pos_func(&copy)) > dist;

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
	pos = get_pos_func(&copy);
	dist = v_abs(pos);

	++iter;
    }
}


static void
calc_ellipse_bertl(machine_state_t *state, vector_t *apogee, vector_t *perigee,
		int *t_to_apogee, int *t_to_perigee,
		get_pos_func_t get_pos_func)
{
    machine_state_t copy = *state;

    vector_t pos = get_pos_func(&copy);
    double dist = v_abs(pos);

    double start_angle = v_angle(pos);
    double sign_delta = a_sign(0);
    double sign_last = sign_delta;
    int sign_flip = 0;
    int iter = 0;

    double min_dist = dist;
    double max_dist = dist;
    vector_t min_pos = pos;
    vector_t max_pos = pos;
    int min_iter = iter;
    int max_iter = iter;

    do {
	double angle;

	timestep(&copy);
	iter++;

	pos = get_pos_func(&copy);
	dist = v_abs(pos);
	angle = v_angle(pos);


	if (dist < min_dist) {
	    min_dist = dist;
	    min_pos = pos;
	    min_iter = iter;
	}

	if (dist > max_dist) {
	    max_dist = dist;
	    max_pos = pos;
	    max_iter = iter;
	}

	sign_delta = a_sign(angle - start_angle);
	if (sign_last != sign_delta) {
	    sign_last = sign_delta;
	    sign_flip++;
	}
    } while (sign_flip < 3);

    if (apogee)
	*apogee = max_pos;
    if (perigee)
	*perigee = min_pos;
    if (t_to_apogee)
	*t_to_apogee = max_iter;
    if (t_to_perigee)
	*t_to_perigee = min_iter;
}


static int
inject_elliptical_to_elliptical (machine_state_t *state, double dest_apogee, double tolerance)
{
    double min, max;
    double start_dist = distance_from_earth(state);
    double max_dist = MAX(start_dist, dest_apogee) * 1.1;
    vector_t speed_norm;

    min = -v_abs(get_speed(state));
    max = state->output[1];

    speed_norm = get_norm_speed(state);

    g_print("norm speed is ");
    print_vec(speed_norm);
    g_print(" russen-speed is ");
    print_vec(get_speed(state));
    g_print(" - trying to get from %f to %f\n", start_dist, dest_apogee);

    while (min < max) {
	double middle = (min + max) / 2;
	vector_t thrust = v_mul_scal(speed_norm, middle);
	int num_iters;

	g_print("trying thrust %f ", middle);
	print_vec(thrust);
	g_print("\n");

	double apogee;

	apogee = calc_apogee_or_perigee(state, thrust, max_dist, &num_iters);

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
    if (sign < 0) {
	speed_norm = v_mul_scal(speed_norm, -1.0);
	max = v_abs(get_speed(state));
    }

    g_print("norm speed (%f) is ", sign);
    print_vec(speed_norm);
    g_print(" russen speed is ");
    print_vec(v_mul_scal(get_speed(state), sign));
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

static double
calc_distance_after_angle (machine_state_t *state, vector_t thrust, double angle, int *num_iters)
{
    machine_state_t copy = *state;
    double max_dist = 2.0 * distance_from_earth(&copy);
    int i;

    set_thrust(&copy, thrust);

    i = timestep_until_angle_delta(&copy, angle, max_dist, NULL);
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

    g_print("norm speed is ");
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

static gboolean
is_winning_state (machine_state_t *state, get_pos_func_t get_pos_func)
{
    machine_state_t copy = *state;
    int i;

    if (v_abs(v_sub(get_pos(&copy), get_pos_func(&copy))) <= TARGET_RADIUS)
	return TRUE;
    return FALSE;

    /*
    for (i = 0; i < WINNING_TIMESTEPS; ++i) {
	do_timestep(&copy);
	if (v_abs(v_sub(get_pos(&copy), get_pos_func(&copy))) > TARGET_RADIUS)
	    return FALSE;
    }
    return TRUE;
    */
}

static gboolean
is_meet_greet_terminated (machine_state_t *state, get_pos_func_t get_pos_func, gpointer user_data)
{
    vector_t our_pos = get_pos(state);
    vector_t sat_pos = get_pos_func(state);

    if (state->num_timesteps_executed >= MAX_TIMESTEPS)
	return TRUE;
    if (state->output[0] != 0.0)
	return TRUE;
    return is_winning_state(state, get_pos_func);
}

#ifdef BIN4
static gboolean
is_sat_hit_terminated (machine_state_t *state, get_pos_func_t get_pos_func, gpointer user_data)
{
    int sat_num = *(int*)user_data;

    if (state->num_timesteps_executed >= MAX_TIMESTEPS)
	return TRUE;
    if (state->output[0] != 0.0)
	return TRUE;

    return get_sat_n_collected(state, sat_num);
}
#endif

static int
do_follower (machine_state_t *state, get_pos_func_t get_pos_func,
	     fuel_divisor_func_t fuel_divisor_func, gpointer fuel_divisor_data,
	     skip_size_func_t skip_size_func, gpointer skip_size_data,
	     termination_condition_func_t termination_condition_func, gpointer termination_condition_data,
	     gboolean print)
{
    while (!termination_condition_func(state, get_pos_func, termination_condition_data)) {
	vector_t our_pos = get_pos(state);
	vector_t sat_pos = get_pos_func(state);
	vector_t our_speed = get_speed(state);
	vector_t sat_speed = get_speed_generic(state, get_pos_func);
	vector_t pos_diff = v_sub(sat_pos, our_pos);

	if (v_abs(pos_diff) > 1.0) {
	    vector_t speed_diff = v_sub(sat_speed, our_speed);
	    int skip_size = skip_size_func(state, skip_size_data);
	    double fuel_divisor = fuel_divisor_func(state, fuel_divisor_data);
	    vector_t scaled_v = v_add(speed_diff, v_mul_scal(v_norm(v_sub(sat_pos, our_pos)),
							     MIN(v_abs(pos_diff),
								 state->output[1]) / fuel_divisor));

	    if (print) {
		g_print("distance %f   fuel %f   our speed (%f) ",
			v_abs(pos_diff), state->output[1], v_abs(our_speed));
		print_vec(our_speed);
		g_print("   sat speed (%f) ", v_abs(sat_speed));
		print_vec(sat_speed);
		g_print("   speed (%f) ", v_abs(scaled_v));
		print_vec(scaled_v);
		g_print("\n");
	    }

	    set_thrust(state, scaled_v);

	    do_n_timesteps(state, skip_size);
	} else {
	    do_n_timesteps(state, 1);
	}
    }

    return state->num_timesteps_executed;
}

static void
do_final_thrust (machine_state_t *state, get_pos_func_t get_pos_func)
{
    vector_t our_pos = get_pos(state);
    vector_t sat_pos = get_pos_func(state);
    vector_t our_speed = get_speed(state);
    vector_t sat_speed = get_speed_generic(state, get_pos_func);
    int i;
    double max_dist = v_abs(v_sub(our_pos, sat_pos));

    g_print("distance before final thrust: %f\n", max_dist);

    set_thrust(state, v_sub(sat_speed, our_speed));
    for (i = 0; i < WINNING_TIMESTEPS; ++i) {
	do_n_timesteps(state, 1);
	our_speed = get_speed(state);
	sat_speed = get_speed_generic(state, get_pos_func);
	double dist = v_abs(v_sub(our_pos, sat_pos));
	if (dist > max_dist)
	    max_dist = dist;
    }

    g_print("max distance after final thrust: %f\n", max_dist);
}

static double
do_follower_and_finish (machine_state_t *state, get_pos_func_t get_pos_func,
			fuel_divisor_func_t fuel_divisor_func, gpointer fuel_divisor_data,
			skip_size_func_t skip_size_func, gpointer skip_size_data,
			termination_condition_func_t termination_condition_func, gpointer termination_condition_data,
			gboolean print)
{
    do_follower(state, get_pos_func,
		fuel_divisor_func, fuel_divisor_data,
		skip_size_func, skip_size_data,
		termination_condition_func, termination_condition_data,
		print);

    do_final_thrust(state, get_pos_func);

    while (state->num_timesteps_executed < MAX_TIMESTEPS && state->output[0] == 0.0)
	do_n_timesteps(state, 1);

    return state->output[0];
}


static double
constant_fuel_divisor_func (machine_state_t *state, gpointer user_data)
{
    return *(double*)user_data;
}

static int
constant_skip_size_func (machine_state_t *state, gpointer user_data)
{
    return *(int*)user_data;
}

#if defined(BIN2) || defined(BIN3)
static double
bertl_search_score_func (void *user_data, int *coords)
{
    machine_state_t *state = user_data;
    machine_state_t copy = *state;
    double divisor = coords[0];
    int step_size = coords[1];
    double score;

    score = do_follower_and_finish(&copy, get_meet_greet_sat_pos,
				   constant_fuel_divisor_func, &divisor,
				   constant_skip_size_func, &step_size,
				   is_meet_greet_terminated, NULL,
				   FALSE);

    return score;
}

static void
do_bertl_search (machine_state_t *state)
{
    int area[2][2] = { { 10, 1 }, { 100, 100 } };

    search_area_int(bertl_search_score_func, state, area, 5);
}
#endif

static void
do_schani_search (machine_state_t *state, get_pos_func_t get_pos_func,
		  termination_condition_func_t termination_condition_func, gpointer termination_condition_data)
{
    static double divisors[] = { 10.0, 20.0, 30.0, 50.0, 75.0, 100.0, 200.0, 500.0,
				 /*1000.0, 2000.0, 5000.0, 10000.0,*/ 0.0 };
    static int skips[] = { 1, 2, 3, 5, 7, 10, 15, 20, 30, 50, 75, 100, 200, 300, 500,
			   /*1000, 2000, 3000, 5000, 10000,*/ 0 };

    double best_score = -1;
    double best_divisor = 0.0;
    int best_skip = 1;
    int i, j;

    /*
    for (i = 0; skips[i] > 0; ++i)
	g_print(";%d", skips[i]);
    g_print("\n");
    */

    for (i = 0; divisors[i] > 0; ++i) {
	double divisor = divisors[i];

	//g_print("%f", divisor);

	for (j = 0; skips[j] != 0; ++j) {
	    int skip = skips[j];
	    double score;
	    machine_state_t copy = global_state;

	    g_print("trying follower with divisor %f skip %d\n", divisor, skip);
	    score = do_follower_and_finish(&copy, get_pos_func,
					   constant_fuel_divisor_func, &divisor,
					   constant_skip_size_func, &skip,
					   termination_condition_func, termination_condition_data,
					   FALSE);
	    //g_print(";%f", score);
	    g_print("score is %f\n", score);

	    if (score > best_score) {
		best_score = score;
		best_divisor = divisor;
		best_skip = skip;
	    }
	}

	g_print("\n");
    }

    if (best_score > 0) {
	do_follower_and_finish(&global_state, get_pos_func,
			       constant_fuel_divisor_func, &best_divisor,
			       constant_skip_size_func, &best_skip,
			       termination_condition_func, termination_condition_data,
			       TRUE);
	g_print("best score %f with divisor %f and skip %d\n", best_score, best_divisor, best_skip);
    }
}

static void
ellipse_to_ellipse_transfer (machine_state_t *state, get_pos_func_t get_pos_func)
{
    gboolean have_angle;
    vector_t our_apogee, our_perigee;
    vector_t sat_apogee, sat_perigee;
    int t_to_our_apogee, t_to_our_perigee;
    int t_to_sat_apogee, t_to_sat_perigee;
    double our_period, sat_period;
    double t2, u1, t5;
    double t34_min, h, h_min;
    double angle_between_apsises, sat_perigee_period;
    double delta;
    int i;
    int num_iters;
    int num_B_revolution_steps = 0;

 restart:

    calc_ellipse_bertl(state, &our_apogee, &our_perigee, &t_to_our_apogee, &t_to_our_perigee, get_pos);

    g_print("s us: t to apogee: %d  perigee: %d\n", t_to_our_apogee, t_to_our_perigee);
    g_print("apogee (%f) ", v_angle(our_apogee) / G_PI * 180.0);
    print_vec(our_apogee);
    g_print("   perigee (%f) ", v_angle(our_perigee) / G_PI * 180.0);
    print_vec(our_perigee);
    g_print("\n\n");

    our_period = m_period(v_abs(v_sub(our_apogee, our_perigee))/2);
    t2 = our_period / 2;
    g_print("(math) period : %f\n", our_period);
    g_print("(math) eccentricity : %f\n", m_eccentricity(v_abs(our_apogee), v_abs(our_perigee)));

    calc_ellipse_bertl(state, &sat_apogee, &sat_perigee, &t_to_sat_apogee, &t_to_sat_perigee, get_pos_func);

    g_print("s sat: t to apogee: %d  perigee: %d\n", t_to_sat_apogee, t_to_sat_perigee);
    g_print("apogee (%f) ", v_angle(sat_apogee) / G_PI * 180.0);
    print_vec(sat_apogee);
    g_print("   perigee (%f) ", v_angle(sat_perigee) / G_PI * 180.0);
    print_vec(sat_perigee);
    g_print("\n\n");

    set_debug_point(sat_perigee);

    delta = t_to_sat_apogee - t_to_our_apogee;
    sat_period = m_period(v_abs(v_sub(sat_apogee, sat_perigee))/2);
    u1 = sat_period / 2;
    g_print("(math) period : %f\n", sat_period);
    g_print("(math) eccentricity : %f\n", m_eccentricity(v_abs(sat_apogee), v_abs(sat_perigee)));

    angle_between_apsises = a_delta(v_angle(sat_apogee), v_angle(our_apogee));
    if (angle_between_apsises < 0)
	angle_between_apsises += 2.0 * G_PI;
    sat_perigee_period = m_period(v_abs(sat_perigee));
    t5 = sat_perigee_period * (angle_between_apsises / (G_PI * 2.0));

    t34_min = (m_period(v_abs(our_perigee)) / 2
	       + m_period((v_abs(our_perigee) + v_abs(sat_perigee)) / 2)) / 2;
    h_min = v_abs(our_perigee);

    set_dump_orbit(v_abs(sat_perigee));

    do_n_timesteps(state, t_to_our_perigee);

    /* we're at point B here, injecting for C. C point must be
       calculated so that we'll meet satellite at E */

    g_print("apsis angle %f degrees - delta %f  u1 %f  t2 %f  t5 %f\n",
	    angle_between_apsises * 180.0 / G_PI, delta, u1, t2, t5);

    i = 0;
    for (;;) {
	double a = v_abs(our_perigee);
	double b = v_abs(sat_perigee);
	double T = delta + u1 + 2 * i * u1 - t2 - t5;

	if (T >= t34_min) {
	    g_print("trying bertl solve with a = %f  b = %f  T = %f  (n=%d)\n", a, b, T, i);

	    /*
	    if (T - t34_min >= our_period) {
		int num_revolutions = (int)floor((T - t34_min) / our_period);
		num_b_revolution_steps = (int)floor(our_period) * num_revolutions;
		g_print("T is more than our period - doing %d revolutions\n", num_revolutions);
		do_n_timesteps(state, num_b_revolution_steps);
		T -= num_b_revolution_steps;
		g_assert(T >= t34_min);
	    }
	    */

	    h = m_solve_t3t4(a, b, T, 0.5);
	    g_assert(h >= h_min);
	    break;

	    /*
	    //h = simulation_calc_h(state, delta + u1 + 2 * i * u1 - t2 - t5);
	    if (h >= h_min)
		break;
	    */
	}

	++i;
    }

    g_print("found target h %f with n = %d\n", h, i);

    g_print("tA  = %f\ntAS = %f\ntB  = %f\ntC  = %f\ntD  = %f\ntE  = %f\ntD2 = %f\n",
	    (double)t_to_our_apogee,
	    t_to_our_apogee + delta,
	    t_to_our_apogee + t2,
	    t_to_our_apogee + t2 + m_period((h + v_abs(our_perigee)) / 2) / 2,
	    t_to_our_apogee + t2 + m_period((h + v_abs(our_perigee)) / 2) / 2 + m_period((h + v_abs(sat_perigee)) / 2) / 2,
	    t_to_our_apogee + delta + u1 + 2 * i * u1,
	    t_to_our_apogee + delta + u1 + 2 * i * u1 - t5);

    num_iters = inject_elliptical_to_elliptical(state, h, 1.0);
    do_n_timesteps(state, num_iters);

    /*
    set_thrust(state, v_mul_scal(get_norm_speed(state), v_abs(get_speed(state)) / 20.0));
    timestep_until_angle_delta(state, G_PI, MAX(v_abs(our_apogee), v_abs(sat_apogee)) * 10.0, &have_angle);
    if (!have_angle) {
	g_print("don't have angle\n");
	return;
    }
    */

    double c_dist = distance_from_earth(state);

    g_print("at C: %f\n", c_dist);

    num_iters = inject_elliptical_to_elliptical(state, v_abs(sat_perigee), 1.0);
    do_n_timesteps(state, num_iters);

    /* at D */
    inject_elliptical_to_circular(state, c_dist < v_abs(sat_perigee) ? 1.0 : -1.0,
				  v_abs(sat_perigee), WINNING_TIMESTEPS, 1.0);
    num_iters = timestep_until_angle_delta(state, angle_between_apsises, v_abs(sat_perigee) * 1.1, &have_angle);
    g_print("took %d timesteps for angle %f\n", num_iters, angle_between_apsises * 180.0 / G_PI);
    g_assert(have_angle);

    /* at E */
    inject_elliptical_to_elliptical(state, v_abs(sat_apogee), 1.0);

    clear_dump_orbit();

    g_print("at end of bertl maneuver: %f off\n", v_abs(v_sub(get_pos(state), get_pos_func(state))));
}

static void
run_trace_file (FILE *file, machine_state_t *state)
{
    guint32 magic, team, scenario;
    guint32 timestep, count, addr;
    double val;
    int i;

    fread(&magic, 4, 1, file);
    g_assert(magic == 0xcafebabe);

    fread(&team, 4, 1, file);
    g_print("team %d\n", team);

    fread(&scenario, 4, 1, file);
    g_print("scenario %d\n", scenario);

    for (;;) {
	fread(&timestep, 4, 1, file);
	fread(&count, 4, 1, file);

	g_assert(timestep >= state->num_timesteps_executed);
	do_n_timesteps(state, timestep - state->num_timesteps_executed);
	g_assert(timestep == state->num_timesteps_executed);

	if (count == 0) {
	    g_print("terminated on timestep %d\n", timestep);
	    break;
	}

	for (i = 0; i < count; ++i) {
	    fread(&addr, 4, 1, file);
	    fread(&val, 8, 1, file);

	    set_input(state, addr, val);

	    g_print("in timestep %d addr %d val %f\n", timestep, addr, val);
	}
    }

    g_print("score %f\n", state->output[0]);
}

int
main (int argc, char *argv[])
{
    FILE *trace_input_file = NULL;
    int num_iters, i;
    double dest_apogee;
    gboolean have_angle = FALSE;
    char *global_trace_name = NULL;
    int opt;
    int scenario = -1;

    //fesetround(FE_TOWARDZERO);

    while ((opt = getopt(argc, argv, "d:i:t:s:")) != -1) {
	switch (opt) {
	    case 'd' :
		dump_file = fopen(optarg, "w");
		g_assert(dump_file != NULL);
		break;

	    case 'i' :
		trace_input_file = fopen(optarg, "r");
		g_assert(trace_input_file != NULL);
		break;

	    case 't' :
		global_trace_name = g_strdup(optarg);
		break;

	    case 's' :
		scenario = atoi(optarg);
		break;

            case '?' :
                printf("USAGE: %s -d <dumpfile> -s <scenario id>  [-i <inputtrace> | -t <outputtrace>]\n", argv[0]);
                break;

	    default :
		g_assert_not_reached();
	}
    }


    if (trace_input_file == NULL && scenario < 0) {
	g_print("need scenario\n");
        printf("USAGE: %s -d <dumpfile> -s <scenario id>  [-i <inputtrace> | -t <outputtrace>]\n", argv[0]);
	return 1;
    }

    if (trace_input_file != NULL && global_trace_name != NULL) {
	g_print("cannot have both input and output traces\n");
        printf("USAGE: %s -d <dumpfile> -s <scenario id>  [-i <inputtrace> | -t <outputtrace>]\n", argv[0]);
	return 1;
    }

    if (global_trace_name != NULL)
	global_trace = open_trace_file(global_trace_name, 19, scenario);

    init_machine(&global_state);
    global_old_inputs = global_state.inputs;

    if (trace_input_file != NULL) {
	run_trace_file(trace_input_file, &global_state);
	return 0;
    }

    global_state.inputs.input_16000 = scenario;

    global_timestep();

#if defined(BIN1)
    dest_apogee = global_state.output[4];

    num_iters = inject_elliptical_to_elliptical(&global_state, dest_apogee, 1.0);

#if 1
    do_n_timesteps(&global_state, num_iters);
    inject_elliptical_to_circular(&global_state, 1.0, dest_apogee, WINNING_TIMESTEPS, 1.0);
#else
    // thrust more
    set_thrust(&global_state, v_mul_scal(get_thrust(&global_state), 1.25));
    timestep_until_angle_delta(&global_state, G_PI, dest_apogee * 100.0, &have_angle);
    if (!have_angle) {
	g_print("don't have angle\n");
	return 1;
    }

    num_iters = inject_bielliptical_to_circular(&global_state, dest_apogee, 1.0);
    for (i = 0; i < num_iters; ++i) {
	global_timestep();
	set_thrust(&global_state, v_zero);
    }

    inject_elliptical_to_circular(&global_state, -1.0, dest_apogee, WINNING_TIMESTEPS, 1.0);

    /*
    for (i = 0; i < WINNING_TIMESTEPS; ++i) {
	global_timestep();
	set_thrust(&global_state, v_zero);
    }
    */
#endif
    //#elif defined(BIN2)
    //    g_assert_not_reached();
#elif defined(BIN2) || defined(BIN3)

    ellipse_to_ellipse_transfer(&global_state, get_meet_greet_sat_pos);
    g_print("done\n");

#if 1
    do_schani_search(&global_state, get_meet_greet_sat_pos, is_meet_greet_terminated, NULL);
#else
    do_bertl_search(&global_state);
#endif

#elif defined(BIN4)

#if 0
    for (;;) {
	int best_sat = -1;
	int best_sat_steps = MAX_TIMESTEPS;
	int steps;
	double divisor = 10.0;
	int step_size = 7;

	for (i = 0; i < 11; ++i) {
	    if (!get_sat_n_collected(&global_state, i)) {
		machine_state_t copy = global_state;

		g_print("trying to hit sat %d\n", i);

		steps = do_follower(&copy, get_get_sat_pos_func(i),
				    constant_fuel_divisor_func, &divisor,
				    constant_skip_size_func, &step_size,
				    is_sat_hit_terminated, &i,
				    FALSE);

		if (get_sat_n_collected(&copy, i)) {
		    g_print("hit in step %d\n", steps);

		    if (steps < best_sat_steps) {
			best_sat = i;
			best_sat_steps = steps;
		    }
		} else {
		    g_print("terminated in step %d\n", steps);
		}
	    }
	}

	if (best_sat == -1) {
	    gboolean need_sats = FALSE;
	    g_print("done - sats not hit:");
	    for (i = 0; i < 11; ++i) {
		if (!get_sat_n_collected(&global_state, i)) {
		    g_print(" %d", i);
		    need_sats = TRUE;
		}
	    }
	    g_print("\n");
	    if (need_sats) {
		g_print("still sats to hit - will wait for 10 steps\n");
		do_n_timesteps(&global_state, 10);
	    } else {
		break;
	    }
	} else {
	    g_print("hitting sat %d at step %d\n", best_sat, best_sat_steps);

	    steps = do_follower(&global_state, get_get_sat_pos_func(best_sat),
				constant_fuel_divisor_func, &divisor,
				constant_skip_size_func, &step_size,
				is_sat_hit_terminated, &best_sat,
				FALSE);
	    g_assert(steps == best_sat_steps);
	    g_assert(get_sat_n_collected(&global_state, best_sat));
	}
    }
#else
    double divisor = 10.0;
    int step_size = 7;
    int sat = 0;

    set_debug_point(get_sat_n_pos(&global_state, sat));
    do_follower(&global_state, get_get_sat_pos_func(sat),
		constant_fuel_divisor_func, &divisor,
		constant_skip_size_func, &step_size,
		is_sat_hit_terminated, &sat,
		TRUE);

    do_n_timesteps(&global_state, 10);
#endif


#else
#error bla
#endif

    while (global_state.num_timesteps_executed < 3000000 && global_state.output[0] == 0.0) {
	do_timestep(&global_state);
	set_thrust(&global_state, v_zero);
    }

    g_print("score is %f\n", global_state.output[0]);

    if (global_trace != NULL) {
	//do_n_timesteps(&global_state, 100);
	write_count(&global_state, 0, global_trace);
    }

    if (global_trace != NULL)
	fclose(global_trace);
    return 0;
}
