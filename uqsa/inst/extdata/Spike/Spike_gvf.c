#include <stdlib.h>
#include <math.h>
#include <gsl/gsl_errno.h>
#include <gsl/gsl_odeiv2.h>
#include <gsl/gsl_math.h>

/* The error code indicates how to pre-allocate memory
 * for output values such as `f_`. The _vf function returns
 * the number of state variables, if any of the args are `NULL`.
 * evaluation errors can be indicated by negative return values.
 * GSL_SUCCESS (0) is returned when no error occurred.
 */

/* ode vector field: y'=f(t,y;p) */
int Spike_vf(double t, const double y_[], double f_[], void *par)
{
	double *p_=par;
	if (!y_ || !f_) return 2;
	double k1=p_[0];
	double k2=p_[1];
	double A=p_[2];
	double CaBase=p_[3];
	double Ca=y_[0];
	double Buffer=y_[1];
	double rf1=Buffer;
	double rf2=-k1*k2*Ca-(k1+k2)*Buffer;
	f_[0] = +rf1;
	f_[1] = +rf2;
	return GSL_SUCCESS;
}
/* Scheduled Event function,
   EventLabel specifies which of the possible transformations to apply,
   dose can specify a scalar intensity for this transformation. */
int Spike_event(double t, double y_[], void *par, int EventLabel, double dose)
{
	double *p_=par;
	if (!y_ || !par || EventLabel<0) return 1;
	enum eventLabel { APCa, numEvents }; /* event name indexes */
	enum stateVariable { var_Ca,var_Buffer, numStateVar }; /* state variable indexes  */
	enum param { par_k1,par_k2,par_A,par_CaBase, numParam }; /* parameter indexes  */
	double k1=p_[0];
	double k2=p_[1];
	double A=p_[2];
	double CaBase=p_[3];
	double Ca=y_[0];
	double Buffer=y_[1];
	double rf1=Buffer;
	double rf2=-k1*k2*Ca-(k1+k2)*Buffer;
	switch(EventLabel){
	case APCa:
		y_[var_Buffer] = A*(k2-k1)*dose; /* state variable transformation */
	break;
	}
	return GSL_SUCCESS;
}

/* ode Jacobian df(t,y;p)/dy */
int Spike_jac(double t, const double y_[], double *jac_, double *dfdt_, void *par)
{
	double *p_=par;
	if (!y_ || !jac_) return 2*2;
	double k1=p_[0];
	double k2=p_[1];
	double A=p_[2];
	double CaBase=p_[3];
	double Ca=y_[0];
	double Buffer=y_[1];
	double rf1=Buffer;
	double rf2=-k1*k2*Ca-(k1+k2)*Buffer;
/* column 1 (df/dy_0) */
	jac_[0] = 0; /* [0, 0] */
	jac_[2] = -k1*k2; /* [1, 0] */
/* column 2 (df/dy_1) */
	jac_[1] = 1; /* [0, 1] */
	jac_[3] = (-k2)-k1; /* [1, 1] */
	return GSL_SUCCESS;
}
/* ode parameter Jacobian df(t,y;p)/dp */
int Spike_jacp(double t, const double y_[], double *jacp_, double *dfdt_, void *par)
{
	double *p_=par;
	if (!y_ || !jacp_) return 2*4;
	double k1=p_[0];
	double k2=p_[1];
	double A=p_[2];
	double CaBase=p_[3];
	double Ca=y_[0];
	double Buffer=y_[1];
	double rf1=Buffer;
	double rf2=-k1*k2*Ca-(k1+k2)*Buffer;
/* column 1 (df/dp_0) */
	jacp_[0] = 0; /* [0, 0] */
	jacp_[4] = (-Ca*k2)-Buffer; /* [1, 0] */
/* column 2 (df/dp_1) */
	jacp_[1] = 0; /* [0, 1] */
	jacp_[5] = (-Ca*k1)-Buffer; /* [1, 1] */
/* column 3 (df/dp_2) */
	jacp_[2] = 0; /* [0, 2] */
	jacp_[6] = 0; /* [1, 2] */
/* column 4 (df/dp_3) */
	jacp_[3] = 0; /* [0, 3] */
	jacp_[7] = 0; /* [1, 3] */
	return GSL_SUCCESS;
}
/* ode Functions F(t,y;p) */
int Spike_func(double t, const double y_[], double *func_, void *par)
{
	double *p_=par;
	if (!y_ || !func_) return 1;
	double k1=p_[0];
	double k2=p_[1];
	double A=p_[2];
	double CaBase=p_[3];
	double Ca=y_[0];
	double Buffer=y_[1];
	double rf1=Buffer;
	double rf2=-k1*k2*Ca-(k1+k2)*Buffer;
	func_[0] = Ca+CaBase; /* OCA */
	return GSL_SUCCESS;
}
/* Function Jacobian dF(t,y;p)/dy */
int Spike_funcJac(double t, const double y_[], double *funcJac_, void *par)
{
	double *p_=par;
	if (!y_ || !funcJac_) return 2;
	double k1=p_[0];
	double k2=p_[1];
	double A=p_[2];
	double CaBase=p_[3];
	double Ca=y_[0];
	double Buffer=y_[1];
	double rf1=Buffer;
	double rf2=-k1*k2*Ca-(k1+k2)*Buffer;
/* column 1 (dF/dy_0) */
	funcJac_[0] = 1; /* [0, 0] */
/* column 2 (dF/dy_1) */
	funcJac_[1] = 0; /* [0, 1] */
	return GSL_SUCCESS;
}
/* Function parameter Jacobian dF(t,y;p)/dp */
int Spike_funcJacp(double t, const double y_[], double *funcJacp_, void *par)
{
	double *p_=par;
	if (!y_ || !funcJacp_) return 4;
	double k1=p_[0];
	double k2=p_[1];
	double A=p_[2];
	double CaBase=p_[3];
	double Ca=y_[0];
	double Buffer=y_[1];
	double rf1=Buffer;
	double rf2=-k1*k2*Ca-(k1+k2)*Buffer;
/* column 1 (dF/dp_0) */
	funcJacp_[0] = 0; /* [0, 0] */
/* column 2 (dF/dp_1) */
	funcJacp_[1] = 0; /* [0, 1] */
	return GSL_SUCCESS;
}
/* ode default parameters */
int Spike_default(double t, void *par)
{
	double *p_=par;
	if (!p_) return 4;
	p_[0] = 0.00978422;
	p_[1] = 0.03448;
	p_[2] = 229.127;
	p_[3] = 100;
	return GSL_SUCCESS;
}
/* ode initial values */
int Spike_init(double t, double *y_, void *par)
{
	double *p_=par;
	if (!y_) return 2;
	double k1=p_[0];
	double k2=p_[1];
	double A=p_[2];
	double CaBase=p_[3];
	/* the initial value of y may depend on the parameters. */
	y_[0] = 0;
	y_[1] = 0;
	return GSL_SUCCESS;
}
