//This version works with chemostat data 
#include <TMB.hpp>
#include <cppad/runge_45.hpp>      // for CppAD::Runge45

template<class Type>
Type posfun(Type x, Type eps, Type &pen)
{
	pen += CppAD::CondExpLt(x,eps,Type(0.01)*pow(x-eps,2),Type(0));
	return CppAD::CondExpGe(x,eps,x,eps/(Type(2)-x/eps));
}
/**********************************************************************************/
template<class Type>
class Fun {
		private:
		Type R0_;
		Type m_;
		Type theta_;
		Type sigma_;
	public:
		// set f = x'(t)
		void Ode(const Type &t, const CppAD::vector<Type> &x,  CppAD::vector<Type> &f)
		{
			Type sqrtR=sqrt(R0_ + m_*t);//could throw warnings, then optimizer should avoid bad region.
			f[0] =  2*sqrtR*(sqrtR+theta_ - x[0]); //dE/dt
			f[1] = -2*sqrtR*x[1] + sigma_*sigma_*(sqrtR+theta_); //dV/dt
			return;
		}
		void setpars(const Type& R0, const Type& m, const Type& theta, const Type& sigma)
		{
			R0_=R0;
			m_=m;
			theta_=theta;
			sigma_=sigma;
		}	
};
/**********************************************************************************/
template<class Type>
Type objective_function<Type>::operator() ()
{
	DATA_VECTOR(times);
	DATA_VECTOR(obs);
	
	PARAMETER(log_R0);
	PARAMETER(log_a);
	PARAMETER(log_theta);
	PARAMETER(log_sigma);
	Type sigma=exp(log_sigma);
	Type theta=exp(log_theta);
	Type R0=exp(log_R0);
	Type a=exp(log_a)+Type(1e-4);
	int n1=times.size();
	int n2=2;//mean and variance
	matrix<Type> xdist(n1,n2); //Ex and Vx
	Type m=(a-Type(1))*R0/times(n1-1);
	Type pen; 

	xdist(0,0)=obs[0];
	xdist(0,1)=Type(0);
	
	Type nll=0;       
	Fun<Type> F;
	F.setpars(R0, m, theta, sigma);

    CppAD::vector<Type> xi(n2);
	xi[1]=Type(0); //Bottinger's code started variance at 0 for all lsoda calls
	Type ti;
	Type tf;
	for(int i=0; i<n1-1; i++)
	{
		xi[0] = Type(obs[i]);
		ti=times[i];
		tf=times[i+1];
		xdist.row(i+1) = vector<Type>(CppAD::Runge45(F, 1, ti, tf, xi));
		xdist(i+1,1)=posfun(xdist(i+1,1), Type(1e-3), pen);//to keep the variance positive
		nll-= dnorm(obs[i+1], xdist(i+1,0), sqrt(xdist(i+1,1)), true);
	}
	nll+=pen; //penalty if the variance is near eps
	return nll;
}	


