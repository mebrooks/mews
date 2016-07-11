#include <TMB.hpp>
template<class Type>
Type objective_function<Type>::operator() ()
{
	DATA_VECTOR(times);
	DATA_VECTOR(obs);
	
	PARAMETER(log_R0);
	PARAMETER(m);
	PARAMETER(log_theta);
	PARAMETER(log_sigma);
	Type theta=exp(log_theta);
	Type sigma=exp(log_sigma);
	Type R0=exp(log_R0);
	
	int n1=times.size();
	int n2=2;//mean and variance
	vector<Type> Dt(n1-1);
	vector<Type> Ex(n1-1);
	vector<Type> Vx(n1-1);
	Type nll=0;
	m=0;
	Dt=diff(times);
	Ex=theta*(Type(1)-exp(-R0*Dt)) + obs.segment(0, n1-1)*exp(-R0*Dt);
	Vx=Type(0.5)*sigma*sigma*(Type(1)-exp(Type(-2)*R0*Dt))/R0;
	
	for(int i=0; i<n1-1; i++)
	{
		nll-= dnorm(obs[i+1], Ex[i], sqrt(Vx[i]), true);
	}
	return nll;
}
