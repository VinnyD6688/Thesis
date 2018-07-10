import numpy as np
from scipy.interpolate import Rbf, InterpolatedUnivariateSpline
import matplotlib.pyplot as plt
from lmfit import Minimizer, Parameters, report_fit
from scipy.optimize import minimize as minimize
from scipy.optimize import differential_evolution, least_squares

newe = []
ksu = np.genfromtxt("ksu27near.txt")
sim = np.genfromtxt("Salt27broad.txt")
channel = ksu[:,0]
counts = ksu[:,1]
energy = sim[:,0]*100000
simcount = sim[:,1]*1000000
"""
ius = InterpolatedUnivariateSpline(x,y)
yi = ius(xi)

rbf = Rbf(x,y)
fi = rbf(xi)

yinterp = np.interp(xi, x, y)

shorty = yi[40:]
shortx = xi[40:]

This section works on the linear model
"""
def residual(pars, channel, simcount, counts):
    parvals = pars.valuesdict() 
    m1 = parvals['slope']
    m2 = parvals['poly']
    b = parvals['intercept']    
    a = parvals['amp']
    newe = m1*channel + m2*channel*channel+ b
    newe[0] = 0.0001
    model = np.interp(energy, newe, counts)
    return model*a-simcount

params = Parameters()
params.add('slope', value=0)
params.add('intercept', value=0)
params.add('poly', value=0)
params.add('amp', value=100, min=0)


minner = Minimizer(residual, params, fcn_args=(channel, simcount, counts), iter_cb=1000, nan_policy='propagate')
result = minner.minimize()
final = simcount + result.residual
"""
report_fit(result)


plt.semilogy(energy, simcount, 'r')
plt.semilogy(channel, counts, 'b')
plt.show()
"""
print(len(simcount))