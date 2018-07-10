import numpy as np
from scipy.interpolate import Rbf, InterpolatedUnivariateSpline
import matplotlib.pyplot as plt
from lmfit import Minimizer, Parameters, report_fit
from scipy.optimize import minimize as minimize

x = np.genfromtxt("originalenergy.txt")
y = np.genfromtxt("y.txt")
xi = np.genfromtxt("rebinenergy.txt")
vars = np.genfromtxt("h2onacl.txt")
ius = InterpolatedUnivariateSpline(x,y)
yi = ius(xi)

rbf = Rbf(x,y)
fi = rbf(xi)

yinterp = np.interp(xi, x, y)
shorty = yi[40:]
shortx = xi[40:]
"""
This section works on the linear model
"""
def residual(params, shortx, shorty):
    x1 = params['H20']
    x2 = params['Na']
    x3 = params['Cl']
    model = x1*vars[40:,0]+x2*vars[40:,1]+x3*vars[40:,2]
    return model-shorty

params = Parameters()
params.add('H20', value=50, min=0)
params.add('Na', value=50000, min=0)
params.add('Cl', value=50000, min= 0)

minner = Minimizer(residual, params, fcn_args=(shortx, shorty))
result = minner.minimize()
final = shorty + result.residual
report_fit(result)

plt.semilogy(xi, yi, 'k')
plt.semilogy(shortx, final, 'r')
plt.semilogy(x, y, 'b')
plt.show()