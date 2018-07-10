import pandas as pd
import numpy as np
from numpy import array
import matplotlib.pyplot as plt
from sklearn.model_selection import train_test_split
from sklearn.preprocessing import scale
from sklearn.linear_model import Lasso, LassoCV, ElasticNet, ElasticNetCV
from sklearn.metrics import mean_squared_error
from scipy.interpolate import Rbf, InterpolatedUnivariateSpline

libraries = pd.read_csv("libraries.csv")
"""
target = pd.read_csv("target.csv")
"""
originalenergy = pd.read_csv("Energy.csv")

X = libraries[40:930]
"""
y = target.y
"""
xep = np.genfromtxt("originalenergy.txt")
yep = np.genfromtxt("y.txt")
xi = np.genfromtxt("rebinenergy.txt")
ius = InterpolatedUnivariateSpline(xep,yep)
yi = ius(xi)

rbf = Rbf(xep,yep)
fi = rbf(xi)

yep = np.interp(xi, xep, yep)
y = yep[40:930]
"""
ax = plt.gca()
ax.plot(y)

ax.set_xscale('log')

plt.axis('tight')

plt.xlabel('Alpha')
plt.ylabel('Coefficients')
plt.title('Optimal Alpha Parameters')
plt.show()

"""
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=.85)

lasso = Lasso(max_iter=10000, positive=True, normalize=True)
coefs = []
alphas = 10**np.linspace(10,-2,100)*.5


for a in alphas:
    lasso.set_params(alpha=a)
    lasso.fit(scale(X_train), y_train)
    coefs.append(lasso.coef_)
"""    
ax = plt.gca()
ax.plot(alphas*2, coefs)
ax.set_xscale('log')
"""

"""
plt.xlabel('Alpha')
plt.ylabel('Coefficients')
plt.title('Optimal Alpha Parameters')
plt.show()
"""
lassocv = LassoCV(alphas=None, cv=10, max_iter=100000, normalize=True)
lassocv.fit(X_train, y_train)
lasso.set_params(alpha=lassocv.alpha_)
lasso.fit(X_train, y_train)
mean_squared_error(y_test, lasso.predict(X_test))

print(pd.Series(lasso.coef_, index=X.columns))
print(mean_squared_error(y_test, lasso.predict(X_test)))
fit = lasso.coef_*X
comb = fit.Na + fit.Cl + fit.Water

plt.semilogy(originalenergy, y, 'k', label='Simulated Data')
plt.semilogy(originalenergy, fit.Na, label='Na Library')
plt.semilogy(originalenergy, fit.Cl, label='Cl Library')
plt.semilogy(originalenergy, fit.Water, label='Water Library')
plt.semilogy(originalenergy, comb, 'y-', label='Fit')
plt.legend()
plt.xlabel('Energy')
plt.ylabel('Counts')
plt.title('LASSO Library Fit')
plt.show()
