import pandas as pd
import numpy as np
from numpy import array
import matplotlib.pyplot as plt
from sklearn.model_selection import train_test_split
from sklearn.preprocessing import scale
from sklearn.linear_model import Lasso, LassoCV, ElasticNet, ElasticNetCV
from sklearn.metrics import mean_squared_error


libraries = pd.read_csv("libraries.csv")
target = pd.read_csv("target.csv")
originalenergy = pd.read_csv("Energy.csv")

X = libraries
y = target.y


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
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=.5)

lasso = Lasso(max_iter=10000, normalize=True)
coefs = []
alphas = 10**np.linspace(10,-2,100)*.5


for a in alphas:
    lasso.set_params(alpha=a)
    lasso.fit(scale(X_train), y_train)
    coefs.append(lasso.coef_)

label_list=['Na', 'Cl', 'Water', 'Fe', 'Cu'] 
ax = plt.gca()
lineObjects = ax.plot(alphas*2, coefs)
ax.set_xscale('log')
plt.ticklabel_format(axis='y', style='sci', scilimits=(0,0))
plt.locator_params(axis='y', nbins=10)
"""

"""
plt.xlabel('Alpha')
plt.ylabel('Coefficients')
plt.title('Optimal Alpha Parameters')
plt.legend(iter(lineObjects), label_list)
plt.show()

lassocv = LassoCV(alphas=None, cv=10, max_iter=100000, normalize=True)
lassocv.fit(X_train, y_train)
lasso.set_params(alpha=lassocv.alpha_)
lasso.fit(X_train, y_train)
mean_squared_error(y_test, lasso.predict(X_test))

print(pd.Series(lasso.coef_, index=X.columns))
print(mean_squared_error(y_test, lasso.predict(X_test)))
fit = lasso.coef_*X
comb = fit.Na + fit.Cl + fit.Water

plt.semilogy(originalenergy, y, color='0.65', label='Simulated Data', linewidth=2)
plt.semilogy(originalenergy, fit.Na, label='Na Library', color='b')
plt.semilogy(originalenergy, fit.Cl, label='Cl Library', color='g')
plt.semilogy(originalenergy, fit.Water, label='Water Library', color='r')
plt.semilogy(originalenergy, comb, 'k:', label='Fit', linewidth=3)
plt.legend()
plt.xlabel('Energy (MeV)')
plt.ylabel('Counts')
plt.title('LASSO Library Fit')
plt.show()
