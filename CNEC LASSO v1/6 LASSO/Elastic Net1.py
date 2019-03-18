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


X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=.9)
coefs = []
alphas = 10**np.linspace(10,-2,100)*.5

en = ElasticNetCV(l1_ratio=1, alphas=None, cv=10, max_iter=100000, normalize=True)
en.fit(X_train, y_train)
ElasticNet = ElasticNet(max_iter=10000, normalize=True, positive=False)

for a in alphas:
    ElasticNet.set_params(alpha=a)
    ElasticNet.fit(scale(X_train), y_train)
    coefs.append(ElasticNet.coef_)
 

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

print("Best for alphas:")
print(en.alpha_)
print("Best l1-ratio:")
print(en.l1_ratio_)
print("Coefficients:")
print(en.coef_)

#evaluate
y_pred = en.predict(X_test)
test_score = mean_squared_error(y_test, y_pred)
print("Test estimator has R^2 %2.2f in the test sample.",test_score)

fit = en.coef_*X
comb = fit.Na + fit.Cl + fit.Water
plt.semilogy(originalenergy, y, color='0.65', label='Simulated Data', linewidth=2)
plt.semilogy(originalenergy, fit.Na, label='Na Library', color='b')
plt.semilogy(originalenergy, fit.Cl, label='Cl Library', color='C1')
plt.semilogy(originalenergy, fit.Water, label='Water Library', color='g')
plt.semilogy(originalenergy, comb, 'k:', label='Fit', linewidth=3)
plt.legend()
plt.xlabel('Energy (MeV)')
plt.ylabel('Counts')
plt.title('Elastic Net Library Fit')
plt.show()
