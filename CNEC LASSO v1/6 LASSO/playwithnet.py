import pandas as pd
import numpy as np
from numpy import array
import matplotlib.pyplot as plt
from sklearn.model_selection import train_test_split
from sklearn.preprocessing import scale
from sklearn.linear_model import Lasso, LassoCV, ElasticNet, ElasticNetCV, lasso_path, enet_path
from sklearn.metrics import mean_squared_error
import pylab as pl
from itertools import cycle


libraries = pd.read_csv("libraries.csv")
target = pd.read_csv("target.csv")
originalenergy = pd.read_csv("Energy.csv")

X = libraries
y = target.y

X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=.5)
alphas = 10**np.linspace(10,-2,100)*.5
en = ElasticNetCV(alphas=None, cv=10, max_iter=100000, normalize=True, positive=False)
model = en.fit(X_train, y_train)


m_log_alphas = -np.log10(model.alphas_)

plt.figure()

plt.plot(m_log_alphas, np.log10(model.mse_path_), ':')
plt.plot(m_log_alphas, np.log10(model.mse_path_.mean(axis=-1)), 'k', label='Average across the folds', linewidth=2)
plt.axvline(-np.log10(model.alpha_), linestyle='--', color='k',label='alpha: CV estimate')
plt.legend()
plt.xlabel('-log(alpha)')
plt.ylabel('Mean square error')
plt.title('Mean square error on each fold: coordinate descent')
plt.axis('tight')
plt.show()

elastic = ElasticNet(max_iter=10000, normalize=True, positive=True)
coefs = []

for a in alphas:
    elastic.set_params(alpha=a)
    elastic.fit(scale(X_train), y_train)
    coefs.append(elastic.coef_)
 
ax = plt.gca()
ax.plot(alphas*2, coefs)
ax.set_xscale('log')

plt.xlabel('Alpha')
plt.ylabel('Coefficients')
plt.axvline(model.alpha_, linestyle='--', color='k',label='alpha: CV estimate')
plt.title('Optimal Alpha Parameters')
plt.show()
en.set_params(alphas=model.alpha_)
en.fit(X_train, y_train)
mean_squared_error(y_test, en.predict(X_test))
print("Best for alphas:")
print(model.alpha_)
"""
print("Best l1-ratio:")
print(lasso.l1_ratio)
"""
print("Coefficients:")
print(pd.Series(en.coef_, index=X.columns))
print(mean_squared_error(y_test, en.predict(X_test)))
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
plt.title('LASSO Library Fit')
plt.show()
