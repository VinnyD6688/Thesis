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

X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=.5)
alphas = 10**np.linspace(10,-2,100)*.5
lassocv = LassoCV(alphas=None, cv=10, max_iter=100000, normalize=True, positive=False)
model = lassocv.fit(X_train, y_train)


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

lasso = Lasso(max_iter=10000, normalize=True, positive=False)
coefs = []

for a in alphas:
    lasso.set_params(alpha=a)
    lasso.fit(scale(X_train), y_train)
    coefs.append(lasso.coef_)
 


label_list=['Na Library', 'Cl Library', 'Water Library', 'Fe Library', 'Cu Library'] 
ax = plt.gca()
lineObjects = ax.plot(alphas*2, coefs)
ax.set_xscale('log')
plt.ticklabel_format(axis='y', style='sci', scilimits=(0,0))
plt.locator_params(axis='y', nbins=10)
"""
    
"""
plt.xlabel('Alpha')
plt.ylabel('Coefficients')
plt.axvline(model.alpha_, linestyle='--', color='k',label='alpha: CV estimate')
plt.title('Optimal Alpha Parameters')
plt.legend(iter(lineObjects), label_list)
plt.show()
lasso.set_params(alpha=lassocv.alpha_)
lasso.fit(X_train, y_train)
mean_squared_error(y_test, lasso.predict(X_test))
print("Best for alphas:")
print(lassocv.alpha_)
"""
print("Best l1-ratio:")
print(lasso.l1_ratio)
"""
print("Coefficients:")
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
