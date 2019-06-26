import pandas as pd
import numpy as np
from numpy import array
import matplotlib.pyplot as plt
from sklearn.model_selection import train_test_split
from sklearn.preprocessing import scale
from sklearn.linear_model import Lasso, LassoCV, ElasticNet, ElasticNetCV, lasso_path, enet_path
from sklearn.metrics import mean_squared_error
import pylab as pl


libraries = pd.read_csv("libraries.csv")
target = pd.read_csv("target.csv")
originalenergy = pd.read_csv("Energy.csv")

X = libraries
y = target.y


X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=.9)
coefs = []
alphas = 10**np.linspace(10,-2,100)*.5

en = ElasticNetCV(l1_ratio=1, alphas=None, cv=10, max_iter=100000, normalize=True, positive=False)
en.fit(X_train, y_train)
ElasticNet = ElasticNet(l1_ratio=1,max_iter=10000, normalize=True, positive=False)
# Display results
model = en.fit(X_train, y_train)
m_log_alphas = -np.log10(model.alphas_)

plt.figure()
plt.plot(m_log_alphas, model.mse_path_, ':')
plt.plot(m_log_alphas, model.mse_path_.mean(axis=-1), 'k',
         label='Average across the folds', linewidth=2)
plt.axvline(-np.log10(model.alpha_), linestyle='--', color='k',
            label='alpha: CV estimate')
plt.legend()

plt.xlabel('-log(alpha)')
plt.ylabel('Mean square error')
plt.show()

for a in alphas:
    ElasticNet.set_params(alpha=a)
    ElasticNet.fit(scale(X_train), y_train)
    coefs.append(ElasticNet.coef_)
 

label_list=['Na Library', 'Cl Library', 'Water Library', 'Fe Library', 'Cu Library'] 
ax = plt.gca()
lineObjects = ax.plot(alphas, coefs)
ax.set_xscale('log')
plt.ticklabel_format(axis='y', style='sci', scilimits=(0,0))
plt.axvline(en.alpha_, linestyle='--', color='k',label='alpha: CV estimate')
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
print(test_score)

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



