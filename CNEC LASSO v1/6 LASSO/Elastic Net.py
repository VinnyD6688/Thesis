import pandas as pd
import numpy as np
from numpy import array
import matplotlib.pyplot as plt
from sklearn.model_selection import train_test_split
from sklearn.preprocessing import scale
from sklearn.linear_model import Lasso, LassoCV, ElasticNet, ElasticNetCV, enet_path
from sklearn.metrics import mean_squared_error


libraries = pd.read_csv("libraries.csv")
target = pd.read_csv("target.csv")
originalenergy = pd.read_csv("Energy.csv")

X = libraries
y = target.y


X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=.5)
coefs = []
alphas = 10**np.linspace(10,-2,100)*.5

en = ElasticNetCV(l1_ratio=[0.1, 0.5, 0.7, 0.9, 0.95, 0.99, 1.0], alphas=None, cv=10, max_iter=100000, normalize=True)
en.fit(X_train, y_train)

eps = 5e-3

alphas_enet, coefs_enet, _ = enet_path(
    X, y, eps=eps, l1_ratio=0.8, fit_intercept=False)

plt.figure(1)
neg_log_alphas_enet = -np.log10(alphas_enet)



plt.xlabel('-Log(alpha)')
plt.ylabel('coefficients')
plt.title('Elastic-Net Path')
plt.legend(l2[-1], ('Elastic-Net'), loc='lower left')
plt.axis('tight')

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
plt.semilogy(originalenergy, y, 'k', label='Simulated Data')
plt.semilogy(originalenergy, fit.Na, label='Na Library')
plt.semilogy(originalenergy, fit.Cl, label='Cl Library')
plt.semilogy(originalenergy, fit.Water, label='Water Library')
plt.semilogy(originalenergy, comb, 'y-', label='Fit')
plt.legend()
plt.xlabel('Energy')
plt.ylabel('Counts')
plt.title('Elastic Net Library Fit')
plt.show()
