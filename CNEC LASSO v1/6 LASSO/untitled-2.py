import matplotlib.pyplot as plt 
import numpy as np 
import sklearn.linear_model as lm
# lambda is alpha ! 
mod = lm.Lasso(alpha=.1)
# Fit models on dataset
n_features, r2_train, r2_test, snr = fit_on_increasing_size(model=mod) 
argmax = n_features[np.argmax(r2_test)]
# plot 
fig, axis = plt.subplots(1, 2, figsize=(9, 3))
# Left pane: all features 
plot_r2_snr(n_features, r2_train, r2_test, argmax, snr, axis[0])
# Right pane: Zoom on 200 first features 
plot_r2_snr(n_features[n_features <= 200], r2_train[n_features <= 200], r2_test[n_features <= 200], argmax, snr[n_features <= 200], axis[1]) 
plt.tight_layout()