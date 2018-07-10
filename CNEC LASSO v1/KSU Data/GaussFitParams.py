import numpy as np
import matplotlib.pyplot as plt
from scipy.optimize import curve_fit

def func(x,a,b):
 return a*np.power(x,b)

data = np.genfromtxt('CeBrResData.txt')
xdata = data[:,0]
ydata = data[:,1]

popt, pcov = curve_fit(func, xdata, ydata, sigma=np.sqrt(ydata))

print(popt)

x_plt = []
y_plt = []
for i in range(1001):
 q = min(xdata) +i*(max(xdata)-min(xdata))/1000
 x_plt.append(q)
 y_plt.append(func(q,popt[0], popt[1]))
 
plt.scatter(xdata, ydata)
plt.plot(x_plt, y_plt)
plt.show()