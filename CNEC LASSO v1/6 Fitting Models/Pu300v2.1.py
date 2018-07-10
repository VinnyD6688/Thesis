from scipy.optimize import minimize as minimize
from scipy.optimize import curve_fit as curve_fit
import matplotlib.pyplot as plt
import numpy as np
import math
import csv

# peak centroids. global variables.
E1 = 332.6
E2 = 335.6
E3 = 341.25
E4 = 344.8

# calibration parameters. global variables.
m_energy = 0.366
b_energy = -0.176
a_fwhm = 0.3531
b_fwhm = 0.2557

def read_data(inp_file):
    channel = []
    counts = []
    f = open(inp_file)
    reader = csv.reader(f)
    for row in reader:
        channel.append(float(row[0]))
        counts.append(float(row[1]))
    f.close()
    return(channel, counts)

def channel2energy(channel, m, b):
    energy = [x*m+b for x in channel]
    return(energy)

def peak_fwhm(a, b):
    fwhm = [0]*4
    fwhm[0] = a*E1**b
    fwhm[1] = a*E2**b
    fwhm[2] = a*E3**b
    fwhm[3] = a*E4**b
    return (fwhm)

def Pu300_region(energy, counts):
    e_start = next(x[0] for x in enumerate(energy) if x[1] > 330)
    e_end = next(x[0] for x in enumerate(energy) if x[1] > 350)
    return(energy[e_start:e_end], counts[e_start:e_end])

def gauss_peak(en, fwhm, mean):
    sig = fwhm/2.35
    return((2*math.pi*sig**2)**-0.5*np.exp(-(en-mean)**2/(2*sig**2)))

def fit_func(en, A1, A2, A3, A4, C):
    fwhm = peak_fwhm(a_fwhm, b_fwhm)
    P1 = A1*gauss_peak(en, fwhm[0], E1)
    P2 = A2*gauss_peak(en, fwhm[1], E2)
    P3 = A3*gauss_peak(en, fwhm[2], E3)
    P4 = A4*gauss_peak(en, fwhm[3], E4)
    return(P1+P2+P3+P4+C)

def plot_results(A1, A2, A3, A4, C, counts_300, energy_300):
    P1 = []
    P2 = []
    P3 = []
    P4 = []
    FC = []
    en_plt = []
    fit = []
    fwhm = peak_fwhm(a_fwhm, b_fwhm)
    for i in range(1001):
        en = 330.+20./1001.*i
        en_plt.append(en)
        P1.append(A1*gauss_peak(en, fwhm[0], E1))
        P2.append(A2*gauss_peak(en, fwhm[1], E2))
        P3.append(A3*gauss_peak(en, fwhm[2], E3))
        P4.append(A4*gauss_peak(en, fwhm[3], E4))
        FC.append(C)
        fit.append(P1[i]+P2[i]+P3[i]+P4[i]+FC[i])
    plt.scatter(energy_300, counts_300, label='unknown', color='r')
    plt.plot(en_plt, P1, label='peak 1')
    plt.plot(en_plt, P2, label='peak 2')
    plt.plot(en_plt, P3, label='peak 3')
    plt.plot(en_plt, P4, label='peak 4')
    #plt.plot(en_plt, FC, label='flat continuum')
    plt.plot(en_plt, fit, label='fit')
    plt.xlabel('energy (keV)')
    plt.ylabel('counts')
    plt.legend()
    plt.show()
    return()

if __name__ == "__main__":
    [channel, counts] = read_data('unknown.csv')
    energy = channel2energy(channel, m_energy, b_energy)
    [energy_300, counts_300] = Pu300_region(energy, counts)
    
    q_val, q_cov = curve_fit(fit_func, energy_300, counts_300, sigma=np.sqrt(counts_300))
    
    A1 = q_val[0]
    A2 = q_val[1]
    A3 = q_val[2]
    A4 = q_val[3]
    C = q_val[4]
    
    s2_A1 = q_cov[0][0]
    s2_A2 = q_cov[1][1]
    s2_A3 = q_cov[2][2]
    s2_A4 = q_cov[3][3]
    s2_C = q_cov[4][4]    
    
    plot_results(A1, A2, A3, A4, C, counts_300, energy_300)

    print('A1:',A1)
    print('A2:',A2)
    print('A3:',A3)
    print('A4:',A4)
    print('C:',C)
    print()       
    print('sig_A1/A1:', np.sqrt(s2_A1)/A1)
    print('sig_A2/A2:', np.sqrt(s2_A2)/A2)
    print('sig_A3/A3:', np.sqrt(s2_A3)/A3)
    print('sig_A4/A4:', np.sqrt(s2_A4)/A4)
    print('sig_C/FC:', np.sqrt(s2_C)/(C*len(counts_300)))