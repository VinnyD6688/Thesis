from scipy.optimize import minimize as minimize
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
    return((2*math.pi*sig**2)**-0.5*math.exp(-(en-mean)**2/(2*sig**2)))
    
def ls_error(x):
    A1 = x[0]
    A2 = x[1]
    A3 = x[2]
    A4 = x[3]
    C = x[4]
    res = 0
    fwhm = peak_fwhm(a_fwhm, b_fwhm)
    for i in range(len(counts_300)):
        en = energy_300[i]
        co = counts_300[i]
        P1 = A1*gauss_peak(en, fwhm[0], E1)
        P2 = A2*gauss_peak(en, fwhm[1], E2)
        P3 = A3*gauss_peak(en, fwhm[2], E3)
        P4 = A4*gauss_peak(en, fwhm[3], E4)
        res += (P1 + P2 + P3 + P4 + C - co)**2
    return(res)

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
    #plt.plot(en_plt, P1, label='peak 1')
    #plt.plot(en_plt, P2, label='peak 2')
    #plt.plot(en_plt, P3, label='peak 3')
    #plt.plot(en_plt, P4, label='peak 4')
    #plt.plot(en_plt, FC, label='flat continuum')
    plt.plot(en_plt, fit, label='fit')
    plt.xlabel('energy (keV)')
    plt.ylabel('counts')
    plt.legend()
    plt.show()
    return()

def rough_UQ(A1, A2, A3, A4, C, counts_300, energy_300, R2):
    X = [[0]*5 for i in range(len(counts_300))]
    fwhm = peak_fwhm(a_fwhm, b_fwhm)
    for i in range(len(counts_300)):
        en = energy_300[i]
        X[i][0] = gauss_peak(en, fwhm[0], E1)
        X[i][1] = gauss_peak(en, fwhm[1], E2)
        X[i][2] = gauss_peak(en, fwhm[2], E3)
        X[i][3] = gauss_peak(en, fwhm[3], E4)
        X[i][4] = 1
    X = np.array(X)
    sig2 = R2/(len(counts_300)-5.)
    V = sig2*np.linalg.inv(np.matmul(X.transpose(), X))
    sig_A1 = V[0][0]**0.5
    sig_A2 = V[1][1]**0.5
    sig_A3 = V[2][2]**0.5
    sig_A4 = V[3][3]**0.5
    sig_C = V[4][4]**0.5
    UQ = [sig_A1/A1, sig_A2/A2, sig_A3/A3, sig_A4/A4, sig_C/(C*len(counts_300))]
    return(UQ)

if __name__ == "__main__":
    [channel, counts] = read_data('unknown.csv')
    energy = channel2energy(channel, m_energy, b_energy)
    [energy_300, counts_300] = Pu300_region(energy, counts)
    
    x0 = [100, 100, 100, 100, 100]
    res = minimize(ls_error, x0, options={'maxiter':1e6})
    
    A1 = res.x[0]
    A2 = res.x[1]
    A3 = res.x[2]
    A4 = res.x[3]
    C = res.x[4]
    R2 = res.fun

    plot_results(A1, A2, A3, A4, C, counts_300, energy_300)
    UQ = rough_UQ(A1, A2, A3, A4, C, counts_300, energy_300, R2)

    print('A1:',A1)
    print('A2:',A2)
    print('A3:',A3)
    print('A4:',A4)
    print('C:',C)
    print()       
    print('sig_A1/A1:', UQ[0])
    print('sig_A2/A2:', UQ[1])
    print('sig_A3/A3:', UQ[2])
    print('sig_A4/A4:', UQ[3])
    print('sig_C/FC:', UQ[4])