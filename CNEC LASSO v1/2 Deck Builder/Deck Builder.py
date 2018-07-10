def write_file(chem, density, file):
    f = open(file, 'w')
    chemstring = ''
    for el in chem:
        chemstring += '      ' + el + '\n'
    
    filestring ='\
c Bare NaI in medium with 14 MeV n\n\
c\n\
c Cell Card\n\
1 1 -3.67 -10 11 -12 13 -14 15 imp:p=1 imp:n=1\n\
2 2 -{} -99 50 imp:p=1 imp:n=1\n\
3 0 -50#(-10 11 -12 13 -14 15) imp:p=1 imp:n=1\n\
4 0 99 imp:n=0 imp:p=0\n\
\n\
c Surface Card\n\
10 pz 3.81\n\
11 pz -3.81\n\
12 px 3.81\n\
13 px -3.81\n\
14 py 3.81\n\
15 py -3.81\n\
50 rcc 0 0 -900 0 0 900 3.81\n\
99 so 1000.0\n\
\n\
c Data Card\n\
sdef pos = 0 0 49 par=1 erg=14\n\
nps 1e9\n\
mode  n\n\
f8:p 2\n\
e8 0.0 512i 12.0\n\
ft8 GEB 0.011131 0.036071 1.4056\n\
c Materials\n\
c NaI\n\
m1\n\
      11023.60c -0.15307\n\
      53127. -0.8457\n\
c\n\
c {}\n\
m2  \n\
{}'.format(density, file.replace('.inp',''), chemstring)
    f.write(filestring)
    f.close()
    return()

file_list = ['h_p1.inp', 'c_p1.inp', 'o_p1.inp', 'si_p1.inp', 'cl_p1.inp', 'h2o_pn.inp', 'crude_p1.inp', 'sio2_p1.inp']

density_list = [1.00]*8

chem_list = [['01001 1'],
             ['06000 1'],
             ['08016 1'],
             ['14028 1'],
             ['16000 1'],
             ['01001 2','08016 1'],
             ['01001 -0.11191', '06000 -0.85', '08016 -0.00775', '16000 -0.03034'],
             ['8016 2', '14028 1']]


for i in range(len(chem_list)):
    chem = chem_list[i]
    density = density_list[i]
    file = file_list[i]
    
    write_file(chem, density, file)