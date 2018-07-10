def read_i(file):
    f = open(file,'r')
    flag = 0
    # qq is the number of unique tallies, needs to be hard-coded in
    E = [[] for i in range(qq)]
    T = [[] for i in range(qq)]
    U = [[] for i in range(qq)]
    for line in f:
        if line == ' cell  1                                                                                                                               \n':
            flag = 1
            q = 0
        elif line == ' cell  2                                                                                                                               \n':
            flag = 1
            q = 1
            # repeat for all starts. increase q by 1 each time
        elif flag == 1:
            flag = 2            
        elif flag == 2:
            line = line.split()
            if 'total' in line:
                flag = 0
                break
            E[q].append(float(line[0]))
            T[q].append(float(line[1]))
            U[q].append(float(line[2]))
    f.close()
    return(E, T, U)

def write_o(E,T,U,file,t_list):
    for q in range(len(t_list)):
        tally = t_list[q]
        f=open(tally+'_'+file,'w')
        for i in range(len(E[q])):
            w_str = str(E[q][i])+' '+str(T[q][i])+' '+str(U[q][i])+' \n'
            f.write(w_str)
        f.close()
    return

if __name__ == '__main__':
    #f_list = ['h2o_10.out', 'h2o_20.out' ... list of input files]
    #t_list = ['T1', 'T2', 'T3', list of tallies]

    
    for f_in in f_list:
        (E, T, U) = read_i(f_in)
        write_o(E, T, U, f_out,t_list)