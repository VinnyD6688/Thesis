clc
clear
close all

%% Import Data

file_list = {'c_p1.col', 'cl_p1.col', 'crude_p1.col', 'h_p1.col' 'h2o_pn.col', 'o_p1.col', 'si_p1.col', 'sio2_p1.col'};

B = cell(1,length(flist));
C = cell(1,length(flist));
U = cell(1,length(flist));

for i=1:length(flist)
    current_file = strcat(pwd,'\Formatted Outputs\',file_list{i}); % need to change directory
    tmp = textread(current_file);
    B{i} = tmp(:,1);
    C{i} = tmp(:,2);
    U{i} = tmp(:,3);
end

%% Noise Model

%% Edit Matrices

%% Save Results