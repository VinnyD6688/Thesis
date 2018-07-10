clc
clear
close all

%% Import Data

file_list = {'c_p1.nos', 'cl_p1.nos', 'crude_p1.nos', 'h_p1.nos' 'h2o_pn.nos', 'o_p1.nos', 'si_p1.nos', 'sio2_p1.nos'};

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

%% Fitting h,c,o,cl to crude - Test A

% Assign variables
h = C{4}
c = C{1}
o = C{6}
cl = C{2}
crude = C{3}

q = [0,0,0,0]

% OLS

ssq = @(q) sum((crude - q(1)*h - q(2)*c - q(3)*o - q(4)*cl).^2);
q0 = q;
options = optimset('MaxFunEvals',5000);

[q, s2] = fminsearch(ssq, q0, options);

q_a_ols = q;
s2_a_ols = s2/(512-4);

X = [h, c, o, cl];
V_a_ols = s2_a_ols*(X.'*X)^(-1);

% DRAM
clear data model options

data.xdata = [h, c, o, cl];
data.ydata = crude;

modelfun = @(C,q) q(1)*C(:,1) + q(2)*C(:,2) + q(3)*C(:,3) + q(4)*C(:,4);
ssfun = @(q,data) sum((data.ydata - modelfun(data.xdata,q).^2);

params = {
{'q1',q_a_ols(1), 0}
{'q2',q_a_ols(2), 0}
{'q3',q_a_ols(3), 0}
{'q4',q_a_ols(4), 0}};

model.ssfun = ssfun;
model.modelfun = modelfun;
options.nsimu = 1e4;
options.updatesigma = 1;

[results,chain,s2chain] = mcmcrun(model,data,params,options);

q1vals = chain(:,1);
q2vals = chain(:,2);
q3vals = chain(:,3);
q4vals = chain(:,4);

range_q1 = max(q1vals) - min(q1vals);
range_q2 = max(q2vals) - min(q2vals);
range_q3 = max(q3vals) - min(q3vals);
range_q4 = max(q4vals) - min(q4vals);

q1_max = max(q1vals)+range_q1/10;
q1_min = min(q1vals)-range_q1/10;
q2_max = max(q2vals)+range_q2/10;
q2_min = min(q2vals)-range_q2/10;
q3_max = max(q3vals)+range_q3/10;
q3_min = min(q3vals)-range_q3/10;
q4_max = max(q4vals)+range_q4/10;
q4_min = min(q4vals)-range_q4/10;

[bandwidth_q1,density_q1,q1mesh,cdf_q1]=kde(q1vals);
[bandwidth_q2,density_q2,q2mesh,cdf_q2]=kde(q2vals);
[bandwidth_q3,density_q3,q3mesh,cdf_q3]=kde(q3vals);
[bandwidth_q4,density_q4,q4mesh,cdf_q4]=kde(q4vals);

%%
q_a_dram = avg(q1vals);

%%
disp('Covariance from DRAM:')
cov(chain)
rslts = chainstats(chain,results);
%%

% LASSO

%% Fitting h,o to h2o - Test B

%% Fitting o,si to sio2 - Test C

%% Fitting h,c,o,si,cl to linear mix - Test D

%% Fitting h2o,crude,sio2 to linear mix - Test E

%% Fitting h,c,o,si,cl to nonlinear mix - Test F

%% Fitting h2o,crude,sio2 to nonlinear mix - Test G

