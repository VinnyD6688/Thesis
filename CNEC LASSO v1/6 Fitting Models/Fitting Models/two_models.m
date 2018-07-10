clc
clear all
close all

%% Import Data
co60_ger_file = fullfile(pwd, '/Normalized Data/norm_co60.ger');
cs137_ger_file = fullfile(pwd, '/Normalized Data/norm_cs137.ger');
mn54_ger_file = fullfile(pwd, '/Normalized Data/norm_mn54.ger');

co60_nai_file = fullfile(pwd, '/Normalized Data/norm_co60.nai');
cs137_nai_file = fullfile(pwd, '/Normalized Data/norm_cs137.nai');
mn54_nai_file = fullfile(pwd, '/Normalized Data/norm_mn54.nai');

mixr_ger_file = fullfile(pwd, '/Normalized Data/norm_mixr.ger');
mixr_nai_file = fullfile(pwd, '/Normalized Data/norm_mixr.nai');

La_ger = zeros(4096,1);
Lb_ger = zeros(4096,1);
Lc_ger = zeros(4096,1);

La_nai = zeros(4096,1);
Lb_nai = zeros(4096,1);
Lc_nai = zeros(4096,1);

D_ger = zeros(4096,1);
D_nai = zeros(4096,1);

tmp = textread(co60_ger_file);
La_ger(1:length(tmp),1) = tmp(:,2); %co60 ger
tmp = textread(cs137_ger_file);
Lb_ger(1:length(tmp),1) = tmp(:,2); %cs137 ger
tmp = textread(mn54_ger_file);
Lc_ger(1:length(tmp),1) = tmp(:,2); %mn54 ger

tmp = textread(co60_nai_file);
La_nai(1:length(tmp),1) = tmp(:,2); %co60 nai
tmp = textread(cs137_nai_file);
Lb_nai(1:length(tmp),1) = tmp(:,2); %cs137 nai
tmp = textread(mn54_nai_file);
Lc_nai(1:length(tmp),1) = tmp(:,2); %mn54 nai

C_ger = tmp(:,1); %channels ger
C_nai = tmp(:,1); %channels nai

tmp = textread(mixr_ger_file);
D_ger(1:length(tmp),1) = tmp(:,2); %data ger

tmp = textread(mixr_nai_file);
%D_nai(1:length(tmp),1) = tmp(:,2); %data nai

% Construct D_nai
D_nai = 0.5733*La_nai + 0.3605*Lb_nai + 0.0657*Lc_nai;

% Add Noise to NaI
La_nai = La_nai + lognrnd(0, 0.11, [4096,1]);
Lb_nai = Lb_nai + lognrnd(0, 0.11, [4096,1]);
Lc_nai = Lc_nai + lognrnd(0, 0.11, [4096,1]);
D_nai = D_nai +  + lognrnd(0, 0.11, [4096,1]);

La_nai = La_nai*1e4/sum(La_nai);
Lb_nai = Lb_nai*1e4/sum(Lb_nai);
Lc_nai = Lc_nai*1e4/sum(Lc_nai);
D_nai = D_nai*1e4/sum(D_nai);

%% Initialize Model
SSQ = @(D,La,Lb,Lc,q) sum((D - q(1)*La - q(2)*Lb - q(3)*Lc).^2);
q0 = [0,0,0];
options = optimset('MaxFunEvals',5000);

%% OLS
SSQ_ger = @(q) SSQ(D_ger, La_ger, Lb_ger, Lc_ger, q);
[q_ger_ols, s2_ger_ols] = fminsearch(SSQ_ger, q0, options);
            
SSQ_nai = @(q) SSQ(D_nai, La_nai, Lb_nai, Lc_nai, q);
[q_nai_ols, s2_nai_ols] = fminsearch(SSQ_nai, q0, options);

q_ger_ols
q_nai_ols

s2_ger_ols = s2_ger_ols/(4096-3)
s2_nai_ols = s2_nai_ols/(4096-3)

X_ger_ols = [La_ger, Lb_ger, Lc_ger];
V_ger_ols = s2_ger_ols*(X_ger_ols.'*X_ger_ols)^(-1)
            
X_nai_ols = [La_nai, Lb_nai, Lc_nai];
V_nai_ols = s2_nai_ols*(X_nai_ols.'*X_nai_ols)^(-1)

%% Plot OLS
figure(1)
semilogy(C_nai,D_nai)
hold on
semilogy(C_nai,q_nai_ols(1)*La_nai)
semilogy(C_nai,q_nai_ols(2)*Lb_nai)
semilogy(C_nai,q_nai_ols(3)*Lc_nai)
semilogy(C_nai, (q_nai_ols*[La_nai, Lb_nai, Lc_nai]')')
legend('Mixture', 'Co-60', 'Cs-137', 'Mn-54', 'Fit', 'Location', 'southwest')
xlabel('Channel')
ylabel('Normalized Counts')
xlim([0,4096])
title('NaI')
hold off

figure(2)
semilogy(C_ger,D_ger)
hold on
semilogy(C_ger,q_ger_ols(1)*La_ger)
semilogy(C_ger,q_ger_ols(2)*Lb_ger)
semilogy(C_ger,q_ger_ols(3)*Lc_ger)
semilogy(C_ger, (q_ger_ols*[La_ger, Lb_ger, Lc_ger]')')
legend('Mixture', 'Co-60', 'Cs-137', 'Mn-54', 'Fit', 'Location', 'southwest')
xlabel('Channel')
ylabel('Normalized Counts')
xlim([0,4096])
title('HPGe')
hold off

%% Run DRAM Ger
clear data model options

data.xdata = [La_ger, Lb_ger, Lc_ger];
data.ydata = D_ger;

modelfun = @(L,q) q(1)*L(:,1) + q(2)*L(:,2) + q(3)*L(:,3);
ssfun = @(q,data) sum((data.ydata - modelfun(data.xdata,q)).^2);

params = {
{'q1',q_ger_ols(1), 0}
{'q2',q_ger_ols(2), 0}
{'q3',q_ger_ols(3), 0}};

model.ssfun = ssfun;
model.modelfun = modelfun;
%model.sigma2 = s2_ger_ols; 
%options.qcov = V_ger_ols;
options.nsimu = 10000;
options.updatesigma = 1;
options.nsimu = 1e4;
options.updatesigma = 1;

[results,chain,s2chain] = mcmcrun(model,data,params,options);

%% Interpret DRAM Ger
q1vals_ger = chain(:,1);
q2vals_ger = chain(:,2);
q3vals_ger = chain(:,3);

range_q1_ger = max(q1vals_ger) - min(q1vals_ger);
range_q2_ger = max(q2vals_ger) - min(q2vals_ger);
range_q3_ger = max(q3vals_ger) - min(q3vals_ger);

q1_max_ger = max(q1vals_ger)+range_q1_ger/10;
q1_min_ger = min(q1vals_ger)-range_q1_ger/10;
q2_max_ger = max(q2vals_ger)+range_q2_ger/10;
q2_min_ger = min(q2vals_ger)-range_q2_ger/10;
q3_max_ger = max(q3vals_ger)+range_q3_ger/10;
q3_min_ger = min(q3vals_ger)-range_q3_ger/10;

[bandwidth_q1_ger,density_q1_ger,q1mesh_ger,cdf_q1_ger]=kde(q1vals_ger);
[bandwidth_q2_ger,density_q2_ger,q2mesh_ger,cdf_q2_ger]=kde(q2vals_ger);
[bandwidth_q3_ger,density_q3_ger,q3mesh_ger,cdf_q3_ger]=kde(q3vals_ger);

disp('Covariance from DRAM:')
cov(chain)
rslts = chainstats(chain,results);

% chain plots
figure(3)
subplot(1,3,1)
plot(q1vals_ger,'-')
xlabel('Chain Iteration')
ylabel('Parameter q1 HPGe')

subplot(1,3,2)
plot(q2vals_ger,'-')
xlabel('Chain Iteration')
ylabel('Parameter q2 HPGe')

subplot(1,3,3)
plot(q3vals_ger,'-')
xlabel('Chain Iteration')
ylabel('Parameter q3 HPGe')

% marginal densities
figure(4)
subplot(1,3,1)
plot(q1mesh_ger,density_q1_ger,'k-', 'linewidth', 2)
xlabel('Parameter q1 HPGe')

subplot(1,3,2)
plot(q2mesh_ger,density_q2_ger,'k-', 'linewidth', 2)
xlabel('Parameter q2 HPGe')

subplot(1,3,3)
plot(q3mesh_ger,density_q3_ger,'k-', 'linewidth', 2)
xlabel('Parameter q3 HPGe')

% joint distributions
figure(5)
subplot(2,2,1)
scatter(q1vals_ger,q2vals_ger)
box on
ylabel('Parameter q2 HPGe')

subplot(2,2,3)
scatter(q1vals_ger,q3vals_ger)
box on
xlabel('Parameter q1 HPGe')
ylabel('Parameter q3 HPGe')

subplot(2,2,4)
scatter(q2vals_ger,q3vals_ger)
box on
xlabel('Parameter q2 HPGe')

%% Run DRAM NaI
clear data model options

data.xdata = [La_nai, Lb_nai, Lc_nai];
data.ydata = D_nai;

modelfun = @(L,q) q(1)*L(:,1) + q(2)*L(:,2) + q(3)*L(:,3);
ssfun = @(q,data) sum((data.ydata - modelfun(data.xdata,q)).^2);

params = {
{'q1',q_nai_ols(1), 0}
{'q2',q_nai_ols(2), 0}
{'q3',q_nai_ols(3), 0}};

model.ssfun = ssfun;
model.modelfun = modelfun;
%model.sigma2 = s2_ger_ols; 
%options.qcov = V_ger_ols;
options.nsimu = 10000;
options.updatesigma = 1;
options.nsimu = 1e4;
options.updatesigma = 1;

[results,chain,s2chain] = mcmcrun(model,data,params,options);

%% Interpret DRAM NaI
q1vals_nai = chain(:,1);
q2vals_nai = chain(:,2);
q3vals_nai = chain(:,3);

range_q1_nai = max(q1vals_nai) - min(q1vals_nai);
range_q2_nai = max(q2vals_nai) - min(q2vals_nai);
range_q3_nai = max(q3vals_nai) - min(q3vals_nai);

q1_max_nai = max(q1vals_nai)+range_q1_nai/10;
q1_min_nai = min(q1vals_nai)-range_q1_nai/10;
q2_max_nai = max(q2vals_nai)+range_q2_nai/10;
q2_min_nai = min(q2vals_nai)-range_q2_nai/10;
q3_max_nai = max(q3vals_nai)+range_q3_nai/10;
q3_min_nai = min(q3vals_nai)-range_q3_nai/10;

[bandwidth_q1_nai,density_q1_nai,q1mesh_nai,cdf_q1_nai]=kde(q1vals_nai);
[bandwidth_q2_nai,density_q2_nai,q2mesh_nai,cdf_q2_nai]=kde(q2vals_nai);
[bandwidth_q3_nai,density_q3_nai,q3mesh_nai,cdf_q3_nai]=kde(q3vals_nai);

disp('Covariance from DRAM:')
cov(chain)
rslts = chainstats(chain,results);

% chain plots
figure(6)
subplot(1,3,1)
plot(q1vals_nai,'-')
xlabel('Chain Iteration')
ylabel('Parameter q1 NaI')

subplot(1,3,2)
plot(q2vals_nai,'-')
xlabel('Chain Iteration')
ylabel('Parameter q2 NaI')

subplot(1,3,3)
plot(q3vals_nai,'-')
xlabel('Chain Iteration')
ylabel('Parameter q3 NaI')

% marginal densities
figure(7)
subplot(1,3,1)
plot(q1mesh_nai,density_q1_nai,'k-', 'linewidth', 2)
xlabel('Parameter q1 NaI')

subplot(1,3,2)
plot(q2mesh_nai,density_q2_nai,'k-', 'linewidth', 2)
xlabel('Parameter q2 NaI')

subplot(1,3,3)
plot(q3mesh_nai,density_q3_nai,'k-', 'linewidth', 2)
xlabel('Parameter q3 NaI')

% joint distributions
figure(8)
subplot(2,2,1)
scatter(q1vals_nai,q2vals_nai)
box on
ylabel('Parameter q2 NaI')

subplot(2,2,3)
scatter(q1vals_ger,q3vals_ger)
box on
xlabel('Parameter q1 NaI')
ylabel('Parameter q3 NaI')

subplot(2,2,4)
scatter(q2vals_ger,q3vals_ger)
box on
xlabel('Parameter q2 NaI')

%% Difference
q_nai_DRAM = sum(q1vals_nai)/1e4;
q_ger_DRAM = sum(q1vals_ger)/1e4;

V_nai_DRAM = 1e-6*[0.5208, -0.0817, -0.1820; -0.0817, 0.1448, -0.0450; -0.1820, -0.0450, 0.2508];
V_ger_DRAM = 1e-5*[0.1085, -0.0056, -0.0090; -0.0056, 0.0251, -0.0012; -0.0090, -0.0012, 0.0344];

RD_q_nai = 100*(q_nai_ols - q_nai_DRAM)./(0.5.*(q_nai_ols + q_nai_DRAM))
RD_q_ger = 100*(q_ger_ols - q_ger_DRAM)./(0.5.*(q_ger_ols + q_ger_DRAM))

RD_V_nai = 100*(V_nai_ols - V_nai_DRAM)./(0.5.*(V_nai_ols + V_nai_DRAM))
RD_V_ger = 100*(V_ger_ols - V_ger_DRAM)./(0.5.*(V_ger_ols + V_ger_DRAM))