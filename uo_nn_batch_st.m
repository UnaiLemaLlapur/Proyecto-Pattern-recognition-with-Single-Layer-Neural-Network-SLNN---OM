%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% F.-Javier Heredia https://gnom.upc.edu/heredia
% https://creativecommons.org/licenses/by-nc/4.0/
% Pattern recognition with Single Layer Neural Network (SLNN).
%
% uo_nn_batch
% 
% Main script to run a batch of recognitions.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

clear all; warning off;
fLOG = fopen('uo_nn_batch.log','w'); diary uo_nn_batch.log
fprintf('[uo_nn_batch]  Starts\n');
%
% Parameters
%
% NN model:
nn.tr_seed = 123456; nn.te_seed = 789101; nn.sg_seed = 565544; % Seeds.
nn.tr_p = 25000; nn.te_q = nn.tr_p /10; nn.tr_freq = 0.5;      % Datasets 
% Training
par.epsG = 10^-2; par.maxiter = 100;                           % Stopping cond.
par.epsG = 10^-3; par.maxiter = 500;                           % Stopping cond.
par.iAC = 4; par.c1 = 0.01; par.c2 = 0.9;                      % Linesearch.
par.sg.seed = nn.sg_seed; par.sg.al0 = 2; par.sg.be = 0.3;     % SGM
par.sg.m = 10; par.sg.emax = 100; par.sg.eworse = 5;
par.log = 1;  % =0, cancels call to [uosolLog] and reduces running time.
% Aux. functions
sig    = @(X)   1./(1+exp(-X));
y      = @(X,w) sig(w'*sig(X));
nn.Acc = @(Xds,yds,wo) 100*sum(yds==round(y(Xds,wo)))/size(Xds,2);
%
% Runs
%
global iheader; iheader = 1;
fCSV = fopen('uo_nn_batch.csv','w');
t1 = clock;
for num_target = [1:10]
    nn.num_target = num_target;
    lambda = [0.0 0.01 0.5];
    for la = lambda
        % Loss function
        nn.la = la;
        nn.L  = @(w,Xds,yds) (norm(y(Xds,w)-yds)^2)/size(yds,2) + (la*norm(w)^2)/2;
        nn.gL = @(w,Xds,yds) (2*sig(Xds)*((y(Xds,w)-yds).*y(Xds,w).*(1-y(Xds,w)))')/size(yds,2)+la*w;
        for isd = [1 3 7]
            par.isd = isd;
            [nnout] = uo_nn_solve_st(nn,par);
            if iheader == 1
                fprintf(fCSV,'num_target;      la; isd;  niter;     tex; tr_acc; te_acc;        L*;\n');
            end
            if ~isempty(nnout)
                fprintf(fCSV,'         %1i; %7.4f;   %1i; %6i; %7.4f;  %5.1f;  %5.1f;  %8.2e;\n', mod(num_target,10), la, isd, nnout.niter, nnout.tex, nnout.tr_acc, nnout.te_acc, nnout.Lo);
            end
            iheader=0;
        end
    end
end
t2 = clock; total_t = etime(t2,t1);
fprintf('[uo_nn_batch]  Stops, wall time = %6.1f s.\n', total_t); fclose(fCSV);
diary off; fclose(fLOG);