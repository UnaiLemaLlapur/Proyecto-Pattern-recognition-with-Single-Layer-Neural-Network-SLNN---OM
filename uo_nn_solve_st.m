%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% F.-Javier Heredia https://gnom.upc.edu/heredia
% https://creativecommons.org/licenses/by-nc/4.0/
% Pattern recognition with Single Layer Neural Network (SLNN).
%
% function [nnout] = uo_nn_solve_st(nn,par)
%
% Trains the SLNN and checks the accuracy for the problem defined in "nn"
% and "par". Student's template.
%
% Input parameters:
%
% nn:
%          L : loss function.
%         gL : gradient of the loss function.
%        Acc : Accuracy function.
% num_target : set of digits to be identified.
%    tr_freq : frequency of the digits target in the data set.
%    tr_seed : seed for the training set random generation.
%       tr_p : size of the training set.
%    te_seed : seed for the test set random generation.
%       te_q : size of the test set.
%         la : coefficient lambda of the decay factor.
% par:
%       epsG : optimality tolerance.
%    maxiter : maximum number of iterations.
%      c1,c2 : (WC) parameters.
%        isd : optimization algorithm.
%     sg.al0 : \alpha^{SG}_0.
%      sg.be : \beta^{SG}.
%       sg.m : m^{SG}.
%    sg.emax : e^{SGÇ_{max}.
%   sg.eworse: e^{SG}_{worse}.
%    sg.seed : seed for the first random permutation of the SG.
%
% Output parameters:
%
% nnout
%    Xtr : X^{TR}.
%    ytr : y^{TR}.
%     wo : w^*.
%     Lo : L^*.
%   ngLo : ||\nabla L^*||, isd <> 7
%        : 0               otherwise.
% tr_acc : Accuracy^{TR}.
%    Xte : X^{TE}.
%    yte : y^{TE}.
% te_acc : Accuracy^{TE}.
%  niter : total number of iterations.
%  nepoc : total number of epochs, isd == 7
%        : 0                       otherwise
%    tex : total running time (see "tic" "toc" Matlab commands).
%
% Calls: uosol
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function [nnout] = uo_nn_solve_st(nn,par)
Xtr=[];ytr=[];wo=[];Lo=0;tr_acc=0;Xte=[];yte=[];te_acc=0;niter=0;tex=0;
%
% Training dataset generation
[Xtr,ytr] = uo_nn_dataset(nn.tr_seed,nn.tr_p,nn.num_target,nn.tr_freq);
%

%
% Test dataset generation
[Xte,yte] = uo_nn_dataset(nn.te_seed,nn.te_q,nn.num_target,0.0); %ponemos freq a 0.0 para que la proporcion de digitos sea igual para cada numero
%

%
% Optimization
P.f = @(w) nn.L(w, Xtr, ytr);
P.g = @(w) nn.gL(w, Xtr, ytr);
P.Xtr = Xtr;
P.ytr = ytr;
P.Xte = Xte;
P.yte = yte;
P.L = nn.L;  P.gL = nn.gL; %para el caso par.isd = 7


w1 = zeros(size(Xtr, 1), 1); 

par.bls_seed = nn.sg_seed; %para fijar la parte aleatoria de uoBLSNW32
par.almax = 1; %Importante porque se actualiza
par.log = 0; %no muestra el log
%
tic;

[sol, par] = uosol_st(P, w1, par); %w1 es x1

tex = toc;

%
% Training accuracy
tr_acc = nn.Acc(Xtr,ytr,sol(end).x);
%

%
% Test accuracy
%
te_acc = nn.Acc(Xte,yte,sol(end).x);
%


%
% Extraccion datos sol
wo = sol(end).x;
Lo = P.f(wo);
niter = size(sol,2);
if par.isd == 7
    opt.etot = sol(end-1).e;
end
gLo = sol(end).g;
%

nnout.Xtr    = Xtr; % training dataset, X^TR
nnout.ytr    = ytr; % Training dataset, y^TR
nnout.wo     = wo;  % Optimal weights
nnout.Lo     = Lo;  % Optimal mean loss with regul. 
if par.isd == 7
    nnout.niter   = opt.etot; % nre. epochs
    nnout.nmbatch = niter;    % nre. of minibatch iterations
    nnout.ngLo  = 0;          % ||gL*|| (void for the SGM)
else
    nnout.niter   = niter;    % nre. of iterations
    nnout.nmbatch = 0;        % nre. of minibatch iter.
    nnout.ngLo  = norm(gLo);  % ||gL*||
end
nnout.tex    = tex;           % Running time
nnout.tr_acc = tr_acc;        % Training accuracy
nnout.Xte    = Xte;           % Test dataset, X^TE
nnout.yte    = yte;           % Test dataset, y^TE
nnout.te_acc = te_acc;        % Test accuracy.

end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% End Procedure uo_nn_solve
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
