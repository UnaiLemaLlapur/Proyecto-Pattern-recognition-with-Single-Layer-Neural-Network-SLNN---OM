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
%     Lo : {\tilde L}^*.
% tr_acc : Accuracy^{TR}.
%    Xte : X^{TE}.
%    yte : y^{TE}.
% te_acc : Accuracy^{TE}.
%  niter : total number of iterations.
%    tex : total running time (see "tic" "toc" Matlab commands).
%
% Calls: uosol
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function [nnout] = uo_nn_solve_st(nn,par)
Xtr=[];ytr=[];wo=[];Lo=0;tr_acc=0;Xte=[];yte=[];te_acc=0;niter=0;tex=0;
%
% Training dataset generation
%

%
% Test dataset generation
%

%
% Optimization
%
tic;

tex = toc;
%
% Training accuracy
%

%
% Test accuracy
%

%
nnout.Xtr    = Xtr;
nnout.ytr    = ytr;
nnout.wo     = wo;
nnout.Lo     = Lo;
nnout.niter  = niter;
nnout.tex    = tex;
nnout.tr_acc = tr_acc;
nnout.Xte    = Xte;
nnout.yte    = yte;
nnout.te_acc = te_acc;

end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% End Procedure uo_nn_solve
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
