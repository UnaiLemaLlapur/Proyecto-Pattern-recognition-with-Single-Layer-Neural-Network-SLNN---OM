%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% F.-Javier Heredia https://gnom.upc.edu/heredia
% https://creativecommons.org/licenses/by-nc/4.0/
%
% [sol,par] = uosol_st(P,x1,par)
%
% Template for the unconstrained optimization with first and second
% derivative methods.
%
% See uolib.mlx for a description of the input/output arguments,
% standard output and calls.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function [sol,par] = uosol_st(P,x,par)

%
% Inicializaciones por defecto
par_default.epsG = 1.0000e-06;
par_default.maxiter = 500;
par_default.isd = 1;
par_default.iAC = 1;
par_default.almax = 1;
par_default.almin = 1.0000e-06;
par_default.rho = 0.9000;
par_default.c1 = 0.0100;
par_default.c2 = 0.9000;
par_default.log = 1;
par_default.delta = 10^(-3);
par_default.bls_seed = 0;

fields = fieldnames(par_default);
for k = 1:numel(fields)
    f = fields{k};
    if ~exist('par','var') %recorre todo par_default, es mejorable
        par = par_default;
    elseif ~isfield(par, f)
        par.(f) = par_default.(f);
    end
end
%
n=size(x,1);
f = P.f;
g = P.g;


k = 1;
sol(k).x = x;
ldescent = true;
rng(par.bls_seed); %fija una semilla para las partes aleatorias (uoBLSNW32)
%
%
% Algorithm
%

if par.isd == 7
    % SGM
    p = size(P.Xtr, 2);             % Columnas (muestras)
    m = par.sg.m;                   % Tamaño del minibatch
    k_e = ceil(p / m);             % Minibatches por época
    k_max_sg = par.sg.emax * k_e;   % Iteraciones máximas
    
    e = 0; s = 0; 
    L_best_TE = inf; 
    w_best = x;
    k_iter = 0;                     % Corresponde a 'k' en el pseudocódigo
    
    alpha_0 = par.sg.al0;
    alpha_sg = 0.01 * alpha_0;
    k_SG_decay = floor(par.sg.be * k_max_sg);
    
    rng(par.sg.seed);               % Semilla para la primera permutación aleatoria y que condiciona las siguientes
    
    while e <= par.sg.emax && s < par.sg.eworse
        P_perm = randperm(p);       % Permutación de índices
        
        for i = 0:(k_e - 1)         %por propiedad ceil(x -1) = ceil(x) -1
            % Seleccionar subconjunto (minibatch)
            idx_start = i * m + 1;
            idx_end = min((i + 1) * m, p);
            S_idx = P_perm(idx_start:idx_end);
            
            X_S = P.Xtr(:, S_idx);
            y_S = P.ytr(:, S_idx);
            
            % Dirección de descenso: Gradiente evaluado solo en el minibatch
            d = -P.gL(x, X_S, y_S);
            
            % Tasa de aprendizaje (Learning rate decay)
            if k_iter <= k_SG_decay
                alpha_k = (1 - k_iter / k_SG_decay) * alpha_0 + (k_iter / k_SG_decay) * alpha_sg;
            else
                alpha_k = alpha_sg;
            end
            
            % Guardar iteración
            sol(k_iter + 1).x = x;
            sol(k_iter + 1).idx = S_idx;
            sol(k_iter + 1).g = -d; % Guardamos gradiente estocástico
            sol(k_iter + 1).ng = norm(d);
            sol(k_iter + 1).d = d;
            sol(k_iter + 1).al = alpha_k;
            sol(k_iter + 1).AC = "SGM";
            sol(k_iter + 1).e = e + 1; %epoch actual
            sol(k_iter + 1).minbatch_iter = i; %epoch actual
            
            % Actualizar pesos
            x = x + alpha_k * d;
            k_iter = k_iter + 1;
        end
        
        % Al finalizar la época, evaluar en el Test Dataset (Early Stopping)
        e = e + 1;
        L_TE = P.L(x, P.Xte, P.yte);
        
        if L_TE < L_best_TE
            L_best_TE = L_TE;
            w_best = x;
            s = 0;
        else
            s = s + 1;
        end
    end
    
    % En SGM, nos quedamos con los pesos de la mejor época
    x = w_best;
    k = k_iter + 1; % Sincronizar índice final

else
    while norm(g(x)) > par.epsG & k < par.maxiter & (ldescent | par.isd == 4)
    if par.isd == 1
        % GM
        d = -g(x);
    elseif par.isd == 3
        % BFGS
        %
        if k == 1
            H = eye(size(x,1),size(x,1));
        end
        d = -H * g(x);
        % Se añaden + cosas despues de BLS
        sol(k).H = H;
    %Omitimos los codigos de NM, MNM-SD, MNM-CMI porque no se usaran para
    %el proyecto
    end
    ldescent = d'*g(x) < 0;
    % LS
    %Omitimos los casos de Unit step length del NM y exact line search
    %porque no son relevantes para el proyecto
    if  par.iAC == 4     % BLS para SNN
        [al,iout] = uoBLSNW32(f,g,x,d,par.almax,par.c1,par.c2);
        if iout == 0
            ACout = "SWC";
        else
            ACout = "";
        end
        sol(k).iout = iout;
        par.almax = 2 * (f(x + al*d) - f(x)) / (transpose(g(x + al*d)) * d);
    elseif par.iAC <= 3      % BLS.
        [al,ACout] = uoBLS_st(x,d,P,par);
    end
    if par.isd == 3 %codigo added para BFGS
        s = x + al*d - x;
        y = g(x + al*d) - g(x);
        rho_k = inv(transpose(y) * s);
        H = (eye(size(x,1),size(x,1)) -rho_k * s * transpose(y)) * sol(k).H * (eye(size(x,1),size(x,1)) -rho_k * y * transpose(s)) + rho_k * s * transpose(s);%afegit
    end
    sol(k).x  = x;
    sol(k).g  = g(x);
    sol(k).ng = norm(g(x));
    sol(k).d  = d;
    sol(k).al = al;
    sol(k).AC = ACout;
    x = x + al*d; k=k+1;
    end %............................................................ main loop
end
sol(k).x  = x;
sol(k).g  = g(x);
sol(k).ng = norm(g(x));
%
% Iterations log
%
if par.log == 1
   [sol] = uosolLog(P,par,sol);
end

end
% [end] Function [uosol_st] %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
