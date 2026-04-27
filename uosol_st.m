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
% Initializations
%
n=size(x,1);
f = P.f;
g = P.g;
h = P.h;
k = 1;
sol(k).x = x;
if par.isd == 7
    sol(k).Xtr = P.Xtr(:, 1:par.sg.m);
    sol(k).ytr = P.ytr(:, 1:par.sg.m);
    sol(k).idx = 1:par.sg.m;
end
ldescent = true;
%
% Algorithm
%
if par.isd == 7
    % SGM
    p = size(P.Xtr, 2);             % Columnas (muestras)
    m = par.sg.m;                   % Tamaño del minibatch
    k_e = floor(p / m);             % Minibatches por época
    k_max_sg = par.sg.emax * k_e;   % Iteraciones máximas
    
    e = 0; s = 0; 
    L_best_TE = inf; 
    w_best = x;
    k_iter = 0;                     % Corresponde a 'k' en el pseudocódigo
    
    alpha_0 = par.sg.al0;
    alpha_sg = 0.01 * alpha_0;
    k_SG_decay = floor(par.sg.be * k_max_sg);
    
    rng(par.sg.seed);               % Semilla para la primera permutación aleatoria
    
    while e <= par.sg.emax && s < par.sg.eworse
        P_perm = randperm(p);       % Permutación de índices
        
        for i = 0:(k_e - 1)
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
            sol(k_iter + 1).Xtr = X_S; 
            sol(k_iter + 1).ytr = y_S; 
            sol(k_iter + 1).idx = S_idx;
            sol(k_iter + 1).g = -d; % Guardamos gradiente estocástico
            sol(k_iter + 1).ng = norm(d);
            sol(k_iter + 1).d = d;
            sol(k_iter + 1).al = alpha_k;
            sol(k_iter + 1).AC = "SGM";
            
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
    % ========================================================
    % GM, BFGS, NM, etc. (Tu código original modificado ligeramente)
    % ========================================================
    while norm(g(x)) > par.epsG && k < par.maxiter && (ldescent || par.isd == 4)
        if par.isd == 1
            % GM
            d = -g(x);
        elseif par.isd == 3
            % BFGS
            if k == 1
                H = eye(n);
            else
                s_step = sol(k-1).al * sol(k-1).d;  
                y_step = g(x) - sol(k-1).g;         
                if y_step'*s_step > 1e-10
                    rho = 1 / (y_step' * s_step);
                    I = eye(n);
                    H_prev = sol(k-1).H;
                    H = (I - rho * s_step * y_step') * H_prev * (I - rho * y_step * s_step') + rho * (s_step * s_step');
                else
                    H = sol(k-1).H; 
                end
            end
            d = -H * g(x);
            sol(k).H = H;
        elseif par.isd == 4
            % NM 
            sol(k).H = h(x);
            d = -sol(k).H \ g(x);
        elseif par.isd == 5
            % MNM-SD 
            [V, D] = eig(h(x)); 
            lam = diag(D);
            lam(lam < par.delta) = par.delta; 
            B = V * diag(lam) * V';
            sol(k).H = B;
            d = -B \ g(x);
        elseif par.isd == 6
            % MNM-CMI
            min_eig = min(real(eig(h(x))));
            if min_eig >= par.delta
                tau = 0;
            else
                tau = par.delta - min_eig;
            end
            B = h(x) + tau * eye(n);
            sol(k).tau = tau;
            sol(k).H = B;
            d = -B \ g(x);
        end
        ldescent = d'*g(x) < 0;
        
        % LS
        if par.isd == 4 
            al = 1;
            ACout = "";
        elseif par.iAC == 0
            al = fminbnd(@(alpha) f(x + alpha*d), 0, par.almax);
            ACout = "ELS";
        elseif par.iAC >= 1      % BLS.
            [al,ACout] = uoBLS_st(x,d,P,par);
        end
        
        sol(k).x  = x;
        sol(k).g  = g(x);
        sol(k).ng = norm(g(x));
        sol(k).d  = d;
        sol(k).al = al;
        sol(k).AC = ACout;
        x = x + al*d; 
        k = k + 1;
    end
end

% Cerrar iteración final
sol(k).x = x;

% Iterations log
%[sol] = uosolLog(P,par,sol);

end
% [end] Function [uosol_st] %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

