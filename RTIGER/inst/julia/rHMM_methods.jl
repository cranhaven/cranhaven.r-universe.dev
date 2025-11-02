"""
    This file contains all methods, which are part of the parameter estimation
"""

## Calculation of the emission probability psi

"""
    getlogpsi(observation::Array, alpha, beta)
Calculation the logarithm of the psi matrix.
# Arguments
- `observations::Array`: One observation sequenz saved as a matrix with two columns;
    Each line has the form [k n], where k is the count of the reference
    allele and n all counts.
- `alpha`: The alpha parameter of the BetaBinomial distribution;
    A vector whose length is the number of different states.
- `beta`: The beta parameter of the BetaBinomial distribution;
    A vector whose length is the number of different states.
# Return
- logpsi: The logarithm of the emission probability for each state and each point of time;
    A Matrix: the rows are corresponding to the states,
    the colums are corresponding to the point of time,
    so logpsi(i,j)=log(psi_i(k_j,n_j))), the log of the probability to be at time j in state i
"""
function getlogpsi(
    observations::Array{Int64},
    a = [0.1; 2; 1.9],
    b = [1.9; 2; 0.1],
)
    T = size(observations)[1]
    k = observations[:, 1]
    n = observations[:, 2]
    s = length(a)
    logpsi = zeros(s, T)
    for i = 1:s
        logpsi[i, :] = logpdf.(BetaBinomial.(n, a[i], b[i]), k)
    end
    return logpsi
end

"""
    productpsi(logpsi::Array,rigidity::Integer)
Calculation of log(product of psi)
# Arguments
- `logpsi::Array`: A matrix nstates × T, where psi(i,j)=log(ψ_i(o_j))
- `rigidity::Integer`: rigidity factor of the model
# Return
- A Matrix of the size nstates × (T+rigidity), where PSI(i,j) is the logarithm
    of the product of emission probability of observation j-r+1 to observation j in state i.

"""
function productpsi(psi::Array, r::Integer)
    # number of states and observations
    (k, T) = size(psi)
    PSI = zeros(k, T + r)
    PSI[:, 1] = psi[:, 1]
    for t = 2:r
        PSI[:, t] = PSI[:, t-1] + psi[:, t]
    end
    for t = r+1:T
        PSI[:, t] = PSI[:, t-1] + psi[:, t] - psi[:, t-r]
    end
    for t = T+1:T+r-1
        PSI[:, t] = PSI[:, t-1] - psi[:, t-r]
    end
    return PSI
end


## Forward
"""
    forward(T::Integer,rigidity::Integer,nStates::Integer,logPI::Array,logPSI::Array,logA::Array,logpsi::Array)
Calculation of the forward algorithm as a part of the Baum-Welch algorithm (EM-algorithm)
# Arguments
- `logPI::Array`: logarithm of the start probalilities
- `logA::Array`: logarithm of the transisition probabilities
- `logpsi::Array`: logarithm of the emission distribution
- `logPSI::Array`: summed up values of logpsi
# Return
- alpha: A matrix with the logarithm of the forward probabilities, where the row refers to the state
    and the column to the point of the observation
"""
function forward(
    T::Integer,
    r::Integer,
    s::Integer,
    logPI::Array,
    logPSI::Array,
    logA::Array,
    logpsi::Array,
)
    alpha = -Inf * ones(s, T)
    alpha[:, r] = logPI + logPSI[:, r]
    b = diag(logA)
    B = logA - Diagonal(logA) - Inf * Diagonal(vec(ones(s, 1)))
    for t = r+1:2*r-1
        alpha[:, t] = logpsi[:, t] + b + alpha[:, t-1]
    end
    for t = 2*r:T-r+1
        alpha[:, t] = logspacesum(
            logpsi[:, t] + b + alpha[:, t-1],
            logPSI[:, t] + logsum(Array((B .+ alpha[:, t-r])')),
        )
    end
    return alpha

end

## Backward
"""
    backward(T::Integer, rigidity::Integer, nStates::Integer, logPSI::Array, logA::Array, logpsi::Array)
Calculation of the backward algorithm as a part of the Baum-Welch algorithm (EM-algorithm)
# Arguments
- `logA::Array`: logarithm of the transisition probabilities
- `logpsi::Array`: logarithm of the emission distribution
- `logPSI::Array`: summed up values of logpsi
# Return
- beta: A matrix with the backward probabilities, where the row refers to the state
    and the column to the point of the observation
"""
function backward(
    T::Integer,
    r::Integer,
    s::Integer,
    logPSI::Array,
    logA::Array,
    logpsi::Array,
)
    beta = -Inf * ones(s, T)
    beta[:, (T-r+1):T] = logPSI[:, (T+1):(T+r)]
    b = diag(logA)
    B = logA - Diagonal(logA) - Inf * Diagonal(vec(ones(s, 1)))
    for i = 0:T-2*r
        t = T - r - i
        beta[:, t] = logspacesum(
            logpsi[:, t+1] + b + beta[:, t+1],
            logsum(B .+ (logPSI[:, t+r] + beta[:, t+r])'),
        )
    end
    return beta
end

## Zeta

"""
    zeta(logalpha::Array, logbeta::Array, logA::Array, logPSI::Array, logpsi::Array, rigidity::Integer)
Part of the expectation step,
Calculation of the zeta values: zeta(t,i,j) is the probability of the observations,
 the last r states are state i and the current state t is state j, given the parameters of the model.
 The return is a normalised version of zeta:
 ``\\tilde{\\zeta} =\\frac{\\zeta}{P(O)}``
# Arguments
- `logalpha::Array`: result of the forward function
- `logbeta::Array`: result of the backward function
- `logA::Array`: logarithm of the transisition probabilities
- `logPSI::Array`: summed up values of logpsi
- `logpsi::Array`: logarithm of the emission distribution
# Return
- A three dimensional Matrix with size T×nStates×nStates, where `` return(t,i,j)=P(s_{[t-r,r-1]}=i,s_t=j|θ_r,O)``
    the returned values are not in log space.

"""
function zeta(
    logalpha::Array,
    logbeta::Array,
    logA::Array,
    logPSI::Array,
    logpsi::Array,
    r::Integer,
)
    (s, T) = size(logalpha)
    zeta = -Inf * ones(T, s, s)
    #Calculation of the logarithm of the zeta values, exactly according to the equations of the paper
    for j = 1:s
        for k = 1:s
            if j != k
                zeta[r+1:T-r+1, j, k] =
                    logalpha[j, r:T-r] .+ logA[j, k] .+ logbeta[k, 2*r:T] .+
                    logPSI[k, 2*r:T]
            else
                zeta[r+1:T-r+1, j, k] =
                    logalpha[k, r:T-r] .+ logA[k, k] .+ logpsi[k, r+1:T-r+1] .+
                    logbeta[k, r+1:T-r+1]
            end
        end
    end
    #Normalisation and exponential
    PO = maximum(zeta)
    return exp.(zeta .- PO)
end

## Gamma
"""
    gamma(zeta::Array, logalpha::Array, logbeta::Array, rigidity::Integer)
Part of the expectation step,
Calculation of the gamma values: `` γ(k,t)=P(s_t=k,O|θ_r)``
# Arguments
- `zeta::Array`: result of the zeta function
- `logalpha::Array`: result of the forward function
- `logbeta::Array`: result of the backward function
"""
function gamma(
    zeta::Array,
    logalpha::Array,
    logbeta::Array,
    rigidity::Integer,
    sample=0,
    chromosom=0,
    iteration=1,
)
    T = size(zeta)[1]
    nstates = size(zeta)[2]
    gam = zeros(nstates, T)

    # In addition to the calculations in the paper, we have to normalise the gamma values
    gammar = logalpha[:, rigidity] + logbeta[:, rigidity]
    gammar = gammar .- maximum(gammar)
    gammar = exp.(gammar)
    gammar = gammar ./ sum(gammar)
    gam[:, 1:rigidity] = repeat(gammar, 1, rigidity)
    for k = 1:nstates
        z = zeta[:, :, k] #Save the zeta values for alle observation points and for all states
        i = findall(!isequal(k), 1:nstates)
        z = z[:, i]
        L = sum(z, dims = 2)
        M = cumsum(L, dims = 1)
        Mminus = verschobenGamma(M, rigidity)
        gam[k, rigidity+1:T-rigidity+1] =
            zeta[rigidity+1:T-rigidity+1, k, k] + M[rigidity+1:T-rigidity+1] -
            Mminus[rigidity+1:T-rigidity+1]
    end
    gam[:, T-rigidity+2:T] = repeat(gam[:, T-rigidity+1], 1, rigidity - 1)
    gam = gam ./ sum(gam, dims = 1)
    if any(x -> x < 0, gam)
        error(
            string(
                "There is a negative Gamma Value in sample: ",
                sample,
                " Chr: ",
                chromosom,
                " .Occured in iteration ",
                iteration,
            ),
            findall((x -> x < 0), gam),
        )
    end
    return gam
end

"""
    verschobenGamma(M::Array, r::Integer)
An auxilary function for the calculation of the gamma values.#
 The values of M are shifted back by r position, so that you get the sum
 of the last r zeta values dy substraction of the not shifted and shifted vectors.
"""
function verschobenGamma(M::Array, r::Integer)
    T = length(M)
    A = zeros(T, 1)
    A[1:r] = M[1:r]
    A[r+1:2r] = repeat([M[r]], 1, r)
    A[2r+1:T] = M[r+1:T-r]
    return A
end

## Update Transition and Start Probability
"""
    transitionMultiple(zeta::Array, rigidity::Integer, nstates::Integer)
Part of the maximization step; Update of the transition probabilities
# Arguments
- `zeta::AbstractDict`: Dict with the zeta values of all samples and all chromosoms,
    the structur is list of the samples with lists of the chromosoms with three
     dimensional arrays for the zeta values
- `rigidity::Integer`: the rigidity faktor
- `nstates::Integer`: the number of different states
# Return
- Matrix with size nstates × nstates containing the transition probabilities,
    so result(i,j) is the probability to have a transition from state i to state j
"""
function transitionMultiple(
    zeta::AbstractDict,
    rigidity::Integer,
    nstates::Integer,
)
    sumZeta = Array{Float64}(undef, 1, nstates, nstates)
    for s in keys(zeta)
        for c in keys(zeta[s])
            aktuell = zeta[s][c]
            T = size(aktuell)[1]
            za = sum(aktuell[rigidity+1:T-rigidity+1, :, :], dims = 1)
            sumZeta = [sumZeta; za]
        end
    end
    sumSamples = sum(sumZeta[2:size(sumZeta)[1], :, :], dims = 1)[1, :, :]
    # Catch out: all transitions equal zero
    summedSumSamples=sum(sumSamples, dims = 2)
    sTzeros=findall(iszero,summedSumSamples)
    if length(sTzeros)>0
        @info "One row of transition probabilities is completely zero, there might be not enough information for one state."
        for indexZ in sTzeros
            summedSumSamples[indexZ]=1
        end
    end
    A = sumSamples ./ summedSumSamples
    # Check:
    !any((x -> ((x < 0) | (x > 1))), A) ||
        error("Some transition probability is not in the range of 0 to 1.")
    return A
end

"""
    startMultiple(gamma::AbstractDict, nstates::Integer)
Part of the maximization step; Update of the start probabilities
# Arguments
- `gamma::AbstractDict`: Dict with the gamma values of all samples and all chromosoms,
    the structur is a list of the samples with lists of the chromosoms #
    with a matrix containing the gamma values (result of the gamma function)
- `nstates::Integer`: the number of different states
# Return
- A vector with the start probability for each state
"""
function startMultiple(gamma::AbstractDict, nstates::Integer)
    start = zeros(nstates, 1)
    nOb = 0
    for s in keys(gamma)
        for c in keys(gamma[s])
            start = start + gamma[s][c][:, 1]
            nOb += 1
        end
    end
    start = start ./ nOb
    !any(x -> x < 0, start) ||
        error("Some start probability is smaller than one.")
    !any(x -> x > 1, start) ||
        error("Some start probability is greater than one.")
    return start
end



## Update Parameter Betabinomialverteilung
"""
    emissionMultiple(O::AbstractDict,gamma::AbstractDict; nstates::Integer = 3, alpha_old = [0.1; 1; 1.9], beta_old = [1.9; 1; 0.1])
Part of the maximization step; Update of the emission distributions
# Arguments
- `O::AbstractDict`: a Dict with the observations
- `gamma::AbstractDict`: a Dict with the gamma values, `gamma` should have
    same structure as `O` (Dict with Dict with values)
# Keywords
- `nstates::Integer = 3`: number of different states, default value is 3
- `alpha_old`: old values of the alpha parameter of the BetaBinomial distribution
- `beta_old`: old values of the beta parameter of the BetaBinomial distribution,
    default values correspond to a first guess for three states: 1->mat,2->het,3->pat

"""
function emissionMultiple(
    O::AbstractDict,
    gamma::AbstractDict;
    nstates::Integer = 3,
    alpha_old = [0.1; 1; 1.9],
    beta_old = [1.9; 1; 0.1],
)
    m = zeros(nstates, 1)
    tau = zeros(nstates, 1)
    anew = zeros(nstates, 1)
    bnew = zeros(nstates, 1)
    for i = 1:nstates
        m[i] = empmean(O, gamma, i)
        if isnan(m[i])
            m[i] = alpha_old[i] / (alpha_old[i] + beta_old[i])
            anew[i] = alpha_old[i]
            bnew[i] = beta_old[i]
            tau[i] = (alpha_old[i] / m[i] + beta_old[i] / (1 - m[i]))
        else
            m[i] >= 0 || error(string(
                "The mean of the Beta Binomial distribution must be greater then 0. Occured in state ",
                i,
                ". m= ",
                m[i],
            ))
            m[i] <= 1 || error(string(
                "The mean of the Beta Binomial distribution must be smaller then 1. Occured in state ",
                i,
            ))
            if m[i] < 0.01
                m[i] = 0.01
            end
            if m[i] > 0.99
                m[i] = 0.99
            end
            tau[i] = (alpha_old[i] / m[i] + beta_old[i] / (1 - m[i]))
            if tau[i] > 100
                tau[i] = 100
            end
            Q(t) = expMultiple(t, gamma, O, m[i], i)
            res = optimize(
                t -> -Q(first(t)),
                max(0, tau[i] - 100),
                max(tau[i] + 1, 100),
                [tau[i]],
            )
            tau[i] = Optim.minimizer(res)[1]
            anew[i] = tau[i] * m[i]
            bnew[i] = tau[i] * (1 - m[i])
        end
    end
    return anew, bnew, m, tau
end

"""
    empmean(O::AbstractDict, gamma::AbstractDict, state::Integer)
Auxilary function for the update of the emission distribution;
    Calculation of the gamma weighted empirical mean of the observations
# Arguments
- `O::AbstractDict`: Dict with the observations
- `gamma::AbstractDict`: Dict with the gamma values
- `state::Integer`: current state
# Return
- The gamma weighted empirical mean
"""
function empmean(O::AbstractDict, gamma::AbstractDict, state::Integer)
    nS = length(O)
    nom = zeros(1, 1)
    denom = zeros(1, 1)
    for s in keys(O)
        for c in keys(O[s])
            Oc = O[s][c]
            gammac = gamma[s][c]
            k = Oc[:, 1]
            n = Oc[:, 2]
            za = sum(k .* gammac[state, :])
            ne = sum(n .* gammac[state, :])
            nom = [nom; za]
            denom = [denom; ne]
        end
    end
    nom = nom[2:size(nom)[1], :]
    denom = denom[2:size(denom)[1], :]
    m = sum(nom) / sum(denom)
    return m
end

"""
    expMultiple(t, G::AbstractDict, Ob::AbstractDict, m, state::Intger)
Auxilary function for the update of the emission distribution;
    the function to be maximized
# Arguments
- `t`: the depedent variable
- `G::AbstractDict`: a Dict with the gamma values
- `Ob::AbstractDict`: a Dict with the observations
- `m`: the gamma weighted empirical mean (result of the empmean function)
- `state::Integer`: the current state
"""
function expMultiple(t, G::AbstractDict, Ob::AbstractDict, m, state::Integer)
    zwe = zeros(1, 1)
    for s in keys(Ob)
        for c in keys(Ob[s])
            Oc = Ob[s][c]
            gammac = G[s][c]
            k = Oc[:, 1]
            n = Oc[:, 2]
            zw = sum(
                gammac[state, :] .*
                logpdf.(BetaBinomial.(n, t * m, t * (1 - m)), k),
            )
            zwe = [zwe; zw]
        end
    end
    return sum(zwe)
end


## Viterbi Algorithmus
"""
    viterbi(PI::Array, PSI::Array, psi::Array, A::Array, r::Integer)
The viterbi algorithm for one observation sequence
# Arguments
- `PI::Array`: Vector with the logarithm of the start probabilities
- `PSI::Array`: Matrix with the logarithm of the product of the emission probabilities
        (equation 6);
        rows refer to the states, colums refer to the point of time
- `psi::Array`: Matrix with the logarithm of the emission propabilities;
    rows refer to the states, colums refer to the point of time
- `A::Array`: Matrix with the logarithm of the transition probabilities
- `r::Integer`: Rigidity
# Return
- `phi,b,v`
- `phi`: maximum of the probability of the previous observation and state sequence
    (equation 25)
- `b`: backward pointers corresponding to `phi`
- `v`: the r-rigid viterbi path
"""
function viterbi(PI::Array, PSI::Array, psi::Array, A::Array, r::Integer)

    a = diag(A)
    (s, T) = size(psi)
    phi = -Inf*ones(s,T)
    b = Array{Int}(undef, s, T)
    phi[:, r] = PSI[:, r] .+ PI
    b[:, 1:r] = repeat(collect(1:s), 1,r)
    # for t=r+1:2*r
    #     phi[:,t]= psi[:, t] + a + phi[:, t-1]
    # end
    for t = r+1:T
        d = psi[:, t] + a + phi[:, t-1]
        nd = A .+ PSI[:, t]' .+ phi[:, t-r]
        indexInf = findall(x -> x == -Inf, Diagonal(nd))
        if length(indexInf) > 0
            for i in indexInf
                nd[i] = 0
            end
        end
        nd = nd - Diagonal(nd)
        nd = nd + Diagonal(d)
        (p, i) = findmax(nd, dims = 1)
        phi[:, t] = p'
        for j = 1:s
            b[j, t] = i[j][1]
        end
    end
    v = Vector{Int}(undef, T)
    v[T] = findmax(phi[:, T])[2]
    t = T
    while t > 1
        pointer = b[v[t], t]
        if pointer != v[t]
            if r>1
                v[max(t - r+1, 1):t-1] .= v[t]
                # v[max(t - r, 1):t-1] .= pointer
                t = t - r+1
                if t < 2
                    break
                end
            end
        end
        v[t-1] = pointer
        t = t - 1
        if t < 2
            break
        end
        # if pointer!=v[t]
        #     v[max(t-r,1):t-1].=pointer
        #     t=t-r
        # else
        #     v[t-1]=pointer
        #     t=t-1
        # end
        # if t<2
        #     break
        # end

    end

    return phi, b, v
end


# Viterbi mit mehreren Beobachtungssequenzen
"""
    viterbi(Observations::AbstractDict, logParameter::AbstractDict)
Call of the viterbi algorithm for all observation chains
# Arguments
- `Observations::AbstractDict`: List of lists with the observation chains
- `logParameter::AbstractDict`: List with the parameter
# Return:
- A list with the same structure and keywords as the observation list
    containing the viterbi path for each observation chain
"""
function viterbi(Observations::AbstractDict, logParameter::AbstractDict)
    O = Observations
    #reading of the parameter
    logStart = logParameter[:logpi]
    logTransition = logParameter[:logtransition]
    nstates = logParameter[:nstates]
    rigidity = logParameter[:rigidity]
    alpha = logParameter[:paraBetaAlpha]
    beta = logParameter[:paraBetaBeta]
    v = Dict()
    pointer = Dict()
    for s in keys(O)
        v[s] = Dict()
        pointer[s] = Dict()
        for c in keys(O[s])
            OChr = O[s][c]
            # Calculation of the emission probabilities with the BetaBinomial distribution
            logpsi = getlogpsi(OChr, alpha, beta)
            logPSI = productpsi(logpsi, rigidity)
            # Calculation of the viterbi path for the chosen observation chain
            resViterbi =
                viterbi(logStart, logPSI, logpsi, logTransition, rigidity)
            v[s][c] = resViterbi[3]
            #pointer[s][c]=resViterbi[2]
        end
    end
    return v
end


## EM Schritt
"""
    EM(Observations::AbstractDict, logParameter::AbstractDict)
One call of the expectation and maximization step
# Arguments
- `Observations::AbstractDict`: List of lists with the observation chains
- `logParameter::AbstractDict`: List with the old parameters
# Return
- `(G, Anew, PInew, a, b, alpha, beta, psi)`
- `G`: list of the gamma values for each observation chain
- `Anew`: new matrix with the transition probabilities
- `PInew`: new start probabilities
- `a`: new alpha parameter of the BetaBinomial distribution
- `b`: new beta parameter of the BetaBinomial distribution
- `alpha`: list with the results of the forward algorithm for each observation chain
- `beta`: list with the results of the backward algorithm for each observation chain
- `psi`: list with psi values for each observation chain
"""
function EM(Observations::AbstractDict, logParameter::AbstractDict, iteration, printbool)
    O = Observations
    logAnfang = logParameter[:logpi]
    logTransition = logParameter[:logtransition]
    nstates = logParameter[:nstates]
    rigidity = logParameter[:rigidity]
    aAlt = logParameter[:paraBetaAlpha]
    bAlt = logParameter[:paraBetaBeta]

    # List for the zeta values
    Z = Dict()
    # list for the gamma values
    G = Dict()
    # list for the results of the forward algorithm
    alpha = Dict()
    # list for the results of the backward algorithm
    beta = Dict()
    # list of the logarithm of the psi values
    psi = Dict()

    if printbool
        d=open("debugInfo.txt","a")
        write(d,string("sample chromosome psi forward backward zeta gamma\n"))
        close(d)
    end
    for c in keys(O)
        Oc = O[c]
        Z[c] = Dict()
        G[c] = Dict()
        alpha[c] = Dict()
        beta[c] = Dict()
        psi[c] = Dict()
        for i in keys(Oc)
            if printbool
                d=open("debugInfo.txt","a")
                write(d,string(c," ",i," "))
                close(d)
                start=time()
            end
            OChr = Oc[i]
            Tc = size(OChr)[1]
            # Calculation of the psi values with the BetaBinomial distribution
            logpsi = getlogpsi(OChr, aAlt, bAlt)
            logPSI = productpsi(logpsi, rigidity)
            if printbool
                t=time()-start
                d=open("debugInfo.txt","a")
                write(d,string(t," "))
                close(d)
                start=time()
            end
            # Calculation of alpha, beta, zeta and gamma for each observation chain
            alphac = forward(
                Tc,
                rigidity,
                nstates,
                logAnfang,
                logPSI,
                logTransition,
                logpsi,
            )
            if printbool
                t=time()-start
                d=open("debugInfo.txt","a")
                write(d,string(t," "))
                close(d)
                start=time()
            end
            betac =
                backward(Tc, rigidity, nstates, logPSI, logTransition, logpsi)
            if printbool
                t=time()-start
                d=open("debugInfo.txt","a")
                write(d,string(t," "))
                close(d)
                start=time()
            end
            zetac = zeta(alphac, betac, logTransition, logPSI, logpsi, rigidity)
            if printbool
                t=time()-start
                d=open("debugInfo.txt","a")
                write(d,string(t," "))
                close(d)
                start=time()
            end
            gammac = gamma(zetac, alphac, betac, rigidity, c, i, iteration)

            if printbool
                t=time()-start
                d=open("debugInfo.txt","a")
                write(d,string(t,"\n"))
                close(d)
                start=time()
            end
            # save in the global lists
            Z[c][i] = zetac
            G[c][i] = gammac
            alpha[c][i] = alphac
            beta[c][i] = betac
            psi[c][i] = logpsi

        end
    end

    Anew = transitionMultiple(Z, rigidity, nstates)
    PInew = startMultiple(G, nstates)
    if printbool
        t=time()-start
        d=open("debugInfo.txt","a")
        write(d,string("transition and start update took ",t," seconds.\n"))
        close(d)
        start=time()
    end
    (a, b, m, tau) = emissionMultiple(
        O,
        G,
        nstates = nstates,
        alpha_old = logParameter[:paraBetaAlpha],
        beta_old = logParameter[:paraBetaBeta],
    )
    if printbool
        t=time()-start
        d=open("debugInfo.txt","a")
        write(d,string("emission update took ",t," seconds.\n"))
        close(d)
    end


    return (G, Anew, PInew, a, b, alpha, beta, psi, m, tau)
end

## postprocessing methods

"""
    postprocessing(observation::AbstractDict,viterbi::AbstractDict,
        parameter::AbstractDict)
Adjusts the borders of short segements (≤ 1.2*rigdity)
# Arguments
- `observation::AbstractDict`: List of lists with the observation chains
- `viterbi::AbstractDict`: List of lists with the viterbi path for each observation chain
- `parameter::AbstractDict`: List with the parameter
# Return
- A List with the same structure as viterbi containing the post processed
    viterbi path for each observation chain
"""
function postprocessing(
    observation::AbstractDict,
    viterbi::AbstractDict,
    parameter::AbstractDict,
)
    # ri = parameter[:rigidity]
    # r = round(Int, 2 * ri, RoundUp)
    postviterbi = Dict()
    changes = []
    for sample in keys(viterbi)
        postviterbi[sample] = Dict()
        for chr in keys(viterbi[sample])
            vit = viterbi[sample][chr]
            seg = segmente(vit)
            # crit = findall(x -> x < r, seg[2:size(seg)[1], 3])
            # if length(crit) > 0
            ob = observation[sample][chr]
            psi = getlogpsi(
                ob,
                parameter[:paraBetaAlpha],
                parameter[:paraBetaBeta],
            )
            # for i in crit
            for i in 1:size(seg)[1]-1
                a = 0
                # if i != 1 & !in(i - 1, crit)
                #     # Left border
                #     decision = psi[:, seg[i, 1]:seg[i+1, 2]]
                #     a = findsplit(seg[i, 4], seg[i+1, 4], decision)
                #     # shift of the left border
                #     seg[i, 2] = Int(seg[i, 1] + a - 2)
                #     seg[i, 3] = Int(a-1)
                #     seg[i+1, 1] = Int(seg[i, 1] + a-1)
                #     seg[i+1, 3] = Int(seg[i+1, 2] - seg[i+1, 1] + 1)
                # end
                if i != size(seg)[1] - 1
                    # Right Border
                    decision = psi[:, seg[i+1, 1]:seg[i+2, 2]]
                    e = findsplit(seg[i+1, 4], seg[i+2, 4], decision)
                    #shift of the right border
                    seg[i+1, 2] = Int(seg[i+1, 1] + e - 2)
                    seg[i+1, 3] = Int(seg[i+1, 2] - seg[i+1, 1] + 1)
                    seg[i+2, 1] = Int(seg[i+1, 2] + 1)
                    seg[i+2, 3] = Int(seg[i+2, 2] - seg[i+2, 1] + 1)
                end
            end
            postviterbi[sample][chr] = getVitSeg(seg)
            push!(changes, string("sample: ", sample, " in chromosome: ", chr))
            # else
            #     postviterbi[sample][chr] = vit
            # end
        end
    end
    return postviterbi #, changes
end


"""
    segmente(b::Vector)
Calculation of the segments (consecutive same states) of one state sequence
# Arguments
- `b::Vector`: state sequence as a vector
# Return
- A Matrix where the first row is a headline, the first column saves the start,
    the seconde the end, the third the length and the fourth the state of the
    belonging segment. So row i refers to the i-1 segment.
"""
function segmente(b::Vector)
    s = ["start" "ende" "length" "state"]
    start = 1
    state = b[1]
    for i = 2:length(b)
        if b[i] != state
            stop = i - 1
            s = [s; start stop stop - start + 1 state]
            start = i
            state = b[i]
        end
    end
    s = [s; start length(b) length(b) - start + 1 state]
    return s
end

"""
    findsplit(left_state::Integer, right_state::Integer, decisionmat)
Calculation of the most likely split position
# Arguments
- `left_state::Integer`: the left state of the considered part
- `right_state::Integer`: the right state of the considered part
- `decisionmat`: matrix with the emission probability of each state for the
    positions, that effect the new splitposition. This is the psi-Matrix for
    the left and right segment.
# Return
- Index of the new split position
"""
function findsplit(left_state, right_state, decisionmat)
    leftsum = cumsum(decisionmat[left_state, :])
    rightsum = reverse(cumsum(reverse(decisionmat[right_state, :])))
    results = [0; leftsum] + [rightsum; 0]
    return findmax(results)[2]
end

"""
    getVitSeg(segmente)
Formation of the viterbi path according to the segments
# Arguments
- `segemente`: A Matrix, that contains all information of the segments. Row one
    is a headline, the first column refers to the start index, the second to
    the end index, the third to the length and the fourth to the state of
    the segment
# Return
- the viterbi path as a vector
"""
function getVitSeg(segmente)
    anzahl = size(segmente)[1] - 1
    v = Vector{Int}(undef, segmente[anzahl+1, 2])
    for i = 1:anzahl
        v[segmente[i+1, 1]:segmente[i+1, 2]] .= segmente[i+1, 4]
    end
    return v
end

"""
    fit(input_Observations,info,initial_parameter, max_iter = 100, eps = 10^(-5),
        trace = false,all = true,random = true,specific = nothing)
# Arguments
- `input_Observations`: list with an entry for each sample, this entry is a
    list with an entry for each chromosom, this entry is the observation chain
    saved as a matrix with two columns;
    Each line has the form ``[k n]``, where ``k`` is the count of the reference
    allele and ``n`` all counts.
- `info`: Information of the model, not used
- `initial_parameter`: first guess of the parameters of the model
- `max_iter=100`: maximum of iterations, default value is 100
- `eps=10^(-5)`: desired accuracy, the function stops if the updated parameter
    of the BetaBinomial distribution change less than `eps`
- `trace=false`: a boolean, if true the function additionaly returns the
    parameter of the BetaBinomial distribution and the transisition probabilities
    of each iteration
- `all=true`: use all observation chains of the input for the fitting
- `random=true`: pick a random supsample for the fitting, `all` has to be `false`,
    random pick of samples, for each chosen sample all chromosomes are used
- `nchromosoms=100`: number of chromosomes that are used for the fitting,
    if a random subsample is chosen
- `specific = nothing`: a vector with the keys of the samples, that should be used
    for the fitting, `all` and `random` have to be false
# Return
- `Dict(:alpha => alpha,:beta => beta,:gamma => Gamma,:parameterSet => parameter,
    :viterbiPath => vit,:psi => psi)`: `:alpha`: results of the forward algorithm,
    `:beta`: results of the backward algorithm, `:gamma`: list with the gamma values,
    `psi`: the logarithm of the psi values, `:parameterSet`: the fitted parameters,
     `:viterbiPath`: the viterbi path for all observation chains of the input observations
"""
function fit(
    input_Observations,
    info,
    initial_parameter,
    max_iter = 100,
    eps = 10^(-5),
    trace = false,
    all = true,
    random = true,
    nsamples = 20,
    specific = nothing,
    post_processing = true,
    DEBUG=false,
)
    if DEBUG
        # display("Start in Julia")
        touch("debugInfo.txt")
        # display(string(pwd()))
        start=time()
    end
    #pick of observations
    if (all)
        Observations = input_Observations
    elseif random
        if nsamples < 1
            error("The number of samples per chromosom must be more than 0.")
        elseif nsamples > length(input_Observations)
            error("The number of samples per chromosom can't be bigger than the number of all samples.")
        end
        Random.seed!(length(input_Observations))
        s = rand(keys(input_Observations))
        count = 0
        Observations = Dict()
        chosensamples = Dict()
        for chr in keys(input_Observations[s])
            randomSamples = sample(
                collect(keys(input_Observations)),
                nsamples,
                replace = false,
            )
            for samp in randomSamples
                if !haskey(Observations, samp)
                    Observations[samp] = Dict()
                    chosensamples[samp] = Vector()
                end
                Observations[samp][chr] = input_Observations[samp][chr]
                push!(chosensamples[samp], chr)
            end
        end
        numberChr = nsamples * length(input_Observations[s])
    elseif specific == nothing
        error("Please specify your choice of samples or take a random supsample.")
    else
        nsample = length(specific)
        Observations = Dict()
        for i = 1:nsample
            s = specific[i]
            if (!haskey(input_Observations, s))
                error("The specification must fit the names of the samples.")
            end
            Observations[s] = input_Observations[s]
        end
    end
    if (DEBUG)
        t=time()-start
        d=open("debugInfo.txt","w")
        write(d,string("Selection of the samples took ", t," seconds.\n 1.EM:\n"))
        close(d)
        start=time()
    end
    # Initial parameters
    parameter = initial_parameter
    nstates = parameter[:nstates]
    rigidity = parameter[:rigidity]
    (Gamma, traNeu, startNeu, aNeu, bNeu, alpha, beta, psi, m, tau) =
        EM(Observations, parameter, 1,DEBUG)
    if (DEBUG)
        t=time()-start
        d=open("debugInfo.txt","a")
        write(d,string("Whole EM step took ", t," seconds.\n"))
        close(d)
        start=time()
    end
    #Change of the parameters of the BetaBinomial distribution
    er = maximum(
        [abs.(parameter[:paraBetaAlpha] - aNeu) abs.(
            parameter[:paraBetaBeta] - bNeu,
        )],
    )
    abbruch = 1
    if trace
        trace_alpha = [parameter[:paraBetaAlpha] aNeu]
        trace_beta = [parameter[:paraBetaBeta] bNeu]
        trace_transition = [exp.(parameter[:logtransition]) traNeu]
        trace_m = [zeros(nstates, 1) m]
        trace_tau = [zeros(nstates, 1) tau]
    end


    #Loop for the fitting
    while er > eps
        if (abbruch >= max_iter)
            break
        end
        abbruch += 1
        if DEBUG
            d=open("debugInfo.txt","a")
            write(d,string(abbruch,". iteration\n"))
            close(d)
        end
        parameter[:paraBetaAlpha] = aNeu
        parameter[:paraBetaBeta] = bNeu
        parameter[:transition] = traNeu
        parameter[:pi] = startNeu
        parameter[:logtransition] = log.(traNeu)
        parameter[:logpi] = log.(startNeu)
        (Gamma, traNeu, startNeu, aNeu, bNeu, alpha, beta, psi, m, tau) =
            EM(Observations, parameter, (abbruch + 1),DEBUG)
        er = maximum(
            [abs.(parameter[:paraBetaAlpha] - aNeu) abs.(
                parameter[:paraBetaBeta] - bNeu,
            )],
        )
        if trace
            trace_alpha = [trace_alpha aNeu]
            trace_beta = [trace_beta bNeu]
            trace_transition = [trace_transition traNeu]
            trace_m = [trace_m m]
            trace_tau = [trace_tau tau]
        end
        # println(aNeu)
        # println(bNeu)


    end
    parameter[:paraBetaAlpha] = aNeu
    parameter[:paraBetaBeta] = bNeu
    parameter[:transition] = traNeu
    parameter[:pi] = startNeu
    parameter[:logtransition] = log.(traNeu)
    parameter[:logpi] = log.(startNeu)

    vit = viterbi(input_Observations, parameter)

    if DEBUG
        start=time()
    end
    if post_processing
        vit_new = postprocessing(input_Observations, vit, parameter)
    end
    if DEBUG
        t=time()-start
        d=open("debugInfo.txt","a")
        write(d,string("Post processing took ",t," seconds.\n"))
        close(d)
    end

    # Returns:
    ret = Dict()
    ret[:alpha] = alpha
    ret[:beta] = beta
    ret[:gamma] = Gamma
    ret[:parameterSet] = parameter
    if post_processing
        ret[:viterbiPath] = vit_new
    else
        ret[:viterbiPath] = vit
    end
    ret[:psi] = psi
    ret[:numberofiterations] = abbruch
    if trace
        ret[:trace_parameter_beta_bin_alpha] = trace_alpha
        ret[:trace_parameter_beta_bin_beta] = trace_beta
        ret[:trace_transition] = trace_transition
        ret[:trace_m] = trace_m
        ret[:trace_tau] = trace_tau
    end
    if !all && random
        ret[:numberchromosomes] = numberChr
        ret[:chosensamples] = chosensamples
    end
    return ret

end

"""
    erstePara( nstates,rigidity,aBeta = [0.25; 0.75],bBeta = [0.75; 0.25],diagdom = 2)
First Guess of the parameters, for runing the Code just in Julia and not from R
"""
function erstePara(
    nstates,
    rigidity;
    aBeta = [0.25; 0.75],
    bBeta = [0.75; 0.25],
    diagdom = 10,
    equalstart=false,
)
    A = 0.1 * ones(nstates, nstates) + diagdom * Diagonal(vec(ones(nstates, 1)))
    A = A + rand(nstates, nstates) ./ 100
    A = A ./ sum(A, dims = 2)
    if equalstart
        pi = 1/nstates*ones(nstates,1)
    else
        pi = rand(nstates, 1)
        pi = pi ./ sum(pi, dims = 1)
    end

    logpara = Dict(
        :logpi => log.(pi),
        :transition => A,
        :logtransition => log.(A),
        :paraBetaAlpha => aBeta,
        :paraBetaBeta => bBeta,
        :nstates => nstates,
        :rigidity => rigidity,
    )
    return logpara
end

function EMdev(logpsi,inital_parameter)
    #reading the input parameter
    parameter=copy(inital_parameter)
    logAnfang = log.(parameter[:pi])
    logTransition = log.(parameter[:transition])
    nstates = parameter[:nstates]
    rigidity=parameter[:rigidity]
    samples=keys(logpsi)
    Z=Dict()
    G=Dict()
    for s in samples
        Z[s]=Dict()
        G[s]=Dict()
        for c in keys(logpsi[s])
            lpsi=logpsi[s][c]
            if size(lpsi)[1]!=nstates
                lpsi=Array(lpsi')

            end
            logPSI = productpsi(lpsi, rigidity)
            T=size(lpsi)[2]
            # Calculation of alpha, beta, zeta and gamma
            alpha = forward(
                T,
                rigidity,
                nstates,
                logAnfang,
                logPSI,
                logTransition,
                lpsi,
            )
            beta =
                backward(T, rigidity, nstates, logPSI, logTransition, lpsi)
            zetac = zeta(alpha, beta, logTransition, logPSI, lpsi, rigidity)
            gammac = gamma(zetac, alpha, beta, rigidity)

            #Save in global lists
            Z[s][c]=zetac
            G[s][c]=gammac
        end

    end

    Anew = transitionMultiple(Z, rigidity, nstates)
    PInew = startMultiple(G, nstates)
    parameter[:pi]=PInew
    parameter[:transition]=Anew
    ret=Dict(:gamma=>G,:parameter=>parameter)
    return ret
end
