"""
    In this file are the functions, that are needed to evalute
 the calculations in the rHMM code correctly.
 """

# Required packages:
using LinearAlgebra
# using Plots
# using CSV
# using DelimitedFiles
using Distributions
# using DataFrames
using Optim
using Random

"""
    logspacesum(a, b)
Calculate  `log(x+y)`, where `log(x)=a` and `log(y)=b`.

"""
function logspacesum(a, b)
    m = max.(a, b)
    ruckgabe = m + log1p.(exp.(min.(a, b) - m))
    i = findall(isequal(-Inf), m)
    if (length(ruckgabe) == 1 & length(i) == 1)
        ruckgabe = -Inf
    else
        for j = 1:length(i)
            index = i[j]
            ruckgabe[index] = -Inf
        end
    end
    return ruckgabe
end


"""
    logspacedif(a, b)

Calculate log(x-y), where log(x)=a and log(y)=b.
"""
function logspacedif(a, b)
    b <= a || error("b must be smaller than a")

    ruckgabe = a + log1p.(-exp.(b - a))

    #wenn a=b ist exp=1 und lop1p=-Inf -> ruckgabe=-Inf
    i = findall(isequal(-Inf), b)
    if (length(ruckgabe) == 1 & length(i) == 1)
        ruckgabe = a
    else
        for j = 1:length(i)
            index = i[j]
            ruckgabe[index] = a[index]
        end
    end
    return ruckgabe
end

"""
    logsum(x::Vector)

Calculate log(sum(pi)), where log(pi)=xi
"""
function logsum(x::Vector)
    (wert, index) = findmax(x[:, 1])
    if (wert == -Inf)
        return -Inf
    else
        e = ones(1, length(x))
        e[index] = 0
        return wert + log1p(dot(e, exp.(x .- wert)))
    end
end


"""
    logsum(x::Array)

Calculate logsum for each row
# Arguments
- `x::Array`: xij=log pij
# Return
- log(summe pi)j
"""
function logsum(x::Array)
    (n, m) = size(x)
    (values, index) = findmax(x; dims = 2)
    div = exp.(x .- values)
    ruckgabe = values + log1p.(sum(div, dims = 2) .- ones(n, 1))
    i = findall(isequal(-Inf), values)
    for j = 1:length(i)
        ruckgabe[i[j]] = -Inf
    end
    return ruckgabe
end
