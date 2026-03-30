## usethis namespace: start
#'
#' @import torch
#'
#' @importFrom gsl hyperg_U
#'
#' @importFrom progress progress_bar
#'
#' @importFrom stats model.response model.matrix model.frame rnorm na.pass delete.response .getXlevels pt rgamma median
#'
#' @importFrom methods formalArgs
#'
#' @importFrom utils packageVersion getFromNamespace
#'
#' @importFrom graphics boxplot
#'
#'
## usethis namespace: end

.onAttach <- function(libname, pkgname) {

  if (!torch::torch_is_installed()) {
    packageStartupMessage("Welcome to shrinkGPR version ", packageVersion("shrinkGPR"),
                          ".\n \nNOTE: No torch installation detected. This package requires torch to function.",
                          "Please install torch by running torch::install_torch()")
  } else {
    if (cuda_is_available()) {
      CUDA_message <- "CUDA installation detected and functioning with torch.
CUDA will be used for GPU acceleration by default."
    } else {
      CUDA_message <- "NOTE: No CUDA installation detected. This may be quite slow for larger datasets.
Consider installing CUDA for GPU acceleration. Information on this can be found at:
https://cran.r-project.org/web/packages/torch/vignettes/installation.html"
    }

    start_message <- paste0("\nWelcome to shrinkGPR version ", packageVersion("shrinkGPR"),
                            ".\n \n", CUDA_message)

    packageStartupMessage(start_message)
  }
}


# Create holding environment for JIT compiled code
.shrinkGPR_internal <- new.env(parent = emptyenv())

.onLoad <- function(libname, pkgname) {
  if (torch::torch_is_installed()) {
    .shrinkGPR_internal$jit_funcs <- jit_compile("
def sylvester_full(
    z: torch.Tensor,
    Q_param: torch.Tensor,
    r1: torch.Tensor,
    r2: torch.Tensor,
    b: torch.Tensor,
    diag1: torch.Tensor,
    diag2: torch.Tensor,
    n_householder: List[int]
) -> Tuple[torch.Tensor, torch.Tensor]:
    Q_t = Q_param.transpose(0, 1)
    norms = torch.norm(Q_t, p=2, dim=0, keepdim=True)
    V = Q_t / norms
    d = V.size(0)
    Q = torch.eye(d, device=z.device)
    for i in range(n_householder[0]):
        v = V[:, i].unsqueeze(1)
        H = torch.eye(d, device=z.device) - 2 * torch.mm(v, v.t())
        Q = torch.mm(Q, H)
    rqzb = torch.matmul(r2, torch.matmul(Q.t(), z.t())) + b.unsqueeze(1)
    h_rqzb = torch.tanh(rqzb)
    zk = z + torch.matmul(Q, torch.matmul(r1, h_rqzb)).t()
    diag_j = diag1 * diag2
    h_deriv = 1.0 - h_rqzb.pow(2)
    diag_j = h_deriv.t() * diag_j
    diag_j = diag_j + 1.0
    log_diag_j = torch.log(torch.abs(diag_j)).sum(dim=1)
    return zk, log_diag_j


def sqdist(
    x: torch.Tensor,
    thetas: torch.Tensor,
    x_star: Optional[torch.Tensor]
) -> torch.Tensor:

    X_thetas = x.unsqueeze(2) * torch.sqrt(thetas.transpose(0, 1))
    sq = torch.sum(X_thetas ** 2, dim=1, keepdim=True)

    if x_star is None:
        sqdist = ((sq + sq.permute(1, 0, 2)).permute(2, 0, 1) -
          2 * torch.bmm(X_thetas.permute(2, 0, 1), X_thetas.permute(2, 1, 0)))
    else:
        X_star_thetas = x_star.unsqueeze(2) * torch.sqrt(thetas.transpose(0, 1))
        sq_star = torch.sum(X_star_thetas ** 2, dim=1, keepdim=True)
        sqdist = ((sq_star + sq.permute(1, 0, 2)).permute(2, 0, 1) -
          2 * torch.bmm(X_star_thetas.permute(2, 0, 1), X_thetas.permute(2, 1, 0)))

    return sqdist

def ldnorm(
    K: torch.Tensor,
    sigma2: torch.Tensor,
    y: torch.Tensor,
    x_mean: Optional[torch.Tensor],
    beta: Optional[torch.Tensor],
) -> torch.Tensor:

    B, N, _ = K.size()
    I = torch.eye(N, device=K.device).unsqueeze(0).expand(B, N, N)
    sigma_term = I * sigma2.view(B, 1, 1)
    K_eps = K + sigma_term

    L = torch.cholesky(K_eps)
    slogdet = 2.0 * torch.sum(torch.log(torch.diagonal(L, dim1=-2, dim2=-1)), dim=1)

    if (beta is not None and x_mean is not None):
        y_demean = (y - torch.matmul(x_mean, beta.transpose(0, 1))).transpose(0, 1).unsqueeze(-1)
        y_batch = y_demean
    else:
        y_batch = y.unsqueeze(0).expand(B, N, 1)


    alpha = torch.cholesky_solve(y_batch, L)
    quad = torch.matmul(y_batch.transpose(1, 2), alpha).squeeze(-1).squeeze(-1)

    log_lik = -0.5 * slogdet - 0.5 * quad
    return log_lik

def ldt(
    K: torch.Tensor,
    sigma2: torch.Tensor,
    y: torch.Tensor,
    x_mean: Optional[torch.Tensor],
    beta: Optional[torch.Tensor],
    nu: torch.Tensor
) -> torch.Tensor:

    B, N, _ = K.size()
    I = torch.eye(N, device=K.device).unsqueeze(0).expand(B, N, N)
    sigma_term = I * sigma2.view(B, 1, 1)
    K_eps = K + sigma_term

    L = torch.cholesky(K_eps)
    slogdet = 2.0 * torch.sum(torch.log(torch.diagonal(L, dim1=-2, dim2=-1)), dim=1)

    if (beta is not None and x_mean is not None):
        y_demean = (y - torch.matmul(x_mean, beta.transpose(0, 1))).transpose(0, 1).unsqueeze(-1)
        y_batch = y_demean
    else:
        y_batch = y.unsqueeze(0).expand(B, N, 1)


    alpha = torch.cholesky_solve(y_batch, L)
    quad = torch.matmul(y_batch.transpose(1, 2), alpha).squeeze(-1).squeeze(-1)

    log_lik = torch.lgamma((nu + N)*0.5) - 0.5 * N * torch.log(nu - 2) - torch.lgamma(0.5 * nu) -0.5 * slogdet - 0.5 * (nu + N) * torch.log(1 + 1/(nu - 2) * quad)
    return log_lik

def kernel_se(thetas: torch.Tensor, tau: torch.Tensor, x: torch.Tensor, x_star: Optional[torch.Tensor]) -> torch.Tensor:
    D = sqdist(x, thetas, x_star)
    return (1.0 / tau.unsqueeze(1).unsqueeze(2)) * torch.exp(-0.5 * D)

def kernel_matern_12(thetas: torch.Tensor, tau: torch.Tensor, x: torch.Tensor, x_star: Optional[torch.Tensor]) -> torch.Tensor:
    D = torch.sqrt(sqdist(x, thetas, x_star) + 1e-4)
    return (1.0 / tau.unsqueeze(1).unsqueeze(2)) * torch.exp(-D)

def kernel_matern_32(thetas: torch.Tensor, tau: torch.Tensor, x: torch.Tensor, x_star: Optional[torch.Tensor]) -> torch.Tensor:
    D = torch.sqrt(sqdist(x, thetas, x_star) + 1e-4)
    sqrt3 = 3.0 ** 0.5
    return (1.0 / tau.unsqueeze(1).unsqueeze(2)) * (1 + sqrt3 * D) * torch.exp(-sqrt3 * D)

def kernel_matern_52(thetas: torch.Tensor, tau: torch.Tensor, x: torch.Tensor, x_star: Optional[torch.Tensor]) -> torch.Tensor:
    D = torch.sqrt(sqdist(x, thetas, x_star) + 1e-4)
    sqrt5 = 5.0 ** 0.5
    return (1.0 / tau.unsqueeze(1).unsqueeze(2)) * (1 + sqrt5 * D + (5.0 / 3.0) * D ** 2) * torch.exp(-sqrt5 * D)
")
  }
}
