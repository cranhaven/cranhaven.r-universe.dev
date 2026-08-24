.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "\n",
    crayon::green("autotesteR "), "carregado com sucesso!\n",
    "----------------------------------------------------------\n",
    "Um pacote desenvolvido para facilitar analises estatisticas\n",
    "com uma linguagem acessivel, diagnosticos automaticos e graficos claros.\n",
    "Site oficial e atualizacoes: https://github.com/Luiz-Garcia-R\n",
    "----------------------------------------------------------\n"
  )
}
