# --- Rotina para gerar o arquivo amostra_np2_para_testes.RDS ---

# Carregar data.table (se não estiver já carregado no seu ambiente de script)
# library(data.table)

# Criar o data.table com os dados de exemplo
# Focando nas colunas que simulam a estrutura do seu np2.Rds
# e que serão usadas pela função identificar_adicionar_nome_proprio
#
# Colunas em np2 que identificar_adicionar_nome_proprio espera para o join (derivadas de 's'):
# - Uma coluna correspondente a s1 (ex: nome_simpl1)
# - Uma coluna correspondente a s2p (ex: nome_simpl2p)
#
# Coluna em np2 que contém o valor a ser adicionado (nome próprio):
# - O nome desta coluna é passado via argumento para identificar_adicionar_nome_proprio
#   (ex: "nome_proprio_final_np2" no exemplo anterior, ou simplesmente "nome_proprio" aqui)

amostra_np2_data <- data.table::data.table(
  # Estas colunas simulam as chaves de junção do seu arquivo np2.Rds
  # Use os nomes exatos que sua função identificar_adicionar_nome_proprio
  # espera encontrar no arquivo np2.Rds após a leitura.
  # Se sua função deriva os nomes das colunas de chave de np2 a partir de um prefixo 's',
  # e.g., s_prefix = "nome_clean", então as colunas em np2 deveriam ser "nome_clean1" e "nome_clean2p".
  nome_clean1 = c(
    "PEDRO",
    "JOSE",
    NA_character_, # Para o caso "NAO CONSTA NADA"
    NA_character_, # Para o caso "TESTE" (onde nome_clean1 pode ser NA se TESTE for o único nome)
    # ou poderia ser "TESTE" se sua lógica de limpeza gerar isso.
    # Ajuste conforme a saída da sua função de limpeza para "TESTE"
    "CORONEL",
    "MARIA"
  ),
  nome_clean2p = c(
    "PEDRO SANTANA",    # Chave para Pedro
    "JOSE DACQUA",      # Chave para Jose
    NA_character_,      # Para "NAO CONSTA NADA"
    "TESTE",            # Chave para Teste (supondo que nome_clean2p possa ser apenas o nome_clean1)
    # Ajuste conforme sua lógica.
    "CORONEL JACINTO",  # Chave para Coronel
    "MARIA DO SOCORRO"  # Chave para Maria
  ),
  # Esta coluna simula o valor que você quer buscar e adicionar aos dados do usuário
  nome_proprio = c(
    "PEDRO",            # O nome próprio para "PEDRO SANTANA"
    NA_character_,      # Para "JOSE DACQUA" (conforme seu exemplo, não tem nome_proprio)
    NA_character_,      # Para "NAO CONSTA NADA"
    NA_character_,      # Para "TESTE" (conforme seu exemplo)
    NA_character_,      # Para "CORONEL JACINTO" (conforme seu exemplo)
    "MARIA DO SOCORRO"  # O nome próprio para "MARIA DO SOCORRO"
  )
)

# --- Salvar o arquivo RDS para uso nos testes ---


output_rds_path <- "testdata/amostra_np2_para_testes.RDS"

# Salvar o data.table como um arquivo .RDS
saveRDS(amostra_np2_data, file = output_rds_path)

