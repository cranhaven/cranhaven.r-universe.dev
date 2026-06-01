##Todas os REGEX definidos na mesma ordem da rotina original

#Provisoriamente - pequena ferramenta usada para criar um dos regexes
spaced_word <-  \(x) {
  # Divide a string em caracteres individuais
  chars <- strsplit(x, "")[[1]]
  
  # Junta os caracteres com a regex que permite 0 ou mais ' ' entre eles
  regexp_string <- paste(chars, collapse = "\\s{0,}")
  
  return(regexp_string)
}


#regex to detect absence of the name (for mother and father name). Either empty or "PAI DESCONHCIDO" and variants of that

NA_nao   <- 'N|N[AO]|NAO.*|N\\s?O|NA\\s?O|ANO|NSO|NAP|NCO|NIA|NAD|'
NA_nao2  <- 'NOA|[BCMRVW]AO|NASO|NA\\s?O|AO|ANO|NSO|NAP|NCO|NIA|NAD|NASO|NBAO|NQAO|NQO|NOS|NALO|NUAM|NCO|NANO|NCO|NAL'
NA_nada  <- 'NADA.*|NDA|ND|NC'
NA_nada2 <- 'MADA|NASA|NAFA|ANDA|NADO|NADFA|NADQA|NACO|NMADA|NODA'
NA_sem <- 'S|SE[NM]|SM'
NA_nao_nada_sem  <- paste0('^(',NA_nao,'|',NA_nada,'|',NA_sem,')$')
NA_nao_nada_sem2 <- paste0('^(',NA_nao2,'|',NA_nada2,'|',NA_sem,')$')

#Testing
# NA_orig         <- c('NADA.*','.*NADA', 'NDA', 'ND', 'MADA', 'NODA', 'NASA', 'NADFA', 'NC', 'NA DA', 
#                      'NAFA', 'ADA','NADO', 'NADQA', 'NMADA', 'NACO', 'ANDA', 'NADA DE','NADA INF',
#                      'N', 'NAO.*', 'NO', 'NOA', 'NDA', 'NA', 'NAOA', 'MAO', 'NAD', 'AO', 'ANO', 'NSO', 'N O', 'NASO', 'NBAO', 'NQAO', 'BAO','NAP', 'NA O', 'CAO',
#                      'NAL', 'NQO', 'NOS', 'NALO', 'NUAM' , 'NCO ', 'NANO' , 'NAOTEM', 'WAO', 'NAOIN', 'VAO', 'RAO', 'NIA',
#                      'S','SEM','SM')
# DT <- data.table(x=NA_orig)
# DT[,x_NA:=ifelse(str_detect(x,NA_nao_nada_sem),1,
#                  ifelse(str_detect(x,NA_nao_nada_sem2),1,0))]
# print(DT)


#regex to detect some possible typing errors CONCEIC AO, CONCEI AO, CL AUDIA, etc... cedilha, acentos, etc. 
regex_cAO <- '(CO[NM]CEIC|CON EIC|INIFORMAC|ANUN?CIAC|ASS?UMPC|RESSURREIC|INCARNAC|ASSUNC|ABRA|VISITAC|ENCARNAC|CRISTOV|GALV|ARAG|^AD|^JO| JO|^LE| LE|SALOM|SIM|GIR|TRIST|EST[EI]?V|SEBASTI|ESTEV|GUSM|SILV|ROM|FALC|DAMI|PAIX|FRAZ|IRM|^N| N) AO( |$)'
regex__AO <- '(CO[NM]CEI|CON EI|INI?FORMA|IDENTIFICA|AU?NUNCIA|ASS?UMP?|ASSUN|ANUNCIA|VISITA|PURIFICA|ENCARNA|RESSUREI|GAL|ARA|TRIS|ESTE|CONSOLA|FRANCIS|C[OA]NCEI|FAL) AO( |$)'
regex_J_AO <- '(^| )J AO( |$)'
regex_apostrofe <- "\\bSANT AN+A|\\bD AR[CK]|\\bD AVIL+A|\\bD ANGELO|\\bD ALESSANDR|\\bD AQUINO|\\bD AMICO|\\bD AMICO|\\bD OR|\\bD A(G|CQ)UA|\\bD [AO]S?|\\b(D E(?!\\s+(SANTO|STO\\b|S\\b)))"
regex_d_vogal_candidato_apostrofo <- "\\bD ([AEIOU]\\w{3,})\\b"
##outros casos com muito alta confiabilidade do d separado por apostrofe ou erro tipografico no cadastro ex D ENADAI quando DENADAI - verificado com screening de todas as possibilidades juntas  d e\w{3,} 
regex_d_e_pos <- "(CC|CH|CLAR|GM|LAQ|LBO|LEUT|LLY|MM|NADAI|PIR|RC|RAS|RRI|SCONHECI|SP|SQ|STE|TT|U|VAN|VE|X|Z|\\s+(SANTO|STO\\b|S\\b))"
regex_d_e_apostrofo <- paste0("\\bD E(?=",regex_d_e_pos,")")
## erros tipograficos como D ELIMA - parecido com limpa acento/apostrofo
regex_d_e_falsopositivo_apostrofo_melhora_sobrenome <- gsub("=","!",regex_d_e_apostrofo)
regex_d_a_falso <- "(CONCE|COSTA|CRUZ|CUNHA|FONSECA|ISLVA|IVLA|LCERDA|LIMA|MATA|MOTA|OCN|PAIXAO|PAZ\\b|PENHA|ROCHA|SIALVA|SILV|SIV|SLV)"
regex_d_a_apostrofo <- paste0("\\bD A(?!",regex_d_a_falso,")")
regex_d_a_falsopositivo_apostrofo <- gsub("!","=",regex_d_a_apostrofo)
regex_d_o_falsopositivo_apostrofo <- "\\bD O(?=(CARMO|NASCIMENTO))"
regex_d_i_falsopositivo_apostrofo_reverter <- "\\bD I(?=(SLVA|SVLA))" #muito poucos casos para tratar
regex_d_u_falsopositivo_apostrofo <- "\\bD U(?=(CARMO))" #muito poucos casos para tratar
regex_nome_acento1  <- 'FL AVI[OA]|JOS E|LU I[SZ]A?|ANDR E|S ERGIO|M ARCIO|F ABIO|M ARIO|L UCI[AO]|R EGIS|C ICERO|J ULI[AO]|L AZARO|F ABI[AO]'
regex_nome_acento2  <- 'PATR ICIA|S ILVIA|F ATIMA|CL AUDIA|T ANIA|NAT ALIA|S ONIA|VIC?T ORIA|ANDR EI?A|M ONICA|AM ELIA|L IVIA|C ELIA|EL I[SZ]A'
regex_nome_acento3  <- 'ANT ONI[AO]|VIN ICI[UO]S?|CEC ILIA|MAIT E|H?ELO ISA|ELO A|LU ISA|O?L IVIA|LAV INIA|LA IS|ZO E|P EROLA|TH? EO|NO E|BEN ICIO|NAT A|NO EMIA'
regex_nome_acento4 <- '(T ASSI|C ASSI)[AO]'
regex_nome_acento   <- paste0( '(?<=^| )(', regex_nome_acento1, '|', regex_nome_acento2, '|', regex_nome_acento3,'|',regex_nome_acento4, ')(?= |$)' )


#regex FELECIDO only
regex_FALECIDO_main    <- '(FALES?CID|MORT|FINAD)[AO]?S?Q?'
regex_FALECIDO_prefix  <- '((ESPOSA|MARIDO|AMBOS|JA|MA[EO]|PA[IU]S?|FALECID[AO]S?|IGNORAR?D?[AO]S?|SEM NOME|E|ESTA|IGNORAR|EL[AE]|DESCONHECID[AO]) .*)?'
regex_FALECIDO_sufix   <- '(.* (I|JA|MA[EO]|PA[IU]|IGNORAR?D?[AO]S?|SEM NOME|MORREU|E|DESCONHECID[AO]))?'
regex_FALECIDO_extra1  <- 'FALECIAD[AO]S?|FALECI[AO]|FALECIDO FALECIIDO|FALECISO|FALECIDADE|FALECIDA FALECIADA|INFORMADO COMO FALECIDO NO CADASTRO'
regex_FALECIDO_extra2  <- 'PAI FALECIOD|FALECICO|JA FALECIODA|IGUINORADO FALECIDPO|FALECIODO OU IGNORADO|NAO INCLUIDO NO REGISTRO POR TER FALECIDO DURANTE A GESTACAO'
regex_FALECIDO_extra3  <- 'IGNORADA POR FALECIMENTO|MARIA FALECIDA QUERO SABER O NOME DA MAE NAO SE ELA ESTA VIVA|CONSTA FALECIDA NO REGISTRO'
regex_FALECIDO_extra   <- paste0(regex_FALECIDO_extra1,'|',regex_FALECIDO_extra2,'|',regex_FALECIDO_extra3) 
regex_FALECIDO         <- paste0('^(',regex_FALECIDO_prefix,regex_FALECIDO_main,regex_FALECIDO_sufix,'|',regex_FALECIDO_extra ,')$')
#reeg NOME + FALECIDO or FALECIDO + NOME
regex_nome_FALECIDO_start <- '^(((JA |E |ESTA ).*)?FALES?CID[AO]?S?Q?)'
regex_nome_FALECIDO_end   <-  '( (((JA |E |ESTA ).*)?FALES?CID[AO]?S?Q?$)| JA FALECIDA COM CERTIDAO DE OBITO| FALECIDO MORTO| ?- FALECI| ?-?FALECID[AO]| FALECIDA I| FALECIDO IGNORADO| FALECIOA|S FALECIDFA)$'
regex_nome_FALECIDO    <- paste0(regex_nome_FALECIDO_start,'|',regex_nome_FALECIDO_end) 

#CARTORIO
regex_CARTORIO         <- '((PRIMEIRO |SEGUNDO |TERCEIRO)? ?(CARTORIO|REGISTRO CIVIL).*(OFICIOS?|CIL?VI?L?S?|NOTAS?|REGISTROS?|ANEXOS?|JUDICIARIO|UNICO|NATURA?I?S?|PAZ|TABELIONATO|COMARCA|DOCUMENTOS|DISTRITAL|SUBDISTRITO|JUDICARIO|PUBLICO)( D[EAO].*)?)|^CARTORIO$' 
regex_CARTORIODE       <- '(PRIMEIRO |SEGUNDO |TERCEIRO)? ?(CARTORIO|REGISTRO CIVIL) D[EAO]S?.*' 
regex_CIDADES          <- 'ITAPIRAPUA PAULISTA|GOIANIA|JUIZ DE FORA|PIACABUCU|CAMPOS NOVOS PAULISTA|BELEM DE SAO FRANCISCO|ANGATURAMA|CAVALEIRO|ENCRUZILHADA|FEIRA GRANDE|PESSOAS NATURAIS'

#suspicous words
regex_P_M_S_N          <- '\\b(PAI|MAE|SEM|NAO)\\b'
#accepted names that contain suspicious words (PAI, MAE, NAO, SEM)
regex_ORO_NAO          <- 'O[RT]OI? ?NAO|OR NAO|OOR NAO|ORO WIN NAO'
regex_nome_aceito1     <- 'O[RT]OI? ?NAO|OR NAO|OOR NAO|ORO WIN NAO|MAE DE DEUS|(PAI|MAE) (D[EA])? ?(ETERNO|GUAJAJARA|FREITAS|PRADO|ONO|SILVA|ROSA|SALES|LOPE[SZ]|DORES|GOMES)'
regex_nome_aceito2     <- '(SILVA|SANTANA|VIRGEM|SANTOS|BATISTA|PINTO|DILMA) (D[EA])? ?(MAE|PAI)|S[EI]M FUEGOS|SEM TERRA|(PAI|MAE) DE | DA PAI|DAL PAI|KATU MAE'
regex_nome_aceito3     <- 'NO?A COSTA'
regex_nome_aceito      <- paste0(regex_ORO_NAO,'|',regex_nome_aceito1,'|',regex_nome_aceito2,'|',regex_nome_aceito3)
regex_naoMora          <- '( OBS A MAE )?NAO MORA( JUNTO| COM OS FILHOS)?$' 

regex_paimae_aceppted_pattern <- 'ORO\\s+NAO'
regex_paimae_desconhecido     <- '^$|^NAO |\\bNAO(\\s|$)|IGNORAD[AO]|(NAO|NADA) CONSTA|NAO TEM|DESCONHECID[AO]|NAO DECLARAD[AO]|IGNORAD[AO]|IGNORAR'


#Main NA words, incluiding their variants
NA_nada_nao     <- c('NADA', 'NDA', 'ND', 'MADA', 'NODA', 'NASA', 'NADFA', 'NC', 'NA DA', 
                     'NAFA', 'NADO', 'NADQA', 'NMADA', 'NACO', 'ANDA', 'NADA DE', 
                     'NADA INF', 'NAO', 'NO', 'NOA', 'NA', 'NAOA', 'MAO', 'NAD', 'AO', 'ANO', 
                     'NSO', 'N O', 'NASO', 'NBAO', 'NQAO', 'BAO', 'NAP', 'NA O', 'CAO', 'NAL', 
                     'NQO', 'NOS', 'NALO', 'NUAM', 'NCO', 'NANO', 'NAOTEM', 'WAO', 'NAOIN', 
                     'VAO', 'RAO', 'NIA')
NA_consta       <- c('COSTA', 'CONTA', 'CONSTA', 'CONST', 'A CONSTAR', 'COSNTA NOME', 'CINSTA', 'A CONSTA', 
                     'COSNTA', 'COMSTA', 'CONSA', 'ONSTA', 'CNSTA', 'CONTSA', 'COSNSTA', 'OCNSTA', 
                     'CONSAT', 'CPNSTA', 'CONSTS', 'CONSTRA', 'CCONSTA', 'COSNTA', 'CONSRA',
                     'CONSYA', 'COPNSTA', 'CONSATA', 'CONMSTA', 'CONTAS', 'COONSTA', 'CONATA', 
                     'CO NSTA', 'CUNSTA', 'CANSTA', 'CONNSTA', 'CONXTA', 'CONSTGA', 'DE CONSTA', 
                     'CSONTA', 'CONSTQA', 'ACONSTA')
NA_nada_nao_copleto <- c('NC', 'ND', 'NO INFORMADO', 'NO DECLARADO', 'NOA DECLARADO',
                         'NOA INFORMADO', 'NADA DECLARADO', 'NADA INFORMADO', 'NA DECLARADO', 'NA INFORMADO',
                         'IG NO RADO', 'NOA TEM', 'IG NO RA DO', 'NO TEM', 'INEXISTE NO DOCUMENTO', 'NAOTEM', 
                         'NADA A DECLARAR', 'AO INFORMADO', 'ND ND', 'NC NC', 'NO IDENTIFICADO', 'ANO DECLARADO', 
                         'ANO INFORMADO', 'NAP INFORMADO', 'MAO INFORMADO', 'NC NC NC', 'NASO INFORMADO', 
                         'EM BRANCO NO RG', 'NA O DECLARADO', 'OMISSO NO DOCUMENTO', 'NA OINFORMADO', 'NA TEM', 
                         'NADACONSTA NO RG', 'NAP DECLARADO', 'CONSTA NADA', 'AO DECLARADO', 'NO SABE', 
                         'NBAO DECLARADO', 'NOA REGISTRADO', 'NADA', 'NA RG EM BRANCO', 'NA O CONS TA', 
                         'INESISTENTE NO CADASTRO', 'DESINFORMADO NA FICHA', 'MAO TEM', 'EM BRANCO NO DOCUMENTO', 
                         'NAP TEM', 'NAOCONSTA NO DOC', 'NO REG', 'NA I', 'NC E NC', 'NAL DECLARADA', 'IG NO', 
                         'NO HA', 'NA SEI', 'NA INF', 'NADA DECLARAR', 'NO INFORMA', 'N O DECLARADO', 'NO INFO', 
                         'ANO TEM', 'NOA INFORMOU', 'NOA DECALARADO', 'ANO REGISTRADO', 'NA IDENTIFICADO', 
                         'NADA INF', 'NA INFORMADA', 'NADA CONSTADO', 'NO DECLARAO')
NA_consta_completo <- c('N CONSTA', 'NAOP CONSTA', 'NAUM CONSTA', 'NAOS CONSTA', 'NAOI CONSTA', 'NAOC ONSTA', 
                        'NAIO CONSTA', 'NAI CONSTA', 'NGO CONSTA', 'NOAO CONSTA', 'NAOO CONSTA', 'NAOM CONSTA', 
                        'CONSTA', 'NAAO CONSTA', 'NNAO CONSTA', 'NANDA CONSTA', 'NAZDA CONSTA', 'ONAO CONSTA', 
                        'CONSTA NULO', 'INADA CONSTA', 'NAQO CONSTA', 'CONSTA EM OUTRO DOCUMENTO', 'NADAS CONSTA', 
                        'NMAO CONSTA', 'NANA CONSTA', 'NAQDA CONSTA', 'IGONARADO NADA CONSTA', 'NAAD CONSTA', 
                        'NAAO COONSTA', 'NAOC CONSTA', 'NP CONSTA', 'NAAO CONSTRA', 'NAADA CONSTA', 'N COSNTA', 
                        'NHAO CONSTA', 'NAOX CONSTA', 'NAN CONSTA', 'NAOS CONTA', 'NADA C ONSTA', 'ANAO CONSTA', 
                        'CANDA CONSTA', 'NDADA CONSTA', 'N CONTAS')
NA_nada_nao_consta    <- outer(NA_nada_nao, NA_consta    , paste, sep = " ") |> as.vector()

regex_NADA_NAO        <- NA_nada_nao        |> paste0(collapse = '|') |> paste0("\\b(", x= _ , ")\\b")
regex_CONSTA          <- NA_consta          |>  paste0(collapse = '|') |> paste0("\\b(", x= _ , ")\\b")
regex_NADA_NAO_CONSTA <- NA_nada_nao_consta |>  paste0(collapse = '|') |> paste0("\\b(", x= _ , ")\\b")
regex_NADA_CONSTA_sufix <- '(.*(CERTIDA?O?|DOCUMENTO|REGISTROS?|REG|ARQUIVOS?|APRESENT?A?D?O?|GENITORA?|(CASA|NAS?CI)MENTO|(MA|PA)TERNO|PAI|MAE|PAIS|RG|CTPS|CPF|TERMOS?|CADASTRO|CAS|IDE?N?T?I?D?A?D?E?|NADA|JUDICIAL|LEI|DESCONHECID?A?O?S?|CIVIL|FILIACAO|OFICIO|LEGAL|X+|NOM?E?|SIST?E?M?A?|CN|CIDADE|IGNORA?D?A?O?|ADOTIVA|ORFANATO|N?A?O?|FICHA|AUSENTE|DIGITADA?O?))?'
regex_NADA_NAO_CONSTA2<- NA_nada_nao_consta |>  paste0(collapse = '|') |> paste0("^(", x= _ ,')',regex_NADA_CONSTA_sufix, "$")

NA_nao   <- 'N|N[AO]|NAO.*|N\\s?O|NA\\s?O|ANO|NSO|NAP|NCO|NIA|NAD|'
NA_nao2  <- 'NOA|[BCMRVW]AO|NASO|NA\\s?O|AO|ANO|NSO|NAP|NCO|NIA|NAD|NASO|NBAO|NQAO|NQO|NOS|NALO|NUAM|NCO|NANO|NCO|NAL'
NA_nada  <- 'NADA.*|NDA|ND|NC'
NA_nada2 <- 'MADA|NASA|NAFA|ANDA|NADO|NADFA|NADQA|NACO|NMADA|NODA'
NA_sem <- 'S|SE[NM]|SM'
NA_nao_nada_sem  <- paste0('^(',NA_nao,'|',NA_nada,'|',NA_sem,')$')
NA_nao_nada_sem2 <- paste0('^(',NA_nao2,'|',NA_nada2,'|',NA_sem,')$')

regex_paimae_desconhecido     <- '^$|^NAO |\\bNAO(\\s|$)|IGNORAD[AO]|(NAO|NADA) CONSTA|NAO TEM|DESCONHECID[AO]|NAO DECLARAD[AO]|IGNORAD[AO]|IGNORAR'









#regex expecificos
NA_cadastrado   <- c('CADASTRADO')
NA_declarado    <- c('DW?E ?CLARAD[AO]', 'A DECLARAR', 'DECLARAR', 'DECLARADA','NA DECLARADO',
                     'DECLARODO', 'DELARADO', 'DECLARA','DELARAR', 'DE CLARADO')  
NA_informado    <- c('INFORMADO','INF', 'INFORMADO NA CI','INFORMA','FOI INFORMADO')
NA_registro     <- c('REGISTRADO', 'NO REGISTRO', 'A REGISTRAR', 'ANOTADO')

NA_nada_nao_cadast  <- outer(NA_nada_nao, NA_cadastrado, paste, sep = " ") |> as.vector()
NA_nada_nao_declar  <- outer(NA_nada_nao, NA_declarado , paste, sep = " ") |> as.vector()
NA_nada_nao_inform  <- outer(NA_nada_nao, NA_informado , paste, sep = " ") |> as.vector()
NA_nada_nao_regist  <- outer(NA_nada_nao, NA_registro  , paste, sep = " ") |> as.vector()

NA_consta  <- 'CON[ST][A]?'
NA_consta2 <- 'C[OA]N[ST][A-Z]*|[A-Z]*CON[ST][A-Z]*|[A-Z]*CONSTA[A-Z]*|COSTA|A CONSTAR*|CINSTA|COSNTA|COMSTA|ONSTA|CNSTA|COSNSTA|OCNSTA|COSNSTA|OCNSTA|CPNSTA|COPNSTA|CONMSTA|COONSTA|CONATA|CONNSTA|CONXTA|DE CONSTA|CSONTA|CUNSTA'
NA_cadastr <- 'CADASTR'
NA_decl    <- 'DECLARAD[OA]|DECLARA'
NA_decl2   <- 'DE[CL]{1,2}ARAD[OA]|DECLARAR|DELARAD[OA]|DELARAR|DE\\s?CLARAD[OA]|A DECLARAR|DECLARADO|DWECLARADO|DECLARODO'
NA_inf     <- 'INFORMAD[OA]|INFORMA|INF'
NA_inf2    <- 'INFORMAD[OA]|FOI\\s+INFORMADO|'
NA_reg     <- 'REGISTRAD[OA]'
NA_reg2    <- 'NO\\s+REGISTRO|A\\s+REGISTRAR|ANOTADO'
NA_ignorado     <- c('INGNORAD[OA]')
NA_outros       <- c('A PREENCHER', 'NADA NO DOCUMENTO', 'NADA CONFIRMADO','NADA NADA','NA DA','NADA NA DA', 'NADA APRESENTADO',
                     'MAE SOLTEIRA', 'AUSNETE', 'OMITIDO', 'EM BRANCO',
                     'INDETERMINADO','NC NC','INEXISTENTE','SEPARADOS') 
NA_const_cadast_decl_inf_reg  <- paste0('^(',NA_consta ,'|',NA_cadastr,'|',NA_decl ,'|',NA_inf ,'|',NA_reg ,')$')
NA_const_cadast_decl_inf_reg2 <- paste0('^(',NA_consta2,'|',NA_cadastr,'|',NA_decl2,'|',NA_inf2,'|',NA_reg2,')$')

NA_orig         <- c(NA_consta,NA_cadastrado,NA_declarado,NA_informado,NA_registro)





NA_strings <- c(NA_nada_nao_consta,NA_nada_nao_cadast,NA_nada_nao_declar,NA_nada_nao_inform,NA_nada_nao_regist,
                NA_ignorado,NA_outros) |> paste0(collapse = '|')

regex_paimae_desconhecido     <- '^((PAI|MAE)? ?(IN?G|IN?GU?I?O?N?O?RAD[AO]|DES? ?CONH?ECID[AO]|DESC|INDETERMINADO|IGNORA?D?[AO]?|FALECID[AO]|AUSENTE|OMITID[AO])|ORFAO ?D?E? ?(PAI|MAE)?|IGN IGN|(N|NAO|NADA|NAI) (CONSTA|INFO?R?M?O?U?)|NAO (TEM|DECLARAD[AO]))$'
regex_paimae_desconhecido2    <- '^(X+|X X X|MESM[OA]|IDEM|JFJDL|JKJKA| A|A A|S N|NAO|SEM|MAE|PAI|I G|IG IG|IGN|N C|ABC|NT|NI|NC|N C|L L|NT|NO|ND|A|C|S|H||N T)$'
regex_paimae_desconhecido_1_2 <- paste0(regex_paimae_desconhecido,'|',regex_paimae_desconhecido2)
regex_paimae_desconhecido3    <- paste0('^(|',NA_strings,')$')
regex_paimae_desconhecido4    <- '(SEM|^SE?M?) *(NOME|PAI|MAE|DADOS|PREENCHIMENTO|DENOMICAO|IN?DEN?TIFICACAO|PATERNIDADE|DECL?A?R?A?(R|CAO)?|RESPONSAVEL|INFO?R?M?A?(R|(CA?O?))?|PREENCHIMENTO|DENOMIN?A?CAO|REGISTRO|FILIACAO|VALOR|PARADEIRO|IDENTIDADE)'

#adicionado 'de las'
regex_base_nome_EDADOS <- c("DE LOS","DE LAS?","DE","DO","DA","DI","DU","DOS","DAS","DEL","D")
regex_base_particula <- paste(regex_base_nome_EDADOS, collapse = "|")
regex_qualquer_particula <- paste0("\\b(", regex_base_particula, ")\\b")
regex_nome_EDADEDOS           <- paste0("\\s+(",regex_base_particula,")\\s+")
regex_nome_EDADEDOS2          <-    paste0("^(",regex_base_particula,")$")
regex_nome_AGNOMES            <- " (FILHO|FILHA|JUNIOR|JR|NETO|NETA|BISNETO|BISNETA|SOBRINHA|SOBRINHO|SEGUNDO|SEGUNDA|TERCEIRO|TERCEIRA|FL)$"

regex_nome_XEDADEDOS          <- "\\s+[A-Z](E|D[AO]S?|DE)\\s+"
regex_nome_XEDADEDOS_allowed  <- "\\s+(DE|ID[AO]|FE|EDO|OSO|DA SE|NE|GE)\\s+"
regex_nome_XEDADEDOS_replace  <- "(\\s+)[BCDFGHJLNMPRSVQW](D[EAO]S?)(\\s+)"
#gsub("\\|E\\)$","",

regex_DEDEDADA <- paste0('(\\b(',regex_base_particula,')(\\s+(',regex_base_particula,')\\b)+)')



#regex SR|SRA

#(?:\\s{0,2}) <- retirado pois \\bS <- S R (falsos positivos sobrenomes abreviados)

regex_SR_SRA <- paste0('^',gsub("H","H?",spaced_word('SENHOR')),'A?\\b')


##regex ignorado/desconhecido adicionais
dicionario_marcas_ignorados <- c(
  "^$", # vazio
  "^.$", # apenas um caractere 
  "DES\\s?CONHEC",
  "\\bINDETER\\S", # indeter + caractere diferente de ' ' - evita ex. Lindeter Maria...
  "\\bINE[XS]IST", #inexistente inesistente
  "[^DS]IN*G[OUI]*NORA[DRAOS]*\\b", #IGNORADA INGONORADO E VARIANTES
  
  # RETIRADO - grepl("AUSENT",no_pessoa),.(no_pessoa,nome_cpf) - 11 CASOS 9 validados com nome cpf, 2 NA
  #"AUSENT",
  
  # omitido, se omite
  ## AJUSTADO PARA EVITAR - omiterio, comite, domitilde, domitilia, jacomite etc 
  ## Problema OMITE como nome, OMITI como sobrenome final - menor caso DARCI OMITI
  ##separar
  ## 1 OMITE + alternativas diferentes de PRIMEIRO NOME Efetivo- menor caso OMITE DOS SANTOS LIMA
  "\\bOMIT[E]\\b.{0,7}$",
  ## 2 OMITI + alternativas
  "^.{0,5}\\bOMIT[I]D?[AO]?\\b"
  
  
  
)




###IGNORADO - 35 CASOS GERAIS FORA DE NOME DE PAI E MAE
regex_IGNORADO <- paste0(dicionario_marcas_ignorados,collapse = "|")


###regex ignorado regra para substituir
regex_ignorados_subst <- c(
  "^$", # vazio
  "^.$", # apenas um caractere
  "DES\\s?CONHEC\\S+\\b",
  "\\bINDETER\\S+\\b", # indeter + caractere diferente de ' ' - evita ex. Lindeter Maria...
  "\\bINE[XS]IST\\S+\\b", #inexistente inesistente
  "[^DS]IN*G[OUI]*NORA[DRAOS]*\\b", #IGNORADA INGONORADO E VARIANTES
  
  # RETIRADO - grepl("AUSENT",no_pessoa),.(no_pessoa,nome_cpf) - 11 CASOS 9 validados com nome cpf, 2 NA
  #"AUSENT",
  
  # omitido, se omite
  ## AJUSTADO PARA EVITAR - omiterio, comite, domitilde, domitilia, jacomite etc 
  ## Problema OMITE como nome, OMITI como sobrenome final - menor caso DARCI OMITI
  ##separar
  ## 1 OMITE + alternativas diferentes de PRIMEIRO NOME Efetivo- menor caso OMITE DOS SANTOS LIMA
  "\\bOMIT[E]\\b",
  ## 2 OMITI + alternativas
  "\\bOMIT[I]D?[AO]?\\b"
  
  
  
)

regex_IGNORADO_subst <- paste0(regex_ignorados_subst,collapse = "|")

## algumas patentes/cargos

regex_DR_CORONEL <- c(
  'DRA?',
  'SGTO',
  'TENENTE',
  'MAJOR',
  'DOUTORA?',
  'CORONEL',
  'GENERAL',
  'GOVERNADORA?',
  'SARGENTO',
  'CAPIT[A\\u00c3]O'
)

regex_DR_CORONEL <- paste0("^(",paste0(regex_DR_CORONEL,collapse="|"),")\\b")





#Regex Letra Inicial repetida

## Lista caracteres repetidos validos ao iniciar da palavra ou em palavra completa a excluir
repeticoes_validas <- c("AARAO","II\\b","III\\b","AARON","LL")


repvalidas <- 
  paste0("(?!",repeticoes_validas,")",collapse="")


regex_LETRA_IN_REPETIDA <-  paste0("\\b",repvalidas,"([A-Z])\\1+")

