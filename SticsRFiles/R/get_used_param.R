get_used_param_xml <- function(file) {
  xml_doc <- xmldocument(file)
  param <- list()
  param$options <- get_options_used_param(xml_doc)
  param$base <- get_base_used_param(xml_doc)

  delete(xml_doc)

  param
}

get_base_used_param <- function(xml_doc) {
  name <- get_attrs_values(xml_doc,
                           path = "//formalisme/param",
                           attr_list = "nom"
  )
  colnames(name) <- "name"
  value <- get_values(xml_doc, path = "//formalisme/param")
  df <- data.frame(name = name,
                   value = value,
                   cultivar = "none",
                   stringsAsFactors = FALSE)

  # cultivar parameters
  namev <- get_attrs_values(xml_doc,
                            path = "//variete/param",
                            attr_list = "nom"
  )
  parv_nb <- length(unique(namev))
  cultivars <- as.vector(
    get_attrs_values(xml_doc, path = "//variete", attr_list = "nom")
  )
  #
  cultivar <- unlist(lapply(cultivars, function(x) rep(x, parv_nb)))

  colnames(namev) <- "name"
  valuev <- get_values(xml_doc, path = "//variete/param")

  dfv <- data.frame(name = namev,
                    value = valuev,
                    cultivar = cultivar,
                    stringsAsFactors = FALSE)

  dplyr::bind_rows(df, dfv)
}

get_options_used_param <- function(xml_doc, param_list = NULL) {
  m_options <- unique(
    get_attrs_values(xml_doc,
                     path = "//formalisme/option",
                     attr_list = c("choix", "nomParam")
    )
  )

  if (is.null(m_options)) {
    return()
  }

  if (is.null(dim(m_options))) {
    options_data <- data.frame(choix = m_options[1],
                               nomParam = m_options[2],
                               stringsAsFactors = FALSE)
  } else {
    options_data <- data.frame(choix = m_options[, 1],
                               nomParam = m_options[, 2],
                               stringsAsFactors = FALSE)
  }

  nb_opt <- dim(options_data)[1]

  for (opt in 1:nb_opt) {

    # get param level 1 option
    # boucle sur option_names
    name <- options_data$nomParam[opt]
    value <- options_data$choix[opt]
    path_param <- paste0("//option[@nomParam=", "'",
                         name,
                         "']/choix[@code=",
                         "'",
                         value,
                         "']/param")
    nodes_set <- get_nodes(xml_doc, path = path_param)


    if (!is.null(nodes_set)) {
      param_names <- as.vector(
        get_attrs_values(xml_doc, path = path_param, "nom")
      )
      param_values <- as.vector(get_values(xml_doc, path = path_param))
      param_list <- rbind(param_list, data.frame(
        option = name, code = value,
        name = param_names, value = param_values,
        cultivar = "none",
        stringsAsFactors = FALSE
      ))
      #
      # data.frame avec codeoption, noms param, valeurs param
    }

    path_suboption <- paste0("//option[@nomParam=",
                             "'",
                             name,
                             "']/choix[@code=",
                             "'",
                             value,
                             "']/option")


    m_sub_options <- unique(
      get_attrs_values(xml_doc,
                       path = path_suboption,
                       attr_list = c("choix", "nomParam")
      )
    )

    if (is.null(m_sub_options)) next

    if (is.null(dim(m_sub_options))) {
      sub_options_data <- data.frame(choix = m_sub_options[1],
                                     nomParam = m_sub_options[2],
                                     stringsAsFactors = FALSE)
    } else {
      sub_options_data <- data.frame(choix = m_sub_options[, 1],
                                     nomParam = m_sub_options[, 2],
                                     stringsAsFactors = FALSE)
    }


    nb_sub_opt <- dim(sub_options_data)[1]

    for (sub_opt in 1:nb_sub_opt) {

      # get param level 1 option
      # boucle sur option_names
      sub_name <- sub_options_data$nomParam[sub_opt]
      sub_value <- sub_options_data$choix[sub_opt]
      sub_path_param <- paste0(path_suboption,
                               "[@nomParam=",
                               "'",
                               sub_name,
                               "']/choix[@code=",
                               "'",
                               sub_value,
                               "']/param")

      nodes_set <- get_nodes(xml_doc, path = sub_path_param)


      if (!is.null(nodes_set)) {
        sub_param_names <- as.vector(
          get_attrs_values(xml_doc, path = sub_path_param, "nom")
        )

        sub_param_values <- as.vector(
          get_values(xml_doc, path = sub_path_param)
        )
        param_list <- rbind(param_list, data.frame(
          option = sub_name, code = sub_value,
          name = sub_param_names, value = sub_param_values,
          cultivar = "none",
          stringsAsFactors = FALSE
        ))
      }
    }
  }


  # cultivar
  #
  cultivars <- as.vector(
    get_attrs_values(xml_doc, path = "//variete", attr_list = "nom")
  )

  m_optionsv <- unique(
    get_attrs_values(xml_doc, path = "//optionv", attr_list = "nom")
  )
  optionv_data <- data.frame(nom = m_optionsv[, 1], stringsAsFactors = FALSE)

  m_all_options <- unique(get_attrs_values(xml_doc,
                                           path = "//option",
                                           attr_list = c("choix", "nomParam")
  ))
  all_options_data <- data.frame(choix = m_all_options[, 1],
                                 nomParam = m_all_options[, 2],
                                 stringsAsFactors = FALSE)

  idx <- all_options_data$nomParam %in% optionv_data$nom
  optionv_codes <- all_options_data$choix[idx]
  optionv_names <- all_options_data$nomParam[idx]

  nb_v_opt <- length(optionv_names)
  for (v_opt in 1:nb_v_opt) {
    v_name <- optionv_names[v_opt]
    v_value <- optionv_codes[v_opt]
    v_path_param <- paste0("//optionv[@nom='",
                           v_name,
                           "']//param[@code='",
                           v_value,
                           "']")
    nodes_set <- get_nodes(xml_doc, path = v_path_param)


    if (!is.null(nodes_set)) {
      v_param_names <- as.vector(
        get_attrs_values(xml_doc, path = v_path_param, "nom")
      )
      v_param_values <- as.vector(get_values(xml_doc, path = v_path_param))
      param_list <- rbind(param_list, data.frame(
        option = v_name, code = v_value,
        name = v_param_names, value = v_param_values,
        cultivar = cultivars,
        stringsAsFactors = FALSE
      ))
      #
      # data.frame avec codeoption, noms param, valeurs param
    }
  }

  param_list
}
