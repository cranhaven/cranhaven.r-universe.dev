#' XML SODAS files to RSDA files.
#' @name SODAS.to.RSDA
#' @aliases SODAS.to.RSDA
#' @author Olger Calderon and Roberto Zuniga.
#' @description To convert XML SODAS files to RSDA files.
#' @param XMLPath Disk path where the SODAS *.XML file is.
#' @param labels If we want to include SODAS XML files lebels in RSDA file.
#'
#' @return A RSDA symbolic data file.
#' @references
#' Bock H-H. and Diday E. (eds.) (2000).
#' Analysis of Symbolic Data. Exploratory methods for extracting statistical information from
#' complex data. Springer, Germany.
#' @seealso SDS.to.RSDA
#' @examples
#' \dontrun{
#' # We can read the file directly from the SODAS XML file as follows:
#' # abalone<-SODAS.to.RSDA('C:/Program Files (x86)/DECISIA/SODAS version 2.0/bases/abalone.xml)
#' # We can save the file in CSV to RSDA format as follows:
#' # write.sym.table(sodas.ex1, file='abalone.csv', sep=';',dec='.', row.names=TRUE,
#' #               col.names=TRUE)
#' # We read the file from the CSV file,
#' # this is not necessary if the file is read directly from
#' # XML using SODAS.to.RSDA as in the first statement in this example.
#' data(abalone)
#' res <- sym.interval.pca(abalone, "centers")
#' sym.scatterplot(sym.var(res$Sym.Components, 1), sym.var(res$Sym.Components, 2),
#'   labels = TRUE, col = "red", main = "PCA Oils Data"
#' )
#' sym.scatterplot3d(sym.var(res$Sym.Components, 1), sym.var(res$Sym.Components, 2),
#'   sym.var(res$Sym.Components, 3),
#'   color = "blue", main = "PCA Oils Data"
#' )
#' sym.scatterplot.ggplot(sym.var(res$Sym.Components, 1), sym.var(res$Sym.Components, 2),
#'   labels = TRUE
#' )
#' sym.circle.plot(res$Sym.Prin.Correlations)
#' }
#' @keywords Symbolic data table
#' @export
#' @importFrom XML xmlInternalTreeParse getNodeSet xmlGetAttr xpathSApply xmlValue xmlName
#'
SODAS.to.RSDA <- function(XMLPath, labels = T) {
  parsed.xml <- XML::xmlInternalTreeParse(XMLPath)

  containsNode <- XML::getNodeSet(parsed.xml, "/assofile/contains")
  if (length(containsNode) == 0) {
    stop("No 'contains' tag is present in the XML file")
  }
  containsNode <- containsNode[[1]]
  if (XML::xmlGetAttr(containsNode, "INDIVIDUALS") != "YES" || XML::xmlGetAttr(
    containsNode,
    "VARIABLES"
  ) != "YES" || XML::xmlGetAttr(containsNode, "RECTANGLE_MATRIX") != "YES") {
    stop("Insufficient data in XML file")
  }

  if (labels) {
    sym.obj.names <- XML::xpathSApply(
      parsed.xml, "/assofile/individus/stindiv/label",
      XML::xmlValue
    )
    variables.names <- XML::xpathSApply(
      parsed.xml, "/assofile/variables/stvar/ident/label",
      XML::xmlValue
    )
  } else {
    sym.obj.names <- XML::xpathSApply(
      parsed.xml, "/assofile/individus/stindiv/name",
      XML::xmlValue
    )
    variables.names <- XML::xpathSApply(
      parsed.xml, "/assofile/variables/stvar/ident/name",
      XML::xmlValue
    )
  }

  variables.types <- XML::xpathSApply(parsed.xml, "/assofile/variables/stvar/*[2]", XML::xmlName)
  result <- data.frame(row.names = sym.obj.names)
  number.of.rows <- nrow(result)

  for (i in 1:length(variables.types)) {
    cat(paste0("Processing variable ", i, ": ", variables.names[[i]], "\n"))

    switch(variables.types[[i]], `inter-cont` = {
      result <- cbind(result, process.inter.cont.variable(
        number.of.rows, parsed.xml,
        i, variables.names[[i]]
      ))
    }, continue = {
      result <- cbind(result, process.continue.variable(
        number.of.rows, parsed.xml,
        i, variables.names[[i]]
      ))
    }, nominal = {
      result <- cbind(result, process.nominal.variable(
        labels, number.of.rows,
        parsed.xml, i, variables.names[[i]]
      ))
    }, mult_nominal = {
      result <- cbind(result, process.mult.nominal.variable(
        labels, number.of.rows,
        parsed.xml, i, variables.names[[i]]
      ))
    }, mult_nominal_Modif = {
      type.modif <- XML::xpathSApply(parsed.xml, paste0(
        "/assofile/variables/stvar[",
        i, "]/mult_nominal_Modif/type_modif"
      ), XML::xmlValue)
      if (type.modif != "proba") {
        cat(
          paste0("Unsupported type.modif in mult_nominal_Modif variable: "),
          type.modif, "\n"
        )
      } else {
        result <- cbind(result, process.mult.nominal.modif.variable(
          labels,
          number.of.rows, parsed.xml, i, variables.names[[i]]
        ))
      }
    }, cat(paste0("Variable type not supported:"), variables.types[[i]], "\n"))
  }
  out <- newSobject(result)
  class(out) <- "sym.data.table"
  out <- to.v3(out)
  return(out)
}


#' process.nominal.variable
#' @importFrom XML xpathSApply xmlValue getNodeSet
#' @keywords internal
process.nominal.variable <- function(labels, number.of.rows, parsed.xml, variable.index,
                                     variable.name) {
  aux <- list()
  aux[[1]] <- rep("$S", number.of.rows)
  if (labels) {
    categories <- XML::xpathSApply(parsed.xml, paste0(
      "/assofile/variables/stvar[",
      variable.index, "]/nominal/nominal-desc/list-nom/label"
    ), XML::xmlValue)
  } else {
    categories <- XML::xpathSApply(parsed.xml, paste0(
      "/assofile/variables/stvar[",
      variable.index, "]/nominal/nominal-desc/list-nom/name"
    ), XML::xmlValue)
  }

  aux[[2]] <- rep(length(categories), number.of.rows)
  nodes <- XML::getNodeSet(parsed.xml, paste0(
    "/assofile/indiv_mat/ligmat/valmat[", variable.index,
    "]"
  ))

  after.evaluator <- function(node) {
    if (length(node["val_nomina"]) == 0) {
      return(rep(NA, length(categories)))
    } else {
      category <- as.numeric(XML::xmlValue(node))
      return(append(rep(0, length(categories) - 1), 1, category - 1))
    }
  }
  node.categories <- t(XML::xmlSApply(nodes, after.evaluator))

  aux <- data.frame(c(aux, as.data.frame(node.categories)))
  colnames(aux) <- c("$S", variable.name, categories)
  return(aux)
}


#' process.mult.nominal.variable
#' @keywords internal
#' @importFrom XML xmlSApply getNodeSet xmlValue
process.mult.nominal.variable <- function(labels, number.of.rows, parsed.xml, variable.index,
                                          variable.name) {
  aux <- list()
  aux[[1]] <- rep("$S", number.of.rows)
  if (labels) {
    categories <- XML::xpathSApply(parsed.xml, paste0(
      "/assofile/variables/stvar[",
      variable.index, "]/mult_nominal/nominal-desc/list-nom/label"
    ), xmlValue)
  } else {
    categories <- XML::xpathSApply(parsed.xml, paste0(
      "/assofile/variables/stvar[",
      variable.index, "]/mult_nominal/nominal-desc/list-nom/name"
    ), xmlValue)
  }

  aux[[2]] <- rep(length(categories), number.of.rows)
  nodes <- XML::getNodeSet(parsed.xml, paste0(
    "/assofile/indiv_mat/ligmat/valmat[", variable.index,
    "]"
  ))

  after.evaluator <- function(node) {
    if (length(node["val_modal"]) == 0) {
      return(NA)
    } else {
      present.mods <- as.numeric(XML::xmlSApply(node, XML::xmlValue))
      modals.vector <- rep(0, length(categories) - length(present.mods))
      for (present.mod in present.mods) {
        modals.vector <- append(modals.vector, 1, present.mod - 1)
      }
      return(modals.vector)
    }
  }
  node.categories <- t(XML::xmlSApply(nodes, after.evaluator))

  aux <- data.frame(c(aux, as.data.frame(node.categories)))
  colnames(aux) <- c("$S", variable.name, categories)
  return(aux)
}

#' process.mult.nominal.modif.variable
#' @keywords internal
#' @importFrom XML xmlSApply xmlValue getNodeSet
process.mult.nominal.modif.variable <- function(labels, number.of.rows, parsed.xml,
                                                variable.index, variable.name) {
  aux <- list()
  aux[[1]] <- rep("$M", number.of.rows)
  if (labels) {
    categories <- XML::xpathSApply(parsed.xml, paste0(
      "/assofile/variables/stvar[",
      variable.index, "]/mult_nominal_Modif/nominal-desc/list-nom/label"
    ), XML::xmlValue)
  } else {
    categories <- XML::xpathSApply(parsed.xml, paste0(
      "/assofile/variables/stvar[",
      variable.index, "]/mult_nominal_Modif/nominal-desc/list-nom/name"
    ), XML::xmlValue)
  }
  aux[[2]] <- rep(length(categories), number.of.rows)

  nodes <- XML::getNodeSet(parsed.xml, paste0(
    "/assofile/indiv_mat/ligmat/valmat[", variable.index,
    "]"
  ))

  get.distributions <- function(node) {
    if (length(node["val_list_modal"]) == 0) {
      return(rep(NA, length(categories)))
    } else {
      moda.nodes <- as.numeric(sapply(
        XML::xmlSApply(node, function(x) x["no_moda"]),
        XML::xmlValue
      ))
      frequencies <- as.numeric(sapply(
        XML::xmlSApply(node, function(x) x["frequency"]),
        XML::xmlValue
      ))
      missing.categories.indexes <- setdiff(1:length(categories), moda.nodes)
      for (missing.cat.index in missing.categories.indexes) {
        frequencies <- append(frequencies, 0, after = missing.cat.index - 1)
      }
      return(frequencies)
    }
  }

  all.frequencies <- t(round(sapply(nodes, get.distributions), 3))
  aux <- data.frame(c(aux, as.data.frame(all.frequencies)))

  colnames(aux) <- c("$M", variable.name, categories)
  return(aux)
}

#' process.inter.cont.variable
#' @keywords internal
#' @importFrom XML xmlValue xmlElementsByTagName getNodeSet
process.inter.cont.variable <- function(number.of.rows, parsed.xml, variable.index,
                                        variable.name) {
  aux <- list()
  aux[[1]] <- rep("$I", number.of.rows)

  after.evaluator <- function(node, element.to.retrieve) {
    if (length(node["val_interv"]) == 0) {
      return(NA)
    } else {
      return(as.numeric(XML::xmlValue(XML::xmlElementsByTagName(node[[1]], element.to.retrieve)[[1]])))
    }
  }

  nodes <- XML::getNodeSet(parsed.xml, paste0(
    "/assofile/indiv_mat/ligmat/valmat[", variable.index,
    "]"
  ))
  aux[[2]] <- sapply(nodes, after.evaluator, element.to.retrieve = "pmin")
  aux[[3]] <- sapply(nodes, after.evaluator, element.to.retrieve = "pmax")

  aux <- data.frame(aux)
  colnames(aux) <- c("$I", variable.name, variable.name)
  return(aux)
}

#' process.continue.variable
#' @keywords internal
#' @importFrom XML xmlValue xpathSApply
process.continue.variable <- function(number.of.rows, parsed.xml, variable.index, variable.name) {
  aux <- list()
  aux[[1]] <- rep("$C", number.of.rows)

  after.evaluator <- function(node) {
    if (length(node["val_conti"]) == 0) {
      return(NA)
    } else {
      return(as.numeric(XML::xmlValue(node[[1]])))
    }
  }

  aux[[2]] <- XML::xpathSApply(parsed.xml, paste0(
    "/assofile/indiv_mat/ligmat/valmat[",
    variable.index, "]"
  ), after.evaluator)

  aux <- data.frame(aux)
  colnames(aux) <- c("$C", variable.name)
  return(aux)
}
