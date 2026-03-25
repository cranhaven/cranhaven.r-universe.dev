#' Format_BaHZING Function
#'
#' This function takes a phyloseq object and performs formatting operations on it,
#' including modifying the taxonomic table, uniting taxonomic levels, and creating
#' matrices based on taxonomic information.
#' @details
#' The Format_BaHZING function is the core function of the Format_BaHZING package.
#' It takes a phyloseq object as input and performs various formatting operations
#' to prepare the data for analysis. The function modifies the taxonomic table to add
#' taxonomic prefixes (e.g., "d__" for Kingdom), unites taxonomic levels, and creates
#' matrices based on taxonomic information. The formatted data is then returned as a list
#' containing different data frames for further analysis.
#'
#' The package relies on the `phyloseq`, `dplyr`, and `stringr` packages for data manipulation,
#' and also uses functions from `tidyr` to unite taxonomic levels.
#'
#' The main function `Format_BaHZING` is exported and can be accessed by other packages or scripts
#' that depend on the functionalities provided by this package.
#'
#'
#' @param phyloseq.object A phyloseq object.
#' @return A list with the following elements:
#'   - `Table`: Formatted microbiome data as a data frame.
#'   - `Species.Genus.Matrix`: Data frame for species-genus relationships (optional).
#'   - `Genus.Family.Matrix`: Data frame for genus-family relationships (optional).
#'   - `Family.Order.Matrix`: Data frame for family-order relationships (optional).
#'   - `Order.Class.Matrix`: Data frame for order-class relationships (optional).
#'   - `Class.Phylum.Matrix`: Data frame for class-phylum relationships (optional).
#' @details The column names 'Kingdom', 'Phylum', 'Class', 'Order', 'Family', 'Genus', and 'Species'
#' in the tax_table of the phyloseq object should be user-defined and assigned in this function.
#' The function will use these column names to perform various operations.
#'
#' @note The column names 'Kingdom', 'Phylum', 'Class', 'Order', 'Family', 'Genus', and 'Species' in the
#' tax_table are expected to be user-defined and assigned within the function.
#'
#' @import phyloseq
#' @import dplyr
#' @import tidyr
#' @import stringr
#'
#' @importFrom phyloseq sample_data otu_table tax_table
#' @importFrom dplyr full_join select mutate %>%
#' @importFrom tidyr unite
#' @importFrom stringr str_c
#' @export
#' @name Format_BaHZING

# Declare global variables
globalVariables(c("Domain"))

Format_BaHZING <- function(phyloseq.object) {
  # Initialize variables to store taxonomic levels and ASV information
  Kingdom <- NULL    # Variable to store Kingdom taxonomic level
  Phylum <- NULL     # Variable to store Phylum taxonomic level
  Class <- NULL      # Variable to store Class taxonomic level
  Order <- NULL      # Variable to store Order taxonomic level
  Family <- NULL     # Variable to store Family taxonomic level
  Genus <- NULL      # Variable to store Genus taxonomic level
  Species <- NULL    # Variable to store Species taxonomic level
  ASV <- NULL        # Variable to store Amplicon Sequence Variant information


  # Check if taxa are stored as rows in the phyloseq object
  if (phyloseq::taxa_are_rows(phyloseq.object)) {
    # If taxa are stored as rows, transpose the phyloseq object
    # to convert the taxa into columns, making further processing easier
    phyloseq.object <- t(phyloseq.object)
  }

  # Extract metadata, OTU table, and taxonomic table from the phyloseq object
  meta.data <- data.frame(phyloseq::sample_data(phyloseq.object))  # Data frame containing metadata information
  otu.table <- data.frame(phyloseq::otu_table(phyloseq.object))    # Data frame containing OTU (Operational Taxonomic Unit) table
  taxa.table <- data.frame(phyloseq::tax_table(phyloseq.object))  # Data frame containing taxonomic table

  # Check if the taxonomic table contains more than one taxonomic level
  if (length(colnames(taxa.table)) < 2) {
    # If the table has less than two taxonomic levels, raise an error and stop the function
    stop("Need > 1 taxonomic level")
  }


  # Add taxonomic prefixes to the taxonomic levels in the taxonomic table
  if ("Kingdom" %in% colnames(taxa.table)) {
    # If 'Kingdom' column is present in the taxonomic table, add 'k__' prefix
    taxa.table <- taxa.table %>%
      mutate(Kingdom=ifelse(grepl("k__",Kingdom),Kingdom,paste0("k__", Kingdom)))
    # taxa.table <- taxa.table %>%
    #   mutate(Kingdom=ifelse(grepl("k__NA",Kingdom),NA,Kingdom))
  }

  if ("Domain" %in% colnames(taxa.table)) {
    # If 'Kingdom' column is present in the taxonomic table, add 'k__' prefix
    taxa.table <- taxa.table %>%
      rename(Kingdom=Domain)%>%
      mutate(Kingdom=case_when(grepl("d__",Kingdom) ~ str_replace(Kingdom,"d__","k__"),
                               grepl("k__",Kingdom) ~ Kingdom,
                               !grepl("d__",Kingdom) & !grepl("k__",Kingdom) ~ paste0("k__", Kingdom)))
  }

  #If missing kingdom level information, stop and produce error message
  if ("k__NA" %in% taxa.table$Kingdom) {
    stop("Missing Kindom-level information for an ASV/OTU. Remove unidentified bacteria and rerun.")
  }

  if ("Phylum" %in% colnames(taxa.table)) {
    # If 'Phylum' column is present in the taxonomic table, add 'p__' prefix
    taxa.table <- taxa.table %>%
      mutate(Phylum=ifelse(grepl("p__",Phylum),Phylum,paste0("p__", Phylum)))
    taxa.table <- taxa.table %>%
      mutate(Phylum=ifelse(grepl("p__NA",Phylum),NA,Phylum))
  }

  if ("Class" %in% colnames(taxa.table)) {
    # If 'Class' column is present in the taxonomic table, add 'c__' prefix
    taxa.table <- taxa.table %>%
      mutate(Class=ifelse(grepl("c__",Class),Class,paste0("c__", Class)))
    taxa.table <-taxa.table %>%
      mutate(Class=ifelse(grepl("c__NA",Class),NA,Class))
  }

  if ("Order" %in% colnames(taxa.table)) {
    # If 'Order' column is present in the taxonomic table, add 'o__' prefix
    taxa.table <- taxa.table %>%
      mutate(Order=ifelse(grepl("o__",Order),Order,paste0("o__", Order)))
    taxa.table <-taxa.table %>%
      mutate(Order=ifelse(grepl("o__NA",Order),NA,Order))
  }

  if ("Family" %in% colnames(taxa.table)) {
    # If 'Family' column is present in the taxonomic table, add 'f__' prefix
    taxa.table <- taxa.table %>%
      mutate(Family=ifelse(grepl("f__",Family),Family,paste0("f__", Family)))
    taxa.table <-taxa.table %>%
      mutate(Family=ifelse(grepl("f__NA",Family),NA,Family))
  }

  if ("Genus" %in% colnames(taxa.table)) {
    # If 'Genus' column is present in the taxonomic table, add 'g__' prefix
    taxa.table <- taxa.table %>%
      mutate(Genus=ifelse(grepl("g__",Genus),Genus,paste0("g__", Genus)))
    taxa.table <-taxa.table %>%
      mutate(Genus=ifelse(grepl("g__NA",Genus),NA,Genus))
  }

  if ("Species" %in% colnames(taxa.table)) {
    # If 'Species' column is present in the taxonomic table, add 's__' prefix
    taxa.table <- taxa.table %>%
      mutate(Species=ifelse(grepl("s__",Species),Species,paste0("s__", Species)))
    taxa.table <- taxa.table %>%
      mutate(Species=ifelse(grepl("s__NA",Species),NA,Species))
  }
  #If species level not present, create species column.
  if (length(grep("Species",colnames(taxa.table)))<1) {
    taxa.table <- taxa.table %>%
      mutate(Species=paste0("unclassified",row_number()))
    taxa.table$Species <- paste0("s__",taxa.table$Species)
    taxa.table <- taxa.table %>%
      mutate(Species=ifelse(grepl("s__NA",Species),NA,Species))
  }

  #Fill in any NAs with unclassified
  if ("Phylum" %in% colnames(taxa.table)) {
    phylum.fill <- sort(unique(which(!grepl("p__",taxa.table$Phylum))))
    phylum.count <- length(which(!grepl("p__",taxa.table$Phylum)))
    unclassified_names <- paste0("p__unclassified", 1:phylum.count)
    taxa.table[2][phylum.fill,] <- unclassified_names
  }

  if ("Class" %in% colnames(taxa.table)) {
    class.fill <- sort(unique(which(!grepl("c__",taxa.table$Class))))
    class.count <- length(which(!grepl("c__",taxa.table$Class)))
    unclassified_names <- paste0("c__unclassified", 1:class.count)
    taxa.table[3][class.fill,] <- unclassified_names
  }

  if ("Order" %in% colnames(taxa.table)) {
    order.fill <- sort(unique(which(!grepl("o__",taxa.table$Order))))
    order.count <- length(which(!grepl("o__",taxa.table$Order)))
    unclassified_names <- paste0("o__unclassified", 1:order.count)
    taxa.table[4][order.fill,] <- unclassified_names
  }

  if ("Family" %in% colnames(taxa.table)) {
    family.fill <- sort(unique(which(!grepl("f__",taxa.table$Family))))
    family.count <- length(which(!grepl("f__",taxa.table$Family)))
    unclassified_names <- paste0("f__unclassified", 1:family.count)
    taxa.table[5][family.fill,] <- unclassified_names
  }

  if ("Genus" %in% colnames(taxa.table)) {
    genus.fill <- sort(unique(which(!grepl("g__",taxa.table$Genus))))
    genus.count <- length(which(!grepl("g__",taxa.table$Genus)))
    unclassified_names <- paste0("g__unclassified", 1:genus.count)
    taxa.table[6][genus.fill,] <- unclassified_names
  }

  if ("Species" %in% colnames(taxa.table)) {
    species.fill <- sort(unique(which(!grepl("s__",taxa.table$Species))))
    species.count <- length(which(!grepl("s__",taxa.table$Species)))
    unclassified_names <- paste0("s__unclassified", 1:species.count)
    taxa.table[7][species.fill,] <- unclassified_names
  }

  if ("Species" %in% colnames(taxa.table)) {
    species.dups <- which(duplicated(taxa.table$Species))
    species.dups.length <- length(which(duplicated(taxa.table$Species)))
    low <- (species.count+1)
    high <- (species.count+species.dups.length)
    species.dups.count <- (low:high)
    duplicate.names <- paste(taxa.table$Species[species.dups],"_",species.dups.count)
    duplicate.names <- str_replace_all(duplicate.names," ","")
    taxa.table[7][species.dups,] <- duplicate.names
  }


  #Create a name vector for all taxa levels in taxa table
  taxa.names <- colnames(taxa.table)
  # Perform the unite operation for all specified taxonomic levels
  ASV.names <- taxa.table %>%
    unite(ASV, all_of(taxa.names), sep = "_")

  # Rename the columns of the otu.table with the values from the ASV.names$ASV column
  table <- otu.table
  colnames(table) <- ASV.names$ASV

  # Create a key data frame to map the original column names to the ASV names
  key <- data.frame(names=colnames(otu.table), ASV=ASV.names$ASV)

  # Add an 'id' column to the table and set its values to row names of the table
  table$id <- rownames(table)
  rownames(table) <- NULL

  # Add an 'id' column to the meta.data and set its values to row names of the meta.data
  meta.data$id <- rownames(meta.data)
  rownames(meta.data) <- NULL

  # Perform a full join between meta.data and table using the 'id' column as the key
  table <- dplyr::full_join(meta.data, table, by="id")

  # Set the row names of the table to be the 'id' column values
  rownames(table) <- table$id

  # Remove the 'id' column from the table
  table <- table %>%
    select(-id)


  # Combine taxonomic levels from 'Kingdom' to 'Species' into their respective columns
  # The 'unite()' function concatenates the taxonomic levels into a single column for each taxonomic rank.
  taxa.table <- taxa.table %>%
    unite(Species, Kingdom:Species, remove = FALSE) %>%
    unite(Genus, Kingdom:Genus, remove = FALSE) %>%
    unite(Family, Kingdom:Family, remove = FALSE) %>%
    unite(Order, Kingdom:Order, remove = FALSE) %>%
    unite(Class, Kingdom:Class, remove = FALSE) %>%
    unite(Phylum, Kingdom:Phylum, remove = FALSE)


  # Create a binary matrix ('speciesgenus') to represent the relationship between species and genera
  if ("Species" %in% colnames(taxa.table) & "Genus" %in% colnames(taxa.table)) {

    unique_genera <- unique(taxa.table$Genus)
    unique_species <- unique(taxa.table$Species)

    # Create a binary matrix with rows representing unique genera and columns representing unique species
    speciesgenus <- matrix(0, nrow = length(unique_genera), ncol = length(unique_species),
                           dimnames = list(unique_genera, unique_species))

    # Populate the matrix with binary values indicating species presence in each genus
    speciesgenus[cbind(match(taxa.table$Genus, unique_genera),
                       match(taxa.table$Species, unique_species))] <- 1
  }


  # Create a binary matrix ('genusfamily') to represent the relationship between genera and families
  if ("Genus" %in% colnames(taxa.table) & "Family" %in% colnames(taxa.table)) {

    unique_families <- unique(taxa.table$Family)
    unique_genera <- unique(taxa.table$Genus)

    # Create a binary matrix with rows representing unique families and columns representing unique genera
    genusfamily <- matrix(0, nrow = length(unique_families), ncol = length(unique_genera),
                          dimnames = list(unique_families, unique_genera))

    # Populate the matrix with binary values indicating the presence of genera in each family
    genusfamily[cbind(match(taxa.table$Family, unique_families),
                      match(taxa.table$Genus, unique_genera))] <- 1
  }


  # Create binary matrix ('familyorder') representing the relationship between Families and Orders
  if ("Family" %in% colnames(taxa.table) & "Order" %in% colnames(taxa.table)) {

    unique_orders <- unique(taxa.table$Order)
    unique_families <- unique(taxa.table$Family)

    # Create a binary matrix with rows representing unique Orders and columns representing unique Families
    familyorder <- matrix(0, nrow = length(unique_orders), ncol = length(unique_families),
                          dimnames = list(unique_orders, unique_families))

    # Populate the matrix with binary values indicating the presence of Families in each Order
    familyorder[cbind(match(taxa.table$Order, unique_orders),
                      match(taxa.table$Family, unique_families))] <- 1
  }

  # Create binary matrix ('orderclass') representing the relationship between Orders and Class
  if ("Order" %in% colnames(taxa.table) & "Class" %in% colnames(taxa.table)) {

    unique_classes <- unique(taxa.table$Class)
    unique_orders <- unique(taxa.table$Order)

    # Create a binary matrix with rows representing unique Classes and columns representing unique Orders
    orderclass <- matrix(0, nrow = length(unique_classes), ncol = length(unique_orders),
                         dimnames = list(unique_classes, unique_orders))

    # Populate the matrix with binary values indicating the presence of Orders in each Class
    orderclass[cbind(match(taxa.table$Class, unique_classes),
                     match(taxa.table$Order, unique_orders))] <- 1
  }


  # Create binary matrix ('classphylum') representing the relationship between Classes and Phyla
  if ("Class" %in% colnames(taxa.table) & "Phylum" %in% colnames(taxa.table)) {

    unique_phyla <- unique(taxa.table$Phylum)
    unique_classes <- unique(taxa.table$Class)

    # Create a binary matrix with rows representing unique Phyla and columns representing unique Classes
    classphylum <- matrix(0, nrow = length(unique_phyla), ncol = length(unique_classes),
                          dimnames = list(unique_phyla, unique_classes))

    # Populate the matrix with binary values indicating the presence of Classes in each Phylum
    classphylum[cbind(match(taxa.table$Phylum, unique_phyla),
                      match(taxa.table$Class, unique_classes))] <- 1
  }

  # Check if the dataframe named 'table' exists in the current environment
  # if (!exists("table")) {
  #   # If the dataframe 'table' does not exist, raise an error and stop the function
  #   stop("Missing 'Table' dataframe")
  # }

  # Create a list named 'Object' to store different matrices representing relationships between taxonomic levels
  Object <- list()

  # Store the 'table' dataframe in the 'Object' list with the key "Table"
  Object[["Table"]] <- list(table)

  # Check if the 'speciesgenus' matrix exists, if so, store it in the 'Object' list with the key "Species.Genus.Matrix"
  if (exists("speciesgenus")) {
    Object[["Species.Genus.Matrix"]] <- speciesgenus
  }

  # Check if the 'genusfamily' matrix exists, if so, store it in the 'Object' list with the key "Genus.Family.Matrix"
  if (exists("genusfamily")) {
    Object[["Genus.Family.Matrix"]] <- genusfamily
  }

  # Check if the 'familyorder' matrix exists, if so, store it in the 'Object' list with the key "Family.Order.Matrix"
  if (exists("familyorder")) {
    Object[["Family.Order.Matrix"]] <- familyorder
  }

  # Check if the 'orderclass' matrix exists, if so, store it in the 'Object' list with the key "Order.Class.Matrix"
  if (exists("orderclass")) {
    Object[["Order.Class.Matrix"]] <- orderclass
  }

  # Check if the 'classphylum' matrix exists, if so, store it in the 'Object' list with the key "Class.Phylum.Matrix"
  if (exists("classphylum")) {
    Object[["Class.Phylum.Matrix"]] <- classphylum
  }

  # Return the 'Object' list containing the matrices representing relationships between taxonomic levels
  return(Object)
}
