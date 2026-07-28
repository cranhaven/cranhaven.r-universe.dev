## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
importedExample <-
  preregr::import_from_html("https://preregr.opens.science/articles/specifying_prereg_content.html");

## -----------------------------------------------------------------------------
importedExample;

## ---- eval=TRUE, echo=FALSE---------------------------------------------------

###-----------------------------------------------------------------------------
###-----------------------------------------------------------------------------
###-----------------------------------------------------------------------------

lightenColor <- function(x,
                         amount = .4) {
  x <- 255 - col2rgb(x);
  x <- amount * x;
  x <- 255 - x;
  x <- rgb(t(x), maxColorValue=255);
  return(x);
}

###-----------------------------------------------------------------------------

### Colors from https://doi.org/10.5281/zenodo.3381071

Okabe_Ito <- c("#E69F00", "#56B4E9",
               "#009E73", "#F0E442",
               "#0072B2", "#D55E00", "#CC79A7");

# scales::show_col(Okabe_Ito);

orange    <- Okabe_Ito[1];
lightBlue <- Okabe_Ito[2];
green     <- Okabe_Ito[3]
yellow    <- Okabe_Ito[4]
darkBlue  <- Okabe_Ito[5];
red       <- Okabe_Ito[6];
pink      <- Okabe_Ito[7];

### Colors: orange, lightBlue, green, yellow, darkBlue, red, pink

orange_l <- lightenColor(orange);
lightBlue_l <- lightenColor(lightBlue);
green_l <- lightenColor(green);
yellow_l <- lightenColor(yellow);
darkBlue_l <- lightenColor(darkBlue);
red_l <- lightenColor(red);
pink_l <- lightenColor(pink);

orangeBg <- lightenColor(orange, amount=.05);
greenBg <- lightenColor(green, amount=.05);

###-----------------------------------------------------------------------------

oldKableViewOption <- getOption("kableExtra_view_html", NULL);
options(kableExtra_view_html = FALSE);

oldSilentOption <- preregr::opts$get("silent");
preregr::opts$set(silent = TRUE);

knitr::opts_chunk$set(echo = FALSE, comment="");

if (!exists('headingLevel') || !is.numeric(headingLevel) || (length(headingLevel) != 1)) {
  headingLevel <- 0;
}

if (is.null(section)) {
  sectionsToShow <- x$form$sections$section_id;
} else {
  sectionsToShow <-
    intersect(
      x$form$sections$section_id,
      section
    );
}


## ---- results='asis', eval=TRUE, echo=FALSE-----------------------------------

preregr::heading(
  x$form$metadata[x$form$metadata$field == "title", "content"],
  idSlug("preregr-prereg-spec"),
  headingLevel=headingLevel
);


## ---- results='asis', eval=TRUE-----------------------------------------------

for (section in sectionsToShow) {

  preregr::heading(
    "Section: ",
    x$form$sections[
      x$form$sections$section_id==section,
      "section_label"
    ],
    idSlug("preregr-prereg-spec"),
    headingLevel=headingLevel + 1
  );
  
  item_ids <- x$form$items$item_id[x$form$items$section_id == section];
  item_labels <- x$form$items$item_label[x$form$items$section_id == section];
  names(item_labels) <- item_ids;

  for (currentItemId in item_ids) {
    cat0("<div class=\"preregr preregr-item-spec ");
    if (x$specs[[currentItemId]] == x$config$initialText) {
      cat0("preregr-unspecified\">\n");
    } else {
      cat0("preregr-specified\">\n");
    }
    cat0("<div class=\"preregr-item-heading\">\n");
    cat0("<div class=\"preregr-item-label\">",
         item_labels[currentItemId],
         "</div>\n");
    cat0("<div class=\"preregr-item-id\">",
         currentItemId,
         "</div>\n");
    cat0("</div>\n");
    cat0("<div class=\"preregr-item-spec-text\">",
         ifelse(!is.null(x$specs[[currentItemId]]$text) &&
                  !is.na(x$specs[[currentItemId]]$text) &&
                  nchar(x$specs[[currentItemId]]$text) > 0,
                x$specs[[currentItemId]]$text,
                "&nbsp;"),
         "</div>\n");
    cat0("</div>\n");
  }

}


## ---- echo=FALSE--------------------------------------------------------------

preregr::opts$set(silent = oldSilentOption);

if (!is.null(oldKableViewOption)) {
  options(kableExtra_view_html = oldKableViewOption);
}


## ---- echo=FALSE--------------------------------------------------------------

### Generate JSON
preregrJSON <-
  preregr::prereg_spec_to_json(x);

### Escape single quotes
preregrJSON <-
  gsub("'",
       "&#39;",
       preregrJSON
  );

### Generate id slug
slug <- paste0("preregr-data-", preregr::randomSlug());


## -----------------------------------------------------------------------------
preregr::prereg_knit_item_content(
  importedExample,
  section="metadata"
);

## -----------------------------------------------------------------------------
freshPrereg <-
  preregr::prereg_initialize(
    importedExample
  );

## -----------------------------------------------------------------------------
freshPrereg;

