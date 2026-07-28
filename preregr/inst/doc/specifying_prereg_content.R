## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  echo = TRUE,
  collapse = TRUE,
  comment = "#>"
);

## ---- preregr-example-1a------------------------------------------------------

preregExample <-
  preregr::prereg_initialize(
    "inclSysRev_v0_92"
  );


## ---- preregr-example-1b------------------------------------------------------

preregr::prereg_next_item(
  preregExample,
  nrOfItems = 4
);


## ---- preregr-example-1c------------------------------------------------------

preregExample <-
  preregExample |>
  preregr::prereg_specify(
    title = "Example Study",
    authors = "Littlebottom, C., Dibbler, C., & Aching, T."
  );


## ---- preregr-example-2-------------------------------------------------------

preregExample <-
  preregExample |>
  preregr::prereg_specify(
    nonExistent_item = "This can't be stored anywhere"
  );


## ---- preregr-example-3-------------------------------------------------------

preregExample <-
  preregExample |>
  preregr::prereg_specify(
    start_date = "2021-9-01"
  );


## ---- preregr-example-4-------------------------------------------------------

preregExample <-
  preregExample |>
  preregr::prereg_specify(
    start_date = "2021-09-01"
  );


## ---- preregr-example-5-------------------------------------------------------

preregExample |>
  preregr::prereg_show_item_content(
    section="metadata"
  );


## ---- preregr-example-6-------------------------------------------------------

preregExample |>
  preregr::prereg_show_item_completion();


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


## ---- preregr-example-7-------------------------------------------------------

preregr::prereg_knit_item_content(
  preregExample,
  section="metadata"
);


## ---- preregr-example-8-------------------------------------------------------

preregExample <-
  preregExample |>
  preregr::prereg_justify(
    item = "start_date",
    decision = "We decided to start on the first, rather than the second, of September 2021.",
    justification = "It's a bit weird to start on the second day of a month."
  );


