## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
systematicReview_exampleForm <-
  get(data(list="form_inclSysRev_v0_92", package='preregr'));

## -----------------------------------------------------------------------------
formExample <-
  preregr::form_create(
    title = "Minimal form with only a few fields",
    version = "0.0.1",
    author = "Stibbons, P."
  ) |>
  preregr::form_show();

## -----------------------------------------------------------------------------
formExample <-
  formExample |>
  preregr::form_add_instruction(
    heading = "Instructions",
    description = paste0(
      "This form is simple, so it doesn't require much ",
      "instruction. Still, it's advisable to always RTFM, ",
      "and in the case of (pre)registration forms, the ",
      "instructions are the manual. So better read closely!"
    )
  ) |>
  preregr::form_add_section(
    id = "only_section",
    label = "Only Section",
    description = paste0(
      "This is the only section in this form. That's ",
      "because this is such a simple form. However, that ",
      "means this section is very important, because it ",
      "contains all the items. Therefore, you may want to ",
      "study this section description very carefully."
    )
  ) |>
  preregr::form_add_item(
    id = "study_title",
    label = "Study Title",
    section_id = "only_section",
    description = paste0(
      "Think of a catchy title, preferably with a colon in ",
      "the middle. Bonus points for pop culture references."
    )
  ) |>
  preregr::form_add_item(
    id = "registration_type",
    label = "Registration type",
    section_id = "only_section",
    description = paste0(
      "Describe briefly why you are (pre)registering this ",
      "study. For example, this might be a preregistration ",
      "to allow others to know you're doing this study; or to ",
      "make it clear you value transparency in science; or to ",
      "remember your original plans later on. Or this might be ",
      "a registration to update your plans after the data came ",
      "in; or to document pragmatic changes in plans."
    )
  ) |>
  preregr::form_show();


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

validSectionIds <-
  x$sections$section_id;

validSectionIds <- validSectionIds[
  !is.na(validSectionIds) &
    nchar(validSectionIds) > 0
];

if (is.null(section)) {
  sectionsToShow <- validSectionIds;
} else {
  sectionsToShow <-
    intersect(
      validSectionIds,
      section
    );
}


## ---- results='asis', eval=TRUE, echo=FALSE-----------------------------------

preregr::heading(
  x$metadata$content[x$metadata$field == "title"],
  idSlug("preregr-form"),
  headingLevel=headingLevel
);


## ---- results='asis', eval=TRUE-----------------------------------------------

if (nrow(x$instructions) > 0) {
  
  preregr::heading("Instructions", headingLevel=headingLevel + 1);
  
  for (i in 1:nrow(x$instructions)) {
  
    preregr::heading(
      x$instructions[i, "heading"],
      idSlug("preregr-form"),
      headingLevel=headingLevel + 2
    );

    cat0("\n\n",
         x$instructions[i, "description"],
         "\n\n");

  }
  
}


## ---- results='asis', eval=TRUE-----------------------------------------------

preregr::heading("Sections and items", headingLevel=headingLevel + 1);

for (section in sectionsToShow) {

  preregr::heading(
    "Section: ",
    x$sections[
      x$sections$section_id==section,
      "section_label"
    ],
    idSlug("preregr-form"),
    headingLevel=headingLevel + 2
  );
  
  item_ids <- x$items$item_id[x$items$section_id == section];
  item_labels <- x$items$item_label[x$items$section_id == section];
  item_descriptions <- x$items$item_description[x$items$section_id == section];
  names(item_labels) <- item_ids;
  names(item_descriptions) <- item_ids;

  for (currentItemId in item_ids) {
    cat0("<div class=\"preregr preregr-form-item-spec ");
    cat0("preregr-form-item\">\n");
    cat0("<div class=\"preregr-item-heading\">\n");
    cat0("<div class=\"preregr-item-label\">",
         item_labels[currentItemId],
         "</div>\n");
    cat0("<div class=\"preregr-item-id\">",
         currentItemId,
         "</div>\n");
    cat0("</div>\n");
    cat0("<div class=\"preregr-item-spec-text\">",
         item_descriptions[currentItemId],
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
  preregr::form_to_json(x);

### Escape single quotes
preregrJSON <-
  gsub("'",
       "&#39;",
       preregrJSON
  );

### Generate id slug
slug <- paste0("preregr-data-", preregr::randomSlug());


## -----------------------------------------------------------------------------
preregr::form_knit(formExample);

## ---- eval=FALSE--------------------------------------------------------------
#  preregr::form_to_xlsx(
#    formExample,
#    file = "C:/Data/Research/amazing-new-project/prereg-form.xlsx"
#  );

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

validSectionIds <-
  x$sections$section_id;

validSectionIds <- validSectionIds[
  !is.na(validSectionIds) &
    nchar(validSectionIds) > 0
];

if (is.null(section)) {
  sectionsToShow <- validSectionIds;
} else {
  sectionsToShow <-
    intersect(
      validSectionIds,
      section
    );
}


## ---- results='asis', eval=TRUE, echo=FALSE-----------------------------------

preregr::heading(
  x$metadata$content[x$metadata$field == "title"],
  idSlug("preregr-form"),
  headingLevel=headingLevel
);


## ---- results='asis', eval=TRUE-----------------------------------------------

if (nrow(x$instructions) > 0) {
  
  preregr::heading("Instructions", headingLevel=headingLevel + 1);
  
  for (i in 1:nrow(x$instructions)) {
  
    preregr::heading(
      x$instructions[i, "heading"],
      idSlug("preregr-form"),
      headingLevel=headingLevel + 2
    );

    cat0("\n\n",
         x$instructions[i, "description"],
         "\n\n");

  }
  
}


## ---- results='asis', eval=TRUE-----------------------------------------------

preregr::heading("Sections and items", headingLevel=headingLevel + 1);

for (section in sectionsToShow) {

  preregr::heading(
    "Section: ",
    x$sections[
      x$sections$section_id==section,
      "section_label"
    ],
    idSlug("preregr-form"),
    headingLevel=headingLevel + 2
  );
  
  item_ids <- x$items$item_id[x$items$section_id == section];
  item_labels <- x$items$item_label[x$items$section_id == section];
  item_descriptions <- x$items$item_description[x$items$section_id == section];
  names(item_labels) <- item_ids;
  names(item_descriptions) <- item_ids;

  for (currentItemId in item_ids) {
    cat0("<div class=\"preregr preregr-form-item-spec ");
    cat0("preregr-form-item\">\n");
    cat0("<div class=\"preregr-item-heading\">\n");
    cat0("<div class=\"preregr-item-label\">",
         item_labels[currentItemId],
         "</div>\n");
    cat0("<div class=\"preregr-item-id\">",
         currentItemId,
         "</div>\n");
    cat0("</div>\n");
    cat0("<div class=\"preregr-item-spec-text\">",
         item_descriptions[currentItemId],
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
  preregr::form_to_json(x);

### Escape single quotes
preregrJSON <-
  gsub("'",
       "&#39;",
       preregrJSON
  );

### Generate id slug
slug <- paste0("preregr-data-", preregr::randomSlug());


## -----------------------------------------------------------------------------
preregr::form_knit(systematicReview_exampleForm);

