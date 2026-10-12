cql <- function(source_frame, cql_string) {
  mystr = cql_string;
  dframe = source_frame;
  #... do more versatile checking so that order doesn't matter
  cql_string = str_replace(cql_string, "[\n\r\t]", " ")

  pattset = c("chart\\s+(bar|scatter|hist|line)(?:\\s+|$)",
              "X\\s+([A-Za-z][A-Za-z0-9_\\.]*?)( *, *([A-Za-z][A-Za-z0-9_\\.]*?))?(?:\\s+|$)",
              "Y\\s+([A-Za-z][A-Za-z0-9_\\.]*?)(?:\\s+|$)",
              "X_label\\s+'(.*?)'(?:\\s+|$)",
              "Y_label\\s+'(.*?)'(?:\\s+|$)",
              "Legend\\s+'(.*?)'(?:\\s+|$)",
              "Colorset\\s+'(.*?)'(?:\\s+|$)",
              "AggFunc\\s+'(.*?)'(?:\\s+|$)",
              "ConfInt\\s+'(.*?)'(?:\\s+|$)",
              "Fit\\s+'(.*?)'(?:\\s+|$)");

  varset = list();
  msgs = list(); #... output messages

  
  # >> Aggregate and generate confidence intervals
  agg_res <- function(x) {
    return (c(aggfunc(x), conf_int_vector(x)))
  }
  conf_int_vector <- function(x) {
    x_sd = sd(x, na.rm=TRUE);
    x_n = sqrt(length(x));
    tval = qt(varset[["confint"]], length(x)-1);
    # >> Return both SE and ConfInt
    return (c(x_sd/x_n, tval * (x_sd/x_n)));
  }
  
  aggfunc <- function(x) {
    agg = varset[["aggfunc"]];
    if(agg=="mean") {
      return (mean(x, na.rm=TRUE))
    }
    else if(agg=="sum") {
      return (sum(x, na.rm=TRUE))
    }
    else if(agg=="median") {
      return (median(x, na.rm=TRUE))
    }
    else if(agg=="count") {
      return (length(x[!is.na(x)]))
    }
  }

  counter=0;
  varset[["aggfunc"]] = "mean";
  varset[["confint"]] = 0.975;
  varset[["smooth_func"]] = "y ~ poly(x, 1)";
  varset[["fit_se"]] = FALSE;
  
  while(TRUE) {
    earliest_found = nchar(mystr)+1;
    best_res = NULL;
    best_loc = NULL;
    best_idx = -1;
    for(i in 1:length(pattset)) {
      patt = paste("(?i)",pattset[i],sep="");
      results = str_match(mystr, patt)[1,];
      loc_point = str_locate(mystr, patt);
      if(!is.na(loc_point[1,c("start")])) {
        #... this is a valid pattern
        curfound =  as.numeric(loc_point[1,c('start')]);
        if(curfound<earliest_found) {
          earliest_found = curfound;
          best_res = results;
          best_loc = loc_point;
          best_idx = i;
        }
      }
    }

    if(best_idx==-1) {
      break;
    }


    mystr = substr(mystr, as.numeric(best_loc[1,c("end")]), nchar(mystr));
    results = best_res;
    i = best_idx;

    if(i==1) {
      #... chart type
      varset[["type"]] = results[2];
    }
    else if(i==2) {
      #... x variable(s)
      varset[["x_var"]] = results[2];
      if(!is.na(results[3])) {
        varset[["x_category"]] = results[4];
      }
    }
    else if(i==3) {
      #... y variable
      varset[["y_var"]] = results[2];
    }
    else if(i==4) {
      #... x label variable
      varset[["x_label"]] = str_replace_all(results[2], "\\\\n", intToUtf8(10));
    }
    else if(i==5) {
      #... y label variable
      varset[["y_label"]] = str_replace_all(results[2], "\\\\n", intToUtf8(10));
    }
    else if(i==6) {
      #... legend label variable
      varset[["legend"]] = str_replace_all(results[2], "\\\\n", intToUtf8(10));
    }
    else if(i==7) {
      #... colorset variable
      varset[["colorset"]] = str_split(gsub(" ", "", results[2]), ",")[[1]];
    }
    else if(i==8) {
      #... barchart aggregating variable
      varset[["aggfunc"]] = str_split(gsub(" ", "", results[2]), ",")[[1]];
    }
    else if(i==9) {
      #... error bars confidence interval variable
      varset[["confint"]] = as.numeric(str_split(gsub(" ", "", results[2]), ",")[[1]]);
      #... get the actual reverse value (0.95 should be at 0.975 so it covers 0.975 and 0.025)
      varset[["confint"]] = varset[["confint"]] + (1 - varset[["confint"]])/2;
      varset[["confFound"]] = TRUE;
    }
    else if(i==10) {
      #... fitting method (for continuous plots)
      varset[["fit"]] = tolower(str_split(gsub(" ", "", results[2]), ",")[[1]]);
      if(varset[["fit"]]=="true") {
        varset[["fit_se"]] = TRUE;
      }
    }

    counter = counter + 1;
    if(counter>30) {
      stop("Malformed string. Both X and Y must be specified.");
      break
    }
  }



  if (!("type" %in% names(varset))) {
    stop("Invalid chart type specified");
  }

  ctype = varset[["type"]];
  x1_var = varset[["x_var"]];
  x2_var = varset[["x_category"]];
  y_var = varset[["y_var"]];
  legend = varset[["legend"]];
  if(!("x_label" %in% names(varset))) {
    varset[["x_label"]] = x1_var;
  }
  if(!("y_label" %in% names(varset))) {
    varset[["y_label"]] = y_var;
  }
  if(!("legend" %in% names(varset))) {
    varset[["legend"]] = x2_var;
  }
  if(ctype=="hist") {
    varset[["y_label"]] = "Count";
  }
  p1 = NULL;


  if(is.null(x1_var) | is.null(y_var)) {
    stop("Malformed string");
    #return (list("response"="Malformed String", "plot_obj"=p1));
  }

  
  
  
  
  #   SCATTER: generate the plot
  if(ctype=="scatter") {
    if(!is.null(x2_var)) {
      dframe[,c(x2_var)] = factor(dframe[,c(x2_var)]);
      p1 = ggplot(data=dframe, aes_string(x=x1_var, y=y_var, color=x2_var)) + geom_point();
      #... see if we should add a fitted curve
      if(!is.null(varset[["fit"]])) {
        p1 <- p1 + geom_smooth(method="lm", formula=varset[["smooth_func"]], se=varset[["fit_se"]], aes_string(x=x1_var, y=y_var, group=x2_var));
      }
      if(!is.null(varset[["colorset"]])) {
        p1 <- p1 + scale_color_manual(values=varset[["colorset"]])
      }
      if(varset[["legend"]]!="<>") {
        p1 <- p1 + guides(color=guide_legend(title=varset[["legend"]]))
      }
    }
    else {
      #... no factor variables
      p1 = ggplot(data=dframe, aes_string(x=x1_var, y=y_var)) + geom_point();
      #... see if we should add a fitted curve
      if(!is.null(varset[["fit"]])) {
        p1 <- p1 + geom_smooth(method="lm", formula=varset[["smooth_func"]], se=varset[["fit_se"]], aes_string(x=x1_var, y=y_var));
      }
    }
  }

  
  

  #   LINE: generate the plot
  if(ctype=="line") {
    #... if this is a factor set, do aggregation
    #if(is.factor(dframe[,c(x1_var)])) {
    x1_fact = is.factor(dframe[,c(x1_var)]);
      if(!is.null(x2_var)) {
        dframe = aggregate(dframe[,c(y_var)], list(dframe[,c(x1_var)], dframe[,c(x2_var)]), agg_res);
        dframe = cbind(dframe[,c("Group.1", "Group.2")], data.frame(dframe$x));
        colnames(dframe) = c("X", "X2", "Y", "SE", "CONF");
        dframe[,c("X2")] = factor(dframe[,c("X2")]);
      }
      else {
        if(x1_fact) {
          dframe = aggregate(dframe[,c(y_var)], list(dframe[,c(x1_var)]), agg_res);
          dframe = cbind(dframe[,c("Group.1")], data.frame(dframe$x));
          colnames(dframe) = c("X", "Y", "SE", "CONF");
        }
        else {
          dframe = dframe[,c(x1_var, y_var)];
          colnames(dframe) = c("X", "Y");
        }
      }
    

    #... see if we need to specify grouping for the line
    pd <- position_dodge(0.1);
    if(!is.null(x2_var)) {
      p1 = ggplot(data=dframe, aes_string(x="X", y="Y", group="X2", color="X2")) + geom_line(position=pd);
      p1 <- p1 + geom_point(shape=19, position=pd);
      if(!is.null(varset[["colorset"]])) {
        p1 <- p1 + scale_color_manual(values=varset[["colorset"]])
      }
      if(varset[["legend"]]!="<>") {
        p1 <- p1 + guides(color=guide_legend(title=varset[["legend"]]))
      }
      if(!is.null(varset[["confFound"]])) {
        p1 <- p1 + geom_errorbar(aes_string(ymin="Y - CONF", ymax="Y + CONF"), position=pd, width=0.2);
      }
      if(!is.null(varset[["fit"]])) {
        p1 <- p1 + geom_smooth(method="lm", formula=varset[["smooth_func"]], se=varset[["fit_se"]], aes_string(x="X", y="Y", group="X2"));
      }
    }
    else {
      #... single variable
      if(x1_fact) {
        #... treat this as a factor variable
        p1 = ggplot(data=dframe, aes_string(x="X", y="Y", group=1)) + geom_line(position=pd);
      }
      else {
        #... treat this as a continous variable
        p1 = ggplot(data=dframe, aes_string(x="X", y="Y")) + geom_line()
      }
      p1 <- p1 + geom_point(shape=19, position=pd);
      if(!is.null(varset[["confFound"]]) & x1_fact) {
        p1 <- p1 + geom_errorbar(aes_string(ymin="Y - CONF", ymax="Y + CONF"), position=pd, width=0.2);
      }
      if(!is.null(varset[["fit"]])) {
        p1 <- p1 + geom_smooth(method="lm", formula=varset[["smooth_func"]], se=varset[["fit_se"]], aes_string(x="X", y="Y", group=1));
      }
    }
    
  }


  
  
  #   BAR: generate the plot
  if(ctype=="bar") {
    
    dframe[,c(x1_var)] = factor(dframe[,c(x1_var)]);
    if(!is.null(x2_var)) {
      dframe = aggregate(dframe[,c(y_var)], list(dframe[,c(x1_var)], dframe[,c(x2_var)]), agg_res);
      dframe = cbind(dframe[,c("Group.1", "Group.2")], data.frame(dframe$x));
      colnames(dframe) = c("X", "X2", "Y", "SE", "CONF");
      dframe[,c("X2")] = factor(dframe[,c("X2")]);

      # >> Plot the new data.
      p1 = ggplot(data=dframe, aes_string(x="X", y="Y", fill="X2")) +
        geom_bar(stat="identity", position="dodge");
      
      # >> Check other options if they were set.
      if(!is.null(varset[["colorset"]])) {
        p1 <- p1 + scale_fill_manual(values=varset[["colorset"]])
      }
      if(varset[["legend"]]!="<>") {
        p1 <- p1 + guides(fill=guide_legend(title=varset[["legend"]]))
      }
      if(!is.null(varset[["confFound"]])) {
        p1 <- p1 + geom_errorbar(aes_string(ymin="Y-CONF", ymax="Y+CONF"), position=position_dodge(0.9), width=0.2);
      }
    }
    else {
      dframe = aggregate(dframe[,c(y_var)], list(dframe[,c(x1_var)]), agg_res);
      dframe = cbind(dframe[,c("Group.1")], data.frame(dframe$x));
      colnames(dframe) = c("X", "Y", "SE", "CONF");
      
      # >> Plot the new data.
      p1 <- ggplot(data=dframe, aes_string(x="X", y="Y")) +
        geom_bar(stat="identity", position="dodge");
    }
    if(!is.null(varset[["confFound"]])) {
      p1 <- p1 + geom_errorbar(aes_string(ymin="Y-CONF", ymax="Y+CONF"), position=position_dodge(0.9), width=0.2);
    }
  }
  
  
  



  #   HIST: generate the plot
  if(ctype=="hist") {
    p1 = ggplot(data=dframe, aes_string(x=x1_var))+
      geom_histogram(position="identity");#, bins=50);
    if(!is.null(x2_var)) {
      dframe[,c(x2_var)] = factor(dframe[,c(x2_var)]);
      p1 = ggplot(data=dframe, aes_string(x=x1_var, fill=x2_var)) +
        geom_histogram( position="identity", alpha=0.9);
      if(!is.null(varset[["colorset"]])) {
        p1 <- p1 + scale_fill_manual(values=varset[["colorset"]])
      }
      if(varset[["legend"]]!="<>") {
        p1 <- p1 + guides(fill=guide_legend(title=varset[["legend"]]))
      }
    }
  }


  
  
  #   Finalize the parameters and output
  if(!is.null(p1)) {
    p1 <- p1 + theme(text = element_text(size=16))+
      xlab(varset[["x_label"]]) + ylab(varset[["y_label"]]);
    if(!is.null(varset[["legend"]])) {
      if(varset[["legend"]]=="<>") {
        p1 <- p1 + guides(fill=FALSE, color=FALSE);
      }
    }
    p1
  }
  return (list("response"=varset, "plot_obj"=p1));

}