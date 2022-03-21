#
# Copyright (C) 2019 University of Amsterdam
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#

LSBuffonsneedlemanipulation   <- function(jaspResults, dataset, options, state = NULL){
  .buffonsNeedleManipulationSummaryTable(jaspResults, options)
  .buffonsNeedleManipulationPropDistPlot(jaspResults, options)
  .buffonsNeedleManipulationPiDistPlot(jaspResults, options)
  return()
}

.buffonsNeedleManipulationSummaryTable <- function(jaspResults, options){
  if(!is.null(jaspResults[["summaryTable"]])) return()
  # example d for computation
  d <- 5
  l <- options[["length"]]*d/100
  
  ## Summary Table
  summaryTable <- createJaspTable(title = gettext("Summary Table"))
  summaryTable$position <- 1
  summaryTable$dependOn(c("k", "n", "a", "b", "length", "CI", "min", "max"))
  #summaryTable$addCitation("JASP Team (2018). JASP (Version 0.9.2) [Computer software].")
  summaryTable$addColumnInfo(name = "NumObservations", title = gettext("Tosses"), type = "integer")
  summaryTable$addColumnInfo(name = "NumCrosses", title = gettext("Crosses"), type = "integer")
  summaryTable$addColumnInfo(name = "MLE", title = gettextf("MLE for %s", "\u03c0"),   type = "number")
  summaryTable$addColumnInfo(name = "Mass", title = gettext("Interval Mass"),   type = "number")
  summaryTable$addColumnInfo(name = "Median", title = gettextf("Median for %s", "\u03c0"),   type = "number")
  summaryTable$addColumnInfo(name = "lowerCI", title = gettext("Lower"), type = "number", 
                            overtitle = gettextf("%s%% Credible Interval", options[["CI"]]*100))
  summaryTable$addColumnInfo(name = "upperCI", title = gettext("Upper"), type = "number", 
                            overtitle = gettextf("%s%% Credible Interval", options[["CI"]]*100)) 

  # fill in the table
  CI95lower <- 2 * l / (qbeta((1-options[["CI"]])/2, options[["k"]], options[["n"]] - options[["k"]], lower.tail = FALSE) * d)
  med <- 2 * l / (qbeta(.5, options[["k"]], options[["n"]] - options[["k"]], lower.tail = FALSE) * d)
  CI95upper <- 2 * l / (qbeta(1-(1-options[["CI"]])/2, options[["k"]], options[["n"]] - options[["k"]], lower.tail = FALSE) * d)
  mass <- pbeta(2*l/(options[["min"]]*d), options[["k"]], options[["n"]] - options[["k"]]) - pbeta(2*l/(options[["max"]]*d), options[["k"]], options[["n"]] - options[["k"]])
  MLE <- 2*l/(options[["k"]]/options[["n"]]*d)
  
  summaryTable$addRows(list(NumCrosses = options[["k"]], NumObservations = options[["n"]], Mass = mass,
                           lowerCI = CI95lower, Median = med, upperCI = CI95upper, MLE = MLE))
  jaspResults[["summaryTable"]] <- summaryTable
}

.buffonsNeedleManipulationPropDistPlot <- function(jaspResults, options){
  if(!is.null(jaspResults[["propDistPlot"]])) return()
  d <- 5
  l <- options[["length"]]*d/100
  ## 1. prior and posterior plot for proportion of crosses
  if (options[["showPropDistPlot"]]){
    propDistPlot <- createJaspPlot(title = gettext("Prior and Posterior for Proportion of Crosses"),
                                   width = 480, height = 320)
    propDistPlot$position <- 2
    #propDistPlot$dependOn(c("k", "n", "a", "b", "length", "CI", "showPropDistPlot"))
    
    propDistPlot$dependOn(optionsFromObject = jaspResults[["summaryTable"]], 
                          options = c("showPropDistPlot","CIPropDistPlot", "legendPropDistPlot"))
    #propDistPlot$addCitation("JASP Team (2018). JASP (Version 0.9.2) [Computer software].")
    
    # values
    xValue <- seq(0,1,0.005)
    propPost <- dbeta(xValue, options[["a"]] + options[["k"]], options[["b"]] + options[["n"]] - options[["k"]])
    propPrior <-dbeta(xValue, options[["a"]], options[["b"]])
    dataProp <- data.frame(values = c(xValue, xValue),
                          density = c(propPost, propPrior),
                          group = c(rep("Posterior",201), rep("Prior",201))
    )
    
    labels <- c(gettext("Posterior"), gettext("Prior"), "\u03c0")
    
    # axis specification
    propDistPlot0 <- ggplot2::ggplot(data = dataProp,  ggplot2::aes(x = values, y = density)) +
      ggplot2::xlab(gettext("Proportion of Crosses")) +
      ggplot2::ylab(gettext("Density")) +
      ggplot2::geom_line(ggplot2::aes(linetype = group), size = 1) +
      ggplot2::scale_linetype_manual("", values = c("Posterior" = "solid",
                                                    "Prior" = "dashed"),
                                     labels = labels)
    
    # fill in the plot
    propDistPlot$plotObject <- jaspGraphs::themeJasp(propDistPlot0)
    
    if (options[["legendPropDistPlot"]]){
      propDistPlot$plotObject <-  propDistPlot$plotObject + 
        ggplot2::theme(legend.position = "right")
    }
    
    if (options[["CIPropDistPlot"]]){
      propCI95lower <- qbeta((1-options[["CI"]])/2, options[["k"]], options[["n"]] - options[["k"]], lower.tail = FALSE) 
      propCI95lower <- round(propCI95lower, digit = 2)
      
      propmed <- qbeta(.5, options[["k"]], options[["n"]] - options[["k"]], lower.tail = FALSE)
      propmed <- round(propmed, digit = 2)
      
      propCI95upper <- qbeta(1-(1-options[["CI"]])/2, options[["k"]], options[["n"]] - options[["k"]], lower.tail = FALSE)
      propCI95upper <- round(propCI95upper, digit = 2)
      
      propDistPlot$plotObject <- propDistPlot$plotObject +
        ggplot2::annotate("text", x = 0.75, y = 1.6*max(propPost), 
                          label = gettextf("%s%% CI: [%s, %s]", options[["CI"]]*100, propCI95lower, propCI95upper),
                          
                          size = 6
        ) + 
        ggplot2::annotate("segment", x = propCI95lower, xend = propCI95upper, 
                          y = 1.45*max(propPost), yend = 1.45*max(propPost),
                          arrow = grid::arrow(ends = "both", angle = 90, length = grid::unit(.2,"cm")),
                          size = 1)
      
    }
    jaspResults[["propDistPlot"]] <- propDistPlot
  }
}
     
.buffonsNeedleManipulationPiDistPlot <- function(jaspResults, options){
  if(!is.null(jaspResults[["piDistPlot"]])) return()
  d <- 5
  l <- options[["length"]]*d/100
  ## 2. Distribution Plot
  if (options[["showPiDistPlot"]]){
    piDistPlot <- createJaspPlot(title = gettextf("Implied Prior and Posterior for %s", "\u03c0"),  width = 480, height = 320)
    piDistPlot$position <- 3
    #piDistPlot$dependOn(c("k", "n", "a", "b", "length", "CI", "showPiDistPlot", "CIArrow"))
    
    piDistPlot$dependOn(optionsFromObject = jaspResults[["summaryTable"]], 
                        options = c("showPiDistPlot", "legendPiDistPlot", "CIPiDistPlot", "min", "max", "highlight"))
    
    #piDistPlot$addCitation("JASP Team (2018). JASP (Version 0.9.2) [Computer software].")
    # values

    
    CI95lower <- 2 * l / (qbeta((1-options[["CI"]])/2, options[["k"]], options[["n"]] - options[["k"]], lower.tail = FALSE) * d)
    CI95lower <- round(CI95lower, digit = 2)
    
    med <- 2 * l / (qbeta(.5, options[["k"]], options[["n"]] - options[["k"]], lower.tail = FALSE) * d)
    med <- round(med, digit = 2)
    
    CI95upper <- 2 * l / (qbeta(1-(1-options[["CI"]])/2, options[["k"]], options[["n"]] - options[["k"]], lower.tail = FALSE) * d)
    CI95upper <- round(CI95upper, digit = 2)
    
    xlimLower <- min(2,CI95lower-0.5)
    xlimUpperer <- max(4,CI95upper+0.5)
    
    x <- seq(xlimLower,xlimUpperer,length.out = 201)
    yPost <- 2 * l / (x^2 * d) * dbeta((2 * l / (x * d)), options[["a"]] + options[["k"]], options[["b"]] + options[["n"]] - options[["k"]])
    yPrior <- 2 * l / (x^2 * d) * dbeta((2 * l / (x * d)), options[["a"]], options[["b"]])
    
    # to avoid crash
    if(max(yPost) == 0){
      yPi <- seq(0, 1.6*max(yPrior), 1.6*max(yPost)/99)
    }else{
      yPi <- seq(0, 1.6*max(yPost), 1.6*max(yPost)/99)
    }
    
    xInterval <- seq(options[["min"]], options[["max"]], length.out = 100)
    pInterval <- 2*l/(xInterval*d)
    y <- 2 * l / (xInterval^2 * d) * dbeta(pInterval, options[["a"]] + options[["k"]], options[["b"]] + options[["n"]] - options[["k"]])
    
    data <- data.frame(values = c(x, x, rep(pi, 100)),
                      density = c(yPost, yPrior, yPi),
                      group = c(rep("Implied Posterior",201), rep("Implied Prior",201), rep("pi", 100))
    )
    
    #data$group<-factor(data$group, levels=c(gettext("Implied Posterior"),gettext("Implied Prior"),"\u03c0"))
    labels <- c(gettext("Implied Posterior"), gettext("Implied Prior"), "\u03c0")
    
    # plot
    
    piDistPlot0 <- ggplot2::ggplot(data = data,  ggplot2::aes(x = values, y = density)) +
      ggplot2::ggtitle("") + # for , pi
      ggplot2::xlab("\u03c0") +
      ggplot2::ylab(gettext("Density")) +
      ggplot2::coord_cartesian(xlim = c(xlimLower, xlimUpperer), ylim = c(0, 1.6*max(yPost)))
    if (options[["highlight"]]){  
    piDistPlot0 <- piDistPlot0 + 
      ggplot2::geom_polygon(data = data.frame(x = c(xInterval,rev(xInterval)), y = c(y, rep(0,100))), 
                            ggplot2::aes(x = x, y = y),
                            fill = "steelblue") 
    }
    piDistPlot0 <- piDistPlot0 +  
      ggplot2::geom_line(ggplot2::aes(color = group, linetype = group), size = 1) +
      ggplot2::scale_color_manual("", values = c("Implied Posterior" = "black",
                                                 "Implied Prior" = "black",
                                                 "pi" = "red"),
                                  labels = labels) +
      ggplot2::scale_linetype_manual("", values = c("Implied Posterior" = "solid",
                                                    "Implied Prior" = "dashed",
                                                    "pi" = "solid"),
                                     labels = labels)
    
    # fill in the plot
    piDistPlot$plotObject <- jaspGraphs::themeJasp(piDistPlot0)
    
    if (options[["legendPiDistPlot"]]){
      piDistPlot$plotObject <-  piDistPlot$plotObject + 
        ggplot2::theme(legend.position = "right")
    }
    
    if (options[["CIPiDistPlot"]]){
      
      piDistPlot$plotObject <- piDistPlot$plotObject +
        ggplot2::annotate("text", x = xlimUpperer*0.8, y = 1.6*max(yPost), 
                          label = gettextf("%s%% CI: [%s, %s]", options[["CI"]]*100, CI95lower, CI95upper),

                          size = 6
        ) + 
        ggplot2::annotate("segment", x = CI95lower, xend = CI95upper, 
                          y = 1.45*max(yPost), yend = 1.45*max(yPost),
                          arrow = grid::arrow(ends = "both", angle = 90, length = grid::unit(.2,"cm")),
                          size = 1)

    }
    jaspResults[["piDistPlot"]] <- piDistPlot
  }
}