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

LSBuffonsneedlesimulation<- function(jaspResults, dataset, options, state = NULL){
  
  # check whether the state is empty
  .buffonsNeedleSimulationCheckErrors(jaspResults, options)

  ## if not, retrieve the values
  .buffonsNeedleSimulationSummaryTable(jaspResults, options)
  .buffonsNeedleSimulationNeedlePlot(jaspResults, options)
  .buffonsNeedleSimulationPropDistPlot(jaspResults, options)
  .buffonsNeedleSimulationPiDistPlot(jaspResults, options)
}
  
.buffonsNeedleSimulationCheckErrors <- function(jaspResults, options){
  if(is.null(jaspResults[["simulateResults"]])){ #test whether the state is empty
    
    # if empty, create a new state
    simulateResults <- createJaspState()
    simulateResults$dependOn(c("n", "length")) #, "a", "b", "options[["length"]]", "options[["CI"]]"))
    jaspResults[["simulateResults"]] <- simulateResults
    jaspResults[["simulateResults"]]$object <- simulateThrows(options[["n"]], options[["length"]])  
    
  }
}  
  
 
.buffonsNeedleSimulationSummaryTable <- function(jaspResults, options){
  if(!is.null(jaspResults[["summaryTable"]])) return()
  crosses <- jaspResults[["simulateResults"]][["object"]][["k"]]
  
  # example d for computation
  d <- 5
  l <- options[["length"]]*d/100
  ## Summary Table
  summaryTable <- createJaspTable(title = gettext("Summary Table"))
  summaryTable$position <- 1
  
  summaryTable$dependOn(c("n", "length", "a", "b", "CI"))
  #summaryTable$addCitation("JASP Team (2018). JASP (Version 0.9.2) [Computer software].")
  
  summaryTable$addColumnInfo(name = "NumCrosses", title = gettext("Crosses"), type = "integer")
  summaryTable$addColumnInfo(name = "NumObservations", title = gettext("Observations"), type = "integer")
  summaryTable$addColumnInfo(name = "Median", title = gettextf("Median for %s", "\u03c0"), type = "number")
  summaryTable$addColumnInfo(name = "lowerCI", title = gettext("Lower"), type = "number", 
                            overtitle = gettextf("%s%% Credible Interval", options[["CI"]]*100))
  summaryTable$addColumnInfo(name = "upperCI", title = gettext("Upper"), type = "number", 
                            overtitle = gettextf("%s%% Credible Interval", options[["CI"]]*100))

  
  
  # fill in the table
  CI95lower <- 2 * l / (qbeta((1-options[["CI"]])/2, crosses, options[["n"]] - crosses, lower.tail = FALSE) * d)
  CI95lower = round(CI95lower, digit = 2)
  
  med <- 2 * l / (qbeta(.5, crosses, options[["n"]] - crosses, lower.tail = FALSE) * d)
  med = round(med, digit = 2)
  
  CI95upper <- 2 * l / (qbeta(1-(1-options[["CI"]])/2, crosses, options[["n"]] - crosses, lower.tail = FALSE) * d)
  CI95upper = round(CI95upper, digit = 2)
  
  summaryTable$addRows(list(NumCrosses = crosses, NumObservations = options[["n"]],
                           lowerCI = CI95lower, Median = med,   upperCI = CI95upper))
  jaspResults[["summaryTable"]] <- summaryTable
}


.buffonsNeedleSimulationNeedlePlot <- function(jaspResults, options) {
  if(!is.null(jaspResults[["needlePlot"]])) return()
  crosses <- jaspResults[["simulateResults"]][["object"]][["k"]]
  xs <- jaspResults[["simulateResults"]][["object"]][["xs"]] 
  xe <- jaspResults[["simulateResults"]][["object"]][["xe"]] 
  ys <- jaspResults[["simulateResults"]][["object"]][["ys"]] 
  ye <- jaspResults[["simulateResults"]][["object"]][["ye"]]
  
  xsCrosses <- jaspResults[["simulateResults"]][["object"]][["xsCrosses"]] 
  xeCrosses <- jaspResults[["simulateResults"]][["object"]][["xeCrosses"]] 
  ysCrosses <- jaspResults[["simulateResults"]][["object"]][["ysCrosses"]] 
  yeCrosses <- jaspResults[["simulateResults"]][["object"]][["yeCrosses"]]  
  # example d for computation
  d <- 5
  l <- options[["length"]]*d/100
  ## 1. Needle Plot 
  if (options[["showNeedlePlot"]]){
   needlePlot <- createJaspPlot(title = gettext("Needle Plot"),  width = 400, height = 400)
   needlePlot$position <- 2
   #needlePlot$dependOn(c("n", "a", "b", "length", "CI", "showNeedlePlot"))
   needlePlot$dependOn(optionsFromObject = jaspResults[["summaryTable"]], 
                       options = c("showNeedlePlot", "color"))
   #needlePlot$addCitation("JASP Team (2018). JASP (Version 0.9.2) [Computer software].")
   
   needlePlot0 <- ggplot2::ggplot(data= NULL) +
     ggplot2::coord_cartesian(xlim = c(-15, 15), ylim = c(-15, 15)) +
     ggplot2::xlab("") +
     ggplot2::ylab("") +
     ggplot2::geom_segment(ggplot2::aes(x = rep(-13,5), y = c(-10,-5,0,5,10), 
                                        xend = rep(13,5), yend = c(-10,-5,0,5,10))) +
     ggplot2::geom_segment(ggplot2::aes(x = xs, y = ys, xend = xe, yend = ye), color = "blue")
   
   # fill in the plot
   if (options[["color"]]){
     needlePlot$plotObject <- jaspGraphs::themeJasp(needlePlot0) +
       ggplot2::geom_segment(ggplot2::aes(x = xsCrosses, y = ysCrosses, 
                                          xend = xeCrosses, yend = yeCrosses), color = "orange") 
   }else{
     needlePlot$plotObject <- jaspGraphs::themeJasp(needlePlot0)
   }
   jaspResults[["needlePlot"]] <- needlePlot
  }
}

.buffonsNeedleSimulationPropDistPlot <- function(jaspResults, options) {
  if(!is.null(jaspResults[["propDistPlot"]])) return()
  # example d for computation
  crosses <- jaspResults[["simulateResults"]][["object"]][["k"]]
  
  d <- 5
  l <- options[["length"]]*d/100
  ## 2. prior and posterior plot for proportion of crosses
  if (options[["showPropDistPlot"]]){
   
   propDistPlot <- createJaspPlot(title = gettext("Prior and Posterior for Proportion of Crosses"),
                                  width = 480, height = 320)
   propDistPlot$position <- 3
   #propDistPlot$dependOn(c("n", "a", "b", "length", "CI", "showPropDistPlot"))
   propDistPlot$dependOn(optionsFromObject = jaspResults[["summaryTable"]], 
                         options = c("showPropDistPlot", "legendPropDistPlot"))
   #propDistPlot$addCitation("JASP Team (2018). JASP (Version 0.9.2) [Computer software].")
   
   # values
   xValue <- seq(0,1,0.005)
   propPost <- dbeta(xValue, options[["a"]] + crosses, options[["b"]] + options[["n"]] - crosses)
   propPrior <-dbeta(xValue, options[["a"]], options[["b"]])
   dataProp = data.frame(values = c(xValue, xValue),
                         density = c(propPost, propPrior),
                         group = c(rep(gettext("Posterior"),201), rep(gettext("Prior"),201))
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
       ggplot2::theme(legend.position = c(.17, .9))
   }
   jaspResults[["propDistPlot"]] <- propDistPlot
  }
}


.buffonsNeedleSimulationPiDistPlot <- function(jaspResults, options) {
  if(!is.null(jaspResults[["piDistPlot"]])) return()
  crosses <- jaspResults[["simulateResults"]][["object"]][["k"]]
  
  # example d for computation
  d <- 5
  l <- options[["length"]]*d/100
  ## 3. Distribution Plot
  if (options[["showPiDistPlot"]]){
   piDistPlot <- createJaspPlot(title = gettextf("Implied Prior and Posterior for  %s", "\u03c0"),
                                width = 480, height = 320)
   piDistPlot$position <- 4
   #piDistPlot$dependOn(c("n", "a", "b", "length", "CI", "showPiDistPlot"))
   piDistPlot$dependOn(optionsFromObject = jaspResults[["summaryTable"]], 
                       options = c("showPiDistPlot", "legendPiDistPlot", "CIArrow"))

   #piDistPlot$addCitation("JASP Team (2018). JASP (Version 0.9.2) [Computer software].")
   
   # values
   x <- seq(2,4,0.01)
   yPost <- 2 * l / (x^2 * d) * dbeta((2 * l / (x * d)), options[["a"]] + crosses, options[["b"]] + options[["n"]] - crosses)
   yPrior <- 2 * l / (x^2 * d) * dbeta((2 * l / (x * d)), options[["a"]], options[["b"]])
   yPi <- seq(0, 1.6*max(yPost), 1.6*max(yPost)/99)
   
   data = data.frame(values = c(x, x, rep(pi, 100)),
                     density = c(yPost, yPrior, yPi),
                     group = c(rep(gettext("Implied Posterior"),201), rep(gettext("Implied Prior"),201), rep(gettext("\u03c0"), 100))
   )

   #data$group<-factor(data$group, levels=c(gettext("Implied Posterior"),gettext("Implied Prior"),gettext("\u03c0")))
   labels <- c(gettext("Implied Posterior"), gettext("Implied Prior"), "\u03c0")

   
   # axis specification
   piDistPlot0 <- ggplot2::ggplot(data = data,  ggplot2::aes(x = values, y = density)) +
     ggplot2::ggtitle("") + # for , pi
     ggplot2::xlab(gettext("\u03c0")) +
     ggplot2::ylab(gettext("Density")) +
     ggplot2::coord_cartesian(xlim = c(2, 4), ylim = c(0, 1.6*max(yPost))) +
    ggplot2::geom_line(ggplot2::aes(color = group, linetype = group), size = 1) +
     ggplot2::scale_color_manual("", values = c("Implied Posterior" = "black",
                                                "Implied Prior" = "black",
                                                "\u03c0" = "red"),
                                 labels = labels) +
     ggplot2::scale_linetype_manual("", values = c("Implied Posterior" = "solid",
                                                   "Implied Prior" = "dashed",
                                                   "\u03c0" = "solid"),
                                    labels = labels)
   # fill in the plot
   piDistPlot$plotObject <- jaspGraphs::themeJasp(piDistPlot0)


   if (options[["legendPiDistPlot"]]){
     piDistPlot$plotObject <-  piDistPlot$plotObject + 
       ggplot2::theme(legend.position = c(.24, .9))
   }
   
   
   if (options[["CIArrow"]]){
     CI95lower <- 2 * l / (qbeta((1-options[["CI"]])/2, crosses, options[["n"]] - crosses, lower.tail = FALSE) * d)
     CI95lower = round(CI95lower, digit = 2)
     
     med <- 2 * l / (qbeta(.5, crosses, options[["n"]] - crosses, lower.tail = FALSE) * d)
     med = round(med, digit = 2)
     
     CI95upper <- 2 * l / (qbeta(1-(1-options[["CI"]])/2, crosses, options[["n"]] - crosses, lower.tail = FALSE) * d)
     CI95upper = round(CI95upper, digit = 2)
     
     piDistPlot$plotObject <- piDistPlot$plotObject +
       ggplot2::annotate("text", x = 3.7, y = 1.6*max(yPost), 
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

  




