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
    simulateResults$dependOn(c("numberOfThrows", "lengthToDistanceProportion")) #, "priorAlpha", "priorBeta", "options[["lengthToDistanceProportion"]]", "options[["ciLevel"]]"))
    jaspResults[["simulateResults"]] <- simulateResults
    jaspResults[["simulateResults"]]$object <- simulateThrows(options[["numberOfThrows"]], options[["lengthToDistanceProportion"]])

  }
}

.buffonsNeedleSimulationSummaryTable <- function(jaspResults, options){
  if(!is.null(jaspResults[["summaryTable"]])) return()
  crosses <- jaspResults[["simulateResults"]][["object"]][["k"]]

  # example d for computation
  d <- 5
  l <- options[["lengthToDistanceProportion"]]*d/100
  ## Summary Table
  summaryTable <- createJaspTable(title = gettext("Summary Table"))
  summaryTable$position <- 1
  summaryTable$dependOn(c("numberOfThrows", "lengthToDistanceProportion", "priorAlpha", "priorBeta", "ciLevel", "min", "max"))
  #summaryTable$addCitation("JASP Team (2018). JASP (Version 0.9.2) [Computer software].")
  summaryTable$addColumnInfo(name = "NumObservations", title = gettext("Tosses"), type = "integer")
  summaryTable$addColumnInfo(name = "NumCrosses", title = gettext("Crosses"), type = "integer")
  summaryTable$addColumnInfo(name = "MLE", title = gettextf("MLE for %s", "\u03c0"), type = "number")
  summaryTable$addColumnInfo(name = "Mass", title = gettext("Interval Mass"),   type = "number")
  summaryTable$addColumnInfo(name = "Median", title = gettextf("Median for %s", "\u03c0"), type = "number")
  summaryTable$addColumnInfo(name = "lowerCI", title = gettext("Lower"), type = "number",
                            overtitle = gettextf("%s%% Credible Interval", options[["ciLevel"]]*100))
  summaryTable$addColumnInfo(name = "upperCI", title = gettext("Upper"), type = "number",
                            overtitle = gettextf("%s%% Credible Interval", options[["ciLevel"]]*100))

  # fill in the table
  CI95lower <- 2 * l / (qbeta((1-options[["ciLevel"]])/2, crosses, options[["numberOfThrows"]] - crosses, lower.tail = FALSE) * d)
  med <- 2 * l / (qbeta(.5, crosses, options[["numberOfThrows"]] - crosses, lower.tail = FALSE) * d)
  CI95upper <- 2 * l / (qbeta(1-(1-options[["ciLevel"]])/2, crosses, options[["numberOfThrows"]] - crosses, lower.tail = FALSE) * d)
  mass <- pbeta(2*l/(options[["min"]]*d), crosses, options[["numberOfThrows"]] - crosses) - pbeta(2*l/(options[["max"]]*d), crosses, options[["numberOfThrows"]] - crosses)
  MLE <- 2*l/(crosses/options[["numberOfThrows"]]*d)

  summaryTable$addRows(list(NumCrosses = crosses, NumObservations = options[["numberOfThrows"]], Mass = mass,
                           lowerCI = CI95lower, Median = med, upperCI = CI95upper, MLE = MLE))
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
  l <- options[["lengthToDistanceProportion"]]*d/100
  ## 1. Needle Plot
  if (options[["needlePlot"]]){
   needlePlot <- createJaspPlot(title = gettext("Needle Plot"),  width = 400, height = 400)
   needlePlot$position <- 2
   #needlePlot$dependOn(c("numberOfThrows", "priorAlpha", "priorBeta", "lengthToDistanceProportion", "ciLevel", "needlePlot"))
   needlePlot$dependOn(optionsFromObject = jaspResults[["summaryTable"]],
                       options = c("needlePlot", "needlePlotCrossingNeedlesColored"))
   #needlePlot$addCitation("JASP Team (2018). JASP (Version 0.9.2) [Computer software].")

   needlePlot0 <- ggplot2::ggplot(data= NULL) +
     ggplot2::coord_cartesian(xlim = c(-15, 15), ylim = c(-15, 15)) +
     ggplot2::xlab("") +
     ggplot2::ylab("") +
     ggplot2::geom_segment(ggplot2::aes(x = rep(-13,5), y = c(-10,-5,0,5,10),
                                        xend = rep(13,5), yend = c(-10,-5,0,5,10))) +
     ggplot2::geom_segment(ggplot2::aes(x = xs, y = ys, xend = xe, yend = ye), color = "blue")

   # fill in the plot
   if (options[["needlePlotCrossingNeedlesColored"]]){
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

  crosses <- jaspResults[["simulateResults"]][["object"]][["k"]]
  # example d for computation
  d <- 5
  l <- options[["lengthToDistanceProportion"]]*d/100
  ## 2. prior and posterior plot for proportion of crosses
  if (options[["priorPosteriorProportion"]]){

   propDistPlot <- createJaspPlot(title = gettext("Prior and Posterior for Proportion of Crosses"),
                                  width = 480, height = 320)
   propDistPlot$position <- 3
   #propDistPlot$dependOn(c("numberOfThrows", "priorAlpha", "priorBeta", "lengthToDistanceProportion", "ciLevel", "priorPosteriorProportion"))
   propDistPlot$dependOn(optionsFromObject = jaspResults[["summaryTable"]],
                         options = c("priorPosteriorProportion", "priorPosteriorProportionCi", "priorPosteriorProportionLegend"))
   #propDistPlot$addCitation("JASP Team (2018). JASP (Version 0.9.2) [Computer software].")

   # values
   xValue <- seq(0,1,0.005)
   propPost <- dbeta(xValue, options[["priorAlpha"]] + crosses, options[["priorBeta"]] + options[["numberOfThrows"]] - crosses)
   propPrior <-dbeta(xValue, options[["priorAlpha"]], options[["priorBeta"]])
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

   if (options[["priorPosteriorProportionLegend"]]){
     propDistPlot$plotObject <-  propDistPlot$plotObject +
       ggplot2::theme(legend.position = "right")
   }

   if (options[["priorPosteriorProportionCi"]]){
       propCI95upper <- qbeta((1-options[["ciLevel"]])/2, crosses, options[["numberOfThrows"]] - crosses, lower.tail = FALSE)
       propCI95upper <- round(propCI95upper, digit = 2)

     propmed <- qbeta(.5, crosses, options[["numberOfThrows"]] - crosses, lower.tail = FALSE)
     propmed <- round(propmed, digit = 2)

     propCI95lower <- qbeta(1-(1-options[["ciLevel"]])/2, crosses, options[["numberOfThrows"]] - crosses, lower.tail = FALSE)
     propCI95lower <- round(propCI95lower, digit = 2)

     propDistPlot$plotObject <- propDistPlot$plotObject +
       ggplot2::annotate("text", x = 0.75, y = 1.6*max(propPost),
                         label = gettextf("%1$s%% CI: [%2$s, %3$s]", options[["ciLevel"]]*100, propCI95lower, propCI95upper),

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


.buffonsNeedleSimulationPiDistPlot <- function(jaspResults, options) {
  if(!is.null(jaspResults[["piDistPlot"]])) return()
  crosses <- jaspResults[["simulateResults"]][["object"]][["k"]]

  # example d for computation
  d <- 5
  l <- options[["lengthToDistanceProportion"]]*d/100
  ## 3. Distribution Plot
  if (options[["priorPosteriorPi"]]){
    piDistPlot <- createJaspPlot(title = gettextf("Implied Prior and Posterior for  %s", "\u03c0"),
                                 width = 480, height = 320)
    piDistPlot$position <- 4
    #piDistPlot$dependOn(c("numberOfThrows", "priorAlpha", "priorBeta", "lengthToDistanceProportion", "ciLevel", "priorPosteriorPi"))
    piDistPlot$dependOn(optionsFromObject = jaspResults[["summaryTable"]],
                        options = c("priorPosteriorPi", "priorPosteriorPiLegend", "priorPosteriorPiCi", "min", "max", "highlight"))

    CI95lower <- 2 * l / (qbeta((1-options[["ciLevel"]])/2, crosses, options[["numberOfThrows"]] - crosses, lower.tail = FALSE) * d)
    CI95lower <- round(CI95lower, digit = 2)

    med <- 2 * l / (qbeta(.5, crosses, options[["numberOfThrows"]] - crosses, lower.tail = FALSE) * d)
    med <- round(med, digit = 2)

    CI95upper <- 2 * l / (qbeta(1-(1-options[["ciLevel"]])/2, crosses, options[["numberOfThrows"]] - crosses, lower.tail = FALSE) * d)
    CI95upper <- round(CI95upper, digit = 2)

    xlimLower <- min(2,CI95lower-0.5)
    xlimUpperer <- max(4,CI95upper+0.5)

    xBreaks <- jaspGraphs::getPrettyAxisBreaks(c(xlimLower, xlimUpperer, options[["min"]], options[["max"]]))

    x <- seq(min(xBreaks), max(xBreaks), length.out = 201)
    yPost <- 2 * l / (x^2 * d) * dbeta((2 * l / (x * d)), options[["priorAlpha"]] + crosses, options[["priorBeta"]] + options[["numberOfThrows"]] - crosses)
    yPrior <- 2 * l / (x^2 * d) * dbeta((2 * l / (x * d)), options[["priorAlpha"]], options[["priorBeta"]])

    yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(0, yPost, yPrior))

    xInterval <- seq(options[["min"]], options[["max"]], length.out = 100)
    pInterval <- 2*l/(xInterval*d)
    yInterval <- 2 * l / (xInterval^2 * d) * dbeta(pInterval, options[["priorAlpha"]] + crosses, options[["priorBeta"]] + options[["numberOfThrows"]] - crosses)

    yPi <- range(yBreaks)

    data <- data.frame(
      values  = c(x, x, rep(pi, 2)),
      density = c(yPost, yPrior, yPi),
      group   = c(rep("Implied Posterior",201), rep("Implied Prior",201), rep("pi", 2))
    )

    labels <- c(gettext("Implied Posterior"), gettext("Implied Prior"), "\u03c0")
    piDistPlot0 <- ggplot2::ggplot(data = data,  ggplot2::aes(x = values, y = density)) +
      jaspGraphs::scale_x_continuous(name = "\u03c0",           breaks = xBreaks, limits = range(xBreaks)) +
      jaspGraphs::scale_y_continuous(name = gettext("Density"), breaks = yBreaks, limits = 1.2*range(yBreaks))

    if (options[["highlight"]]){
      piDistPlot0 <- piDistPlot0 +
        ggplot2::geom_ribbon(
          data    = data.frame(x = xInterval, y = yInterval),
          mapping = ggplot2::aes(x = x, ymin = 0, ymax = y),
          fill    = "steelblue", inherit.aes = FALSE)
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

    if (options[["priorPosteriorPiLegend"]]){
      piDistPlot$plotObject <-  piDistPlot$plotObject +
        ggplot2::theme(legend.position = "right")
    }

    if (options[["priorPosteriorPiCi"]]){


      piDistPlot$plotObject <- piDistPlot$plotObject +
        ggplot2::annotate("text", x = (CI95lower + CI95upper)/2, y = max(yBreaks),
                          label = gettextf("%1$s%% CI: [%2$s, %3$s]", options[["ciLevel"]]*100, CI95lower, CI95upper),
                          size = 6, vjust = -1
        ) +
        ggplot2::annotate("segment", x = CI95lower, xend = CI95upper,
                          y = max(yBreaks), yend = max(yBreaks),
                          arrow = grid::arrow(ends = "both", angle = 90, length = grid::unit(.2,"cm")),
                          size = 1)

    }
    jaspResults[["piDistPlot"]] <- piDistPlot
  }
}
