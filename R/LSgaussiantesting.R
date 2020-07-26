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

LSgaussiantesting  <- function(jaspResults, dataset, options, state = NULL){

  # a vector of two, first for data, second for hypotheses
  ready <- .readyGaussianLS(options)
  
  # evaluate the expressions in priors
  if(ready[2])options[["priors"]] <- .evaluate_priors(options[["priors"]])
  
  # load, check, transform and process data
  if(ready[1])data <- .readDataGaussianLS(dataset, options)
  
  # data summary table if requested (but not if the data counts were added directly)
  if(options[["dataSummary"]] && !options[["dataType"]] == "dataCounts").summaryGaussianLS(jaspResults, data, ready)
  
  
  return()
}
