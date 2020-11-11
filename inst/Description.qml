import QtQuick 		2.12
import JASP.Module 	1.0

Description
{
	title :			"Learn Bayes"
	name: 			"jaspLearnBayes"
	icon:			"learning-stats.png"
	description:	"Learning Bayesian Statistics with JASP"
	version:		"0.1"
	requiresData:	false
	author:			"JASP Team"
	maintainer:		"JASP Team <info@jasp-stats.org>"
	website:		"www.jasp-stats.org"
	license:		"GPL (>= 2)"

	GroupTitle
	{
		title: 	"Counts"
		icon:	"analysis-bayesian-crosstabs.svg"
	}
	
	Analysis
	{
		title:	"Binomial Estimation"
		qml:	"LSbinomialestimation.qml"
		func:	"LSbinomialestimation"
	}

	Analysis
	{
		title:	"Binomial Testing"
		qml:	"LSbinomialtesting.qml"
		func:	"LSbinomialtesting"
	}

	GroupTitle
	{
		title: 	"Continuous"
		icon:	"analysis-bayesian-ttest.svg"
	}
	
	Analysis
	{
		title:	"Gaussian Estimation"
		qml:	"LSgaussianestimation.qml"
		func:	"LSgaussianestimation"
	}

	Analysis
	{
		title:	"Gaussian Testing"
		qml:		"LSgaussiantesting.qml"
		func:	"LSgaussiantesting"
	}
	
	GroupTitle
	{
		title: 	"Problem of Points"
	}
	
	Analysis
	{
		title:	"Game of Chance"
		qml:	"LSgameofchance.qml"
		func:	"LSgameofchance"
	}

	Analysis
	{
		title:	"Game of Skills"
		qml:	"LSgameofskills.qml"
		func:	"LSgameofskills"
	}
}
