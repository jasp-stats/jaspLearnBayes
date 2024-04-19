import QtQuick 		2.12
import JASP.Module 	1.0

Description
{
	title :			qsTr("Learn Bayes")
	name: 			"jaspLearnBayes"
	icon:			"learning-stats.png"
	description:	qsTr("Learn Bayesian statistics with simple examples and supporting text")
	version:		"0.19.0"
	requiresData:	false
	author:			"JASP Team"
	maintainer:		"JASP Team <info@jasp-stats.org>"
	website:		"www.jasp-stats.org"
	license:		"GPL (>= 2)"

	GroupTitle
	{
		title:	qsTr("Binary Classification")
		icon:	"ellipsis.svg"
	}

	Analysis
	{
		title:	qsTr("Binary Classification")
		qml:	"LSbinaryclassification.qml"
		func:	"LSbinaryclassification"
	}

	GroupTitle
	{
		title: 	qsTr("Counts")
		icon:	"analysis-bayesian-crosstabs.svg"
	}

	Analysis
	{
		title:	qsTr("Binomial Estimation")
		qml:	"LSbinomialestimation.qml"
		func:	"LSbinomialestimation"
	}

	Analysis
	{
		title:	qsTr("Binomial Testing")
		qml:	"LSbinomialtesting.qml"
		func:	"LSbinomialtesting"
	}
/*
	GroupTitle
	{
		title: 	qsTr("Continuous")
		icon:	"analysis-bayesian-ttest.svg"
	}

	Analysis
	{
		title:	qsTr("Gaussian Estimation")
		qml:	"LSgaussianestimation.qml"
		func:	"LSgaussianestimation"
	}

	Analysis
	{
		title:	qsTr("Gaussian Testing")
		qml:	"LSgaussiantesting.qml"
		func:	"LSgaussiantesting"
	}
*/
	GroupTitle
	{
		title:	qsTr("The Problem of Points")
		icon:	"learn-bayes-epees.svg"
	}

	Analysis
	{
		title:	qsTr("Game of Chance")
		qml:	"LSgameofchance.qml"
		func:	"LSgameofchance"
	}

	Analysis
	{
		title:	qsTr("Game of Skill")
		qml:	"LSgameofskill.qml"
		func:	"LSgameofskill"
	}

	GroupTitle
	{
		title:	qsTr("Buffon's Needle")
		icon:	"learning-stats-Buttons-needle.svg"
	}

	Analysis
	{
		title:	qsTr("Simulating Buffon's Needle")
		qml:	"LSBuffonsneedlesimulation.qml"
		func:	"LSBuffonsneedlesimulation"
	}

	Analysis
	{
		title:	qsTr("Manipulating Buffon's Needle")
		qml:	"LSBuffonsneedlemanipulation.qml"
		func:	"LSBuffonsneedlemanipulation"
	}


}
