import QtQuick		2.12
import JASP.Module	1.0

Upgrades
{

	Upgrade
	{
		functionName: 	"MixedModelsCommon"
		fromVersion:	"0.17.0"
		toVersion:		"0.18.0"
		
		ChangeRename { from: "dataType";	to: "dataInputType"}
		ChangeJS
		{
			name:		"dataType"
			jsFunction:	function(options)
			{
				switch(options["dataType"])
				{
					case "dataVariable":	return "variable";
					case "dataCounts":		return "counts";
					case "dataSequence":	return "sequence"
				}
			}
		}
		
		ChangeRename { from: "selectedVariable";		to: "dataVariableSelected"}
		ChangeRename { from: "allVariables";			to: "dataVariableAvailable"}
		ChangeRename { from: "dataVariableSD";			to: "dataVariableSd"}
		ChangeRename { from: "selectedVariable";		to: "dataVariableSelected"}
		ChangeRename { from: "levelsVar";				to: "dataVariableLevels"}
		ChangeRename { from: "keySuccessVar";			to: "dataVariableSuccesses"}
		ChangeRename { from: "keyFailureVar";			to: "dataVariableFailures"}
		ChangeRename { from: "dataCountsMean";			to: "dataCountsMean"}
		ChangeRename { from: "dataCountsSD";			to: "dataCountsSD"}
		ChangeRename { from: "dataCountsN";				to: "dataCountsN"}
		ChangeRename { from: "nSuccesses";				to: "dataCountsSuccesses"}
		ChangeRename { from: "nFailures";				to: "dataCountsFailures"}
		ChangeRename { from: "dataSequenceSD";			to: "dataSequenceSequenceSd"}
		ChangeRename { from: "dataSequenceInput";		to: "dataSequenceSequenceOfObservations"}
		ChangeRename { from: "levelsSeq";				to: "dataSequenceLevels"}
		ChangeRename { from: "keySuccessSeq";			to: "dataSequenceSuccesses"}
		ChangeRename { from: "keyFailureSeq";			to: "dataSequenceFailures"}

	}

}
