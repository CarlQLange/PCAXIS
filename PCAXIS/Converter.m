
PCAXIS`ConvertLinesToStatements[lines_] :=
 Association@
  Flatten[PCAXIS`StatementConvert /@ PCAXIS`StatementExtract[lines]]
  
PCAXIS`StatementExtract[lines_] := 
 StringSplit[StringReplace[lines, "\n" -> ""], ";"]
 
PCAXIS`StatementConvert[statement_String] := With[{
   wideintr =
    (*Interpreter[
     "Number" |
      Restricted[
       DelimitedSequence["String", "\",\""], {2, \[Infinity]}] |
      "String"],*)
      StringReplace["\"" -> ""] /@ StringSplit[#,"\",\""]&,
   dateintr = DateObject[{#,{"Year", "Month", "Day", " ", "Hour", ":", "Minute"}}],
   repl = StringReplace["\"" -> ""]
   },
  StringCases[statement, {
    "DATA=" ~~ rest___ :>
     "DATA" -> 
      ToExpression /@ ToExpression /@ StringSplit[StringReplace[rest, ".."-> "Missing[]"], " "],
    
    "DECIMALS=" ~~ rest___ :>
     "DECIMALS" -> ToExpression /@ StringSplit[rest, " "],
    
    "CREATION-DATE=" ~~ rest___ :> 
     "CREATION-DATE" -> dateintr@repl@rest,
    "FIRST-PUBLISHED=" ~~ rest___ :> 
     "FIRST-PUBLISHED=" -> dateintr@repl@rest,
    "LAST-UPDATED=" ~~ rest___ :> 
     "LAST-UPDATED=" -> dateintr@repl@rest,
    "LAST-UPDATED(" ~~ k__ ~~ ")=" ~~ rest___ :> 
     "LAST-UPDATED(" ~~ k ~~ ")=" -> dateintr@repl@rest,
    
    (*"LANGUAGE=" ~~ rest___ :> 
     "LANGUAGE" -> Interpreter["Language"][repl@rest],*)
    
    "TITLE=" ~~ rest___ :> "TITLE" -> repl@rest,
    "NOTE=" ~~ rest___ :> "NOTE" -> repl@rest,
    "CONTENTS=" ~~ rest___ :> "CONTENTS" -> repl@rest,
    "MATRIX=" ~~ rest___ :> "MATRIX" -> repl@rest,
    "SUBJECT-AREA=" ~~ rest___ :> "SUBJECT-AREA" -> repl@rest,
    "SUBJECT-CODE=" ~~ rest___ :> "SUBJECT-CODE" -> repl@rest,
    
    "HEADING=" ~~ rest___ :> "HEADING" -> repl /@ StringSplit[rest, "\",\""],
    "STUB=" ~~ rest___ :> "STUB" -> repl /@ StringSplit[rest, "\",\""],
    
    k__ ~~ "=" ~~ rest___ :>
     repl[k] -> With[{res = wideintr[rest]},
       Which[
        ListQ[res], If[StringQ[#], repl[#], #] & /@ res,
        StringQ[res], repl[res],
        True, res]]}]]
        
PCAXIS`HeaderDataSplit[rules_Association] := With[{
   df = {
     "VALUES*", 
     "CODES*",
     "DATA*", 
     "STUB*", 
     "DOMAIN*", 
     "KEYS*",
     "HEADING*",
     "UNITS*"}},
  <|"Data" -> 
    PCAXIS`QuantifyData[KeySelect[rules, StringMatchQ[#, {df}] &]],
   "Header" -> KeySelect[rules, Not[StringMatchQ[#, {df}]] &]|>]
   
PCAXIS`QuantifyData[data_] := data

PCAXIS`GetValuesForDimension[dimension_String, data_] :=
 Flatten[{data["VALUES(" ~~ dimension ~~ ")"]}]
 
PCAXIS`TreeBuilder[tl_] :=
 GroupBy[tl, First -> Rest, PCAXIS`TreeBuilder]
PCAXIS`TreeBuilder[{{n_}}] := n

PCAXIS`TreeBuilder2Wrap[dims_,vals_]:=Module[{
	fvals = Flatten[vals],
	i=1,
	PCAXIS`TreeBuilder2
	},
  PCAXIS`TreeBuilder2[{first_, rest__}] := 
    AssociationThread[first -> Table[PCAXIS`TreeBuilder2[{rest}], Length@first]];
  PCAXIS`TreeBuilder2[{first_}] := AssociationThread[first, fvals[[i++]]];
  PCAXIS`TreeBuilder2[dims]
]

PCAXIS`TreeFromDimensionsAndValues[vals_, dims_] := 
 (*PCAXIS`TreeBuilder[Transpose[Append[Flatten@vals][Transpose[Tuples[dims]]]]]*)
  PCAXIS`TreeBuilder2Wrap[dims,vals]
  
  
PCAXIS`GetDataset[rawdata_] :=
 Dataset[
  PCAXIS`TreeFromDimensionsAndValues[
   rawdata[["DATA"]],
   PCAXIS`GetValuesForDimension[#, rawdata] & /@ 
    Join[rawdata["STUB"], rawdata["HEADING"]]
   ]]
   
PCAXIS`PCAXISImport[filename_String, options___] := 
 Module[{alldata =
    PCAXIS`HeaderDataSplit[
     PCAXIS`ConvertLinesToStatements[
      Import[filename, "Text"]]]},
  {
   "Header" -> Dataset[alldata["Header"]],
   "Data" -> PCAXIS`GetDataset[alldata["Data"]],
   "RawData" -> alldata["Data"]}
  ]