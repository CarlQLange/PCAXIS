ImportExport`RegisterImport[
 "PCAXIS",
 PCAXIS`PCAXISImport,
 "AvailableElements"->{"Data","Header","RawData"},
 "DefaultElement"->"Data"
 ]
 
Unprotect[Import];

Import[name_String, opts___?OptionQ] :=
    Import[name, "PCAXIS", opts] /; FileExtension[name] === "px";

Protect[Import];