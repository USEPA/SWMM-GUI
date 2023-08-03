unit swmm5_mgw;

{ Declarations of imported procedures from the EPASWMM DLL engine }
{ (SWMM5.DLL) }

interface

function   swmm_run(F1: PAnsiChar; F2: PAnsiChar; F3: PAnsiChar): Integer; stdcall;
function   swmm_open(F1: PAnsiChar; F2: PAnsiChar; F3: PAnsiChar): Integer; stdcall;
function   swmm_start(SaveFlag: Integer): Integer; stdcall;
function   swmm_step(var ElapsedTime: Double): Integer; stdcall;
function   swmm_end: Integer; stdcall;
function   swmm_report: Integer; stdcall;
function   swmm_getMassBalErr(var Erunoff: Single; var Eflow: Single;
               var Equal: Single): Integer; stdcall;
function   swmm_close: Integer; stdcall;
function   swmm_getVersion: Integer; stdcall;
function   swmm_getError(ErrMsg: PAnsiChar; MsgLen: Integer): Integer; stdcall;
function   swmm_getWarnings: Integer; stdcall;

implementation

function   swmm_run;    external 'SWMM5.DLL' name 'swmm_run@12';
function   swmm_open;   external 'SWMM5.DLL' name 'swmm_open@12';
function   swmm_start;  external 'SWMM5.DLL' name 'swmm_start@4';
function   swmm_step;   external 'SWMM5.DLL' name 'swmm_step@4';
function   swmm_end;    external 'SWMM5.DLL' name 'swmm_end@0';
function   swmm_report; external 'SWMM5.DLL' name 'swmm_report@0';
function   swmm_getMassBalErr; external 'SWMM5.DLL' name 'swmm_getMassBalErr@12';
function   swmm_close;  external 'SWMM5.DLL' name 'swmm_close@0';
function   swmm_getVersion; external 'SWMM5.DLL' name 'swmm_getVersion@0';
function   swmm_getError; external 'SWMM5.DLL' name 'swmm_getError@8';
function   swmm_getWarnings; external 'SWMM5.DLL' name 'swmm_getWarnings@0';
end.
