unit DAQ_Setup;

{$mode delphi}

interface

uses
  Classes,
  SysUtils,
  Platform,
  GlobalConst,
  GlobalConfig,
  IniFiles,
  FileSystem,
  DAQ_Globals,
  ADS8320,
  BMP280,
  SampleBufferUnit,
  Console;

function Find_DataDrive:LongWord;
procedure Prepare_DataFolders;
procedure init_Devices;
function FileCopy(Source, Target: string): boolean;
procedure move_ConfigFile;
procedure move_KernelFile;
function move_Boxfile:LongWord;
procedure Process_Config;

implementation

function Find_DataDrive:LongWord;
var
 DirsToCheck:array[1..26] of String=('A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z');
 DirToCheck:String;
 SearchRec:TSearchRec;
 Count:LongWord;
 N_Drive:LongWord;
begin
N_Drive:=0;
DATA_DRIVE:='';
OS_DRIVE:='';

while not DirectoryExists('C:\') do Sleep(1000);

For Count:=1 to length(DirsToCheck) do
 begin
 DirToCheck:=DirsToCheck[Count]+':\';
 if DirectoryExists(DirToCheck) then
  begin
   Inc(N_Drive);
   if FileExists(DirToCheck+'bootcode.bin') then
    begin
     OS_DRIVE:=DirToCheck;
     if FindFirst((DirToCheck+'BOX_*'),faAnyFile,SearchRec) = 0 then
      begin
       BOX_N:=SearchRec.Name[5..length(SearchRec.Name)];
      end
     else BOX_N:='0';
    end;
   FindClose(SearchRec);
   if DirToCheck<>OS_DRIVE then DATA_DRIVE:= DirToCheck;
  end;

 end;
 if DATA_DRIVE='' then
  begin
   DATA_DRIVE:=OS_DRIVE;
   Result:=1;
  end
 else Result:=0;
end;
procedure Prepare_DataFolders;
var
 n : Integer;
begin
DATA_DIR:=DATA_DRIVE+'Data_'+BOX_N+'\';

if not DirectoryExists(DATA_DIR) then CreateDir(DATA_DIR);
DATA_DIR:=DATA_DIR+FormatDateTime('yyyy_mm_dd__hh-mm-ss',Now);

if DirectoryExists(DATA_DIR+'\') then
 begin
  n := 1;
  while DirectoryExists(DATA_DIR+'_'+IntToStr(n)+'\') do Inc(n);
  DATA_DIR := DATA_DIR + '_' + IntToStr(n);
 end;

DATA_DIR:= DATA_DIR+'\';
CreateDir(DATA_DIR);
if not DirectoryExists(DATA_DIR+'ADC\') then CreateDir(DATA_DIR+'ADC\');
if not DirectoryExists(DATA_DIR+'BMP\') then CreateDir(DATA_DIR+'BMP\');
FileCopy(OS_DRIVE+'DAQ_config.ini',DATA_DIR+'DAQ_config.ini');
end;

procedure init_Devices;

begin
{Set GPIO pin 17 which is our switch to Pull Up so that when the switch is open
the value read from the pin will be High}
GPIOPullSelect(GPIO_PIN_17,GPIO_PULL_DOWN);
GPIOFunctionSelect(GPIO_PIN_17,GPIO_FUNCTION_IN);

ADCDevice_glob:=ADC_startDevice;

BMP280_startDevice(BMPDevice_glob);

{Display a startup message on the console}
ConsoleWindowWriteLn(LeftWindow,'Preparing data sampling');

SampleBuffer:=BufferArrayInit(SampleBuffer);
BMPSampleBuffer:=BMP_BufferArrayInit(BMPSampleBuffer);

{Start our dedicated CPU thread that does nothing but taking samples and write it to the buffer(s)}
READY_SWITCH:=False;
end;

procedure move_ConfigFile;
begin
 if FileExists(DATA_DRIVE+'DAQ_config.ini') then
  begin
    FileCopy(DATA_DRIVE+'DAQ_config.ini',OS_DRIVE+'DAQ_config.ini');
    if not FileExists(DATA_DRIVE+'FLASHDRIVE.txt') then DeleteFile(DATA_DRIVE+'DAQ_config.ini');
  end;
end;

function move_BoxFile:LongWord;
var
 SearchRec1, SearchRec2:TSearchRec;
begin
 if FindFirst((DATA_DRIVE+'BOX_*'),faAnyFile,SearchRec1) = 0 then
  begin
   if FindFirst((OS_DRIVE+'BOX_*'),faAnyFile,SearchRec2)  = 0 then
   begin
    DeleteFile(OS_DRIVE+SearchRec2.Name);
    FindClose(SearchRec2);
   end;
   FindClose(SearchRec1);
   FileCopy(DATA_DRIVE+SearchRec1.Name,OS_DRIVE+SearchRec1.Name);
   if not FileExists(DATA_DRIVE+'FLASHDRIVE.txt') then
    begin
     DeleteFile(DATA_DRIVE+SearchRec1.Name);
     Result:=1;
    end
   else Result:=2;
  end
 else Result:=0;
end;

procedure move_KernelFile;
begin
 if FileExists(DATA_DRIVE+'kernel7.img') then
  begin
    FileCopy(DATA_DRIVE+'kernel7.img',OS_DRIVE+'kernel7.img');
    if not FileExists(DATA_DRIVE+'FLASHDRIVE.txt') then
     begin
      DeleteFile(DATA_DRIVE+'kernel7.img');
      SystemRestart(0);
     end
    else SystemShutdown(0);
  end;
end;

procedure Process_Config;

var
 Config_ini: TINIFile;
begin
 Config_ini := TINIFile.Create(OS_DRIVE+'DAQ_config.ini');
 ADC_Fs     := Config_ini.ReadInteger('config', 'ADC_sample_rate',50000);
 NEW_FILE_RATE := Config_ini.ReadInteger('config', 'New_file_rate',60);
 BMP_mode   := Config_ini.ReadInteger('config', 'BMP_mode',3);
 GPS_mode   := Config_ini.ReadInteger('config', 'Require_GPS',1);
 GPS_SAVE_RATE := Config_ini.ReadInteger('config', 'GPS_save_rate',60);
 MAX_GPS_FIX_WAIT := Config_ini.ReadInteger('config', 'GPS_max_wait',0);
 CHECK_REBOOT_TIME := Config_ini.ReadInteger('config', 'check_reboot_time',60);
 Config_ini.Free;
end;

function FileCopy(Source, Target: string): boolean;
// Copies source to target; overwrites target.
// Caches entire file content in memory.
// Returns true if succeeded; false if failed.
var
  MemBuffer: TMemoryStream;
begin
  result := false;
  MemBuffer := TMemoryStream.Create;
  try
    MemBuffer.LoadFromFile(Source);
    MemBuffer.SaveToFile(Target);
    result := true
  except
    //swallow exception; function result is false by default
  end;
  // Clean up
  MemBuffer.Free
end;

end.

