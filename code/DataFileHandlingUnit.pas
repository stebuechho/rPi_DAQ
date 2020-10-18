unit DataFileHandlingUnit;

{This Unit contains all procedures and functions to deal with the data files where the samples
are saved into}

{$mode objfpc}{$H+}

interface

uses
  Classes,
  Console,
  GlobalTypes,
  GlobalConfig,
  DAQ_Globals,
  DWCOTG,
  FATFS,
  NTFS,
  FileSystem,
  SysUtils;

procedure PrepareDataFile(WindowHandle:TWindowHandle;fname:String);
procedure SaveSampleToFile(WindowHandle:TWindowHandle;fname:String;SampleData:TSampleArray);
procedure HandleFile(sensor,signal:String;NewFileRate:LongWord; DT_now:TDateTime; LeadString:String; var f:File; var DT_file:TDateTime; var fname:String);
function GenerateFileName(DT:TDateTime;LeadString:String):String;
procedure WriteBuffer2Disk(SBuffer:PSampleBuffer;NewFileRate:LongWord; LeadString,PPSLeadString:String; var f,PPSf:File; var fname,PPSfname:String; var DT_file:TDateTime);
procedure WriteBMPBuffer2Disk(SBuffer:PBMPSampleBuffer;NewFileRate:LongWord; LeadString,PPSLeadString:String; var f,PPSf:File; var fname,PPSfname:String; var DT_file:TDateTime);
implementation

// This Procedure looks for file named fname and deletes it (!) It's run only
// once before sampling is started. Was implemented for TESTING!
procedure PrepareDataFile(WindowHandle:TWindowHandle;fname:String);
 begin
  {We may need to wait a couple of seconds for any drive to be ready}
  ConsoleWindowWriteLn(WindowHandle,'Waiting for drive C:\');
 while not DirectoryExists('C:\') do
  begin
   {Sleep for a second}
   Sleep(1000);
  end;
 ConsoleWindowWriteLn(WindowHandle,'C:\ drive is ready');
 ConsoleWindowWriteLn(WindowHandle,'');

 {We should check if the file exists first before trying to create it}
 ConsoleWindowWriteLn(WindowHandle,'Checking to see if ' + fname + ' exists');
 if FileExists(fname) then
  begin
   {If it does exist we can delete it}
   ConsoleWindowWriteLn(WindowHandle,'Deleting the file ' + fname);
   DeleteFile(fname);
  end;
 end;

procedure SaveSampleToFile(WindowHandle:TWindowHandle;fname:String;SampleData:TSampleArray);
var
 f:File;
begin
Assign(f,fname);
if FileExists(fname) then
 begin
    Reset(f,SizeOf(TSample));
    seek(f,filesize(f));                      // put pointer to end of file, so new samples are added afterwards
 end
else
 begin
    Rewrite(f,SizeOf(TSample));               // File fname doesn't exist, create it
 end;
 BlockWrite(f, SampleData, SAMPLE_COUNT);     // write all samples at once to file
 CloseFile(f);
end;

 procedure SaveBMPSampleToFile(WindowHandle:TWindowHandle;fname:String;SampleData:TBMPSampleArray);
var
 f:File;
begin
Assign(f,fname);
if FileExists(fname) then
 begin
    Reset(f,SizeOf(TBMPSample));
    seek(f,filesize(f));                      // put pointer to end of file, so new samples are added afterwards
 end
else
 begin
    Rewrite(f,SizeOf(TBMPSample));               // File fname doesn't exist, create it
 end;
 BlockWrite(f, SampleData, BMP_SAMPLE_COUNT);     // write all samples at once to file
 CloseFile(f);
end;

procedure HandleFile(sensor,signal:String;NewFileRate:LongWord; DT_now:TDateTime; LeadString:String; var f:File; var DT_file:TDateTime; var fname:String);

var
 newFileTicks  : QWord;
 t_diff_in_mus : QWord;
 dataSize      : LongWord;
 signame       : String;
begin
 newFileTicks  := 60*QWord(NewFileRate)*CLOCK_FREQUENCY;

 t_diff_in_mus := Round((DT_now-DT_file)*US_PER_DAY);

 if signal='PPS' then
  begin
   dataSize:=SizeOf(TPPSSample);
   signame:=signal;
  end
 else
  begin
     signame:=sensor;
   if sensor='ADC' then dataSize:=SizeOf(TSample)
   else if sensor='BMP' then dataSize:=SizeOf(TBMPSample);
  end;

 if (t_diff_in_mus>=newFileTicks) then
  begin
   // Close last file and create a new one and open it for writing
   CloseFile(f);

   fname:=GenerateFileName(DT_now,(sensor+'\'+LeadString+signame+'_'));
   Assign(f,fname);
   Rewrite(f,dataSize);

   // change saved timestep
   DT_file:=DT_now;
  end;
end;

function GenerateFileName(DT:TDateTime;LeadString:String):String;
begin
 Result:=DATA_DIR+LeadString+FormatDateTime('yyyy_mm_dd__hh-mm-ss-zzz',DT)+'_'+BOX_N+'.dat';
end;

procedure WriteBuffer2Disk(SBuffer:PSampleBuffer;NewFileRate:LongWord; LeadString,PPSLeadString:String; var f,PPSf:File; var fname,PPSfname:String; var DT_file:TDateTime);
var
 DT_now,DT_backup:TDateTime;

begin
if SBuffer^.State=2 then
 begin
  IS_RECORDING:=True;
  DT_backup:=DT_file;
  DT_now:=SBuffer^.StartTime;
  HandleFile('ADC','Data',NewFileRate,DT_now,LeadString,f,DT_file,fname);
  HandleFile('ADC','PPS',NewFileRate,DT_now,PPSLeadString,PPSf,DT_backup,PPSfname);
  BlockWrite(f, SBuffer^.Samples, SAMPLE_COUNT);
  BlockWrite(PPSf, SBuffer^.PPS, SAMPLE_COUNT);

  SBuffer^.Count:=0;
  SBuffer^.State:=1;
 end;
end;

procedure WriteBMPBuffer2Disk(SBuffer:PBMPSampleBuffer;NewFileRate:LongWord; LeadString,PPSLeadString:String; var f,PPSf:File; var fname,PPSfname:String; var DT_file:TDateTime);
var
 DT_now,DT_backup:TDateTime;

begin
if SBuffer^.State=2 then
 begin
  DT_backup:=DT_file;
  DT_now:=SBuffer^.StartTime;
  HandleFile('BMP','Data',NewFileRate,DT_now,LeadString,f,DT_file,fname);
  HandleFile('BMP','PPS',NewFileRate,DT_now,PPSLeadString,PPSf,DT_backup,PPSfname);
  BlockWrite(f, SBuffer^.Samples, BMP_SAMPLE_COUNT);
  BlockWrite(PPSf, SBuffer^.PPS, BMP_SAMPLE_COUNT);

  SBuffer^.Count:=0;
  SBuffer^.State:=1;
 end;
end;
end.

