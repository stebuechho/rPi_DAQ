program DAQ_main;

//{$mode objfpc}{$H+}
{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}
uses
  InitUnit,     {Include InitUnit to allow us to change the startup behaviour}
  RaspberryPi3, {Include RaspberryPi3 to make sure all standard functions are included}
  GlobalConst,
  GlobalConfig,
  GlobalTypes,
  Platform,
  Threads,
  Console,
  SysUtils,
  ThreadUnit,
  DWCOTG,
  DAQ_Globals,
  UltimateGPS,
  DAQ_Setup;

var
  reboot_needed: LongWord;
begin
 {Create a console window to show what is happening}
 LeftWindow:=ConsoleWindowCreate(ConsoleDeviceGetDefault,CONSOLE_POSITION_LEFT,True);
 Sleep(3000);

 GPIOPullSelect(GPIO_PIN_21,GPIO_PULL_DOWN);
 GPIOFunctionSelect(GPIO_PIN_21,GPIO_FUNCTION_OUT);

 if Find_DataDrive=0 then
 begin
  reboot_needed := move_BoxFile;
  move_KernelFile;
  move_ConfigFile;
  if reboot_needed=1 then SystemRestart(0)
  else if reboot_needed=2 then SystemShutdown(0);
 end;

 Process_Config;
 ConsoleWindowWriteLn(LeftWindow,'OS: '+OS_DRIVE+'  Data: '+DATA_DRIVE+'  Sensor: '+BOX_N);

 ConsoleWindowWriteLn(LeftWindow,'ADC_Fs = '+IntToStr(ADC_Fs));
 ConsoleWindowWriteLn(LeftWindow,'BMP_mode = '+IntToStr(BMP_mode));
 ConsoleWindowWriteLn(LeftWindow,'GPS_mode = '+IntToStr(GPS_mode));

 if GPS_mode=1 then Init_GPSData;
 ConsoleWindowWriteLn(LeftWindow,FormatDateTime('yyyy_mm_dd__hh-mm-ss',Now));
 Prepare_DataFolders;
 GPSPosition_Thread;

 init_Devices;
 ConsoleWindowWriteLn(LeftWindow,'BMP_Fs = '+IntToStr(BMP_Fs));
 StartSampleThreads(LeftWindow);

 ThreadHalt(0);

end.
 
