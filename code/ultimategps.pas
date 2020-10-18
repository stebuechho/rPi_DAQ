unit UltimateGPS;

{$mode delphi}
{$H+}
{$inline on}   {Allow use of Inline procedures}
interface

uses
  Classes,
  Ultibo,
  SysUtils,
  Platform,
  Threads,
  GlobalConst,
  GlobalConfig,
  Console,
  StrUtils,
  DateUtils,
  DAQ_Globals,
  LED;

type
  TGPSMessage=array[1..20] of String;
  PGPSMessage= ^TGPSMessage;

  TGPSPosition=array[1..5] of String;
function Get_GPS_Data:TGPSMessage;
function Read_GPSWord(var EOM:Boolean):String;
function Read_GPSMessage(var GPSMessageID:String; var WordCount:LongWord):TGPSMessage;
function Get_GPSMessageWithID(GPSMessageID:String):TGPSMessage;
Function Get_GPSFixStatus:Byte;
procedure WaitFor_GPSFix;
function Get_GPSDateTime(GPSMessagePointer:PGPSMessage):TDateTime;
function Set_TimeToGPSTime(GPSMessagePointer:PGPSMessage):TDateTime;
procedure GPSPosition_Thread;
function Init_GPSData:TDateTime;
implementation

function Get_GPS_Data:TGPSMessage;
var
 GPSMessageID:String;
 WordCount:LongWord;
 GPSMessage:TGPSMessage;
 MessageFound:Boolean;
begin
 MessageFound:=False;
 WordCount:=0;
 GPSMessageID:='';
 SerialOpen(9600,SERIAL_DATA_8BIT,SERIAL_STOP_1BIT,SERIAL_PARITY_NONE,SERIAL_FLOW_NONE,0,0);
 while not(MessageFound) do
  begin
    GPSMessage:=Read_GPSMessage(GPSMessageID,WordCount);
    ConsoleWindowWriteLn(LeftWindow,GPSMessageID + '  '+IntToStr(WordCount));
    if (GPSMessageID = '$GPGGA') and (WordCount=15) then MessageFound:=True;
  end;
 SerialClose;
 Result:=GPSMessage;
end;

function Read_GPSWord(var EOM:Boolean):String;
var
 Count:LongWord;
 Character:Char;
 GPSWord:String;
 EOW:Boolean;
begin
 Count:=0;
 EOW:=False;
 GPSWord:='';
 while not(EOW) and not(EOM) do
  begin
    SerialRead(@Character,SizeOf(Character),Count);
    if Character = #13 then EOM:=True
    else if (Character = ',') or (Character = '*') then EOW:=True
    else GPSWord:=GPSWord + Character;
  end;
 Result:=DelChars(GPSWord,#10);
end;
function Read_GPSMessage(var GPSMessageID:String; var WordCount:LongWord):TGPSMessage;
var
 EOM:Boolean;
 GPSMessage:TGPSMessage;
 GPSWord:String;
 i,Count:LongWord;
begin
 EOM:=False;
 GPSMessageID:='';
 Count:=0;
 for i:=1 to length(GPSMessage) do GPSMessage[i]:='';
 while not(EOM) do
  begin
    Inc(Count);
    GPSWord:=Read_GPSWord(EOM);
    WordCount:=Count;
    GPSMessage[WordCount]:=GPSWord;
  end;
  GPSMessageID:=GPSMessage[1];
  Result:=GPSMessage;
end;
function Get_GPSMessageWithID(GPSMessageID:String):TGPSMessage;
const
 AllGPSMessageIDs    : array[1..5] of String = ('$GPGGA','$GPGSA','$GPGSV','$GPRMC','$GPVTG');
 AllGPSMessageLength : array[1..5] of Byte = (16,19,21,14,11);
var
 WordCount,WordCurrentCount,i:LongWord;
 GPSMessage:TGPSMessage;
 MessageFound:Boolean;
 GPSMessageCurrentID:String;
begin
 MessageFound:=False;
 WordCurrentCount:=0;
 i:=AnsiIndexText(GPSMessageID,AllGPSMessageIDs)+1;
 if i>0 then WordCount:=AllGPSMessageLength[i];
 SerialOpen(9600,SERIAL_DATA_8BIT,SERIAL_STOP_1BIT,SERIAL_PARITY_NONE,SERIAL_FLOW_NONE,0,0);
 while not(MessageFound) do
  begin
    GPSMessage:=Read_GPSMessage(GPSMessageCurrentID,WordCurrentCount);
    if (GPSMessageCurrentID=GPSMessageID) and (WordCurrentCount=WordCount) then MessageFound:=True;
  end;
 SerialClose;
 Result:=GPSMessage;
end;

Function Get_GPSFixStatus:Byte;
var
 GPSMessage:TGPSMessage;
begin
 GPSMessage:=Get_GPSMessageWithID('$GPGSA');
 Result:=Byte(StrToint(GPSMessage[3]));
end;

procedure WaitFor_GPSFix;
begin
 GPIOOutputSet(GPIO_PIN_21,GPIO_LEVEL_HIGH);
 while Get_GPSFixStatus<3 do Sleep(1000);
 GPIOOutputSet(GPIO_PIN_21,GPIO_LEVEL_LOW);
end;

function Get_GPSTime(GPSMessagePointer:PGPSMessage):TTime;
var
 WordCount:LongWord;
 GPSMessageID, TimeStr:String;
 GPSMessage:TGPSMessage;
 MessageFound:Boolean;
begin
 if GPSMessagePointer=nil then
 begin
  MessageFound:=False;
  SerialOpen(9600,SERIAL_DATA_8BIT,SERIAL_STOP_1BIT,SERIAL_PARITY_NONE,SERIAL_FLOW_NONE,0,0);
  while not(MessageFound) do
   begin
    GPSMessage:=Read_GPSMessage(GPSMessageID,WordCount);
    if ((GPSMessageID='$GPGGA') and (WordCount=16)) or ((GPSMessageID='$GPRMC') and (WordCount=14)) then MessageFound:=True;
   end;
  end
 else
  begin
   GPSMessage:=GPSMessagePointer^;
  end;
 TimeStr:=GPSMessage[2];
 Result:=EncodeTime(StrToInt(TimeStr[1..2]),StrToInt(TimeStr[3..4]),StrToInt(TimeStr[5..6]),StrToInt(TimeStr[8..10]));
end;

function Get_GPSDateTime(GPSMessagePointer:PGPSMessage):TDateTime;
var
 TimeStr,DateStr:String;
 GPSDateTS,GPSTimeTS:TTimeStamp;
 GPSDate,GPSTime:TDateTime;
 GPSMessage:TGPSMessage;
begin
 if GPSMessagePointer=nil then GPSMessage:=Get_GPSMessageWithID('$GPRMC') else GPSMessage:=GPSMessagePointer^;
 TimeStr:=GPSMessage[2];
 DateStr:=GPSMessage[10];
 GPSTime:=EncodeTime(StrToInt(TimeStr[1..2]),StrToInt(TimeStr[3..4]),StrToInt(TimeStr[5..6]),StrToInt(TimeStr[8..10]));
 GPSDate:=EncodeDate(2000+StrToInt(DateStr[5..6]),StrToInt(DateStr[3..4]),StrToInt(DateStr[1..2]));
 GPSTimeTS:=DateTimeToTimeStamp(GPSTime);
 GPSDateTS:=DateTimeToTimeStamp(GPSDate);
 GPSDateTS.Time:=GPSTimeTS.Time;
 Result:=TimeStampToDateTime(GPSDateTS);
end;

function Get_GPSPosition(GPSMessagePointer:PGPSMessage):TGPSPosition;
var
 GPSPosition:TGPSPosition;
 GPSMessage:TGPSMessage;
begin
 if GPSMessagePointer=nil then GPSMessage:=Get_GPSMessageWithID('$GPGGA') else GPSMessage:=GPSMessagePointer^;
 GPSPosition[1]:='Lat:'+GPSMessage[3]+GPSMessage[4];
 GPSPosition[2]:='Lon:'+GPSMessage[5]+GPSMessage[6];
 GPSPosition[3]:='Alt:'+GPSMessage[10]+'m';
 GPSPosition[4]:='HDOP:'+GPSMessage[9];
 GPSPosition[5]:='GS:'+GPSMessage[12]+'m';
 Result:=GPSPosition;
end;

function Set_TimeToGPSTime(GPSMessagePointer:PGPSMessage):TDateTime;
var
 GPSDateTime:TDateTime;
 Count:Word;
 GPSTicksTime:Int64;
begin
 Count := 0;
 GPSDateTime:=Get_GPSDateTime(GPSMessagePointer);
 Result:=GPSDateTime;
 GPSTicksTime:= Int64(DateTimeToFileTime(GPSDateTime));
 ClockSetTime(GPSTicksTime,True);
 while YearOf(Now)<2000 do
  begin
   GPSDateTime  := Get_GPSDateTime(GPSMessagePointer);
   Result:=GPSDateTime;
   GPSTicksTime := Int64(DateTimeToFileTime(GPSDateTime));
   ClockSetTime(GPSTicksTime,True);
   if YearOf(Now)<2000 then
   begin
    LED_Blink_2x_background;
    Inc(Count);
    ConsoleWindowWriteLn(LeftWindow,IntToStr(Count)+' GPS time sync Failed: '+FormatDateTime('yyyy_mm_dd__hh-mm-ss',Now));
    Sleep(1000);
    if Count > 30 then SystemRestart(0);
   end;
  end;
end;

function Init_GPSData:TDateTime;
var
 time_in, time_out : Int64;
begin
  time_in := ClockGetTime;
  WaitFor_GPSFix;
  time_out := ClockGetTime;
  if (MAX_GPS_FIX_WAIT>0) and ((time_out - time_in) > MAX_GPS_FIX_WAIT * TIME_TICKS_PER_SECOND) then SystemRestart(0)
  else Result:=Set_TimeToGPSTime(nil);
end;

function Save_GPSPosition(Parameter:Pointer):PtrInt;
var
 GPSPosition:TGPSPosition;
 GPSPosStr:String;
 i:LongWord;
 fname:String;
 tf:TextFile;
 append2File:Boolean;
 Message:TMessage;
begin
 Result:=0; // To prevent compiler warning
 while True do
  begin
   append2File:=False;
   GPSPosition:=Get_GPSPosition(nil);

   GPSPosStr:=FormatDateTime('dd.mm.yyyy, hh:mm:ss',DateTimeNow) + ';  GPS Position:';
   for i:=1 to length(GPSPosition) do GPSPosStr:=GPSPosStr+'  '+GPSPosition[i]+';';

   fname:=DATA_DIR+'GPS_Position_'+BOX_N+'.txt';

   if FileExists(fname) then append2File:=True;

   AssignFile(tf,fname);
   if append2File then append(tf)
   else rewrite(tf);

   writeln(tf,GPSPosStr);

   CloseFile(tf);

   ThreadReceiveMessage(Message)
  end;
 end;

procedure GPSPosition_Thread;
begin
 GPSPosThread:=BeginThread(@Save_GPSPosition,nil,GPSPosThread,THREAD_STACK_DEFAULT_SIZE);
 ThreadSetPriority(GPSPosThread,2);
end;

end.

