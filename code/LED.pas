unit LED;

{$mode delphi}
{$H+}
{$inline on}   {Allow use of Inline procedures}

interface

uses
  Classes,
  GlobalConst,
  GlobalConfig,
  GPIO,
  Platform,
  Threads,
  DAQ_Globals,
  SysUtils;


procedure LED_Blink(time:LongWord);
procedure LED_Blink_1sec;
procedure LED_Blink_1sec_background;
procedure LED_Blink_2x_background;
implementation

procedure LED_Blink(time:LongWord);

begin
 GPIOOutputSet(GPIO_PIN_21,GPIO_LEVEL_HIGH);
 Sleep(time);
 GPIOOutputSet(GPIO_PIN_21,GPIO_LEVEL_LOW);
end;

procedure LED_Blink_1sec;

begin
 GPIOOutputSet(GPIO_PIN_21,GPIO_LEVEL_HIGH);
 Sleep(3000);
 GPIOOutputSet(GPIO_PIN_21,GPIO_LEVEL_LOW);
end;

procedure LED_Blink_2x;
var
  n : Word;
begin
 for n:=1 to 2 do
  begin
   GPIOOutputSet(GPIO_PIN_21,GPIO_LEVEL_HIGH);
   Sleep(300);
   GPIOOutputSet(GPIO_PIN_21,GPIO_LEVEL_LOW);
   Sleep(200);

  end;
end;

procedure LED_Blink_1sec_background;
begin
 BlinkThread:=BeginThread(@LED_Blink_1sec,nil,BlinkThread,THREAD_STACK_DEFAULT_SIZE);
end;

procedure LED_Blink_2x_background;
begin
 BlinkThread:=BeginThread(@LED_Blink_2x,nil,BlinkThread,THREAD_STACK_DEFAULT_SIZE);
end;

end.

