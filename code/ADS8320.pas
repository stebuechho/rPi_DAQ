unit ADS8320;

//{$mode objfpc}
{$mode delphi} {Default to Delphi compatible syntax}
{$H+}
{$inline on}   {Allow use of Inline procedures}

interface

uses
  Classes,
  SysUtils,
  GlobalConst,
  GlobalConfig,
  BCM2710,
  Platform{$IFNDEF CONSOLE_EARLY_INIT},PlatformRPi3{$ENDIF},
  BCM2837,
  Devices,
  SPI;

type

  TSPIBytes = array[0..2] of Byte;
  PSPIBytes =  ^TSPIBytes;


function ADC_startDevice: PSPIDevice;
function ADC_getSample(ADCDevice:PSPIDevice):Word;
function ADS8320SPI0WriteRead(SPI:PSPIDevice;ChipSelect:Word;Source,Dest:Pointer;Size,Flags:LongWord;var Count:LongWord):LongWord;
function BCM2710SPI0ReadWritePoll(SPI:PBCM2710SPI0Device):LongWord;

implementation

function ADC_startDevice: PSPIDevice;
var
   ADCDevice:PSPIDevice;
begin

  //Locate the SPI device (Adjust for boards other than Pi3)
  ADCDevice := SPIDeviceFindByDescription(BCM2710_SPI0_DESCRIPTION);
  if ADCDevice = nil then
    Exit;

  //Configure SPI Chip Select 0
  if SPIDeviceSetClockRate(ADCDevice ,SPI_CS_0, 6000000) <> ERROR_SUCCESS then
    Exit;

  //Start the SPI device
  if SPIDeviceStart(ADCDevice, SPI_MODE_3WIRE, 6000000, SPI_CLOCK_PHASE_HIGH, SPI_CLOCK_POLARITY_HIGH) <> ERROR_SUCCESS then
    Exit;

  Result:=ADCDevice;
end;

function ADC_getSample(ADCDevice:PSPIDevice):Word;
var
  n_Bytes       : LongWord;
  ADS_WR_Return : LongWord;
  SampleBytes   : TSPIBytes;
begin
  n_Bytes:=0;

  //Read 3 bytes from the SPI device
  ADS_WR_Return:=ADS8320SPI0WriteRead(ADCDevice ,SPI_CS_0, nil, @SampleBytes, 3, SPI_TRANSFER_NONE, n_Bytes);
  if ADS_WR_Return <> ERROR_SUCCESS then
   begin
    Result:= ADS_WR_Return;
   end
  else
   begin
    Result:= (((SampleBytes[0] and 3) << 14) or (SampleBytes[1] << 6) or (SampleBytes[2] and (63)));
   end;
end;


function ADS8320SPI0WriteRead(SPI:PSPIDevice;ChipSelect:Word;Source,Dest:Pointer;Size,Flags:LongWord;var Count:LongWord):LongWord;
var
 Control:LongWord;
begin
 {}
 {Setup Result}
 Count:=0;
 Result:=ERROR_INVALID_PARAMETER;

 {Check Buffers}
 if (Source = nil) and (Dest = nil) then Exit;


 {Check SPI}
 if SPI = nil then Exit;

 {$IF DEFINED(BCM2710_DEBUG) or DEFINED(SPI_DEBUG)}
 if SPI_LOG_ENABLED then SPILogDebug(SPI,'BCM2710: SPI0 Write Read (ChipSelect=' + SPIChipSelectToString(ChipSelect) + ' Size=' + IntToStr(Size) + ')');
 {$ENDIF}

 {Check Size}
 if Size > BCM2710_SPI0_MAX_SIZE then Exit;

 {Check Chip Select}
 if (ChipSelect <> SPI_CS_NONE) and (ChipSelect > SPI_CS_2) then Exit;

 {Update Statistics}
 Inc(SPI.TransferCount);

 {Write from Source / Read to Dest}
 if Size > 0 then
  begin
   {Setup Data}
   PBCM2710SPI0Device(SPI).Source:=Source;
   PBCM2710SPI0Device(SPI).Dest:=Dest;
   PBCM2710SPI0Device(SPI).Count:=0;
   PBCM2710SPI0Device(SPI).SourceRemain:=Size;
   PBCM2710SPI0Device(SPI).DestRemain:=Size;

   {Memory Barrier}
   DataMemoryBarrier; {Before the First Write}

   {Get Control and Status}
   Control:=PBCM2837SPI0Registers(PBCM2710SPI0Device(SPI).Address).CS and not(BCM2837_SPI0_CS_CS_MASK);

   {Set Mode}
   if (SPI.SPIMode = SPI_MODE_3WIRE) and (Dest <> nil) then
    begin
     Control:=Control or BCM2837_SPI0_CS_REN;
    end
   else
    begin
     Control:=Control and not(BCM2837_SPI0_CS_REN);
    end;

   {Set Chip Select}
   if ChipSelect = SPI_CS_NONE then
    begin
     Control:=Control or (BCM2837_SPI0_CS_CS_MASK); {Select the reserved value}
    end
   else
    begin
     Control:=Control or (ChipSelect and BCM2837_SPI0_CS_CS_MASK);
    end;

   {Check Clock Rate}
   if (ChipSelect = SPI_CS_NONE) or (SPI.ChipSelects[ChipSelect].ClockRate = 0) then
    begin
     {Set Clock Divider}
     PBCM2837SPI0Registers(PBCM2710SPI0Device(SPI).Address).CLK:=(SPI.Divider and BCM2837_SPI0_CLK_CDIV);
    end
   else
    begin
     {Set Clock Divider}
     PBCM2837SPI0Registers(PBCM2710SPI0Device(SPI).Address).CLK:=(SPI.ChipSelects[ChipSelect].Divider and BCM2837_SPI0_CLK_CDIV);
    end;

   {Update Data}
   PBCM2710SPI0Device(SPI).Mode:=BCM2710_SPI0_MODE_IRQ;

   {Note: Cannot fill FIFO when TA bit is not set}

   PBCM2837SPI0Registers(PBCM2710SPI0Device(SPI).Address).DLEN:=Size;
   {Set Control (Active/Interrupt/Clear)}
   Control:=Control and not(BCM2837_SPI0_CS_INTR or BCM2837_SPI0_CS_INTD);
   Control:=Control or (BCM2837_SPI0_CS_TA or BCM2837_SPI0_CS_CLEAR_RX or BCM2837_SPI0_CS_CLEAR_TX);

   {Set Control and Status}
   PBCM2837SPI0Registers(PBCM2710SPI0Device(SPI).Address).CS:=Control;

   {Memory Barrier, not necessary, since we will be running on a dedicated thread anyway?}
   //DataMemoryBarrier; {After the Last Read}

   {Wait for Completion}
   if BCM2710SPI0ReadWritePoll(PBCM2710SPI0Device(SPI)) = ERROR_SUCCESS then

    begin
     {Get Count}
     Count:=PBCM2710SPI0Device(SPI).Count;

     {Check Count}
     if Count < Size then
      begin

       if SPI_LOG_ENABLED then SPILogError(SPI,'BCM2710: Write failure or timeout');

       {Update Statistics}
       Inc(SPI.TransferErrors);
      end;
    end
   else
    begin
     if SPI_LOG_ENABLED then SPILogError(SPI,'BCM2710: Wait failure on write');

     Result:=ERROR_OPERATION_FAILED;
    end;

   {Reset Data}
   PBCM2710SPI0Device(SPI).Source:=nil;
   PBCM2710SPI0Device(SPI).Dest:=nil;
   PBCM2710SPI0Device(SPI).Count:=0;
   PBCM2710SPI0Device(SPI).SourceRemain:=0;
   PBCM2710SPI0Device(SPI).DestRemain:=0;
  end;

 {$IF DEFINED(BCM2710_DEBUG) or DEFINED(SPI_DEBUG)}
 if SPI_LOG_ENABLED then SPILogDebug(SPI,'BCM2710:  Return Count=' + IntToStr(Count));
 {$ENDIF}

 {Return Result}
 if (Size = Count) then Result:=ERROR_SUCCESS;
end;

function BCM2710SPI0ReadWritePoll(SPI:PBCM2710SPI0Device):LongWord;
var
 Control:LongWord;
begin
 {}
 {Check SPI}
 if SPI = nil then Exit;

 {Loop until Completion}
 while SPI.DestRemain > 0 do
  begin
   {Memory Barrier}
   DataMemoryBarrier; {Before the First Write}

   {Read FIFO}
   BCM2710SPI0ReadFIFO(SPI);
   //Control:=PBCM2837SPI0Registers(SPI.Address).CS;

   {Write FIFO}
   BCM2710SPI0WriteFIFO(SPI);

   {Get Control and Status}
   Control:=PBCM2837SPI0Registers(SPI.Address).CS;

   {Check Done}
   if ((Control and BCM2837_SPI0_CS_DONE) <> 0) and (SPI.SourceRemain = 0) then
   //if (SPI.SourceRemain = 0) then
    begin
     {Read remaining FIFO}
     BCM2710SPI0ReadFIFO(SPI);

     {Reset Control (Active/Interrupt/Deassert/DMA/Clear)}
     Control:=Control and not(BCM2837_SPI0_CS_INTR or BCM2837_SPI0_CS_INTD or BCM2837_SPI0_CS_ADCS or BCM2837_SPI0_CS_DMAEN or BCM2837_SPI0_CS_TA);
     Control:=Control or (BCM2837_SPI0_CS_CLEAR_RX or BCM2837_SPI0_CS_CLEAR_TX);

     {Set Control and Status}
     PBCM2837SPI0Registers(SPI.Address).CS:=Control;

     {Set Data Length}
     PBCM2837SPI0Registers(SPI.Address).DLEN:=0;

    end;

   {Memory Barrier}
   DataMemoryBarrier; {After the Last Read}
  end;
 Result:=ERROR_SUCCESS; // This needs to have some actual use and not be ERROR_SUCCESS after every call
end;

function GPIOReadPin(Pin:LongWord):LongWord;
var
 Reg:LongWord;
 Shift:LongWord;
begin

 {Get Shift}
 Shift:=Pin mod 32;

 {Get Register}
 Reg:=BCM2837_GPLEV0 + ((Pin div 32) * SizeOf(LongWord));

 {Read Register}
 Result:=(PLongWord(GPIO_REGS_BASE + Reg)^ shr Shift) and BCM2837_GPLEV_MASK;

 {Memory Barrier}
 //DataMemoryBarrier; {After the Last Read}
end;

end.

