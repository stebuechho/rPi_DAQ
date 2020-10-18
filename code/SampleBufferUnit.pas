unit SampleBufferUnit;

{$mode objfpc}{$H+}

{ Unit to handle / setup the buffer(s) for the data samples}

interface

uses
  Classes,
  SysUtils,
  DAQ_Globals;

 procedure BufferWrite(locSampleBuffer:PSampleBuffer;Sample:TSample);

 function BufferInit(locSampleBuffer:PSampleBuffer):PSampleBuffer;
 function BufferArrayInit(SampleBufferArray:PSampleBufferArray):PSampleBufferArray;
 function BufferRead(locSampleBuffer:PSampleBuffer):TSample;
 function BufferReadAll(locSampleBuffer:PSampleBuffer):TSampleArray;

 function BMP_BufferInit(locSampleBuffer:PBMPSampleBuffer):PBMPSampleBuffer;
 function BMP_BufferArrayInit(SampleBufferArray:PBMPSampleBufferArray):PBMPSampleBufferArray;
 procedure BMP_BufferWrite(locSampleBuffer:PBMPSampleBuffer;Sample:TBMPSample);
 function BMP_BufferReadAll(locSampleBuffer:PBMPSampleBuffer):TBMPSampleArray;

implementation

function BufferInit(locSampleBuffer:PSampleBuffer):PSampleBuffer;
begin
 locSampleBuffer:=AllocMem(SizeOf(TSampleBuffer));
 locSampleBuffer^.Count:=0;
 locSampleBuffer^.State:=1;
 locSampleBuffer^.StartTime:=0;
 Result:=locSampleBuffer
end;

function BMP_BufferInit(locSampleBuffer:PBMPSampleBuffer):PBMPSampleBuffer;
begin
 locSampleBuffer:=AllocMem(SizeOf(TBMPSampleBuffer));
 locSampleBuffer^.Count:=0;
 locSampleBuffer^.State:=1;
 locSampleBuffer^.StartTime:=0;
 Result:=locSampleBuffer
end;

function BufferArrayInit(SampleBufferArray:PSampleBufferArray):PSampleBufferArray;
// Probably can be changed to BufferArrayInit():PSampleBufferArray;
var
 i :  Integer;
begin
 for i:=1 to N_BUFFER do
 begin
  SampleBufferArray[i]:= BufferInit(SampleBufferArray[i]);
 end;

 Result:=SampleBufferArray
end;
function BMP_BufferArrayInit(SampleBufferArray:PBMPSampleBufferArray):PBMPSampleBufferArray;
// Probably can be changed to BufferArrayInit():PSampleBufferArray;
var
 i :  Integer;
begin
 for i:=1 to BMP_N_BUFFER do
 begin
  SampleBufferArray[i]:= BMP_BufferInit(SampleBufferArray[i]);
 end;

 Result:=SampleBufferArray
end;

//Adding a sample to the buffer
procedure BufferWrite(locSampleBuffer:PSampleBuffer;Sample:TSample);
begin
 //Write the sample
 if locSampleBuffer^.State=3 then
  begin
   locSampleBuffer^.Samples[(locSampleBuffer^.Count) mod SAMPLE_COUNT]:=Sample;

   //Increment the count
   Inc(locSampleBuffer^.Count);

   //if Buffer is full, set State to 4 (full)
   if locSampleBuffer^.Count=SAMPLE_COUNT then  locSampleBuffer^.State:=4;

  end;
end;

//Adding a sample to the buffer
procedure BMP_BufferWrite(locSampleBuffer:PBMPSampleBuffer;Sample:TBMPSample);
begin
 //Write the sample
 if locSampleBuffer^.State=3 then
  begin
   locSampleBuffer^.Samples[(locSampleBuffer^.Count) mod BMP_SAMPLE_COUNT]:=Sample;

   //Increment the count
   Inc(locSampleBuffer^.Count);

   //if Buffer is full, set State to 4 (full)
   if locSampleBuffer^.Count=BMP_SAMPLE_COUNT then  locSampleBuffer^.State:=4;

  end;
end;

//Removing a sample from the buffer
function BufferRead(locSampleBuffer:PSampleBuffer):TSample;
begin
 //Read the sample
 if locSampleBuffer^.State=2 then
  begin
   Result:=locSampleBuffer^.Samples[(SAMPLE_COUNT-(locSampleBuffer^.Count))];

   //Decrement the count
   Dec(locSampleBuffer^.Count);

   //if Buffer is empty (all samples are read), set State to 1 (empty)
   if locSampleBuffer^.Count=0 then  locSampleBuffer^.State:=1;

  end;
end;

//Read all samples from the buffer to an array
function BufferReadAll(locSampleBuffer:PSampleBuffer):TSampleArray;
begin
 if locSampleBuffer^.State=2 then
 begin
  Result:=locSampleBuffer^.Samples;
  //Reset the counter
  locSampleBuffer^.Count:=0;
  //Set State to 1 (empty)
  locSampleBuffer^.State:=1;
 end;
end;

function BMP_BufferReadAll(locSampleBuffer:PBMPSampleBuffer):TBMPSampleArray;
begin
 if locSampleBuffer^.State=2 then
 begin
  Result:=locSampleBuffer^.Samples;
  //Reset the counter
  locSampleBuffer^.Count:=0;
  //Set State to 1 (empty)
  locSampleBuffer^.State:=1;
 end;
end;



end.

