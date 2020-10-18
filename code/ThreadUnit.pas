unit ThreadUnit;

{$mode objfpc}{$H+}

interface

uses
  GlobalConfig,
  GlobalConst,
  GlobalTypes,
  DAQ_Globals,
  Platform,
  Threads,
  SysUtils,
  Console,          {Include the console unit so we can output logging to the screen}
  SPI,
  I2C,
  GPIO,
  BCM2710,
  ADS8320,
  MeasureUnit;


{The start function which does all the setup work for our dedicated thread}
procedure StartSampleThreads(Handle:TWindowHandle);

implementation

{Forward declaration of our dedicated CPU thread function}
function ADC_Sampling_Execute(Parameter:Pointer):PtrInt; forward;
function PPS_Sampling_Execute(Parameter:Pointer):PtrInt; forward;
function checkToReboot(Parameter:Pointer):PtrInt; forward;
function StartDedicatedThread(Handle:TWindowHandle;ThreadFunction : tthreadfunc; cpu_id : Byte):TThreadHandle; forward;

{This is the startup function which creates the dedicated CPU thread and handles all of
 the setup work to migrate other threads away from the selected CPU. The comments contain
 a lot of important information}

function StartDedicatedThread(Handle:TWindowHandle;ThreadFunction : tthreadfunc; cpu_id : Byte):TThreadHandle;
var
 Count:Integer;
 Message:TMessage;
 CurrentCPU:LongWord;
 DedicatedThread:TThreadHandle;
 ThreadCurrent:PThreadSnapshot;
 ThreadSnapshot:PThreadSnapshot;
 MigrationComplete:Boolean;
 n_migration:Integer;
begin

 {Some initial housekeeping just to be safe, check the number of CPUs available}
 if CPUGetCount < 4 then 
  begin
   {Less than 4 is bad, we can't continue}
   Exit;
  end;
 
 {First step is to create a new thread and assign it to the CPU that we want to take
  over, this is just the same as creating any other thread except we want to explicitly
  set the CPU for it to run on and also the affinity so that it cannot run on any other CPU. 
  
  We can do this in one step by calling the SysBeginThreadEx() function but we can also do it
  by using the normal BeginThread() function and then adjusting the CPU and affinity later}
 DedicatedThread:=BeginThread(ThreadFunction,nil,DedicatedThread,THREAD_STACK_DEFAULT_SIZE);
 Result:=DedicatedThread;

 {Let's set the name of our thread so we can see it in the thread list}
 ThreadSetName(DedicatedThread,'Dedicated Sampling Thread');

 {Now we can set the affinity of our thread to CPU 3 and wait for the scheduler to migrate it for us}
 ThreadSetAffinity(DedicatedThread,(1 shl cpu_id));
 
 
 {Migrations happen during context switches, so our thread may not be instantly on the new CPU, instead
  we check where our thread is and wait for it to migrate if needed}
 CurrentCPU:=ThreadGetCPU(DedicatedThread);
 if CurrentCPU <> cpu_id then
  begin
   {Keep checking until it is migrated}
   while ThreadGetCPU(DedicatedThread) <> cpu_id do
    begin
     Sleep(1000);
    end;
  end;
 
 {Now we disable thread migrations temporarily so that we don't have threads moving around while we
  are trying to setup our dedicated CPU, you can see this on the "Scheduler" page in web status}
 SchedulerMigrationDisable;
 
 {disable thread allocation but only for that CPU and not the others}
 SchedulerAllocationDisable(cpu_id);

 {migrate all of the other threads away from our dedicated CPU. We can use the ThreadSnapshotCreate() function to get a current list}
 
 {We also want to count how many threads need to be migrated so we'll start with zero}
 Count:=0;
 
 {Then create a thread snapshot, the snapshot contains all of the thread information at a precise 
  point in time. The real thread information changes hundreds of times per second and so isn't easy
  to read directly}
 ThreadSnapshot:=ThreadSnapshotCreate;
 if ThreadSnapshot <> nil then
  begin
  
   {Get the first thread in the snapshot}
   ThreadCurrent:=ThreadSnapshot;
   while ThreadCurrent <> nil do
    begin
    
     {Check the handle of the thread to make sure it is not our dedicated CPU thread}
     if ThreadCurrent^.Handle <> DedicatedThread then
      begin
      
       {Check the CPU to see if it is on CPU 3}
       if ThreadCurrent^.CPU = cpu_id then
        begin
         
         {Check for one of the special threads and if it is not then ask it to migrate}
         if ThreadCurrent^.Handle = SchedulerGetThreadHandle(cpu_id,THREAD_TYPE_IDLE) then
          begin
          
           {This is the idle thread, we can't migrate this one}
           // ...skipping
          end
         else if ThreadCurrent^.Handle = SchedulerGetThreadHandle(cpu_id,THREAD_TYPE_IRQ) then
          begin
          
           {This one is the IRQ thread and it can't be migrated either}
           // ...skipping
          end
         else if ThreadCurrent^.Handle = SchedulerGetThreadHandle(cpu_id,THREAD_TYPE_FIQ) then
          begin
          
           {FIQ threads also can't be migrated but they never run so it doesn't matter}
           // ...skipping
          end
         else if ThreadCurrent^.Handle = SchedulerGetThreadHandle(cpu_id,THREAD_TYPE_SWI) then
          begin
          
           {And the SWI threads are the same so we can ignore them as well}
           // ...skipping
          end
         else
          begin
          
           {If the thread is not any of those then it must be a normal thread. Ask the scheduler to migrate it
            to CPU 0 instead, we could specify any CPU}
           ThreadSetCPU(ThreadCurrent^.Handle,CPU_ID_0);
           {Add one to our migrated thread count}
           Inc(Count);
          end;          
        end; 
      end
     else
      begin
      
       {No need to migrate our own thread, that wouldn't make any sense!}
       // ...skipping
      end;
     
     {Get the next thread from the snapshot}
     ThreadCurrent:=ThreadCurrent^.Next;
    end; 
   
   {Remember to destroy the snapshot when we have finished using it}
   ThreadSnapshotDestroy(ThreadSnapshot);
  end; 
  
 {Print the number of threads that we asked to migrate}
 ConsoleWindowWriteLn(Handle,'Migrated ' + IntToStr(Count) +  ' threads from ' + CPUIDToString(cpu_id));
 MigrationComplete:=False;
 n_migration:=0;
 while not(MigrationComplete) and (n_migration<60) do
  begin
 {Thread migrations only happen during context switches.
  Sleep for a second and then quickly run through a new snapshot to check if everyone has migrated}
 Sleep(1000);
 {Create the snapshot and reset the count}
 Count:=0;
 ThreadSnapshot:=ThreadSnapshotCreate;
 if ThreadSnapshot <> nil then
  begin
   {Get the first thread}
   ThreadCurrent:=ThreadSnapshot;
   while ThreadCurrent <> nil do
    begin
     {Check the handle and the CPU}
     if (ThreadCurrent^.Handle <> DedicatedThread) and (ThreadCurrent^.CPU = cpu_id) then
      begin
       if (ThreadCurrent^.Handle <> SchedulerGetThreadHandle(cpu_id,THREAD_TYPE_IDLE))
        and (ThreadCurrent^.Handle <> SchedulerGetThreadHandle(cpu_id,THREAD_TYPE_IRQ))
        and (ThreadCurrent^.Handle <> SchedulerGetThreadHandle(cpu_id,THREAD_TYPE_FIQ))
        and (ThreadCurrent^.Handle <> SchedulerGetThreadHandle(cpu_id,THREAD_TYPE_SWI)) then
        begin
         {Add one to our count}
         Inc(Count);
        end;
      end;
      
     {Get the next thread}
     ThreadCurrent:=ThreadCurrent^.Next;
    end;
    
   {Destroy the snapshot}
   ThreadSnapshotDestroy(ThreadSnapshot);
  end;
   inc(n_migration);
   if Count=0 then MigrationComplete:=True;
  end;

 {Check the count to see if any threads have not migrated yet, we won't proceed if there are any.}
 if not(MigrationComplete) then
  begin
   ConsoleWindowWriteLn(Handle,'Error, ' + IntToStr(Count) +  ' threads remaining on ' + CPUIDToString(cpu_id));
   Sleep(3000);
   SystemRestart(0);
  end;

 {Send a message to our dedicated CPU thread to tell it know we are done and it can go ahead}
 FillChar(Message,SizeOf(TMessage),0);
 ThreadSendMessage(DedicatedThread,Message);

 {Enable thread migrations now that we are all done, the scheduler will not touch our dedicated CPU}
 SchedulerMigrationEnable;

end;

procedure StartSampleThreads(Handle:TWindowHandle);
var
 PPSThread, CheckRebootThread:TThreadHandle;

begin
 Sleep(3000);
 READY_SWITCH:=False;
 IS_RECORDING:=False;
 {Create another console window so we can track the progress of our thread later}
 RightWindow:=ConsoleWindowCreate(ConsoleDeviceGetDefault,CONSOLE_POSITION_RIGHT,False);

 {Thread that samples from the ADC to a buffers at high sample rates. Needs to run as dedicated thread on own cpu
 to garantee timing accuracy}
 StartDedicatedThread(Handle,@ADC_Sampling_Execute,CPU_ID_3);

 {Thread that samples from BMP to a buffer at low sample rates. No need for a dedicated thread. But increase priority}
 BMPThread:=BeginThread(@WriteData2Buffer_BMP_Thread,nil,BMPThread,THREAD_STACK_DEFAULT_SIZE);
 ThreadSetPriority(BMPThread,7);

 {Thread that writes the buffers to disk (USB stick)}
 WriteThread:=BeginThread(@WriteData2Disk,nil,WriteThread,THREAD_STACK_DEFAULT_SIZE);

 {Thread that records the PPS signal. PPS is connected to GPIO 17 and it is set up to wait for a PPS pulse, bypassing interrupts.
 The moment the pulse arrives, the current time is recorded}
 PPSThread:=BeginThread(@PPS_Sampling_Execute,nil,PPSThread,THREAD_STACK_DEFAULT_SIZE);

 CheckRebootThread:=BeginThread(@checkToReboot,nil,CheckRebootThread,THREAD_STACK_DEFAULT_SIZE);

end;

{This is the thread function for our dedicated CPU thread.}
function ADC_Sampling_Execute(Parameter:Pointer):PtrInt;
var

 Message:TMessage;
 ADCDevice:PSPIDevice;
 SampleBytes:PSPIBytes;

 //writingSampleBuffer:PSampleBuffer;
begin
 ADCDevice   := ADCDevice_glob;
 SampleBytes := SampleBytes_glob;

 Result:=0;
 {Do a loop while we are not on our dedicated CPU}
 ConsoleWindowWriteLn(RightWindow,'Waiting for migration to ' + CPUIDToString(CPU_ID_3));
 while ThreadGetCPU(ThreadGetCurrent) <> CPU_ID_3 do
  begin
   Sleep(1000);
  end;
 {Wait for a message from the main thread to say we are ready to go}
 ThreadReceiveMessage(Message);
 SchedulerPreemptDisable(CPU_ID_3);
 DisableFIQ;
 DisableIRQ;

 WriteData2Buffer(ADC_Fs,BMP_Fs,ADCDevice,SampleBytes);

end;

procedure Inc_PPS_count(Parameter:Pointer;Pin,Trigger:LongWord);
begin
 PPS_time:=ClockGetTotal;
 Inc(PPS_count);
end;

procedure Get_PPS_Time(Parameter:Pointer;Pin,Trigger:LongWord);
begin
 PPS_time:=ClockGetTotal;
end;

function PPS_Sampling_Execute(Parameter:Pointer):PtrInt;
var
 GPIODevice:PGPIODevice;

begin
 GPIODevice := GPIODeviceGetDefault;
 PPS_count  :=0;
 PPS_time   :=0;

 Result:=0;
 if GPS_mode=1 then
  begin
   GPIODeviceStart(GPIODevice);
   GPIODeviceInputEvent(GPIODevice, GPIO_PIN_17, GPIO_TRIGGER_RISING, GPIO_EVENT_FLAG_INTERRUPT or GPIO_EVENT_FLAG_REPEAT, INFINITE, @Inc_PPS_count, nil);
   while PPS_time=0 do Sleep(1);
   READY_SWITCH:=True;
  end
 else
  begin
   READY_SWITCH:=True;
   while True do
    begin
     Sleep(1000);
     PPS_time:=ClockGetTotal;
     Inc(PPS_count);
    end;
  end;

end;

function checkToReboot(Parameter:Pointer):PtrInt;

begin
 Result:=0;
 while True do
  begin
   Sleep(CHECK_REBOOT_TIME*1000);
   if (not IS_RECORDING) or BUFFER_OverFlow or BMP_BUFFER_OverFlow then SystemRestart(0);
   end
end;



end.
