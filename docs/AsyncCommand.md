# Asynchronous command

## Introduction

Command component (`TCommand`) can be  converted into asynchronous one using `TAsyncCommand` class. Asynchronous means that all code implemented in `DoExecute` method will be processed in a separate background thread. Today when each machine has access multiple CPU cores this functionality will allow to execute domain code in background, even in parallel, without any negative influence on displayed UI.

Introducing parallel programing into your project is not very simple in general, usually developers are struggling with many issues coming from that area, but in this days there is no other alternative and `TAsyncCommand` pattern can make this transition much easier.

One of the simplest async commands can look like this code:
```pas
type
  TSimpleAsyncCommand = class(TAsyncCommand)
  protected
    procedure DoExecute; override;
  end;

procedure TSimpleAsyncCommand.DoExecute;
begin
   DoSampleJobInBackgroundThread;
end;
```

The only difference between command executed in main thread and this one executed in background thread is base class `TAsyncCommand`. This command launching looks the same like any other command:

```pas
TSimpleAsyncCommand.Create(aOwner).Execute;
```

## Main and background thread

This is very important to be sure which code on the async command is executed in background thread and which in main thread. Writing code working in background developer has very restricted access to outside "world". To force some portion of code to be executed in main thread you can use Synchronize method:

```pas
procedure TSimpleAsyncCommand.DoExecute;
begin
   for i:=0 to fDataNames.Count-1 do
   begin
      LoadData (fDataNames[i]);
      Synchronize(procedure begin 
        fReportMemo.Lines.Add('Step '+i.ToString
          +',  Data: '+fDataNames[i]);
      end);
   end;
end;
```

In this sample adding report line into TMemo component has to be done in main thread and data can be loaded in background thread.

> **Warning!** Whereas using Synchronize looks like very simple solution it not recommended one. This should be used with full understanding that switching to main thread is very costly and during this time working thread (DoExecute code) is blocked, till the end of the Synchronize method.

### Debugging background thread

Delphi gives developers a very easy method of testing background thread processing. Usually it's enough to set a breakpoint inside DoExecute method and verify processing flow and inspect a variable values. In more complex situations there could be needed to define thread name, by default TAsyncCommand background thread is named using following formula: `'TAsyncCommand - '+ClassName`, where ClassName is a name of this particular command class.

## TAsyncCommand methods and properties

| Method | Description |
| --- | --- |
| `Execute` | Starts a new background thread and run DoExecute |
| `WithEventBeforeStart( aProc )` | Provided method is called before DoExecute |
| `WithEventAfterFinish( aProc )` | Provided method is called when DoExecute will finish |
| `WithEventOnProgress( aProc )` | Provided method is called during command executing once a defined time (`ProgressInterval`) |
| `Terminate` | Allows to break execution of background thread |
| `IsBusy: boolean` | Returns is command started and being processed |
| `GetElapsedTime` | Returns time consumed by commands |

Events defined in methods: `WithEventBeforeStart`, `WithEventAfterFinish` and `WithEventOnProgress` are processed in the main thread and can access all the VCL resources, but not directly background threads data and structures (this requires thread safe, critical section solution).

Event defined in `WithEventOnProgress` method is called every defined interval (in milliseconds). The AsyncCommand is using an internal timer which is triggered with that interval. Then OnProgress event is executed in the main thread and if developer wants to access thread (command internal) data structures he has to use proper thread safe mechanism.

| Property | Description |
| --- | --- |
| `ProgressInterval: integer` | Defined interval of internal command timer (in milliseconds) which is calling OnProgress event. Default value = 100 ms |


Sample execution of TAsyncCommand:

```pas
cmdGenerateSampelCSV
  .WithInjections([fCustomerID,fOrdersProxy])
  .WithEventBeforeStart(
    procedure
    begin
      aProgressBar.Position := 0;
    end)
  .WithEventOnUpdate(
    procedure
    begin
      aProgressBar.Position := fMyAsyncCommand.GetProgressPercent;
    end)
  .WithEventAfterFinish(
    procedure
    begin
      aProgressBar.Position := 100;
      fCSVExporter.SaveToFile(aFileName, fMyAsyncCommand.ReportData);
      aSeconds := cmdGenerateSampelCSV.GetElapsedTime.TotalSeconds;
      LogAppPerformance(aReportName, aSeconds);
    end)
  .Execute;
```

## Async Command Rules

1) Remove code manipulating UI controls
    - Remove as much of that code as it is possible
    - The best approach is to remove all such code  from `DoExecute` method
    - `TAsyncCommand` has a dedicated support for updating UI controls
1) Use synchronize method if UI assess is required
    - if assess to UI elements is required from background thread (`DoExecute` code) wrap such code accessing UI elements into `Synchronize` method - example bellow
    - Synchronize reduce a lot parallel processing capabilities and reduce a thread performance, therefore it is not the recommended solution
1) Do not share memory structures with main thread
   - Use memory structures only internally (inside `DoExecute`)
   - for example if you want to access SQL server and fetch data it's better to create a new SQL connection component dedicated only for the async command
   - Suggested solution is to: crate a structure colones before async execution, process everything using internal structures and get the results after processing
1) Access shared memory structures inside critical section
   - Use proper concurrency control structures like `TMonitor` to prevent parallel access to the same memory area by many threads
1) Avoid memory sharing between multiple background threads
   - Try to avoid such memory sharing because this is the most challenging scenario of parallel computing
   - Proper solutions and patterns covering that scenario are far beyond the scope of this documentation
