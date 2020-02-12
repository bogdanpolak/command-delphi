# Command Pattern for Delphi

![ Delphi Support ](https://img.shields.io/badge/Delphi%20Support-%20XE8%20..%2010.3%20Rio-blue.svg)
![ version ](https://img.shields.io/badge/version-%200.7-yellow.svg)

-----------------------------------------
PLAN

1. TAsyncCommand documentation
   - events: WithEventBeforeStart, WithEventAfterFinish
   - methods: GetElapsedTime: TTimeSpan; IsBusy; Terminate; GetElapsedTime / GetElapsedTimeMs
   - thread name for the debugging 
      - `fThread.NameThreadForDebugging('TAsyncCommand - '+Self.ClassName);`
   - sample
1. TAsyncCommand - OnUpdate with TTimer
   * WithEventOnProgress(aProc)
   * property ProgressInterval: integer;
1. Command evolution 
   - describe evolution from component and injection via property to Command Pattern with constructor injection
-----------------------------------------

## Overview

Simplified version of the GoF Command Pattern, created for the purposes of modernization of VCL projects. Also added action factory to this project, which is wrapping a command into VCL action.

## The Command Pattern

![](docs/resources/gof-command.png)

## Implementation

The project contains two versions of the pattern implementation:
1) classic Gang of Four `ICommand` interface
1) VCL `TCommand` class based on TComponent

## Modernization process

The `TCommand` component was created to help the **modernization of the legacy VCL code**. It assists the extraction of tangled code, which after securing it with unit tests, can be refactored into cleaner and cheaper to maintain object-oriented code.

`TCommand` component is a transition object that should be refactored after clearing extracted code and after removing UI dependencies

![](/docs/resources/moderniz-process.png)

## TCommand component

The easiest way to use the `TCommand` component is to create a new class, paste long method into Execute method and add all dependencies as published properties. See sample bellow.

Diagram of TCommand usage in the VCL application:

![](./docs/resources/tcommand-vcl.png)

## Creating / implementing new command

Developer to build new command needs to define new class derived from `TCommand` (unit: `Pattern.Command.pas`). He has to implement two protected methods: `DoGuard` and `DoExecute`: 
* *method* `DoGuard` - can be empty if there is no injection (injection system is explained bellow)
* *method* `DoExecute` - contains code which is main logic of the command

Both methods are `virtual` then defining their interface have to use `override` keyword. You can remove (not to add) `inherited` call from `DoExecute` implementation and you have to remove this call from `DoGuard` implementation, if not then during first call exception will be raise with message that you cant call base `TCommand.DoGuard` code. This safeguards that developer implemented its own `DoGuard` logic.

Sample command without injection (empty guard):
```pas
type
  TDiceRollCommand = class (TCommand)
  protected
    procedure DoGuard; override;
    procedure DoExecute; override;
  end;

procedure TDiceRollCommand.DoGuard;
begin
  // Required: even if no injection are provided 
end;

procedure TDiceRollCommand.DoExecute;
begin
  ShowMessage('Dice roll: '+RandomRange(1,6).ToString);
end;
```

To execute command you should create object and call `Execute` public method, which call `DoGuard` and then `DoExecute`. You shouldn't put any business logic into guard method, see bellow section about injection system for more details.

Sample call:

```pas
cmd := TDiceRollCommand.Ceate(Self);
cmd.Execute;
```

## TCommand injection system

`TCommand` component has built in automated injection system based on classic `RTTI` mechanism used by IDE Form Designer (Object Inspector). Properties exposed to be injectable have to be defined in `published` section of the component (command). All component based classes have switched on run-time type information generation during compilation process (compiler option `{$TYPEINFO ON}`). Thanks of that during creation of new command all dependencies can be easily provided and assigned to published properties automatically. More information about classic RTTI engine can be find in Delphi documentation: [Run-Time Type Information](http://docwiki.embarcadero.com/RADStudio/Rio/en/Run-Time_Type_Information_\(Delphi\))

Sample command with two dependencies (one required and one optional):
```pas
type
  TDiceRollCommand = class (TCommand)
  const
    RollCount = 100;
  private
    fOutput: TStrings;
    fProgressBar: TProgressBar;
    procedure ShowProgress(aRoll: integer);
  protected
    procedure DoGuard; override;
    procedure DoExecute; override;
  published
    property OutputRolls: TStrings read fOutput 
      write fOutput;
    property ProgressBar: TProgressBar read fProgressBar 
      write fProgressBar;
  end;

procedure TDiceRollCommand.DoGuard;
begin
  System.Assert(fOutput<>nil); 
end;

procedure TDiceRollCommand.ShowProgress(aRoll: integer);
begin
  if Assigned(fProgressBar) then begin
    if aRoll=0 then
      fProgressBar.Max := RollCount;
    fProgressBar.Position := aRoll;
  end;
end

procedure TDiceRollCommand.DoExecute;
begin
  ShowProgress(0);
  for var i := 0 to RollCount-1 do
  begin
    fOutput.Add(RandomRange(1,7).ToString);
    ShowProgress(i+1);
  end;
end;
```

Available published properties of TCommand are matched against types of parameters passed in parameters (open array). Following rules are used by matching algorithm:

1. The same object types are matched
1. If there is two or more object of the same class passed and more matching properties then parameter are assigned to properties according to order first with first, second with second, etc.
1. More specific object passed as parameter is matching to more general object in properties list
1. Numeric integer parameters are assigned to numeric properties
1. Strings to strings
1. Supported are also decimals, enumerable and boolean types.

**Warning!** Injected object are accessed by address in memory (pointer), thanks of that any changes made to object are visible inside and outside of the TCommand. Simple types and strings are accessed via value and properties have to updated manually to be updated.

Sample code injecting objects to properties of TDiceRollCommand:
```pas
cmd := TDiceRollCommand.Create(Self)
  .Inject([Memo1.Lines,ProgressBar1]);
```

Most popular and usually advised method of injecting dependencies is a constructor injection. This solution introduced here (TCommand pattern) is more component based approach. This pattern is more like a transition stage which allow quickly extract and execute important parts of big application. Final target point in that process is the best architectural solution, means injection through the constructor and use interfaces instead of objects.

## TCommand execution

1) Instant (ad-hoc) command execution
    * `TCommand.AdhocExecute<T>` - executes a command (creates a command, injects dependencies executes it and removes)
1) Full command construction and execution 
    * Create command with standard (component) constructor
    * Call method `Inject`
    * Execute command with `Execute`
1) Build command invoker `TCommandAction` which executes the command when the action is invoked
    * `TCommandAction` class is classic VCL action
    * This class has special methods to allow rapid construction and initialization

## Asynchronous Command

Business logic, extracted into the command, can be easily converted into asynchronous command, processed in a separate background thread. Replacing `TCommand` class with `TAsyncCommand` is first steep in such transformation:

```pas
uses
  Pattern.AsyncCommand;
type
  TAsyncDiceRollCommand = class (TAsyncCommand)
     ...
  end;
```

Although the change is very simple, but in general, multi-threaded processing is a much more serious subject and requires deeper knowledge of this area. In this example (`TDiceRollCommand`) two topics are problematic:

1. Access to UI control `fProgressBar: TProgressBar`
1. Access to shared memory `fOutputRolls: TStrings`

You can easily deal with them, but this requires more general multithread processing knowledge. More info you can find in dedicated documentation: [Asynchronous Command](docs/AsyncCommand.md)

## TCommandAction - VCL command invoker

`TCommandAction` is a wrapper class based on `TAction` and is able to execute commands based on `TCommand` class. Developer, when building VCL application, can easily bind this action to many controls (visual components which are driven by actions or are action-aware). For example `TCheckBox` has `Action` property which is executed when used is changing checkbox state (checked). Actions have some other advantages like build in notification system, precisely two such engines: one for updating visual state and another, more internal, for notifying about creation of new and deletion of existing components. Both engines are too complex to be described in this section, more information can be found in the Delphi online documentation.

Looking form architectural perspective `TCommandAction` can be used as an Invoker object and after migration can be replaced by more elastic custom solution.

Sample construction on `TCommandAction` invoker:

```pas
Button1.Action := TCommandAction.Create(Button1)
  .WithCaption('Run sample command')
  .WithCommand(TSampleCommand.Create(Button1))
  .WithInjections([Memo1, Edit1]);
```

### TCommandAction methods

| Utility method | Description |
| --- | --- |
| `WithCaption(aCaption)` | Sets an action caption which is displayed in a control |
| `WithShortCut(aShortcut)` | Sets a shortcut which is activating an action |
| `WithCommand(aCommand)` | Sets a command to execute |
| `WithInjections(aInjections)` | Injects values into the command's properties |
| `WithEventOnUpdate(aProc)` | Event triggered after action onUpdate event |
| `WithEventAfterExecution(aProc)` | Event triggered when command will be finished |

Sample setup OnUpdate event in `TCommandAction`:

```pas
Button2.Action := TCommandAction.Create(Self)
  .WithCaption('Run sample command')
  .WithCommand(MySampleCommand)
  .WithEventOnUpdate(
    procedure(cmd: TCommandAction)
    begin
      cmd.Enabled := CheckBox1.Checked;
    end);
```

## Command evolution 

TBD - describe evolution from component and injection via property to Command Pattern with constructor injection

## Samples

Ad-hoc command execution (create, inject, execute, remove)
```pas
TCommand.AdhocExecute<TSampleCommand>([Memo1, Edit1]);
```

Creates command and inject dependencies:
```pas
cmdSampleCommand := TSampleCommand.Create(AOwner);
cmdSampleCommand.Inject([Memo1, Edit1]);
```

Sample `TCommand` component:
```pas
type
  TSampleCommand = class (TCommand)
  private
    FMemo: TMemo;
    FEdit: TEdit;
  protected
    procedure DoGuard; override;
    procedure DoExecute; override;
  published
    property Memo: TMemo read FMemo write FMemo;
    property Edit: TEdit read FEdit write FEdit;
  end;

procedure TSampleCommand.DoGuard;
begin
  System.Assert(Memo<>nil);
  System.Assert(Edit<>nil);
end;

procedure TSampleCommand.DoExecute;
begin
  Memo.Lines.Add('Getting Edit text and put it here ...');
  Memo.Lines.Add('  * Edit.Text: '+Edit.Text);
end;
```
