# Command Pattern for Delphi

![ Delphi Support ](https://img.shields.io/badge/Delphi%20Support-%20XE8%20..%2010.3%20Rio-blue.svg)

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

## Developing new command

> TBD (delivered in ver 0.6)

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

## TCommand injection system

> TBD (delivered in ver 0.6)

## TCommandAction - VCL command invoker

> TBD (delivered in ver 0.6)

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

Create invoker `TCommandAction`:
```pas
Button1.Action := cmdSampleCommand := TCommandAction.Create(Button1)
  .SetupCaption('Run sample command')
  .SetupCommand(TSampleCommand.Create(Button1)
    .Inject([Memo1, Edit1])
  );
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

procedure TSampleCommand.DoGuard; override;
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
