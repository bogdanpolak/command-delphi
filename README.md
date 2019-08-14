# Command Pattern for Delphi

## Overview

Simplified version of the GoF Command Pattern, created for the purposes of modernization of VCL projects. Also added action factory to this project, which is wrapping a command into VCL action.

## The Command Pattern

![](docs/resources/gof-command.png)

## Implementation

The project contains two versions of the pattern implementation:
1) classic Gang of Four `ICommand` interface
1) VCL `TCommand` class based on TComponent

## Modernization legacy VCL projects

The `TCommand` component was created to help the **modernization of the legacy VCL code**. It assists the extraction of tangled code, which after securing it with unit tests, can be refactored into cleaner and cheaper to maintain object-oriented code.

`TCommand` component is a transition object that should be refactored after clearing extracted code and after removing UI dependencies

![](/docs/resources/moderniz-process.png)

## TCommand component

The easiest way to use the `TCommand` component is to create a new class, paste long method into Execute method and add all dependencies as properties to published section. See sample bellow.

Created object you can wrap into TCommandAction object and use it as classic VCL action. You can use the factory for this task (described below)

Diagram of TCommand usage in the VCL application:

![](./docs/resources/tcommand-vcl.png)

## VCL TCommand factory

Methods of the class `TCommandVclFactory`:

1) `function CreateCommand` - creates a single command component (TCommand descendant) and inject dependencies into it
1)  `procedure ExecuteCommand` - executes a command (creates a command, injects dependencies executes it and removes)
1)  `function CreateCommandAction` - creates TAction, which contains embedded TCommand and injects dependencies

**Samples**

```pas
ACommand := TCommandVclFactory.CreateCommand<TButon2Command>(
    AOwner, [Memo1, Edit1]);
```
```pas
TCommandVclFactory.ExecuteCommand<TButon2Command>(
    [Memo1, Edit1]);
```
```pas
act := TCommandVclFactory.CreateCommandAction
    <TButon1Command>(Self, 'Run command: Button1',
    [Memo1]);
Button1.Action := act;
```

Sample `TCommand` component:

```pas
type
  TButon2Command = class (TCommand)
  private
    FMemo: TMemo;
    FEdit: TEdit;
  protected
    procedure Guard; override;
  public
    procedure Execute; override;
  published
    property Memo: TMemo read FMemo write FMemo;
    property Edit: TEdit read FEdit write FEdit;
  end;

procedure TButon2Command.Guard; override;
begin
  Assert(Memo<>nil);
  Assert(Edit<>nil);
end;

procedure TButon2Command.Execute;
begin
  inherited;
  Memo.Lines.Add('');
  Memo.Lines.Add('Getting Edit text and put it here ...');
  Memo.Lines.Add('  * Edit.Text: '+Edit.Text);
end;
```