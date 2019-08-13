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

## VCL TCommand factory

Methods of the class `TCommandVclFactory`:

1) `function CreateCommand` - creates a single command component (TCommand descendant) and inject dependencies into it
1)  `procedure ExecuteCommand` - executes a command (creates a command, injects dependencies executes it and removes)
1)  `function CreateCommandAction` - creates TAction, which contains embedded TCommand and injects dependencies
