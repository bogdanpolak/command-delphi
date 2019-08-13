# Command Pattern for Delphi

## Overview

Simplified version of the GoF Command Pattern, created for the purposes of modernization of VCL projects. Also added action factory to this project, which is wrapping a command into VCL action.

## The Command Pattern

![](docs/resources/gof-command.png)

## Implementation

The project contains two versions of the pattern implementation:
1) classic - ICommand interface
1) VCL - TCommand class based on TComponent

## Modernization legacy VCL projects

The TCommand component was created to help the modernization of the legacy VCL code. It assists the extraction of tangled code, which, after securing with unit tests, can be refactored into cleaner and cheaper to maintain object-oriented code.

![](/docs/resources/moderniz-process.png)

## Command factory
