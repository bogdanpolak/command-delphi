# Asynchronous command

## Introduction

Command component (`TCommand`) can be  converted into asynchronous one using `TAsyncCommand` class. Asynchronous means that all code implemented in `DoExecute` method will be processed in a separate background thread. Today when each machine has access multiple CPU cores this functionality will allow to execute domain code in background, even in parallel, without any negative influence on displayed UI.

Introducing parallel programing into your project is not very simple in general, usually developers are struggling with many issues coming from that area, but in this days there is no other alternative and `TAsyncCommand` pattern can make this transition much easier.

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
   - Proper solutions and patterns covering this senarion are far beyond the scope of this documentation
