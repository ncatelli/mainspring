# mainspring
A CPU device and emulation framework focused on extensibility and flexibility of hardware layouts.

<!-- TOC -->

- [mainspring](#mainspring)
    - [Building](#building)
    - [Included](#included)
        - [Traits and Types](#traits-and-types)
            - [Addressable](#addressable)
            - [CPU](#cpu)
            - [StepState](#stepstate)
            - [Register](#register)
            - [Offset](#offset)
            - [Cyclable](#cyclable)
        - [Reference Implementations](#reference-implementations)
            - [MOS6502](#mos6502)
    - [Warnings](#warnings)

<!-- /TOC -->

## Building
Mainspring is intendended to be included as a library and an [examples](./examples/) directory has been included to show basic usage.

## Included
The framework comes with both traits and types to assist users with implementations of additional CPUs/Architectures as well as reference implementations. The framework tries to keep the Traits as generic as possible to prevent as few reference implementations from being imposed on the user as possible.

### Traits and Types
#### Addressable
The Addressable trait represents any type that can be registered with a CPUs address map. These can include ReadOnly and ReadWrite memory, Peripheral Devices, IO... etc. and requires only that the device have a defined address space and read/write methods.

#### CPU
CPU requires only a single method, `run`. This method will attempt to run the cpu for the number of cycles specified as it's argument and return the new state of the CPU.

#### StepState
A wrapper around the CPU trait that also implements CPU. This type is meant to store additional metadata about a CPUs execution as it moves inbetween cycles. 

#### Register
Register functions very similarly to Addressable, requiring only that a `read` and `write` method be implemented and functions as a wrapper around a CPU register.

#### Offset
Contains a single `offset` method that returns a usize count of the byte offset of an operation.

#### Cyclable
Contains a single method `cycles` which returns a usize count representing the cycles of an operations.

### Reference Implementations
#### MOS6502
The [MOS6502](https://en.wikipedia.org/wiki/MOS_Technology_6502) is the first cpu emulated in this example and is used heavily as the basis for most of the traits and examples. Basic usages and hardware layouts can be found in the [examples](./examples/) directory.

## Warnings
This is a built to support the other projects I've implemented in the First Principles of Computing project and may be subject to API change.