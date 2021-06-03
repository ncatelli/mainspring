# mainspring
A CPU device and emulation framework focused on extensibility and flexibility of hardware layouts.

<!-- TOC -->

- [mainspring](#mainspring)
    - [Building](#building)
    - [Included](#included)
        - [Reference Implementations](#reference-implementations)
            - [MOS 6502](#mos-6502)
            - [CHIP-8](#chip-8)
    - [Warnings](#warnings)

<!-- /TOC -->

## Building
Mainspring is intendended to be included as a library and an [examples](./examples/) directory has been included to show basic usage.

## Included
The framework comes with both traits and types to assist users with implementations of additional CPUs/Architectures as well as reference implementations. The framework tries to keep the Traits as generic as possible to prevent as few reference implementations from being imposed on the user as possible.

### Reference Implementations
#### MOS 6502
The [MOS 6502](https://en.wikipedia.org/wiki/MOS_Technology_6502) is the first cpu emulated in this example and is used heavily as the basis for most of the traits and examples. Basic usages and hardware layouts can be found in the [examples](./examples/) directory.

#### CHIP-8
[CHIP-8](https://en.wikipedia.org/wiki/CHIP-8).

## Warnings
This is a built to support the other projects I've implemented in the First Principles of Computing project and may be subject to API change.