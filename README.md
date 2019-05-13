# <span style="color:#F3B73B">Interface IR to Littlefoot Tool</span>

As noted this tool is part of the larger [Muses](https://muses-dmi.github.io/)
project and is a component within a larger set of tools for working with SVG
interfaces.

#  <span style="color:#F3B73B">Dependencies</span> 

The system requires Haskell and uses its build and package mamager **Stack**. If
you don't already have Stack installed, then on  Un*x operating systems,
including Mac OS, run the following command:
```
curl -sSL https://get.haskellstack.org/ | sh
```

and on Window you can download and install the [Windows 64-bit
Installer.](https://get.haskellstack.org/stable/windows-x86_64-installer.exe)

#  <span style="color:#F3B73B">Building</span>

Clone the repo and change into its root directory, then run the following
command:

```
stack build
```

#  <span style="color:#F3B73B">Using it</span>


#  <span style="color:#F3B73B">More Information</span>

Parent project

   - [Muses](https://muses-dmi.github.io/).

Tool and documentation for specification of interfaces as SVGs:

   - [SVG Creator tool](https://github.com/muses-dmi/svg-creator). (This repo.)
   - [SVG Interface Documentation](https://github.com/muses-dmi/svg-creator/blob/master/docs/interfaces.md).

Tools for translating SVG Interfaces to the JSON intermidiate representation and different backends:

   - [SVG Interface to IR tool](https://github.com/muses-dmi/svg_interface).
   - [Interface IR to Littlefoot tool](https://github.com/muses-dmi/svg-littlefoot).
   - [SVG Sensel Driver](https://github.com/muses-dmi/sensel_osc).

#  <span style="color:#F3B73B">License</span>

Some of the code for the underlying SVGß DSL comes from
[svg-builder](https://github.com/diagrams/svg-builder), and even in the case of
changes to those original files its license applies.

Otherwise licensed under either of

 * Apache License, Version 2.0 ([LICENSE-APACHE](LICENSE-APACHE) or http://www.apache.org/licenses/LICENSE-2.0)
 * MIT license ([LICENSE-MIT](LICENSE-MIT) or http://opensource.org/licenses/MIT)
 * [Mozilla Public License 2.0](https://www.mozilla.org/en-US/MPL/2.0/)

at your option.

Dual MIT/Apache2 is strictly more permissive