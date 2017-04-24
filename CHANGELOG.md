# Change Log

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/)

## [Unreleased]

nothing yet

## [0.8.1.0] - 2017-03-29

### Added
- This change log.
- `Shen.Pack` can now pull release notes from change log.
- Additional default type aliases for all C# type keywords.
- CLR reverse aliasing to render typenames as aliases.
- Normalized framework version to 4.5.

## [0.8.0.0] - 2017-03-15

### Added
- `Shen.Pack` which repacks assemblies together and builds NuGet package.
- `Obj of obj` as a case of `Value`.
- CLR interop functions:
  - `clr.alias` - assign alias to full CLR class name.
  - `clr.new` - creating CLR objects, including generics: `new Class(x)`.
  - `clr.unbox` - gets Kλ values from CLR primitve values.
  - `clr.null`, `clr.bool`, `clr.int`, `clr.decimal`, `clr.string` - creating primitive values from Kλ values.
  - `clr.get`, `clr.set` - get/set instance properties on objects: `dateTime.Hour`.
  - `clr.get-static`, `clr.set-static` - get/set properties on classes: `DateTime.Now`.
  - `clr.get-index`, `clr.set-index` - accessing indexer properties: `obj[x]`.
  - `clr.invoke`, `clr.invoke-static` - invoking methods: `obj.Method(x)`.
- `shensharp.globals`: direct access to the port's runtime.
- `--help` option to Shen.Repl.
- Conversion optimization to `Kl.Make` for functions.

### Changed
- Interpreted functions now use captured scope inlining instead of carrying around `Map` of locals.
- Moved helper extension methods from code generated in `Kl.Make` to `Shen.Api`.
- Simplified project logo by removing sharp symbol.

### Fixed
- Top-level REPL function re-named back from `shen.shen` to `shen`.
- Fixed use of `ILRepack`.

## [0.7.0.0] - 2017-02-26

### Changed
- Shen version updated to 19.3.1.
- Top-level REPL function re-named from `shen.shen` to `shen`.
- Split code out into multiple solution files and `Shen.*` projects depend on artifacts generated by `Kl.Make`.

## [0.6.0.0] - 2017-02-23

Don't remember.

## [0.5.0.0] - 2017-02-16

Don't remember.

## [0.4.0.0] - 2017-01-26

Don't remember.

## [0.3.0.0] - 2017-01-20

Don't remember.

## [0.2.0.0] - 2017-01-13

Don't remember.

## [0.1.0.0] - 2016-07-19

Don't remember.

[Unreleased]: https://github.com/rkoeninger/ShenSharp/compare/v0.8.1.0...HEAD
[0.8.1.0]: https://github.com/rkoeninger/ShenSharp/compare/v0.8.0.0...v0.8.1.0
[0.8.0.0]: https://github.com/rkoeninger/ShenSharp/compare/v0.7.0.0...v0.8.0.0
[0.7.0.0]: https://github.com/rkoeninger/ShenSharp/compare/v0.6.0.0...v0.7.0.0
[0.5.0.0]: https://github.com/rkoeninger/ShenSharp/compare/v0.4.0.0...v0.5.0.0
[0.4.0.0]: https://github.com/rkoeninger/ShenSharp/compare/v0.3.0.0...v0.4.0.0
[0.6.0.0]: https://github.com/rkoeninger/ShenSharp/compare/v0.5.0.0...v0.6.0.0
[0.3.0.0]: https://github.com/rkoeninger/ShenSharp/compare/v0.2.0.0...v0.3.0.0
[0.2.0.0]: https://github.com/rkoeninger/ShenSharp/compare/v0.1.0.0...v0.2.0.0
[0.1.0.0]: https://github.com/rkoeninger/ShenSharp/compare/cf371a7bd5829d6c1a39ac1b07782518e60e6d40...v0.1.0.0