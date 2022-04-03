## MMark Ext 0.2.1.5

* The test suite now passes with `modern-uri-0.3.4.4`.

## MMark Ext 0.2.1.4

* Prevent the test suite from failing ever again. [Issue
  25](https://github.com/mmark-md/mmark-ext/issues/25).

## MMark Ext 0.2.1.3

* The test suite passes with `modern-uri-0.3.4` and later.

* Dropped support for GHC 8.6 and older.

## MMark Ext 0.2.1.2

* Made the tests pass with `skylighting-0.7.6`.

## MMark Ext 0.2.1.1

* Made the tests pass with `skylighting-0.7.4`.

## MMark Ext 0.2.1.0

* Added the module `Text.MMark.Extension.GhcSyntaxHighlighter` with
  `ghcSyntaxHighlighter` in it. This only works with GHC 8.4.1 and newer,
  with older GHCs the extension just won't have any effect.

## MMark Ext 0.2.0.0

* The `skylighting` extension no longer accepts any arguments. Skylighting
  built-in facilities for rendering to HTML are not acceptable as they
  produce invalid HTML (with duplicate ids), so they were re-implemented.

## MMark Ext 0.1.1.0

* Added the `Text.MMark.Extension.MathJax` module.

* Added the `Text.MMark.Extension.Footnotes` module.

## MMark Ext 0.1.0.0

* This version has little in common with previous versions.

## MMark Ext 0.0.1.2

* Compiles with `modern-uri-0.2.0.0` and later.

## MMark Ext 0.0.1.1

* Fixed test suite failure with newer MMark versions. This version is to be
  used with `mmark-0.0.2.0` and later.

## MMark Ext 0.0.1.0

* Initial release.
