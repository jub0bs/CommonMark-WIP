# commonmark

`commonmark` is a parsing and rendering library for CommonMark.
CommonMark is a standard, unambiguous syntax specification for Markdown.
See http://commonmark.org for more details.

# Remarks

- This project was made possible by Google Summer of Code 2015. Thanks!

- `commonmark` is in debt to John MacFarlane's Cheapskate project.
  In many ways, the former is a rewrite of the latter, but it is powered by
  `attoparsec` (instead of a custom combinator parsing library) and is
  intended to, eventually, become fully compliant with the CommonMark Spec.

- The library is still in development. I intend to complete it in my spare
  time over the next few weeks. At this stage of development, `commonmark`
  can only parse a subset of the CommonMark syntax.

- The current export list of the CommonMark module is tentative; the module
currently exports some entities only for testing purposes.

# Usage (tentative)

Parsers for CommonMark syntactic elements can be tested at the command line:

```haskell
λ> commonmarkTest hRule "---rule?---"
No parse: horizontal rule: endOfInput

λ> commonmarkTest hRule "   ------"
Parsed: Rule

λ> commonmarkTest hRule "    ------"
No parse: horizontal rule: Failed reading: empty
```
