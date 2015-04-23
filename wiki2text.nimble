[Package]
name          = "wiki2text"
version       = "0.1.0"
author        = "Rob Speer"
description   = "Quickly extracts natural-language text from a MediaWiki XML file."
license       = "MIT"
bin           = "wiki2text"
SkipExt       = "nim"
SkipFiles     = "Makefile"

[Deps]
Requires: "nim >= 0.10.3"
