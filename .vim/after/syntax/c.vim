syn match    cCustomParen    "?=(" contains=cParen,cCppParen
syn match    cCustomFunc     "\<\h\+\w*\s*(\@=" contains=cCustomParen
syn match    cCustomScope    "::"
syn match    cCustomClass    "\<\h\+\w*\s*::" contains=cCustomScope

hi def link cCustomFunc  Function
hi def link cCustomClass Function
