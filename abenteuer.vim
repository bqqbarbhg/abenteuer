if exists("b:current_syntax")
	finish
endif

syn keyword abKeyword table nextgroup=abTable skipwhite
syn keyword abKeyword entity external define
syn region abBlock start="{" end="}" fold transparent
syn match abComment "#.*$"
syn match abString '"\(\\[\\"nrt]\|[^"\\]\)*"'
syn match abNumber '\d\+'

syn match abTable '\w\+' contained
syn match abLineStart '^\s*\w\+' contains=abTable,abKeyword

let b:current_syntax = "abenteuer"

hi def link abKeyword     Statement
hi def link abComment     Comment
hi def link abBlock       Statement
hi def link abString      Constant
hi def link abNumber      Constant
hi def link abTable       Type

