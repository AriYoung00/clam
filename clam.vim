" Vim syntax file
" Language:    MyScript (or your language name)
" Maintainer:  Your Name <your.email@example.com>
" Updated:     YYYY MM DD

if exists("b:clam")
  finish
endif

" Keywords
syn keyword myscriptKeyword fn let if else for while in break struct return impl nextgroup=myscriptComment skipwhite skipempty
" Types
syn keyword myscriptType bool int float string cmd
" Boolean literals
syn keyword myscriptBoolean true false
" Identifiers
syn match myscriptIdentifier "\w\+"
" Special symbols
syn match myscriptSymbol "->\|=>\|?\|.\{-}"

" Comments
syn match myscriptComment "//.*" contains=ALLBUT,myscriptKeyword,myscriptType,myscriptBoolean,myscriptIdentifier,myscriptSymbol,myscriptNumber,@Spell

" C-like string syntax
syn region myscriptString start=/"/ end=/"/

" Load Bash syntax for code blocks
syn include @Bash syntax/sh.vim

" Define a region for Bash code blocks
syn region bashCodeBlock start=/```/ end=/```/ contains=@Bash
syn region bashCodeSnip start=/`/ end=/`/ " contains=@Bash

" Integral literals
syn match myscriptNumber "\d\+\|\d\+\.\d\*"

" Linking
hi def link myscriptKeyword Keyword
hi def link myscriptType Type
hi def link myscriptBoolean Boolean
hi def link myscriptIdentifier Identifier
hi def link myscriptSymbol SpecialChar
hi def link myscriptString String
hi def link bashCodeSnip String
hi def link myscriptNumber Number
hi def link myscriptComment Comment

let b:current_syntax = "clam"
