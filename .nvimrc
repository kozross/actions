" Enable hlint and GHC via Cabal
let g:ale_linters = {'haskell': ['hlint', 'cabal-build']}
" ... only
let g:ale_linters_explicit = 1
" Don't lint until I save
let g:ale_lint_on_text_changed = 'never'
let g:ale_lint_on_insert_leave = 0
let g:ale_lint_on_enter = 0

call ale#Set('haskell_cabal_build_options', 'all --disable-optimization --enable-benchmarks --enable-tests')

function! GetCabalCommand(buffer) abort
  let l:flags = ale#Var(a:buffer, 'haskell_cabal_build_options')
  return 'cabal new-build ' . l:flags
endfunction

call ale#linter#Define('haskell', {
      \ 'name': 'cabal_build',
      \ 'aliases': ['cabal-build'],
      \ 'output_stream': 'stderr',
      \ 'executable': 'cabal',
      \ 'command': function('GetCabalCommand'),
      \ 'callback': 'ale#handlers#haskell#HandleGHCFormat',
      \})

" Configure Neoformat to use cabal-fmt for Cabal files
let g:neoformat_cabal_cabalfmt = { 'exe': 'cabal-fmt', 'args': [] }
let g:neoformat_enabled_cabal = ['cabalfmt']

" Configure Neoformat to use ormolu for Haskell
let g:neoformat_haskell_ormolu = { 'exe': 'ormolu', 'args': [] }
let g:neoformat_enabled_haskell = ['ormolu']

" Enable automagic autoformatting
augroup fmt
  autocmd!
  autocmd BufWritePre * undojoin | Neoformat
augroup end
