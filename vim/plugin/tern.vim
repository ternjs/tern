if exists('g:loaded_tern')
  finish
endif
let g:loaded_tern = 1

let s:save_cpo = &cpo
set cpo&vim

autocmd FileType javascript :call tern#Enable()

let &cpo = s:save_cpo
unlet s:save_cpo
