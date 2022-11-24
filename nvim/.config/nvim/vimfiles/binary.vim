" The function Nr2Bin() returns the binary string representation of a number.
func Nr2Bin(nr)
  let n = a:nr
  let r = ""
  while n
    let r = '01'[n % 2] . r
    let n = n / 2
  endwhile
  return r
endfunc

command! Nr2Bin call Nr2Bin()

" TODO convert to lua and make it a plugin
