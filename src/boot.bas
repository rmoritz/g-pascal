   0 rem basic loader for reu-enhanced g-pascal
  10 s=1:a=a+s:d=8:n=3:data "gpreu","gpascal",""
  20 for i=s to a:read f$:next
  30 if a=s then print chr$(147)"loading, please wait..."
  40 if a=n then sys 32768:rem start program
  50 print a"/"n-s,f$
  60 load f$,d,s:rem load a part
