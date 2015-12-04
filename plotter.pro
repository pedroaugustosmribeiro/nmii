;pro plotter
;this routine is for general 1D plotting
;the input file shall start with an integer number indicating number
;of data point, followed by double precision float-point data
;firt the x-axis,imeddiatelly followed by y-axis
;it allows over-plotting and save to png file
  a=0
  tmp=.0
  res=dialog_message('Select input text data file',/center,/information)
  file=dialog_pickfile(/read,filter='*.txt',/fix_filter)
  openr,1,file
  readf,1,a
  b=dblarr(2*a)
  i=0
  while ~eof(1) do begin
     readf,1,tmp
     b(i)=tmp
     ;print,b(i)
     i++
  endwhile
  close,1
  device,set_font='Helvetica',/tt_font,decomposed=0
  loadct,39 ;loads rainbow+white color table
  !p.font=1
  !p.background=255
  !p.charsize=2
  !p.charthick=1.5
  !p.thick=2
  if n_elements(init) eq 0 then begin
     xtit=''
     ytit=''
     tit=''
     read,xtit,prompt='Enter x-axis title: '
     read,ytit,prompt='Enter y-axis title: '
     read,tit,prompt='Enter graphics title: '
     plot,b[0:a-1],b[a:*],color=0,title=tit,xtitle=xtit,ytitle=ytit
     init=1
  endif else oplot,b[0:a-1],b[a:*],color=init*10
  init++
  res=dialog_message('Do you want to save current graphics?',/center,/question)
  if res eq 'Yes' then write_png,file_basename(file,'.txt')+'.png',tvrd(/true),transparent=255
end
