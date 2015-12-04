  ;res=dialog_message('Select input text data file',/center,/information)
  file='min.txt';dialog_pickfile(/read,filter='*.txt',/fix_filter)
  nlines=file_lines(file)
  x=intarr(2)
  j=dblarr(2)
  jp=dblarr(2)
  openr,1,file
  while ~eof(1) do begin
     readf,1,xt,jt,jpt
     append(x,xt)
     append(j,jt)
     append(jp,jpt)
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
     plot,x,j,color=0,title=tit,xtitle=xtit,ytitle=ytit
     oplot,x,jp
     init=1
  endif
  init++
  res=dialog_message('Do you want to save current graphics?',/center,/question)
  if res eq 'Yes' then write_png,file_basename(file,'.txt')+'.png',tvrd(/true),transparent=255
end
