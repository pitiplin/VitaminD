del /Q ".\PDF\*.txt"
del /Q ".\TEXT\*.*"
for /r %%i in (*.pdf) do ".\XPDF\pdftotext" -layout %%i
del /Q ".\TEXT\*.*"
move /Y ".\PDF\*.txt" ".\TEXT\"