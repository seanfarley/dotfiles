#use xelatex instead of pdflatex
$pdflatex = 'xelatex --shell-escape -interaction=nonstopmode -file-line-error -synctex=1';

#create a PDF by default
$pdf_mode = 1;

#no error messages, and no stopping for errors
$pdflatex_silent_switch = "--interaction=batchmode";
$biber_silent_switch = "--onlylog";
$bibtex_silent_switch = "-quiet";

#use Skim as PDF previewer 
$pdf_previewer = "open -a /Applications/Skim.app"; 

$clean_ext = "paux lox pdfsync out tdo";
