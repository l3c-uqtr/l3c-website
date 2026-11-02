@echo off
rem Double-cliquer pour mettre le site a jour a partir de CVdata.xlsx et des .bib.
rem Netlify republie le site tout seul quelques secondes apres l'envoi sur GitHub.
chcp 65001 >nul
cd /d "%~dp0"

set "QUARTO=C:\Program Files\Quarto\bin\quarto.exe"
if not exist "%QUARTO%" set "QUARTO=C:\Program Files\RStudio\resources\app\bin\quarto\bin\quarto.exe"
if not exist "%QUARTO%" (
  echo Quarto est introuvable. Installer Quarto ou RStudio.
  goto erreur
)
for /d %%d in ("C:\Program Files\R\R-*") do set "PATH=%PATH%;%%d\bin"

echo.
echo === Compilation du site ===
"%QUARTO%" render
if errorlevel 1 goto erreur

echo.
echo === Envoi sur GitHub ===
git add -A
git diff --cached --quiet
if not errorlevel 1 (
  echo Aucun changement a publier.
  goto fin
)
git commit -m "Mise a jour du site"
if errorlevel 1 goto erreur
git push
if errorlevel 1 goto erreur

echo.
echo Site mis a jour. Il sera en ligne dans moins d'une minute.
goto fin

:erreur
echo.
echo ERREUR : le site n'a pas ete publie. Lire le message ci-dessus.

:fin
echo.
pause
