@if (@X)==(@Y) @end /* JScript comment
    @echo off

    cscript //E:JScript //nologo "%~f0" %*

    exit /b %errorlevel%

@if (@X)==(@Y) @end JScript comment */

var source=WScript.Arguments.Item(0);
var wb = new ActiveXObject("Excel.Application");
var file = wb.workbooks.Open(source);
file.Save();
file.Close();
wb.Quit();
