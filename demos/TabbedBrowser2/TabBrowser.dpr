program TabBrowser;

{$I cef.inc}

uses
  Forms,
  Windows,
  uCEFView,
  uCEFApplication,
  uMainForm in 'uMainForm.pas' {MainForm},
  uPopupForm in 'uPopupForm.pas' {FrmPopupForm};

{$R *.res}

{$SetPEFlags IMAGE_FILE_LARGE_ADDRESS_AWARE}



begin
  // GlobalCEFApp creation and initialization moved to a different unit to fix the memory leak described in the bug #89
  // https://github.com/salvadordf/CEF4Delphi/issues/89
  CreateGlobalCEFApp;
  GlobalCEFApp.SingleProcess := False;
  GlobalCEFApp.BrowserSubprocessPath := 'SubProcess.exe';
  if GlobalCEFApp.StartMainProcess then
  begin
    Application.Initialize;
    Application.MainFormOnTaskbar := True;
    Application.CreateForm(TMainForm, MainForm);
    Application.Run;
  end;

  DestroyGlobalCEFApp;
end.
