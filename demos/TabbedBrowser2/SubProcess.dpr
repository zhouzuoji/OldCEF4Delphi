program SubProcess;

{$I cef.inc}

uses
  {$IFDEF DELPHI16_UP}
  WinApi.Windows,
  {$ELSE}
  Windows,
  {$ENDIF}
  uCEFApplication,
  uCEFView;

// CEF3 needs to set the LARGEADDRESSAWARE flag which allows 32-bit processes to use up to 3GB of RAM.
{$SetPEFlags IMAGE_FILE_LARGE_ADDRESS_AWARE}

begin
  CreateGlobalCEFApp;

  // The main process and the subprocess *MUST* have the same FrameworkDirPath, ResourcesDirPath,
  // LocalesDirPath, cache, cookies and UserDataPath paths

  // The demos are compiled into the BIN directory. Make sure SubProcess.exe and SimpleBrowser.exe are in that
  // directory or this demo won't work.

  // In case you want to use custom directories for the CEF3 binaries, cache, cookies and user data.
{
  GlobalCEFApp.FrameworkDirPath     := 'cef';
  GlobalCEFApp.ResourcesDirPath     := 'cef';
  GlobalCEFApp.LocalesDirPath       := 'cef\locales';
  GlobalCEFApp.cache                := 'cef\cache';
  GlobalCEFApp.cookies              := 'cef\cookies';
  GlobalCEFApp.UserDataPath         := 'cef\User Data';
}

  GlobalCEFApp.StartSubProcess;
  GlobalCEFApp.Free;
end.

