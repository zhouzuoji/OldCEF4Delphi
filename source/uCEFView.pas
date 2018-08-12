unit uCEFView;

{$IFNDEF CPUX64}
  {$ALIGN ON}
  {$MINENUMSIZE 4}
{$ENDIF}

{$I cef.inc}

interface

uses
  {$IFDEF DELPHI16_UP}
  {$IFDEF MSWINDOWS}WinApi.Windows, WinApi.Messages, Vcl.Controls, Vcl.Graphics, Vcl.Forms{$ENDIF}
  System.SysUtils, System.Classes,
  {$ELSE}
  SysUtils, Classes, Windows, Messages, Controls, Graphics, Forms,
  {$ENDIF}
  Generics.Collections, Generics.Defaults,
  uCEFMiscFunctions, uCEFChromium, uCEFWindowParent, uCEFInterfaces, uCEFApplication, uCEFTypes, uCEFConstants;

const
  CM_CHROMIUM_NOTIFY = WM_APP + $100;
  CM_CEF_INITIALIZED = WM_APP + $101;

type
  TChromiumNotify = (chroimumInitialized, chroimumBeforeClosed, chroimumClosed);
  TChromiumNotifyId = WPARAM;
  TCMChromiumNotify = packed record
    Msg: Cardinal;
    notifyId: TChromiumNotifyId;
    unused: LPARAM;
    Result: LRESULT;
  end;

  TCefFormBase = class;

  TCEFView = class(TWinControl)
  private
    FStartPage: string;
    FIsClosing: Boolean;
    FBrowserId: Integer;
    FChromium: TChromium;
    FWindowParent: TCEFWindowParent;
    FOnInitialized: TNotifyEvent;
    FOnFinalized: TNotifyEvent;
    function GetForm: TCefFormBase;
    procedure checkBrowser(const eventName: string; Sender: TObject; const browser: ICefBrowser);
    procedure onChromiumClosed(Sender: TObject; const browser: ICefBrowser);
    procedure onChromiumBeforeClose(Sender: TObject; const browser: ICefBrowser; out Result: Boolean);
    procedure onChromiumAfterCreated(Sender: TObject; const browser: ICefBrowser);
    procedure CMChromiumNotify(var msgr : TCMChromiumNotify); message CM_CHROMIUM_NOTIFY;
    function GetInitialized: Boolean;
    function GetChromium: TChromium;
  protected
    procedure DoOnInit;
    procedure DoOnFinal;
  public
    //constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CreateBrowser(const url: string = '');
    procedure CloseBrowser(ForceClose: Boolean=True);
    property Chromium: TChromium read GetChromium;
    property Initialized: Boolean read GetInitialized;
  published
    property  Align;
    property  Anchors;
    property  Color;
    property  Constraints;
    property  TabStop;
    property  TabOrder;
    property  Visible;
    property  Enabled;
    property  ShowHint;
    property  Hint;
    property  DoubleBuffered;
    {$IFDEF DELPHI12_UP}
    property  ParentDoubleBuffered;
    {$ENDIF}
    property OnFinalized: TNotifyEvent read FOnFinalized write FOnFinalized;
    property OnInitialized: TNotifyEvent read FOnInitialized write FOnInitialized;
  end;

  EMultiMainFormException = class(Exception);

  TCefFormBase = class(TForm)
  private
    FIsNotFirstShow: Boolean;
    FInAsyncCleanup: Boolean;
    FCEFViewList: TList<TCEFView>;
    function IsMainForm: Boolean;
    function HasActiveChromium: Boolean;
    function CanClose: Boolean;
    procedure CloseChromiums;
    procedure AsyncCleanup;
    procedure CMCefInitialized(var msgr : TMessage); message CM_CEF_INITIALIZED;
  private
    function forceCEFViewList: TList<TCEFView>;
    procedure NotifyMoveOrResizeStarted;
    procedure WMMove(var msgr : TWMMove); message WM_MOVE;
    procedure WMMoving(var msgr : TMessage); message WM_MOVING;
    procedure WMEnterMenuLoop(var msgr: TWMEnterMenuLoop); message WM_ENTERMENULOOP;
    procedure WMExitMenuLoop(var msgr: TWMExitMenuLoop); message WM_EXITMENULOOP;
    function GetChromiumCount: Integer;
  protected
    procedure DoShow; override;
    procedure DoClose(var Action: TCloseAction); override;
    procedure OnCefLibInitialized; virtual;
    procedure OnChromiumCreated(view: TCEFView);
    procedure OnChromiumClosed(view: TCEFView);
  public
    destructor Destroy; override;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    property ChromiumCount: Integer read GetChromiumCount;
  end;

  TCefMainFormBase = class(TCefFormBase)
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  end;

procedure CreateGlobalCEFApp;

implementation

var
  CefMainForm: TCefMainFormBase;
  CefForms: TList<TCefFormBase>;
  TotalChromiumCount: Integer;

procedure OutputDebugMessage(const aMessage : string);
begin
  OutputDebugString(PChar(aMessage));
end;

function GetEnvVar(const name: string): string;
var
  valueLen: Integer;
begin
  valueLen := Windows.GetEnvironmentVariable(PChar(name), nil, 0);
  if valueLen = 0 then
    Result := ''
  else
  begin
    SetLength(Result, valueLen - 1);
    Windows.GetEnvironmentVariable(PChar(name), PChar(Result), valueLen);
  end;
end;

function SetEnvVar(const name, value: string): Boolean;
begin
  Result := Windows.SetEnvironmentVariable(PChar(name), PChar(value));
end;

function EnvPathAdd(const dir: string): Boolean;
var
  strs: TStringList;
  pathList, dir2: string;
begin
  if dir = '' then
    Result := False
  else
  begin
    if dir[length(dir)] = '\' then
      dir2 := Copy(dir, 1, length(dir) - 1)
    else
      dir2 := dir + '\';

    strs := TStringList.Create;
    try
      strs.CaseSensitive := False;
      strs.delimiter := ';';
      strs.StrictDelimiter := True;
      pathList := GetEnvVar('PATH');
      strs.DelimitedText := pathList;
      if (strs.IndexOf(dir) = -1) and (strs.IndexOf(dir2) = -1) then
        Result := SetEnvVar('PATH', dir + ';' + pathList)
      else
        Result := True;
    finally
      strs.Free;
    end;
  end;
end;

procedure FindCefLibDir;
var
  exeDir, filter, missingFiles: string;
begin
  exeDir := ExtractFilePath(ParamStr(0));
  if FileExists(exeDir + LIBCEF_DLL) then Exit;

  filter := exeDir + IntToStr(CEF_SUPPORTED_VERSION_MAJOR) + '.' + IntToStr(CEF_SUPPORTED_VERSION_MINOR)
    + '.' + IntToStr(CEF_SUPPORTED_VERSION_RELEASE) + '\';

  if DirectoryExists(filter) then
  begin
    if not CheckDLLs(filter, missingFiles) or not CheckLocales(filter + 'locales', missingFiles, GlobalCEFApp.LocalesRequired)
      or not CheckResources(filter, missingFiles, False) then
    begin
      if (SizeOf(Pointer)=4) and (DirectoryExists(filter + 'Win32'))  then
        filter := filter + 'Win32\'
      else if (SizeOf(Pointer)=8) and (DirectoryExists(filter + 'Win64'))  then
        filter := filter + 'Win64\';
      if not CheckDLLs(filter, missingFiles) or not CheckLocales(filter + 'locales', missingFiles, GlobalCEFApp.LocalesRequired)
        or not CheckResources(filter, missingFiles, False) then Exit;
    end;

    EnvPathAdd(filter);
    GlobalCEFApp.FrameworkDirPath := filter;
    GlobalCEFApp.ResourcesDirPath := filter;
    GlobalCEFApp.LocalesDirPath := filter + 'locales';
  end;
end;

procedure GlobalCEFApp_OnContextInitialized;
var
  cefrm: TCefFormBase;
  i: Integer;
begin
  if CefForms <> nil then
  begin
    for i := 0 to CefForms.Count - 1 do
    begin
      cefrm := CefForms[i];
      if (cefrm <> nil) and cefrm.HandleAllocated then
        PostMessage(cefrm.Handle, CM_CEF_INITIALIZED, 0, 0);
    end;
  end;
end;

procedure CreateGlobalCEFApp;
begin
  GlobalCEFApp := TCefApplication.Create;
  GlobalCEFApp.CheckCEFFiles := False;
  GlobalCEFApp.LocalesRequired := 'en-US,zh-CN,zh-TW';
  FindCefLibDir;
  GlobalCEFApp.OnContextInitialized := GlobalCEFApp_OnContextInitialized;
end;

{ TCEFView }

const
  CHROMIUM_NOTIFY_INIT = 0;
  CHROMIUM_NOTIFY_BEFORE_CLOSE = 1;
  CHROMIUM_NOTIFY_CLOSE = 2;

procedure TCEFView.checkBrowser(const eventName: string; Sender: TObject; const browser: ICefBrowser);
var
  x: Integer;
begin
  if FChromium.Browser<>nil then
    x := FChromium.Browser.Identifier
  else
    x := 0;
  OutputDebugMessage(Format('%s Sender(%p, %p, %d): Receiver(%p, %p, %d, %d)',
    [eventName, Pointer(Sender),Pointer(browser), browser.Identifier,
    Pointer(FChromium), Pointer(FChromium.Browser), x, FChromium.BrowserId]));
end;

procedure TCEFView.CloseBrowser(ForceClose: Boolean);
begin
  if FIsClosing or (FWindowParent=nil) or (FChromium=nil) then Exit;
  FIsClosing := True;
  FChromium.CloseBrowser(ForceClose);
end;

procedure TCEFView.CMChromiumNotify(var msgr: TCMChromiumNotify);
begin
  case msgr.notifyId of
    CHROMIUM_NOTIFY_INIT:
      begin
        GetForm.OnChromiumCreated(Self);
        if (FStartPage<>'') then
          Self.Chromium.LoadURL(FStartPage);
        FStartPage := '';
        DoOnInit;
      end;
    CHROMIUM_NOTIFY_BEFORE_CLOSE:
      begin
        FreeAndNil(FWindowParent);
      end;
    CHROMIUM_NOTIFY_CLOSE:
      begin
        FreeAndNil(FChromium);
        FIsClosing := False;
        GetForm.OnChromiumClosed(Self);
        DoOnFinal;
      end;
  end;
end;

procedure TCEFView.CreateBrowser(const url: string);
begin
  if FWindowParent<>nil then Exit;
  
  FWindowParent := TCEFWindowParent.Create(Self);
  FWindowParent.Parent := Self;
  FWindowParent.Color := clWhite;
  FWindowParent.Align := alClient;
  GetChromium.CreateBrowser(FWindowParent, '');
  FStartPage := url;
end;

destructor TCEFView.Destroy;
begin
  FWindowParent.Free;
  FChromium.Free;
  inherited;
end;

procedure TCEFView.DoOnFinal;
begin
  if Assigned(FOnFinalized) then
    FOnFinalized(Self);
end;

procedure TCEFView.DoOnInit;
begin
  if Assigned(FOnInitialized) then
    FOnInitialized(Self);
end;

function TCEFView.GetChromium: TChromium;
begin
  if FChromium=nil then
  begin
    FChromium := TChromium.Create(Self);
    FChromium.OnClose := onChromiumBeforeClose;
    FChromium.OnBeforeClose := onChromiumClosed;
    FChromium.OnAfterCreated := onChromiumAfterCreated;
  end;
  Result := FChromium;
end;

function TCEFView.GetForm: TCefFormBase;
var
  c: TControl;
begin
  c := Self;
  while (c<>nil) and not (c is TCustomForm) do
    c := c.Parent;
  if (c<>nil) and not (c is TCefFormBase) then
    c := nil;
  Result := TCefFormBase(c);
end;

function TCEFView.GetInitialized: Boolean;
begin
  Result := (FChromium<>nil) and (FWindowParent<>nil) and not FIsClosing;
end;

procedure TCEFView.onChromiumAfterCreated(Sender: TObject; const browser: ICefBrowser);
begin
  checkBrowser('onAfterCreated', Sender, browser);
  if FBrowserId=0 then
    FBrowserId := FChromium.BrowserId;
  if FChromium.BrowserId=browser.Identifier then
  begin
    InterlockedIncrement(TotalChromiumCount);
    PostMessage(Self.Handle, CM_CHROMIUM_NOTIFY, CHROMIUM_NOTIFY_INIT, LPARAM(Self));
  end;
end;

procedure TCEFView.onChromiumBeforeClose(Sender: TObject; const browser: ICefBrowser; out Result: Boolean);
begin
  checkBrowser('onBeforeClose', Sender, browser);
  OutputDebugMessage(browser.MainFrame.Url);
  Result := FChromium.BrowserId=browser.Identifier;
  if Result then
    PostMessage(Self.Handle, CM_CHROMIUM_NOTIFY, CHROMIUM_NOTIFY_BEFORE_CLOSE, LPARAM(Self));
  // set Result=True: prevent cef SEND WM_CLOSE to close our top level window
end;

procedure TCEFView.onChromiumClosed(Sender: TObject; const browser: ICefBrowser);
begin
  checkBrowser('onClosed', Sender, browser);
  if FBrowserId=browser.Identifier then
  begin
    FBrowserId := 0;
    InterlockedDecrement(TotalChromiumCount);
    PostMessage(Self.Handle, CM_CHROMIUM_NOTIFY, CHROMIUM_NOTIFY_CLOSE, LPARAM(Self));
  end;
end;

{ TCefMainFormBase }

procedure TCefMainFormBase.AfterConstruction;
begin
  if CefMainForm<>nil then
    raise EMultiMainFormException.Create('multiple main form not allowed.');
  CefMainForm := Self;
  inherited;
end;

procedure TCefMainFormBase.BeforeDestruction;
begin
  inherited;
  CefMainForm := nil;
end;

{ TCefFormBase }

procedure TCefFormBase.AfterConstruction;
begin
  inherited;
  if CefForms=nil then
    CefForms := TList<TCefFormBase>.Create;
  CefForms.Add(Self);
end;

procedure TCefFormBase.AsyncCleanup;
begin
  Self.FInAsyncCleanup := True;
  CloseChromiums;
end;

procedure TCefFormBase.BeforeDestruction;
begin
  CefForms.Remove(Self);
  inherited;
end;

function TCefFormBase.HasActiveChromium: Boolean;
begin
  Result := ChromiumCount > 0;
end;

function TCefFormBase.CanClose: Boolean;
begin
  Result := (IsMainForm and (TotalChromiumCount = 0)) or
    (not IsMainForm and not HasActiveChromium);
end;

procedure TCefFormBase.CloseChromiums;
var
  i: Integer;
begin
  for i := ChromiumCount - 1 downto 0 do
    FCEFViewList[i].CloseBrowser;
end;

procedure TCefFormBase.CMCefInitialized(var msgr: TMessage);
begin
  Self.OnCefLibInitialized;
end;

destructor TCefFormBase.Destroy;
begin
  FCEFViewList.Free;
  inherited;
end;

procedure TCefFormBase.DoClose(var Action: TCloseAction);
var
  NeedCleanupChromiums: Boolean;
  i: Integer;
begin
  if Self.FInAsyncCleanup then
  begin
    if CanClose then
      Action := caFree
    else
      Action := caNone;
    Exit;
  end;

  inherited DoClose(Action);
  if Action = caNone then Exit;

  if Application.MainForm = Self then
    NeedCleanupChromiums := TotalChromiumCount>0
  else
    NeedCleanupChromiums := (Action=caFree) and Self.HasActiveChromium;

  if NeedCleanupChromiums then
  begin
    if Application.MainForm = Self then
    begin
      for i := 0 to CefForms.Count - 1 do
      begin
        CefForms[i].AsyncCleanup;
        CefForms[i].Hide;
      end;
    end
    else begin
      Self.AsyncCleanup;
      Self.Hide;
    end;
    Action := caNone;
  end;
end;

procedure TCefFormBase.DoShow;
begin
  inherited;
  if not FIsNotFirstShow then
  begin
    FIsNotFirstShow := True;
    if (GlobalCEFApp <> nil) and GlobalCEFApp.GlobalContextInitialized then
      Self.OnCefLibInitialized;
  end;
end;

function TCefFormBase.forceCEFViewList: TList<TCEFView>;
begin
  if FCEFViewList=nil then
    FCEFViewList := TList<TCEFView>.Create;
  Result := FCEFViewList;
end;

function TCefFormBase.GetChromiumCount: Integer;
begin
  if FCEFViewList = nil then
    Result := 0
  else
    Result := FCEFViewList.Count;
end;


function TCefFormBase.IsMainForm: Boolean;
begin
  Result := Application.MainForm = Self;
end;

procedure TCefFormBase.NotifyMoveOrResizeStarted;
var
  i: integer;
begin
  if not Showing or FInAsyncCleanup then Exit;
  for i := 0 to ChromiumCount - 1 do
    FCEFViewList[i].Chromium.NotifyMoveOrResizeStarted;
end;

function CountDescendants(container: TWinControl; cls: TWinControlClass): Integer;
var
  i: Integer;
  child: TWinControl;
begin
  Result := 0;
  for i := 0 to container.ControlCount - 1 do
  begin
    child := TWinControl(container.Controls[i]);
    if child is cls then
      Inc(Result)
    else if child is TWinControl then
      Inc(Result, CountDescendants(child, cls));
  end;
end;

procedure GetDescendants(container: TWinControl; cls: TWinControlClass; ctrls: TList<TWinControl>);
var
  i: Integer;
  child: TWinControl;
begin
  for i := 0 to container.ControlCount - 1 do
  begin
    child := TWinControl(container.Controls[i]);
    if child is cls then
      ctrls.Add(child)
    else if child is TWinControl then
      GetDescendants(child, cls, ctrls);
  end;
end;

procedure TCefFormBase.OnCefLibInitialized;
begin

end;

procedure TCefFormBase.OnChromiumClosed(view: TCEFView);
begin
  if Self<>nil then
  begin
    Self.forceCEFViewList.Remove(view);
    if FInAsyncCleanup and CanClose then
    begin
      Self.Release;
      if IsMainForm then
        Application.Terminate;
    end;
  end;
end;

procedure TCefFormBase.OnChromiumCreated(view: TCEFView);
begin
  if Self<>nil then
    Self.forceCEFViewList.Add(view);
end;

procedure TCefFormBase.WMEnterMenuLoop(var msgr: TWMEnterMenuLoop);
begin
  inherited;
  if not FInAsyncCleanup and not msgr.IsTrackPopupMenu and (GlobalCEFApp <> nil) then
    GlobalCEFApp.OsmodalLoop := True;
end;

procedure TCefFormBase.WMExitMenuLoop(var msgr: TWMExitMenuLoop);
begin
  inherited;
  if not FInAsyncCleanup and not msgr.IsTrackPopupMenu and (GlobalCEFApp <> nil) then
    GlobalCEFApp.OsmodalLoop := False;
end;

procedure TCefFormBase.WMMove(var msgr: TWMMove);
begin
  inherited;
  NotifyMoveOrResizeStarted;
end;

procedure TCefFormBase.WMMoving(var msgr: TMessage);
begin
  inherited;
  NotifyMoveOrResizeStarted;
end;

initialization

finalization
  FreeAndNil(CefForms);

end.
