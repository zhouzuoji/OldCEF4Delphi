unit uMainForm;

{$I cef.inc}

interface

uses
  {$IFDEF DELPHI16_UP}
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.Buttons, Vcl.ExtCtrls, Vcl.StdCtrls,
  {$ELSE}
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, ComCtrls, Buttons, ExtCtrls, StdCtrls,
  {$ENDIF}
  uCEFChromium, uCEFWindowParent, uCEFInterfaces, uCEFApplication, uCEFTypes, uCEFConstants,
  uCEFChromiumWindow, uCEFView;

const
  CM_NEW_TAB = WM_APP + 100;

type
  TMainForm = class(TCefMainFormBase)
    PageControl1: TPageControl;
    ButtonPnl: TPanel;
    NavButtonPnl: TPanel;
    BackBtn: TButton;
    ForwardBtn: TButton;
    ReloadBtn: TButton;
    StopBtn: TButton;
    ConfigPnl: TPanel;
    GoBtn: TButton;
    URLEditPnl: TPanel;
    URLCbx: TComboBox;
    AddTabBtn: TButton;
    RemoveTabBtn: TButton;
    pnl1: TPanel;
    mmoScript: TMemo;
    btn1: TSpeedButton;
    procedure AddTabBtnClick(Sender: TObject);
    procedure RemoveTabBtnClick(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
    procedure BackBtnClick(Sender: TObject);
    procedure ForwardBtnClick(Sender: TObject);
    procedure ReloadBtnClick(Sender: TObject);
    procedure StopBtnClick(Sender: TObject);
    procedure GoBtnClick(Sender: TObject);
    procedure btn1Click(Sender: TObject);
  protected
    procedure CMNewTab(var msgr: TMessage); message CM_NEW_TAB;
    procedure CreateNewTab(const url: string);
    function GetCurrentView: TCEFView;
    function IsCurrentChromium(aChromium: TObject): Boolean;
    function TabOfChromium(aChromium: TObject): TTabSheet;
    procedure OnCefLibInitialized; override;
    procedure OnCEFViewPrepared(Sender: TObject);
    procedure OnCEFViewClosed(Sender: TObject);
    procedure OnBeforeContextMenu(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame;
      const params: ICefContextMenuParams; const model: ICefMenuModel);
    procedure OnContextMenuCommand(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame;
      const params: ICefContextMenuParams; commandId: Integer; eventFlags: TCefEventFlags;
      out Result: Boolean);
    procedure Chromium_OnAddressChange(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; const url: ustring);
    procedure Chromium_OnTitleChange(Sender: TObject; const browser: ICefBrowser; const title: ustring);
    procedure Chromium_OnBeforePopup(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame;
      const targetUrl, targetFrameName: ustring; targetDisposition: TCefWindowOpenDisposition;
      userGesture: Boolean; const popupFeatures: TCefPopupFeatures;
      var windowInfo: TCefWindowInfo; var client: ICefClient; var settings: TCefBrowserSettings;
      var noJavascriptAccess: Boolean; var Result: Boolean);

  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

uses
  uPopupForm;

// This is just a simplified demo with tab handling.
// It's not meant to be a complete browser or the best way to implement a tabbed browser.

// In this demo all browsers share the buttons and URL combobox.
// All TChromium components share the same functions for their events sending the
// PageIndex of the Tab where they are included in the Message.lParam parameter if necessary.

// For simplicity the Button panel and the PageControl are disabled while adding or removing tab sheets.
// The Form can't be closed if it's destroying a tab.

// This is the destruction sequence when a user closes a tab sheet:
// 1. RemoveTabBtnClick calls TChromium.CloseBrowser of the selected tab which triggers a TChromium.OnClose event.
// 2. TChromium.OnClose sends a CEFBROWSER_DESTROYWNDPARENT message to destroy TCEFWindowParent in the main thread which triggers a TChromium.OnBeforeClose event.
// 3. TChromium.OnBeforeClose sends a CEFBROWSER_DESTROYTAB message to destroy the tab in the main thread.

// This is the destruction sequence when the user closes the main form
// 1. FormCloseQuery hides the form and calls CloseAllBrowsers which calls TChromium.CloseBrowser in all tabs and triggers the TChromium.OnClose event.
// 2. TChromium.OnClose sends a CEFBROWSER_DESTROYWNDPARENT message to destroy TCEFWindowParent in the main thread which triggers a TChromium.OnBeforeClose event.
// 3. TChromium.OnBeforeClose sends a CEFBROWSER_CHECKTAGGEDTABS message to set the TAG property to 1 in the TabSheet containing the TChromium. Then sends WM_CLOSE in case all tabsheets have a TAG = 1.

procedure TMainForm.AddTabBtnClick(Sender: TObject);
begin
  CreateNewTab('chrome://version/')
end;

procedure TMainForm.RemoveTabBtnClick(Sender: TObject);
var
  v : TCEFView;
begin
  v := GetCurrentView;
  if v <> nil then
    v.CloseBrowser();
end;

procedure TMainForm.ForwardBtnClick(Sender: TObject);
var
  v : TCEFView;
begin
  v := GetCurrentView;
  if v <> nil then
    v.Chromium.GoForward;
end;

procedure TMainForm.GoBtnClick(Sender: TObject);
var
  v : TCEFView;
begin
  v := GetCurrentView;
  if v <> nil then
    v.Chromium.LoadURL(URLCbx.Text);
end;

function TMainForm.IsCurrentChromium(aChromium: TObject): Boolean;
begin
  Result := (PageControl1.ActivePage<>nil)
    and (TCEFView(PageControl1.ActivePage.Controls[0]).Chromium=aChromium)
  
end;

procedure TMainForm.ReloadBtnClick(Sender: TObject);
var
  v : TCEFView;
begin
  v := GetCurrentView;
  if v <> nil then
    v.Chromium.Reload;
end;

procedure TMainForm.BackBtnClick(Sender: TObject);
var
  v : TCEFView;
begin
  v := GetCurrentView;
  if v <> nil then
    v.Chromium.GoBack;
end;

procedure TMainForm.StopBtnClick(Sender: TObject);
var
  v : TCEFView;
begin
  v := GetCurrentView;
  if v <> nil then
    v.Chromium.StopLoad;
end;

function TMainForm.TabOfChromium(aChromium: TObject): TTabSheet;
var
  i: Integer;
begin
  for i := 0 to PageControl1.PageCount - 1 do
    if TCEFView(PageControl1.Pages[i].Controls[0]).Chromium=aChromium then
    begin
      Result := PageControl1.Pages[i];
      Exit;
    end;
  Result := nil;
end;

procedure TMainForm.btn1Click(Sender: TObject);
var
  v : TCEFView;
begin
  v := GetCurrentView;
  if v <> nil then
    v.Chromium.Browser.MainFrame.ExecuteJavaScript(mmoScript.Text, '', 0);
end;

procedure TMainForm.Chromium_OnAddressChange(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; const url: ustring);
begin
  if IsCurrentChromium(Sender) then
    URLCbx.Text := url;
end;

function TMainForm.GetCurrentView: TCEFView;
begin
  if PageControl1.ActivePageIndex>=0 then
    Result := TCEFView(PageControl1.ActivePage.Controls[0])
  else
    Result := nil;
end;

procedure TMainForm.Chromium_OnTitleChange(Sender: TObject; const browser: ICefBrowser; const title: ustring);
var
  tab : TTabSheet;
begin
  tab := TabOfChromium(Sender);
  if tab <> nil then
    tab.Caption := title;
end;

procedure TMainForm.CMNewTab(var msgr: TMessage);
begin
  CreateNewTab(PString(msgr.LParam)^);
  Dispose(PString(msgr.LParam));
end;

procedure TMainForm.CreateNewTab(const url: string);
var
  tab: TTabSheet;
  v: TCEFView;
begin
  tab := TTabSheet.Create(PageControl1);
  tab.Caption := 'New Tab';
  tab.PageControl := PageControl1;

  v := TCEFView.Create(tab);
  v.Parent := tab;
  v.Color := clWhite;
  v.Align := alClient;
  v.OnInitialized := OnCEFViewPrepared;
  v.OnFinalized := OnCEFViewClosed;
  v.Chromium.OnAddressChange := Chromium_OnAddressChange;
  v.Chromium.OnTitleChange   := Chromium_OnTitleChange;
  v.Chromium.OnBeforePopup   := Chromium_OnBeforePopup;
  v.Chromium.OnBeforeContextMenu := OnBeforeContextMenu;
  v.Chromium.OnContextMenuCommand := OnContextMenuCommand;
  v.CreateBrowser(url);
end;

procedure TMainForm.Chromium_OnBeforePopup(Sender: TObject;
  const browser: ICefBrowser; const frame: ICefFrame; const targetUrl,
  targetFrameName: ustring; targetDisposition: TCefWindowOpenDisposition;
  userGesture: Boolean; const popupFeatures: TCefPopupFeatures;
  var windowInfo: TCefWindowInfo; var client: ICefClient;
  var settings: TCefBrowserSettings; var noJavascriptAccess: Boolean;
  var Result: Boolean);
var
  s: PString;
begin
  OutputDebugString(PWideChar(IntToStr(MainThreadID) + '/' + IntToStr(GetCurrentThreadId) + '  ' + targetFrameName + ' - ' + targetUrl));
  // For simplicity, this demo blocks all popup windows and new tabs
  Result := (targetDisposition in [WOD_NEW_FOREGROUND_TAB, WOD_NEW_BACKGROUND_TAB, WOD_NEW_POPUP, WOD_NEW_WINDOW]);
  if Result and (targetUrl<>'') then
  begin
    New(s);
    s^ := targetUrl;
    PostMessage(Self.Handle, CM_NEW_TAB, 0, LPARAM(s));
  end;
end;

procedure TMainForm.OnBeforeContextMenu(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame;
  const params: ICefContextMenuParams; const model: ICefMenuModel);
begin
  if params.LinkUrl <> '' then
    model.AddItem(MENU_ID_USER_FIRST+1, 'Open In New Window');
end;

procedure TMainForm.OnCefLibInitialized;
begin
  inherited;
  if not(ButtonPnl.Enabled) then
    begin
      ButtonPnl.Enabled := True;
      Caption           := 'Tab Browser';
      cursor            := crDefault;
      if (PageControl1.PageCount = 0) then AddTabBtn.Click;
    end;
end;

procedure TMainForm.OnCEFViewClosed(Sender: TObject);
begin
  TCEFView(Sender).Parent.Free;
end;

procedure TMainForm.OnCEFViewPrepared(Sender: TObject);
begin

end;

procedure TMainForm.OnContextMenuCommand(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame;
  const params: ICefContextMenuParams; commandId: Integer; eventFlags: TCefEventFlags; out Result: Boolean);
begin
  OutputDebugString(PWideChar(IntToStr(MainThreadID) + '/' + IntToStr(GetCurrentThreadId)));
  if commandId = MENU_ID_USER_FIRST + 1 then
    Popup(params.LinkUrl);
end;

procedure TMainForm.PageControl1Change(Sender: TObject);
var
  TempChromium : TChromium;
begin
  if PageControl1.ActivePage<>nil then
  begin
    TempChromium := TCEFView(PageControl1.ActivePage.Controls[0]).Chromium;
    if (TempChromium<>nil) then
      URLCbx.Text := TempChromium.DocumentURL;
  end;
end;

end.
